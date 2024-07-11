# Run auction-level part of the processing

# Load definitions, check parameters, source code, and run setup
setup_dir <- file.path(getwd(), "scripts") #change if needed
source(file.path(setup_dir, "01_setup.R"))

# Load packages (for multi-core processing and reporting)
library(future)
library(foreach)
library(doFuture)
library(progressr)

# Split shapefile
message_ts("Splitting shapefile into individual flooding areas")
floodarea_files <- split_flooding_area(axn_file_clean,
                                       field_column_name = "BidFieldID",  #created in 01_setup.R
                                       guide_raster = ref_file,
                                       output_dir = spl_dir,              #defined in definitions.R
                                       do_rasterize = FALSE,
                                       overwrite = overwrite_global)

# Function that runs the auction, using progress handlers and multiple cores as 
# set by 'plan' in the future package
evaluate_auction <- function(flood_areas, overwrite = FALSE, 
                             retry_times = 0, retry_counter = NULL,
                             verbose_level = 1, p = NULL, global_prog_mult = 1) {
  
  # Check/set retry counter
  if (retry_times > 1) stop(add_ts("Only supports one level of retry upon error."))
  if (is.null(retry_counter)) { 
    retry_counter <- 0
  } else {
    retry_counter <- retry_counter + 1
  }
  
  # Count files for progress reporter
  n_fas <- length(unlist(flood_areas))
  n_mths <- length(axn_mths)
  n_lcs <- length(lc_files)
  n_mdls <- length(shorebird_model_files_reallong)
  
  n_rst <- n_fas
  n_imp <- n_fas * n_mths
  n_wxl <- n_imp * n_lcs
  n_fcl <- n_wxl * 2
  n_prd <- n_imp * n_mdls
  n_att <- n_fas
  
  # Start progress reporter
  if (is.null(p)) {
    p <- progressor(steps = n_rst + n_imp + n_wxl + n_fcl + n_prd + n_att, 
                    auto_finish = FALSE)
  }
  
  # Set reporting level
  if (verbose_level > 1) {
    prg_msg <- TRUE
    fxn_msg <- TRUE
  } else if (verbose_level == 0) {
    prg_msg <- FALSE
    fxn_msg <- FALSE
  } else {
    prg_msg <- TRUE
    fxn_msg <- FALSE
  } 
  
  foreach(fa = flood_areas) %dofuture% {
    
    # Set terra memory options
    terraOptions(memfrac = 0.1, memmax = 8)#, steps = 55)
    
    # Labels for messages
    fxn <- "setup"
    lbl <- paste0(fa, " ")
    prg_mult <- 1
    if (prg_msg) p(add_ts(lbl, "started."), class = "sticky", amount = 0)
    
    # Streamlined error catching
    withCallingHandlers({
      
      # Get flood area files for specified fa
      fa_files <- list.files(spl_dir, pattern = ".shp$", full.names = TRUE)
      fa_files <- fa_files[grepl(paste0("((", paste0(fa, collapse = ")|("), "))"), fa_files)]
      if (length(fa_files) == 0) stop("No matching flooding area shapefiles; check that split_flooding_area ran")
      
      # Rasterize and buffer flood areas
      if (prg_msg) p(add_ts(lbl, "rasterizing..."), class = "sticky", amount = 0)
      fxn <- "rasterize"
      fa_rst_files <- rasterize_flooding_area(fa_files,
                                              guide_raster = ref_file,
                                              output_dir = spl_dir,         #defined in definitions.R
                                              buffer_dist = 10000,
                                              overwrite = overwrite,
                                              verbose = fxn_msg)
      
      if (prg_msg) p(add_ts("Rasterized files (", length(fa_rst_files), "): ", 
                            paste0(basename(fa_rst_files), collapse = ", ")), 
                     class = "sticky", amount = 0)
      p(amount = length(fa_rst_files) * global_prog_mult)
      gc()
      
      # Impose flooding
      if (prg_msg) p(add_ts(lbl, "imposing flooding..."), class = "sticky", amount = 0)
      fxn <- "impose-water"
      water_imp_files <- impose_flooding(lt_wtr_files,
                                         fa_rst_files,
                                         output_dir = imp_wtr_dir,
                                         mask = TRUE, #significantly speeds up processing in later steps
                                         overwrite = overwrite,
                                         verbose = fxn_msg)
      
      if (prg_msg) p(add_ts("Imposed files (", length(water_imp_files), "): ", 
                            paste0(basename(water_imp_files), collapse = ", ")), 
                     class = "sticky", amount = 0)
      p(amount = length(water_imp_files) * global_prog_mult)
      gc()
      
      # Overlay water on landcover
      if (prg_msg) p(add_ts(lbl, "overlaying water and landcover..."), class = "sticky", amount = 0)
      fxn <- "overlay-water-landcover"
      wxl_files <- overlay_water_landcover(water_imp_files, 
                                           lc_files,
                                           output_dir = imp_wxl_dir,
                                           overwrite = overwrite,
                                           verbose = fxn_msg)
      
      if (prg_msg) p(add_ts("WxL files (", length(wxl_files), "): ", 
                            paste0(basename(wxl_files), collapse = ", ")), 
                     class = "sticky", amount = 0)
      p(amount = length(wxl_files) * global_prog_mult)
      gc()
      
      # Calculate neighborhood water by landcover
      if (prg_msg) p(add_ts(lbl, "calculating neighborhood water..."), class = "sticky", amount = 0)
      fxn <- "mean-neighborhood-water"
      imp_fcl_files <- mean_neighborhood_water(wxl_files, #previously-created water x landcover files
                                               distances = c(250, 5000), #250m and 5km
                                               output_dir = imp_fcl_dir,
                                               trim_extent = TRUE,  #only set for TRUE with splits
                                               overwrite = overwrite,
                                               verbose = fxn_msg)
      
      if (prg_msg) p(add_ts("Focal files (", length(imp_fcl_files), "): ", 
                            paste0(basename(imp_fcl_files), collapse = ", ")), 
                     class = "sticky", amount = 0)
      p(amount = length(imp_fcl_files) * global_prog_mult)
      gc()
      
      # Predict
      if (prg_msg) p(add_ts(lbl, "predicting..."), class = "sticky", amount = 0)
      fxn <- "predict-bird-rasters"
      prd_files <- predict_bird_rasters(water_files_realtime = imp_fcl_files,
                                        water_files_longterm = lt_fcl_files,
                                        scenarios = "imposed",
                                        water_months = axn_mths,
                                        model_files = shorebird_model_files_reallong,
                                        model_names = shorebird_model_names_reallong,
                                        static_cov_files = bird_model_cov_files,
                                        static_cov_names = bird_model_cov_names,
                                        monthly_cov_files = tmax_files,
                                        monthly_cov_months = tmax_mths,
                                        monthly_cov_names = tmax_names,
                                        output_dir = imp_prd_dir,
                                        overwrite = overwrite,
                                        verbose = fxn_msg)
      
      if (prg_msg) p(add_ts("Pred files (", length(prd_files), "): ", paste0(basename(prd_files), collapse = ", ")), 
                     class = "sticky", amount = 0)
      p(amount = length(prd_files) * global_prog_mult)
      gc()
      
      # Extract predictions
      if (prg_msg) p(add_ts(lbl, "extracting predictions..."), class = "sticky", amount = 0)
      fxn <- "extract-predictions"
      stat_files <- extract_predictions(prd_files,
                                        fa_files,
                                        field_column = "BidFieldID",
                                        area_column = "AreaAcres",
                                        output_dir = imp_stat_dir,
                                        overwrite = overwrite,
                                        verbose = fxn_msg)
      
      if (prg_msg) p(add_ts("Stat files (", length(stat_files), "): ", paste0(basename(stat_files), collapse = ", ")), 
                     class = "sticky", amount = 0)
      p(amount = length(stat_files) * global_prog_mult)
      gc()
      
    # On interrupt
    # Add explicitly to make interrupts more reliable
    }, interrupt = function(i) {
      
      plan(sequential) #close orphan threads and release memory #may not work when called here
      p(add_ts("USER INTERRUPT - Execution terminated."), class = "sticky", amount = 0)
    
    # On error 
    }, error = function(e) {
      
      if (retry_counter == 0) {
        lbl <- ""
      } else if (retry_counter == 1) {
        lbl <- paste0("FA ", fa, " - ")
      } else {
        lbl <- paste0("FA ", fa, ", retry ", retry_counter, " - ")
      }
      
      p(add_ts("ERROR - ", fxn, " - ", lbl, e), class = "sticky", amount = 0)
      
      saveRDS(e, file.path(log_dir, paste0("error_function-", fxn, "_FA-", paste0(unlist(fa), collapse = "-"),
                                           "_retry-", retry_counter, "-of-", retry_times,
                                           "_date-", format(Sys.time(), format = "%Y-%m-%d"), ".rds")))
      
      # If retry request, set overwrite to TRUE
      if (retry_times >= 1 & retry_counter == 0) {
        
        p(add_ts("Retrying problematic file with overwrite == TRUE..."), class = "sticky", amount = 0)
        evaluate_auction(unlist(fa), overwrite = TRUE,
                         retry_times = min(retry_times, 1), retry_counter = retry_counter,
                         p = p, global_prog_mult = 0, verbose_level = 3)
        
      # If failed on last attempt (including an attempt that is first & only), remove progress for it
      } else {
        
        p(amount = -1 * prg_mult)
        
      }
      
    }, message = function(m) {
      if (fxn_msg) {
        msg <- m$message
        if (verbose_level > 2) {
          p(gsub("\\s", " ", msg), class = "sticky", amount = 0)
        } else if (grepl("(Output file)|(Complete.)", msg)) {
          #nothing
          #idea for future: trigger progress based on text or class of raised condition
        } else {
          p(gsub("\\s", " ", msg), class = "sticky", amount = 0)
        }
      }
    }
    )
    
  }
  
}

# Setup progress reporter

handlers(global = TRUE)
handlers(handler_progress(
  format = "[:bar] :percent (:current/:total) - Elapsed: :elapsed, ETA: :eta", #:spin widget only spins when p() is called
  clear = FALSE))

# Run sequentially (for testing purposes or fixing errors)
#plan(sequential)
#evaluation <- evaluate_auction(flood_areas[FIELD_INDEX_TO_RUN], verbose_level = 2, retry_times = 1, overwrite = TRUE)
#evaluation <- evaluate_auction(flood_areas, verbose_level = 2, retry_times = 1, overwrite = TRUE)

# Set number of cores to use if running 
# flood_areas pulled from axn_shp in 01_setup.R
cores_to_use <- 4
n_sessions <- min(length(flood_areas), cores_to_use, cores_max_global, availableCores() - 1)

# Run multisession evaluation
plan(multisession, workers = n_sessions)
message_ts("Starting bid evaluation using ", n_sessions, " cores.")
evaluation <- evaluate_auction(flood_areas, verbose_level = 1, retry_times = 1, overwrite = overwrite_global)

# Summarize
stat_files <- list.files(imp_stat_dir, pattern = ".rds$", full.names = TRUE)
sum_files <- summarize_predictions(stat_files, 
                                   field_shapefile = axn_file_clean, 
                                   output_dir = imp_stat_dir, 
                                   overwrite = TRUE)


