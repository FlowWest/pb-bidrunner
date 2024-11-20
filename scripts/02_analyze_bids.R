# Run auction-level part of the processing

args <- commandArgs(trailingOnly = TRUE)

# determine whether we are running locally or on aws

# Setup logging package --------------------------------------------------------
# Check logging package first with non-logger error message if not found
if (!require("logger")) {
  stop(paste0("FATAL [", format(Sys.time(), "%Y-%m-%d %H:%m:%S"), "] - ", 
              "The package 'logger' is required but not installed."))
} else {
  log_threshold(DEBUG)
  log_debug("Logging started to console")
}

# Get arguments from command line
withCallingHandlers(args <- commandArgs(trailingOnly = TRUE),
         error = \(e) stop(log_fatal("Error getting command line args:\n* ", paste(e))$default$message),
         warning = \(w) log_warn("Warning getting command line args:\n* ", paste(w)),
         message = \(m) log_info(m$message))

withCallingHandlers(source("global.R"),
         error = \(e) stop(log_fatal("Error sourcing global.R:\n* ", paste(e))$default$message),
         warning = \(w) log_warn("Warning sourcing global.R:\n* ", paste(w)),
         message = \(m) log_info(m$message))

compute_engine <- get_computing_backend()

if (length(args) == 0 && compute_engine == "aws") {
  logger::log_fatal("bid running in REMOTE mode but no args passed in. Stopping.")
  stop(call. = FALSE)  
}


# Load definitions -------------------------------------------------------------
# Load definitions.R to set auction and processing parameters
# Assumes definitions.R is located in the working directory
log_trace("Loading definitions and checking parameters")
def_dir <- if (compute_engine == "aws") "." else getwd()
withCallingHandlers(source(file.path(def_dir, "definitions.R")),
         error = \(e) stop(log_fatal("Error sourcing definitions.R:\n* ", paste(e))$default$message),
         warning = \(w) log_warn("Warning sourcing definitions.R:\n* ", paste(w)),
         message = \(m) log_info(m$message))

print("the files in the /mnt/efs/bidunner-data")
print(list.files("/mnt/efs/"))

# update to reflect model run

withCallingHandlers({
  set_runner_definitions(
    auction_id = "2024-02-B4B", #args[1], # when run locally you provide the id here (e.g., "2024-02-B4B",#  )
    base_dir = "E:/data/auctions",#  paste0("/mnt/efs/", args[3]), #local example: "E:/data/auctions",#  
    repo_dir = ".", # where the code is stored
    data_dir = "E:/data/auctions/auction_data", #"path/to/data", #"E:/data/auctions/auction_data",#  
    shapefile_name = "B4B_spring_24_fields_all.shp", #args[2]
  )},
  error = \(e) stop(log_fatal("Error setting definitions for run:\n* ", paste(e))$default$message),
  warning = \(w) log_warn("Warning setting definitions for run:\n* ", paste(w)),
  message = \(m) log_info(m$message)
  )

# Load definitions, check parameters, source code, and run setup
setup_dir <- file.path(def_dir, "scripts") #change if needed

# setup does not need remote args, therefore hardcoding it to false for now
source(file.path(setup_dir, "01_setup.R"))
# setup does not need remote args, therefore hardcoding it to false for now
withCallingHandlers(source(file.path(setup_dir, "01_setup.R")),
         error = \(e) stop(log_fatal("Error running setup.R:\n* ", paste(e))$default$message),
         warning = \(w) log_warn("Warning running setup.R:\n* ", paste(w)),
         message = \(m) log_info(m$message))

# Load packages (for multi-core processing and reporting)
library(future)
library(foreach)
library(doFuture)
library(progressr)

# Split shapefile
log_info("Splitting shapefile into individual flooding areas")
withCallingHandlers({
  floodarea_files <- split_flooding_area(axn_file_clean,
                                         field_column_name = "BidFieldID",  #created in 01_setup.R
                                         guide_raster = ref_file,
                                         output_dir = spl_dir,              #defined in definitions.R
                                         do_rasterize = FALSE,
                                         overwrite = overwrite_global)},
  error = \(e) stop(log_fatal("Error splitting shapefile:\n* ", paste(e))$default$message),
  warning = \(w) log_warn("Warning splitting shapefile:\n* ", paste(w)),
  message = \(m) log_info(m$message))

# Function that runs the auction, using progress handlers and multiple cores as 
# set by 'plan' in the future package
evaluate_auction <- function(flood_areas, overwrite = FALSE, 
                             retry_times = 0, retry_counter = NULL,
                             verbose_level = 1, p = NULL, global_prog_mult = 1) {
  
  log_info("Starting auction evaluation")
  
  # Check/set retry counter
  if (retry_times > 1) {
    log_warn("evaluation_auction() only supports a maximum of one level of retry upon error; ",
             "setting retry_times to 1.")
    retry_times <- 1
  }
  if (is.null(retry_counter)) { 
    retry_counter <- 0
  } else {
    retry_counter <- retry_counter + 1
  }
  
  # Count files for progress reporter
  log_debug("Estimating processing time")
  
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
  log_debug("Starting progress reporter")
  
  if (is.null(p)) {
    #p <- progressor(steps = n_rst + n_imp + n_wxl + n_fcl + n_prd + n_att, 
    #                auto_finish = FALSE)
  }
  
  log_debug("Progress reporter started")    #no messages show up after progressor established
  
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
    
    log_debug("Splitting by core") #no messages show up in logger after split processing established even if progressor isn't established
    
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
    
    }, 
    
    # On interrupt
    # Add explicitly to make interrupts more reliable
    interrupt = function(i) {
      
      plan(sequential) #close orphan threads and release memory #may not work when called here
      p(add_ts("USER INTERRUPT - Execution terminated."), class = "sticky", amount = 0)
     
    }, 
    
    # On error
    error = function(e) {
      
      if (retry_counter == 0) {
        lbl <- ""
      } else if (retry_counter == 1) {
        lbl <- paste0("FA ", fa, " - ")
      } else {
        lbl <- paste0("FA ", fa, ", retry ", retry_counter, " - ")
      }
      
      p(add_ts("ERROR - ", fxn, " - ", lbl, e), class = "sticky", amount = 0)
      #log_error("Error in ", fxn, ", ", lbl, ":\n* ")#, paste(e$message)) #not caught by logger package
      
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
    },
    
    # On warning
    warning = function(w) {
      #log_warn("Warning in ", fxn, ":\n* ", paste(w)) #not caught by logger package
      p(gsub("\\s", " ", w), class = "sticky", amount = 0)
    }, 
    
    # On message
    message = function(m) {
      if (fxn_msg) {
        msg <- m$message
        #log_info(msg) #not caught by logger package
        if (verbose_level > 2) {
          p(gsub("\\s", " ", msg), class = "sticky", amount = 0)
        } else if (grepl("(Output file)|(Complete.)", msg)) {
          #nothing
          #idea for future: trigger progress based on text or class of raised condition
        } else {
          p(gsub("\\s", " ", msg), class = "sticky", amount = 0)
        }
      }
    })
    
  }
  
}

# Setup progress reporter
log_debug("Setting progress reporter")

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
cores_to_use <- 2
n_sessions <- min(length(flood_areas), cores_to_use, cores_max_global, availableCores() - 1)

# Setup multisession evaluation
log_debug("Setting up multisession evaluation with {n_sessions} workers")
plan(multisession, workers = n_sessions)

# Run
log_info("Starting auction evaluation")
withCallingHandlers({
  evaluation <- evaluate_auction(flood_areas, 
                                 verbose_level = 1, 
                                 retry_times = 2, 
                                 overwrite = overwrite_global)
  },
  error = \(e) stop(log_fatal("Error running evaluate_auction():\n* ", paste(e))$default$message),
  warning = \(w) log_warn("Warning running evaluate_auction():\n* ", paste(w)),
  message = \(m) log_info(m$message))

# Summarize
stat_files <- list.files(imp_stat_dir, pattern = ".rds$", full.names = TRUE)
withCallingHandlers({
  sum_files <- summarize_predictions(stat_files, 
                                     field_shapefile = axn_file_clean, 
                                     output_dir = imp_stat_dir, 
                                     overwrite = TRUE)
  },
  error = \(e) stop(log_fatal("Error running summarize_predictions():\n* ", paste(e))$default$message),
  warning = \(w) log_warn("Warning running summarize_precitions():\n* ", paste(w)),
  message = \(m) log_info(m$message))

