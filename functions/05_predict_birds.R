# Predict bird rasters using the combination model of realtime and longterm
# If using a realtime-only or longterm-only model, pass the same set of layers to both water_files arguments
#   The model object will only use the files from either the realtime or longterm
# Returns character vector of processed files
predict_bird_rasters <- function(water_files_realtime, water_files_longterm, scenarios, water_months,
                                 model_files, model_names,
                                 static_cov_files, static_cov_names,
                                 monthly_cov_files, monthly_cov_months, monthly_cov_names,
                                 output_dir, overwrite = FALSE,
                                 on_missing_landcover = "stop",
                                 verbose = TRUE) {

  # Load required packages
  if (!require(terra)) stop(add_ts("Library terra is required"))
  if (!require(gbm)) stop(add_ts("Library gbm is required"))
  #if (!require(sp)) stop(add_ts("Library sp is required, though soon to be obsolete"))

  # Check flood areas and scenarios
  if (!is.character(scenarios)) stop(add_ts("Argument 'scenarios' must be a character vector"))
  if (!is.character(water_months)) stop(add_ts("Argument 'water_months' must be a character vector"))
  if (!(all(water_months %in% month.abb))) stop(add_ts("Argument 'water_months' must consist of valid 3-letter month abbreviations"))

  # Check input files
  realtime_files <- FALSE
  if (!is.null(water_files_realtime)) {
    realtime_files <- TRUE
    if (!all(file.exists(water_files_realtime))) stop(add_ts("The following water_files do not exist:\n",
                                                             paste0(water_files_realtime[!file.exists(water_files_realtime)], collapse = ", ")))  
  }
  longterm_files <- FALSE
  if (!is.null(water_files_longterm)) {
    if (!all(file.exists(water_files_longterm))) stop(add_ts("The following water_files do not exist:\n",
                                                             paste0(water_files_longterm[!file.exists(water_files_longerm)], collapse = ", ")))
    longterm_files <- TRUE
  }
  if (!all(file.exists(model_files))) stop(add_ts("The following model_files do not exist:\n",
                                                      paste0(model_files[!file.exists(model_files)], collapse = ", ")))
  if (!all(file.exists(static_cov_files))) stop(add_ts("The following static_cov_files do not exist:\n",
                                                  paste0(static_cov_files[!file.exists(static_cov_files)], collapse = ", ")))
  if (!all(file.exists(monthly_cov_files))) stop(add_ts("The following monthly_cov_files do not exist:\n",
                                                       paste0(monthly_cov_files[!file.exists(monthly_cov_files)], collapse = ", ")))

  # Check model names
  if (!is.character(model_names)) stop(add_ts("Argument 'model_names' must be a character vector"))
  if (length(model_files) != length(model_names)) stop(add_ts("Arguments 'model_files' and 'model_names' must be of the same length"))

  # Check static covariate names
  if (!is.character(static_cov_names)) stop(add_ts("Argument 'static_cov_names' must be a character vector"))
  if (length(static_cov_files) != length(static_cov_names)) stop(add_ts("Arguments 'static_cov_files' and 'static_cov_names' must be of the same length"))

  # Check monthly covariate months and names
  if (!is.character(monthly_cov_months)) stop(add_ts("Argument 'monthly_cov_months' must be a character vector"))
  if (!(all(monthly_cov_months %in% month.abb))) stop(add_ts("Argument 'monthly_cov_months' must consit of valid 3-letter month abbreviations"))
  if (length(monthly_cov_files) != length(monthly_cov_months)) stop(add_ts("Arguments 'monthly_cov_files' and 'monthly_cov_months' must be of the same length"))

  if (!is.character(monthly_cov_names)) stop(add_ts("Argument 'monthly_cov_names' must be a character vector"))
  if (length(monthly_cov_files) != length(monthly_cov_names)) stop(add_ts("Arguments 'monthly_cov_files' and 'monthly_cov_names' must be of the same length"))

  # Check dirs
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))

  # Check other parameters
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  if (!(on_missing_landcover %in% c("next", "stop"))) stop(add_ts("Argument 'on_missing_landcover' must either be 'stop' or 'next'"))

  # Define what water files are required
  # Long term (average)
  landcover_lt_df <- data.frame(NameModel = c("long_rXw_250m", "long_2wetlandXw250m", "long_treatXw_250m",
                                              "long_cornXw_250m", "long_grainXw_250m", "long_otherXw_250m",
                                              "long_rXw_5km", "long_2wetlandXw5km", "long_treatXw_5km", "long_allcropXw_5km"),
                                NameLandcover = c("Rice", "Wetland_SemiSeas", "TreatedWetland", "Corn", "Grain", "AltCrop",
                                                  "Rice", "Wetland_SemiSeas", "TreatedWetland", "NonRiceCrops"),
                                Distance = c(rep("250m", 6), rep("5000m", 4)))
  landcover_lt_df$LandcoverDistance <- paste(landcover_lt_df$NameLandcover, landcover_lt_df$Distance, sep = "_")

  # Real-time (imposed)
  landcover_rt_df <- data.frame(NameModel = c("rXw_250m", "wetland2Xw250m", "treatXw_250m", "cornXw_250m", "grainXw_250m", "otherXw_250m",
                                              "rXw_5km", "wetland2Xw5km", "treatXw_5km", "allcropXw_5km"),
                                NameLandcover = c("Rice", "Wetland_SemiSeas", "TreatedWetland", "Corn", "Grain", "AltCrop",
                                                  "Rice", "Wetland_SemiSeas", "TreatedWetland", "NonRiceCrops"),
                                Distance = c(rep("250m", 6), rep("5000m", 4)))
  landcover_rt_df$LandcoverDistance <- paste(landcover_rt_df$NameLandcover, landcover_rt_df$Distance, sep = "_")

  # Load static covariates
  static_cov_stk <- rast(static_cov_files) #stack()
  names(static_cov_stk) <- static_cov_names

  # Initialize output
  processed_files <- c()

  # Get unique flooding areas
  if (!is.null(water_files_realtime)) {
    flood_areas <- unique(extract_subelement(strsplit(basename(water_files_realtime), "_"), 1))
    loop_files <- water_files_realtime
    loop_rt <- TRUE
  } else {
    flood_areas <- unique(extract_subelement(strsplit(basename(water_files_longterm), "_"), 1))
    loop_files <- water_files_longterm
    loop_rt <- FALSE
  }
  
  #process_flood_areas <- function(fa) {
  for (fa in flood_areas) {

    if (verbose) message_ts("Working on flooding area ", fa)
    fac <- clean_string(fa)
    
    # Files for this flooding area
    # trailing underscore required to prevent field-1 from matching field-14
    fa_files <- loop_files[grepl(paste0(fa, "_"), loop_files)]
    #print(fa_files)

    # Loop across unique scenarios
    for (scn in scenarios) {

      if (verbose) message_ts("Working on scenario ", scn)
      scn_files <- fa_files[grepl(scn, fa_files)]
      if (length(scn_files) == 0) {
        if (verbose) message_ts("WARNING: no files found matching scneario ", scn, " for flood area ", fa, ". Moving to next...")
        #print(fa_files)
        next
      }

      # Loop across months      
      for (mth in water_months) {

        if (verbose) message_ts("Working on month ", mth)
        mth_files <- scn_files[grepl(mth, scn_files)]
        if (length(mth_files) == 0) {
          if (verbose) message_ts("WARNING: no files found matching month ", mth, " for flood area ", fa, " and scenario ", scn, ". Moving to next...")
          next
        }

        # Check if all models have been predicted
        prd_files <- file.path(output_dir, paste0(fac, "_", mth, "_", scn, "_", model_names, ".tif"))
        if (all(file.exists(prd_files))) {

          if (verbose) message_ts("All bird predictions have been created for this scenario. Moving to next...")
          processed_files <- c(processed_files, prd_files)
          next

        }
        
        # REALTIME WATER (imposed)
        if (exists("wtr_rt_stk")) rm(wtr_rt_stk)
        if (realtime_files) {
          
          if (verbose) message_ts("Checking and loading realtime water x landcover moving window files...")
          
          # Check files
          landcover_rt_df$File <- mapply(nm = landcover_rt_df$NameLandcover, dst = landcover_rt_df$Distance,
                                         FUN = function(nm, dst) {
                                           y <- water_files_realtime[grep(paste0(fa, "_.*", mth, "_.*", nm, "_.*", dst), water_files_realtime)]
                                           if (length(y) > 1) {
                                             msg <- paste0("Name ", nm, "; Distance ", dst, "; Matches:\n", 
                                                        paste0(y, collapse = ", "))
                                             stop(add_ts("Multiple matches found for real-time (imposed) water files.\n", msg))
                                           } else if (length(y) == 0) {
                                             y <- NA
                                           }
                                           y
                                         })
          
          if (any(is.na(landcover_rt_df$File))) {
            
            if (on_missing_landcover == "stop") {
              stop(add_ts("The following imposed water x landcover moving window combinations are missing from water_files_realtime for flood area ", fa,
                          ", scenario ", scn, ", and month ", mth, ":\n\t",
                          paste0(landcover_rt_df$LandcoverDistance[is.na(landcover_rt_df$File)], collapse = "\n\t"),
                          "\n\nHalting execultion."))
            } else {
              if (verbose) message_ts("The following imposed water x landcover moving window combinations are missing from water_files_realtime for flood area ", fa,
                         ", scenario ", scn, ", and month ", mth, ":\n\t",
                         paste0(landcover_rt_df$LandcoverDistance[is.na(landcover_rt_df$File)], collapse = "\n\t"),
                         "\n\nMoving to next...")
              next
            }
            
          }
          
          # Load required imposed water files and match names
          if (verbose) message_ts("Loading imposed water x landcover moving window files...")
          wtr_rt_stk <- rast(landcover_rt_df$File)
          names(wtr_rt_stk) <- landcover_rt_df$NameModel
          
        }
        
        # LONGTERM WATER (average)
        if (exists("wtr_lt_stk")) rm(wtr_lt_stk)
        if (longterm_files) {
          
          if (verbose) message_ts("Checking and loading long-term water x landcover moving window files...")
          
          # Check files
          landcover_lt_df$File <- mapply(nm = landcover_lt_df$NameLandcover, dst = landcover_lt_df$Distance,
                                         FUN = function(nm, dst) {
                                           y <- water_files_longterm[grep(paste0(".*_", mth, "_.*", nm, "_.*", dst), water_files_longterm)]
                                           if (length(y) > 1) {
                                             stop(add_ts("Multiple matches found for long-term water files"))
                                           } else if (length(y) == 0) {
                                             y <- NA
                                           }
                                           y
                                         })
          
          if (any(is.na(landcover_lt_df$File))) {
            
            if (on_missing_landcover == "stop") {
              stop(add_ts("The following longterm water x landcover moving window combinations are missing from water_files_realtime for flood area ", fa,
                          ", scenario ", scn, ", and month ", mth, ":\n\t",
                          paste0(landcover_lt_df$LandcoverDistance[is.na(landcover_lt_df$File)], collapse = "\n\t"),
                          "\n\nHalting execultion."))
            } else {
              if (verbose) message_ts("The following longterm water x landcover moving window combinations are missing from water_files_realtime for flood area ", fa,
                         ", scenario ", scn, ", and month ", mth, ":\n\t",
                         paste0(landcover_lt_df$LandcoverDistance[is.na(landcover_lt_df$File)], collapse = "\n\t"),
                         "\n\nMoving to next...")
              next
            }
            
          }
          
          # Load required imposed water files and match names
          wtr_lt_stk <- rast(landcover_lt_df$File)
          names(wtr_lt_stk) <- landcover_lt_df$NameModel
          
        }
        
        # Load basic covariates
        if (verbose) message_ts("Loading model covariates...")
        static_cov_stk <- rast(static_cov_files)
        names(static_cov_stk) <- static_cov_names

        # Load month file(s)
        mth_match <- which(monthly_cov_months == mth)
        mth_cov_stk <- rast(monthly_cov_files[mth_match])
        names(mth_cov_stk) <- monthly_cov_names[mth_match]

        # Crop stacks to field area
        #print(ext(static_cov_stk))
        #print(ext(mth_cov_stk))
        #print(ext(wtr_lt_stk))
        #print(ext(wtr_rt_stk))
        if (all(ext(mth_cov_stk)[1:4] == ext(static_cov_stk)[1:4])) {
          if (verbose) message_ts("Cropping covariate to field as stack...")
          cov_stk <- c(static_cov_stk, mth_cov_stk)
          if (realtime_files) {
            cov_stk <- crop(cov_stk, wtr_rt_stk)
          }
          if (longterm_files) {
            cov_stk <- crop(cov_stk, wtr_lt_stk)
          }
        } else {
          if (verbose) message_ts("Cropping covariate to field individually (non-matching extents)...")
          if (realtime_files) {
            static_cov_stk <- crop(static_cov_stk, wtr_rt_stk)
            mth_cov_stk <- crop(mth_cov_stk, wtr_rt_stk) 
          }
          if (longterm_files) {
            static_cov_stk <- crop(static_cov_stk, wtr_lt_stk)
            mth_cov_stk <- crop(mth_cov_stk, wtr_lt_stk) 
          }
          cov_stk <- c(static_cov_stk, mth_cov_stk)
        }
        if (realtime_files) {
          wtr_lt_stk <- crop(wtr_lt_stk, wtr_rt_stk)
        }

        # Check extents and combine
        #if (!identical(ext(wtr_lt_stk), ext(wtr_rt_stk))) {
        #  if (verbose) message_ts("Real-time water layers:\n", paste(landcover_rt_df$File, collapse = "\n"))
        #  if (verbose) message_ts("Long-term water layers:\n", paste(landcover_lt_df$File, collapse = "\n"))
        #  stop(add_ts("Real-time and long-term water layers have different extents. Check specified files."))
        #}
        
        full_stk <- c(cov_stk, wtr_lt_stk, wtr_rt_stk)

        # Loop across bird models
        for (mn in 1:length(model_files)) {

          mdl_file <- model_files[mn]
          mdl_name <- model_names[mn]

          if (verbose) message_ts("Predicting bird model ", mdl_name, "...")

          prd_file <- file.path(output_dir, paste0(fac, "_", mth, "_", scn, "_", mdl_name, ".tif"))

          # Check if predicted
          if (file.exists(prd_file) & overwrite != TRUE) {

            if (verbose) message_ts("Surface already predicted and overwrite != TRUE.  Moving to next.")
            processed_files <- c(processed_files, prd_file)
            next

          }

          # Load model
          mdl <- readRDS(mdl_file)

          # Predict
          #if (verbose) message_ts("Predicting surface...")
          if (verbose) message_ts("Output file: ", prd_file)

          # Must define factors when predicting
          prd_rst <- predict(full_stk, mdl, n.trees = mdl$gbm.call$best.trees, 
                                     type = "response", factors = list("COUNT_TYPE2" = c(1, 2)))
          summary(prd_rst, warn = FALSE)
          if (verbose) message_ts("Exporting...")
          writeRaster(prd_rst, filename = prd_file, overwrite = TRUE)

          # Append to output
          processed_files <- c(processed_files, prd_file)

        }

      }

    }

  }
  
  return(processed_files)
  
}
