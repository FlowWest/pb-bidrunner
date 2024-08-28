# Checks input parameters and shapefile structure
#   Ensures parameters specified in definitions.R are acceptable
#   Evaluates structure and contents of axn_file for errors

# Logging package --------------------------------------------------------------
# Check logging package first with non-logger error message if not found
if (!require("logger")) {
  stop(paste0("FATAL [", format(Sys.time(), "%Y-%m-%d %H:%m:%S"), "] - ", 
              "The package 'logger' is required but not installed."))
}

# Definitions ------------------------------------------------------------------
# Load definitions.R to set auction and processing parameters
# Assumes definitions.R is located in the working directory
is_remote <- local({
  x <- Sys.getenv("REMOTE")
  
  if (x == "AWS") {
    TRUE
  } else {
    FALSE
  }
})

# The file is already source when running the analyze bids
if (!is_remote) {
  
  def_dir <- getwd() #change if needed
  source(file.path(def_dir, "definitions.R"))
  
}

# Stop if any of the following parameters are undefined (should be loaded in definitions.R)
parameters <- c("auction_id", "base_dir", "axn_dir", "axn_file", "required_cols", "axn_extent", 
                "cores_max_global", "overwrite_global",
                "temp_dir", "repo_dir")
missing_parameters <- parameters[!unlist(lapply(parameters, exists))]
if (length(missing_parameters) > 0) {
  stop(log_fatal("Check sourcing of definitions.R, as the following parameters have not been defined: ",
                 paste(missing_parameters, collapse = ", ")))
}

# Code files -------------------------------------------------------------------
fxn_dir <- file.path(".", "functions")
code_files <- file.path(fxn_dir, c("00_shared_functions.R",
                                   "01_process_field_file.R",
                                   "02_impose_flooding.R",
                                   "03_water_x_landcover.R",
                                   "04_water_moving_window.R",
                                   "05_predict_birds.R",
                                   "06_extract_predictions.R",
                                   "07_summarize_predictions.R"))
code_files_exist <- file.exists(code_files)
if (!all(code_files_exist)) {
  stop(paste0("{", Sys.getpid(), "} [", Sys.time(), "] - ",
              "required code files not found. Please check that you have cloned the GitHub repo to ",
              "the directory specified in definitions.R. Missing file(s):\n\t",
              paste0(code_files[!code_files_exist], collapse = "\n\t")))
}
tryCatch({ret <- lapply(code_files, source)},
         error = function(e) {
           stop(paste0("{", Sys.getpid(), "} [", Sys.time(), "] - Error loading code files.\n\t", e))
         })

# Packages ---------------------------------------------------------------------
# Check if all are installed
required_packages <- c("parallelly", "future", "foreach", "doFuture", #parallel processing
                       "terra", #spatial data manipulation
                       "progressr", "logger", #logging
                       "gbm", #predicting
                       "dplyr", "tidyr") #data summarization (inlcuding tidyr for pivot)
installed_packages <- as.character(installed.packages()[,1])
missing_packages <- required_packages[!(required_packages %in% installed_packages)]
if (length(missing_packages) > 0) {
  log_error("The following packages are required but not installed:\n\t",
              paste0(missing_packages, collapse = "\n\t"))
}

# Load those required for input testing / processing
if (!require(parallelly)) log_error("Library parallelly is required")     #for checking number of cores
if (!require(terra)) log_error("Library terra is required")               #for checking shapefile

# Check passed parameters ------------------------------------------------------
# Check shapefile existence (auction parameters checked at end of this script)
if (!file.exists(axn_file)) {
  print(paste0("Trying to read the auction file at: ", axn_file))
  stop(add_ts(paste0("auction shapefile axn_file specified in definitions.R not found. Missing file:\n\t",
                     axn_file)))
}

# Check processing_extent
allowed_scenes <- c("p44r33", "p44r34", "p43r34", "p42r35", "valley")
if (!length(axn_extent) == 1) stop("axn_extent must be a single entry")
if (!(axn_extent %in% allowed_scenes)) {
  stop(add_ts(paste0("invalid processing extent specified in definitions_local.R. ", 
                     "Defined axn_extent must be one of the following values:\n\t",
                     paste0(allowed_scenes, collapse = "\n\t"))))
}

# Check repo_dir
if (!file.exists(repo_dir)) {
  stop(add_ts(paste0("required code directory not found. Please check that you have cloned the GitHub repo to ",
                     "the directory specified in definitions_local.R. Missing directory:\n\t",
                     repo_dir)))
}

# Check overwrite_global
if (!is.logical(overwrite_global)) stop("overwrite_global parameter must be either TRUE or FALSE.")

# Check cores_max_global
if (!is.numeric(cores_max_global)) stop("cores_max_global parameter must be an integer.")
cores_available <-  availableCores() - 1 #keep one core free for this
if (cores_max_global > cores_available)  {
  warning(add_ts(paste0("Specified cores_max_global for multi-core processing of ", cores_max_global, 
                        " is higher than available cores. Setting to ", cores_available, ".")))
  cores_max_global <- cores_available
}

# Set directories --------------------------------------------------------------
data_dir <- file.path(repo_dir) # This should change to be a stable location outside the repo

lc_dir <- file.path(data_dir, "landcover")
run_dir <- file.path(data_dir, "runoff")
pcp_dir <- file.path(data_dir, "precip")
wtr_dir <- file.path(data_dir, "water")
avg_dir <- file.path(data_dir, "water_averages")
cov_dir <- file.path(data_dir, "other_covariates")

mdl_dir <- file.path(data_dir, "models")
brd_mdl_dir <- file.path(mdl_dir, "birds")
fld_dir <- file.path(axn_dir, "fields")
spl_dir <- file.path(fld_dir, "splits")

# Average flooding (baseline)
lt_avg_dir <- file.path(data_dir, "longterm_averages")
lt_wtr_dir <- file.path(lt_avg_dir, "water")
lt_wxl_dir <- file.path(lt_avg_dir, "water_x_landcover")
lt_fcl_dir <- file.path(lt_avg_dir, "water_focal")
lt_prd_dir <- file.path(lt_avg_dir, "bird_predictions")

# Average flooding (auction-level)
scn_avg_dir <- file.path(axn_dir, "scenario_average_water")
avg_wtr_dir <- file.path(scn_avg_dir, "water")
avg_wxl_dir <- file.path(scn_avg_dir, "water_x_landcover")
avg_fcl_dir <- file.path(scn_avg_dir, "water_focal")
avg_prd_dir <- file.path(scn_avg_dir, "bird_predictions")
avg_stat_dir <- file.path(scn_avg_dir, "stats")

# Imposed flooding (field or bid-level)
scn_imp_dir <- file.path(axn_dir, "scenario_imposed_water")
imp_wtr_dir <- file.path(scn_imp_dir, "water")
imp_wxl_dir <- file.path(scn_imp_dir, "water_x_landcover")
imp_fcl_dir <- file.path(scn_imp_dir, "water_focal")
imp_prd_dir <- file.path(scn_imp_dir, "bird_predictions")
imp_stat_dir <- file.path(scn_imp_dir, "stats")

# Logs
log_dir <- file.path(axn_dir, "logs")

# Create missing directories ---------------------------------------------------
dirs <- c(data_dir, 
          axn_dir, 
          fld_dir,
          spl_dir,
          scn_avg_dir,
          avg_wtr_dir,
          avg_wxl_dir, 
          avg_fcl_dir, 
          avg_prd_dir, 
          avg_stat_dir,
          scn_imp_dir, 
          imp_wtr_dir, 
          imp_wxl_dir, 
          imp_fcl_dir, 
          imp_prd_dir, 
          imp_stat_dir,
          log_dir)

check_dir(dirs, create = TRUE)

# Model definitions and data files ---------------------------------------------
# Reference file
ref_file <- file.path(cov_dir, paste0("data_type_constant_ebird_", axn_extent, ".tif"))

# Landcovers
landcovers <- c("Rice", "Corn", "Grain", "NonRiceCrops", "TreatedWetland", "Wetland_SemiSeas", "AltCrop")
lc_files <- file.path(lc_dir, paste0(landcovers, "_valley.tif"))
print(paste("the path to landcover files:", lc_dir))
if (!all(file.exists(lc_files))) { stop(add_ts("Missing landcover files."))}

# Bird definitions
bird_df <- data.frame("CommonName" = c("American Avocet", "Black-necked Stilt", "Dowitcher", "Dunlin", 
                                       "Northern Pintail", "Northern Shoveler", "Green-winged Teal"),
                      "CommonCode" = c("AMAV", "BNST", "DOWI", "DUNL", "NOPI", "NSHO", "GWTE"),
                      "ScientificCode" =c("REAM", "HIME", "LISPP", "CALA", "ANAC", "ANCL", "ANCR"))

# Bird models
shorebird_sci_base <- paste(rep(c("CALA", "HIME", "LISPP", "REAM"), each = 2), c("N", "S"), sep = "_")
shorebird_com_base <- paste(rep(c("DUNL", "BNST", "DOWI", "AMAV"), each = 2), c("N", "S"), sep = "_")
# Two models for each species and model type, one for the north valley one and for the south
# Ensemble at (2N + 1S) / 3 for north and (1N + 2S) / 3 for south
# Reallong models that combine long-term and real-time water data
shorebird_model_files_reallong <- file.path(brd_mdl_dir, paste0(shorebird_sci_base, "_Reallong_nopattern_subset.rds"))
shorebird_model_names_reallong <- paste0(shorebird_com_base, "_reallong")

# Long models that just use the long-term average
shorebird_model_files_long <- file.path(brd_mdl_dir, paste0(shorebird_sci_base, "_Long_lowN_subset.rds"))
shorebird_model_names_long <-  paste0(shorebird_com_base, "_longterm")

# Static covariates
bird_model_cov_files <- file.path(cov_dir, c("data_type_constant_ebird_valley.tif", "roads_valley.tif"))
bird_model_cov_names <- c("COUNT_TYPE2", "roads5km")

# Monthly covariates (tmax)
tmax_mths <- month.abb[1:12]
tmax_files <- file.path(cov_dir, 
                        paste0("tmax_", 
                               ifelse(tmax_mths %in% month.abb[10:12], "1990-2019_", "1991-2020_"), 
                               tmax_mths, 
                               "_mean_snapped.tif"))
tmax_names <- rep("tmax250m", length(tmax_mths))

# Long-term water
lt_wtr_files <- list.files(lt_wtr_dir, 
                           pattern = paste0(axn_extent, ".*((", paste(month.abb, collapse = ")|("), ")).*tif$"), 
                           full.names = TRUE)
lt_fcl_files <- list.files(lt_fcl_dir, 
                           pattern = paste0(axn_extent, ".*((", paste(month.abb, collapse = ")|("), ")).*tif$"), 
                           full.names = TRUE)

# Check repo for required data files
data_files <- c(ref_file, lc_files, 
                shorebird_model_files_reallong, shorebird_model_files_long,
                bird_model_cov_files, tmax_files,
                lt_wtr_files, lt_fcl_files)
data_files_exist <- file.exists(data_files)
if (!all(data_files_exist)) {
  stop(add_ts("Required data files not found. If you cloned the repo from GitHub, please check ",
              "that you have Git Large File Storage installed and that you have cloned the GitHub repo to ",
              "the directory specified in definitions.R. Missing file(s):\n\t",
              paste0(data_files[!data_files_exist], collapse = "\n\t")))
}

# Check auction file -----------------------------------------------------------
axn_file_clean <- file.path(fld_dir, gsub(".shp", "_clean.shp", shp_fn))

# Clean shapefile
if (!file.exists(axn_file_clean) | overwrite_global == TRUE) {
  
  message_ts("Checking and cleaning field shapefile...")
  
  # Load
  axn_shp <- vect(axn_file)
  
  # Add any ad-hoc edits to shapefile here
  # TODO: figure out better place and/or way to do this
  #axn_shp$StartDate[axn_shp$BidID == "23-FDF-404"] <- "2023/09/25"
  
  # Check for invalid geometries
  shp_invalid <- !is.valid(axn_shp)
  if (any(shp_invalid)) {
    message_ts("WARNING - Found ", sum(shp_invalid), " polygons with invalid geometries. Attempting to fix...")
    axn_shp <- makeValid(axnShp)
  }
  
  # Check required columns
  # required_cols set in definitions.R
  missing_cols <- required_cols[!(required_cols %in% names(axn_shp))]
  if (length(missing_cols) > 0) {
    stop(add_ts(paste0("The following required column(s) are missing from the auction shapefile:\n\t",
                       paste0(missing_cols, collapse = "\n\t"))))
  }
  
  # Drop bids specified as bids_to_remove in definitions.R
  if (length(bids_to_remove) > 1) {
    message_ts("WARNING - Attempting to remove the following bids (specified as bids_to_remove in definitions.R):\n\t",
               paste0(bids_to_remove, collapse = "\n\t"))
    
    # Loop across bids to remove
    for (b in bids_to_remove) {
      
      matches <- grepl(b, axn_shp$BidID)
      if (sum(matches) == 0) {
        
        stop(add_ts("Found no matches for bid ", b, ". Please check specification of bids_to_remove in definitions.R"))
      
      } else {
        
        message_ts("Found and removing ", sum(matches), " bid fields for bid ", b, ":")
        print(as.data.frame(axn_shp[matches,]))
        axn_shp <- axn_shp[!matches,]
        
      }
      
    }
    
  }
  
  # Check date format
  # as.Date will throw an error if dates are in an unrecognized format
  #   use lapply so that each element is checked individually, otherwise there will
  #   be no error so long as the first element is in proper format
  tryCatch({
    
      start_dates <- lapply(axn_shp$StartDate, as.Date)
      end_dates <- lapply(axn_shp$EndDate, as.Date)
      
    }, error = function(e) {
      
      stop(add_ts("Unable to parse date values. Please ensure they are in a standard, ",
                  "unambiguous format.\n\t", e))
      
    })
  
  
  # Check end dates are after start dates
  # Re-pull dates because unlisting listed dates doesn't work well format-wise
  start_dates <- as.Date(axn_shp$StartDate)
  end_dates <- as.Date(axn_shp$EndDate)
  day_diff <- difftime(end_dates, start_dates)
  if (any(day_diff  <= 0)) {
    
    bad_date_df <- unique(as.data.frame(axn_shp[day_diff <= 0, c("BidID", "StartDate", "EndDate")]))
    print(bad_date_df)
    stop(add_ts("EndDate must be after StartDate. Fix dates for the following bids:\n\t",
                paste0(bad_date_df$BidID, collapse = "\n\t")))

  }
  
  # Get months from shapefile
  mth_nums <- sort(as.numeric(unique(format(as.Date(c(start_dates, end_dates)), format = "%m"))))
  axn_mths <- month.abb[mth_nums]
    
  # Check for multiple start/end dates in one bid
  bid_start_df <- unique(as.data.frame(axn_shp)[c("BidID", "StartDate")])
  bids_multi_start <- unique(bid_start_df$BidID[duplicated(bid_start_df$BidID)])
  if (length(bids_multi_start) > 0) {
    message_ts("WARNING - the following bids have multiple start dates:\n\t",
               paste0(bids_multi_start, collapse = "\n\t"))
    message_ts("WARNING - Bids will be split and analyzed separately by start date. ", 
               "This behavior is not tested, so results may be incorrect or difficult to interpret. ",
               "To fix, either change StartDates to match within bids or create a ", 
               "new BidID for each StartDate (e.g., by appending a letter).")
  }
  
  bid_end_df <- unique(as.data.frame(axn_shp)[c("BidID", "EndDate")])
  bids_multi_end <- unique(bid_end_df$BidID[duplicated(bid_end_df$BidID)])
  if (length(bids_multi_end) > 0) {
    message_ts("WARNING - the following bids have multiple end dates:\n\t",
               paste0(bids_multi_end, collapse = "\n\t"))
    message_ts("WARNING - Bids will be split and analyzed separately by end date. ", 
               "This behavior is not tested, so results may be incorrect or difficult to interpret. ",
               "To fix, either change EndDates to match within bids or create a ", 
               "new BidID for each EndDate (e.g., by appending a letter).")
  }
  
  # Check to see if any bids are splittable
  nas_fld_split <- is.na(axn_shp$Split)
  if (any(nas_fld_split)) {
    message_ts("WARNING - the following bids have at least one row with an NA value in the ", 
               "column 'Split' and each will be grouped (check if desired):\n\t",
               paste0(unique(axn_shp$BidID[nas_fld_split]), collapse = "\n\t"))
  }
  
  split_bool <- axn_shp$Split == TRUE | 
                 axn_shp$Split == 1 |
                 axn_shp$Split == "1" |
                 axn_shp$Split == "Y" |
                 axn_shp$Split == "Yes"
  axn_shp$Split <- split_bool
  
  if (sum(split_bool) == 0) {
    message_ts("WARNING - no bids are coded as splitable, please check that this is correct.")
  }
  
  # Check for multiple split y/n parameters in one bid
  bid_split_df <- unique(as.data.frame(axn_shp)[c("BidID", "Split")])
  bids_multi_split <- unique(bid_split_df$BidID[duplicated(bid_split_df$BidID)])
  if (length(bids_multi_split) > 0) {
    message_ts("WARNING - the following bids have fields coded with Split both TRUE and FALSE:\n\t",
               paste0(bids_multi_split, collapse = "\n\t"))
    message_ts("WARNING - Fields with split == FALSE will be grouped (within a bid) and ", 
               "fields with split == TRUE will be analyzed separately. ", 
               "This behavior is not tested, so results may be incorrect or difficult to interpret. ",
               "To fix, either change Split parameter to match within bids or create a ", 
               "new BidID for splitabble vs non-splittable fields within a bid.")
  }
  
  # Clean names
  if (any(grepl("[^a-zA-Z0-9_\\-]", axn_shp$BidID))) {
    message_ts("Removing invalid characters from BidID")
    axn_shp$BidID <- clean_string(axn_shp$BidID, "")
  }
  if (any(grepl("[^a-zA-Z0-9_\\-]", axn_shp$FieldID))) {
    message_ts("Removing invalid characters from FieldID")
    axn_shp$FieldID <- clean_string(axn_shp$FieldID)
  }
  
  # Fill blanks
  if (any(is.na(axn_shp$FieldID))) {
    message_ts("Filling blank field names with 'Field'")
    axn_shp$FieldID <- ifelse(is.na(axn_shp$FieldID) | axn_shp$FieldID == "", "Field", axn_shp$FieldID)  
  }
  
  # Check for duplicates
  bid_field_df <- as.data.frame(axn_shp)[c("BidID", "FieldID")]
  dup_df <- unique(bid_field_df[duplicated(bid_field_df),])
  if (nrow(dup_df) > 0) {
    
    message_ts("WARNING - ", nrow(dup_df), " duplicate bid-field combinations found. ",
               "Will make unique by appending a number to 'FieldID' in ascending order.")
    
    # Make duplicates unique by appending an ascending number (-1, -2, etc)
    for (n in 1:nrow(dup_df)) {
      bid <- dup_df$BidID[n]
      fld <- dup_df$FieldID[n]
      n_dups <- length(axn_shp$FieldID[axn_shp$BidID == bid & axn_shp$FieldID == fld])
      axn_shp$FieldID[axn_shp$BidID == bid & axn_shp$FieldID == fld] <- paste(fld, 1:n_dups, sep = "-")
    }
    
  }
  
  # Keeping non-splittable bids together is faster and more accurate for them, but may unfairly benefit them
  # in comparison to splittable fields/bids score due to landscape effects of multiple fields, so eval all individually
  #axn_shp$BidFieldID <- ifelse(axn_shp$Split == 1, paste(axn_shp$BidID, axn_shp$FieldID, sep = "-"), axn_shp$BidID)
  axn_shp$BidFieldID <- paste(axn_shp$BidID, axn_shp$FieldID, sep = "-")
  
  # Check projection
  ref_rst <- rast(ref_file)
  if (!identical(crs(axn_shp, proj = TRUE), crs(ref_rst, proj = TRUE))) {
    
    message_ts("Field shapefile in wrong projection. Attempting to reproject to match...")
    axn_shp_prj <- project(axn_shp, crs(ref_rst))
    message_ts("Reprojection complete.")
    
  } else {
    
    axn_shp_prj <- axn_shp
    
  }
  
  # Export
  writeVector(axn_shp_prj, filename = axn_file_clean, filetype = "ESRI Shapefile", overwrite = TRUE)
  message_ts("Cleaned shapefile exported.")
  
  # Flood areas
  flood_areas <- axn_shp$BidFieldID
  
  # Clean up
  rm(ref_rst, axn_shp, axn_shp_prj)
  
} else {
  
  axn_shp <- vect(axn_file_clean)
  message_ts("Shapefile already cleaned and overwrite != TRUE; cleaned shapefile loaded.")
  flood_areas <- axn_shp$BidFieldID
  
  # Get months from shapefile
  mth_nums <- sort(as.numeric(unique(format(as.Date(c(axn_shp$StartDate, axn_shp$EndDate)), format = "%m"))))
  axn_mths <- month.abb[mth_nums]
  rm(axn_shp)
  
}

# Final parameters -------------------------------------------------------------
# Subset monthly covars to auction months
lt_wtr_files <- lt_wtr_files[grepl(paste0(".*_((", paste0(axn_mths, collapse = ")|("), ")).tif$"), lt_wtr_files)]
lt_fcl_files <- lt_fcl_files[grepl(paste0(".*_((", paste0(axn_mths, collapse = ")|("), "))_.*tif$"), lt_fcl_files)]
