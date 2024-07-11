# Extract predicted values for a set of flooding areas and prediction files

# Returns character vector of created files
extract_predictions <- function(prediction_files, floodarea_shapefiles, field_column, area_column,
                                output_dir, n_predictions = NULL, overwrite = FALSE,
                                verbose = TRUE) {
  
  # Load required packages
  if (!require(terra)) stop(add_ts("Library terra is required"))
  
  # Check input files
  if (!all(file.exists(prediction_files))) stop(add_ts("The following prediction_files do not exist:\n",
                                                       paste0(prediction_files[!file.exists(prediction_files)], collapse = ", ")))
  if (!all(file.exists(floodarea_shapefiles))) stop(add_ts("The following floodarea_shapefiles do not exist:\n",
                                                           paste0(floodarea_shapefiles[!file.exists(floodarea_shapefiles)], collapse = ", ")))
  
  # Check output dir
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))
  
  # Check other parameters
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  
  # Initialize output
  processed_files <- c()
  
  # Loop across floodarea_shapefiles
  for (fas in floodarea_shapefiles) {
    
    if (verbose) message_ts("Working on shapefile ", fas)
    fa <- substr(basename(fas), 0, nchar(basename(fas)) - 4)
    
    # Check if flooding area shapefile exists and load
    fa_shp <- vect(fas)
    
    # Check column names
    if (!(field_column) %in% names(fa_shp)) stop(add_ts("field_column ", field_column, " does not exist in fa_shp ", fa, "."))
    if (!(area_column) %in% names(fa_shp)) stop(add_ts("area_column ", area_column, " does not exist in fa_shp ", fa, "."))
    
    # Subset to matching flood areas
    # Need terminal underscore in search pattern to distinguish 'Field-1' from 'Field-14'
    prd_files <- prediction_files[grepl(paste0(fa, "_"), prediction_files)]
    n_files <- length(prd_files)
    if (verbose) message_ts(length(prd_files), " matching predictions found.")
    if (n_files == 0) {
      
      if (verbose) message_ts("No matches found; moving to next.")
      next
      
    } else if (!is.null(n_predictions)) {
      
      if (n_files != n_predictions) {
        
        if (verbose) message_ts("Incorrect number of matches. Found ", n_files, "; expecting ", n_predictions, ". Skipping...")
        next
        
      }
      
    }
    
    if (any(is.na(fa_shp[[field_column]])))	{
      field_is_na <- is.na(fa_shp[[field_column]])
      n_replacements <- sum(field_is_na)
      if (verbose) message_ts("Replacing ", n_replacements, " missing field names...")
      fa_shp[[field_column]][field_is_na] <- paste0("Unnamed-Field-", 1:n_replacements)
    }
    
    fields <- unique(fa_shp[[field_column]])
    
    if (length(fields) < nrow(fa_shp)) {
      if (verbose) message_ts("Field names are not unique. Appending numbers...")
      fa_shp[[field_column]] <- paste0(fa_shp[[field_column]], "-", 1:nrow(fa_shp))
      fields <- unique(fa_shp[[field_column]])
    }
    
    # Loop across fields
    for (fld in fields) {
      
      if (verbose) message_ts("Working on field ", fld)
      fld_shp <- fa_shp[fa_shp[[field_column]] == fld,]
      
      fld_cln <- clean_string_remove_underscores(fld)
      
      # Check if already processed
      clean_name <- paste0(clean_string_remove_underscores(fa), "_", fld_cln, "_summary.rds")
      prd_data_file <- file.path(output_dir, clean_name)
      if (file.exists(prd_data_file) & overwrite != TRUE) {
        
        if (verbose) message_ts("Data for flooding area ", fa, " and field ", fld, " already calculated. Moving to next...")
        processed_files <- c(processed_files, prd_data_file)
        next
        
      }
      
      # Build data frame for extracted values
      fn_split <- strsplit(basename(prd_files), "_")
      prd_df <- data.frame("PredictionFilename" = basename(prd_files),
                           "FloodingArea" = extract_subelement(fn_split, 1),
                           "FieldName" = rep(fld_cln),
                           "FieldAreaAcres" = rep(sum(fld_shp[[area_column]])), #need to check data type of area_column
                           #"PredictionYear" = extract_subelement(fn_split, 2), #need to add
                           "PredictionMonth" = extract_subelement(fn_split, 2),
                           "Species" = extract_subelement(fn_split, 4),
                           "Model" = substr(extract_subelement(fn_split, 6), 0, nchar(extract_subelement(fn_split, 6)) - 4),
                           "ModelLocation" = extract_subelement(fn_split, 5),
                           "PredictionMean" = rep(NA),
                           "PredictionSum" = rep(NA),
                           "PredictionMean_Landscape" = rep(NA))
      
      # Load prediction rasters
      if (verbose) message_ts("Loading prediction stack...")
      prd_stk <- rast(prd_files)
      
      # Flooding area mean
      if (verbose) message_ts("Extracting mean flooding area suitability...")
      means <- as.numeric(terra::extract(prd_stk, fld_shp, fun = mean, na.rm = TRUE, ID = FALSE))
      if (length(means) == 17) means <- means[2:17]
      prd_df$PredictionMean <- means
      
      # Flooding area sum
      if (verbose) message_ts("Extracting total flooding area suitability...")
      sums <- as.numeric(terra::extract(prd_stk, fld_shp, fun = sum, na.rm = TRUE, ID = FALSE))
      if (length(sums) == 17) sums <- sums[2:17]
      prd_df$PredictionSum <- sums
      
      # Landscape mean
      # Values need to be NA outside area of interest
      if (verbose) message_ts("Calculating mean landscape suitability...")
      prd_df$PredictionMean_Landscape <- as.numeric(unlist(global(prd_stk, "mean", na.rm = TRUE)))
      
      # Landscape sum
      # Values need to be NA outside area of interest
      if (verbose) message_ts("Calculating total landscape suitability...")
      prd_df$PredictionSum_Landscape <- as.numeric(unlist(global(prd_stk, "sum", na.rm = TRUE)))
      
      # Export
      saveRDS(prd_df, prd_data_file)
      if (verbose) message_ts("Data exported.")
      
      # Append to output
      processed_files <- c(processed_files, prd_data_file)
      
    }
    
  }
  
  return(processed_files)
  
}
