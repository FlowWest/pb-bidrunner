# Define function to split and buffer flooding areas
# Shapefile must have a column containing the names you wish to group and split the analysis
# Recommended to rasterize separately rather than as part of this call for better paralellization
# Returns a vector of created files
split_flooding_area <- function(field_shapefile, field_column_name, guide_raster, output_dir, 
                                do_rasterize = FALSE, buffer_dist = NULL, overwrite = FALSE, 
                                verbose = TRUE) {
  
  # Load required packages
  if (!require(terra)) stop(add_ts("Library terra is required"))
  
  # Check simple parameters
  if (!is.logical(do_rasterize)) stop(add_ts("Argument 'rasterize' must be TRUE or FALSE"))
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  if (!is.null(buffer_dist)) {
    if (!is.numeric(buffer_dist)) {
      stop(add_ts("Argument 'buffer_dist' must either be NULL for no buffering or a number specifying the buffer distance"))
    }
  }
  
  # Check output dir
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))
  
  # Check and load shapefile
  if (class(field_shapefile) == "SpatVector") {
    
    field_shp <- field_shapefile
    
  } else if (class(field_shapefile) == "SpatialPolygonsDataFrame") {
    
    field_shp <- vect(field_shapefile)
    
  } else if (is.character(field_shapefile)) {
    
    if (length(field_shapefile) != 1) {
      stop(add_ts("field_shapefile must be a single shapefile or filename"))
    }
    if (!file.exists(field_shapefile)) {
      stop(add_ts("field_shapefile does not exist: ", field_shapefile, " not found."))
    }
    
    # Load
    field_shp <- vect(field_shapefile)
    
  } else {
    
    stop(add_ts("field_shapefile must be a SpatVector, SpatialPolygonsDataFrame, ", 
                "or the filename of an ESRI shapefile"))
    
  }
  
  # Check field column name
  if (!(field_column_name) %in% names(field_shp)) { 
    stop(add_ts("Column ", field_column_name, " does not exist in field_shapefile."))
  }
  
  # Check and load guide raster
  if (class(guide_raster) == "SpatRaster") {
    
    guide_rst <- guide_raster
    
  } else if (is.character(guide_raster)) {
    
    if (length(guide_raster) != 1) stop(add_ts("guide_raster must be a single SpatRaster or filename"))
    if (!file.exists(guide_raster)) stop(add_ts("guide_raster does not exist: ", guide_raster, " not found."))
    
    guide_rst <- rast(guide_raster)
    
  } else {
    
    stop(add_ts("guide_raster must be a SpatRaster or filename of a raster"))
    
  }
  
  # Reproject shapefile if needed
  if (!identical(crs(field_shp, proj = TRUE), crs(guide_rst, proj = TRUE))) {
    
    if (verbose) message_ts("Reprojecting shapefile to match guide_rst...")
    field_shp <- project(field_shp, crs(guide_rst))
    
  }
  
  # Initialize output
  processed_files <- c()
  
  # Check if all created and skip if not rasterizing
  flooding_areas <- unique(field_shp[[field_column_name]][, 1])
  fa_files <- file.path(output_dir, paste0(clean_string_remove_underscores(flooding_areas), ".shp"))
  if (all(file.exists(fa_files)) & overwrite != TRUE & do_rasterize != TRUE) {
    
    if (verbose) message_ts("Shapefile already split, overwrite != TRUE, and not rasterizing. Returning.")
    processed_files <- c(processed_files, fa_files)
    return(processed_files)
    
  }
  
  # Loop across flooding areas
  for (fan in 1:length(flooding_areas)) {
    
    fa <- flooding_areas[fan]
    fa_file <- fa_files[fan]
    message_ts("Working on flooding area ", fa)
    
    # Check existence
    if (file.exists(fa_file) & overwrite == FALSE) {
      
      if (verbose) message_ts("Split shapefile for flooding area ", fa, " already created and overwrite == FALSE. ", 
                              "Loading already-created file...")
      fld_shp <- vect(fa_file)
      
    } else {
      
      if (verbose) message_ts("Subsetting area ", fa, "...")
      fld_shp <- field_shp[field_shp[[field_column_name]] == fa,]
      
      writeVector(fld_shp, fa_file, filetype = "ESRI Shapefile", overwrite = TRUE)
      if (verbose) message_ts("Complete.")
      
    }
    
    # Append to output
    processed_files <- c(processed_files, fa_file)
    
    # Rasterize field files if called
    if (do_rasterize == TRUE) {
      
      fa_rst_file <- rasterize_flooding_area(fa_file, guide_raster = guide_rst, 
                                                 output_dir = output_dir, buffer_dist = buffer_dist, 
                                                 overwrite = overwrite, verbose = verbose)
      # Append to output
      processed_files <- c(processed_files, fa_rst_file)
      
    }
    
  }
  
  return(processed_files)
  
}

# Use of buffer_dist is recommended to speed processing; set as 2x your largest moving window
# Returns a vector of created files
rasterize_flooding_area <- function(field_shapefiles, guide_raster, output_dir, 
                                    buffer_dist = NULL, overwrite = FALSE, verbose = TRUE) {
  
  # Load required packages
  if (!require(terra)) stop(add_ts("Library terra is required"))
  
  # Check simple parameters
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  if (!is.null(buffer_dist)) {
    if (!is.numeric(buffer_dist)) {
      stop(add_ts("Argument 'buffer_dist' must either be NULL for no buffering or a number specifying the buffer distance"))
    }
  }
  
  # Check output dir
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))
  
  # Check field_shapefiles
  if (!is.character(field_shapefiles)) stop(add_ts("field_shapefiles must be a character vector"))
    
  if (!all(file.exists(field_shapefiles))) {
    stop(add_ts("The following element(s) of field_shapefile do not exist:\n\t", 
                paste0(field_shapefiles[!file.exists(field_shapefiles)], collapse = "\n\t")))
  }
  
  # Check and load guide raster
  if (class(guide_raster) == "SpatRaster") {
    
    guide_rst <- guide_raster
    
  } else if (is.character(guide_raster)) {
    
    if (length(guide_raster) != 1) stop(add_ts("guide_raster must be a single SpatRaster or filename"))
    if (!file.exists(guide_raster)) stop(add_ts("guide_raster does not exist: ", guide_raster, " not found."))
    
    guide_rst <- rast(guide_raster)
    
  } else {
    
    stop(add_ts("guide_raster must be a SpatRaster or filename of a raster"))
    
  }
  
  # Initialize output
  processed_files <- c()
  
  # Loop across field shapefiles
  for (fs in field_shapefiles) {
    
    message_ts("Working on file ", fs)
    
    # Create output file name and check if already created
    fa <- basename(fs)
    out_file <- file.path(output_dir, gsub(".shp", ".tif", fa))
    if (file.exists(out_file) & overwrite == FALSE) {
      
      if (verbose) message_ts("File already rasterized and overwrite == FALSE. Moving to next")
      
      # Append to output
      processed_files <- c(processed_files, out_file)
      
      next
      
    }
    
    # Load
    fa_shp <- vect(fs)
    
    # Reproject shapefile if needed
    if (!identical(crs(fa_shp, proj = TRUE), crs(guide_rst, proj = TRUE))) {
      
      if (verbose) message_ts("Reprojecting shapefile to match guide_rst...")
      fa_shp <- project(fa_shp, crs(guide_rst))
      
    }
    
    # Rasterize, taking buffer distance into account
    if (is.null(buffer_dist)) {
      
      if (verbose) message_ts("Rasterizing...")
      fa_out_rst <- rasterize(fa_shp, guide_rst, field = 1)
      
    } else {
      
      if (verbose) message_ts("Rasterizing...")
      fa_rst <- rasterize(fa_shp, guide_rst, field = 1) #keep in memory, as overwriting in subsequent call causes error
      
      # Turn values within buffer distance of field to 2s instead of NAs
      # Used for masking later to speed processing
      # Width is in meters
      if (verbose) message_ts("Calculating ", buffer_dist, "m buffer for ", fa, "...")
      #fa_buf_rst <- buffer(fa_rst, width = buffer_dist) #buffering raster stopped working Feb 2024; turned all pixels in grid to TRUE
      fa_buf_shp <- buffer(fa_shp, width = buffer_dist)
      
      if (verbose) message_ts("Rasterizing buffer...")
      fa_buf_rst <- rasterize(fa_buf_shp, guide_rst, field = 1)
      
      if (verbose) message_ts("Adding buffer to flooding area raster...")
      fa_out_rst <- lapp(c(fa_rst, fa_buf_rst), 
                         fun = function(x, y) { ifelse(is.na(x) & y == 1, 2, x) })
      
    }
    
    if (verbose) message_ts("Exporting...")
    writeRaster(fa_out_rst, filename = out_file, overwrite = TRUE)

    # Append to output
    processed_files <- c(processed_files, out_file)
  
  }  
  
  return(processed_files)
  
}
