# Moving window analysis

# Function to calculate moving window sums of water
# Takes water (by landcover) files and distances as input
# Distance is in meters
# Returns a vector of created files
mean_neighborhood_water <- function(water_files, distances, output_dir, trim_extent = FALSE, 
                                    overwrite = FALSE, verbose = TRUE) {
  #message_ts <- message # to keep messages short
  
  # Load required packages
  if (!require(terra)) stop(add_ts("Library terra is required"))

  # Check input files
  if (!all(file.exists(water_files))) stop(add_ts("The following water_files do not exist:\n",
                                                  paste0(water_files[!file.exists(water_files)], collapse = ", ")))

  # Check distances
  if (!is.numeric(distances)) stop(add_ts("Argument 'distances' must be a numeric vector"))

  # Check output dir
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))

  # Check other parameters
  if (!is.logical(trim_extent)) stop(add_ts("Argument 'trim_extent' must be TRUE or FALSE"))
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))

  # Initialize output
  processed_files <- c()

  # Loop across passed water files
  for (wf in water_files) {

    wfn <- basename(wf)
    if (verbose) message_ts("Summing neighborhood water for ", wfn)

    # Check output files
    out_files <- file.path(output_dir, paste0(substr(wfn, 0, nchar(wfn) - 4), "_", distances, "m.tif"))
    if (all(file.exists(out_files)) & overwrite != TRUE) {

      if (verbose) message_ts("All moving windows calculated for this file. Moving to next.")
      processed_files <- c(processed_files, out_files)
      next

    }

    # Load
    wtr_rst <- rast(wf)

    # Trim NAs before processing if requested (can dramatically speed up processing)
    if (trim_extent) {

      if (verbose) message_ts("Trimming water file to remove rows and columns that contain no data (only NAs)...")

      # Use terra's trim
      wtr_rst <- trim(wtr_rst)

    }

    # Loop across passed distances
    for (d in distances) {

      # Check file existence
      out_file <- file.path(output_dir, paste0(substr(wfn, 0, nchar(wfn) - 4), "_", d, "m.tif"))
      if (file.exists(out_file) & overwrite != TRUE) {

        if (verbose) message_ts("File already processed and overwrite not set to TRUE. Moving to next.")
        processed_files <- c(processed_files, out_file)
        next

      }
      
      #if (verbose) message_ts("Creating focal matrix for distances ", d, "m...")
      #fwm <- focalMat(wtr_rst, d, type = "circle")
      fwm <- round(2 * d / res(wtr_rst)[1])

      # Calculate moving window
      if (verbose) message_ts("Calculating moving window for distance ", d, "m...")
      if (verbose) message_ts("Output file: ", basename(out_file))
      fcl_rst <- focal(wtr_rst, fwm, fun = mean, na.rm = TRUE)
      if (verbose) message_ts("Exporting...")
      writeRaster(fcl_rst, filename = out_file, overwrite = TRUE)

      # Append to output
      processed_files <- c(processed_files, out_file)

    }

  }

  # Return
  return(processed_files)

}
