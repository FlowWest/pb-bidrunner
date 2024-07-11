# Function to overlay water and landcover files
# Takes water and landcover files as inputs
# Returns a vector of created files
overlay_water_landcover <- function(water_files, landcover_files, output_dir = NULL, 
                                    overwrite = FALSE,  verbose = TRUE) {
  
  # Load required packages
  if (!require(terra)) stop(add_ts("Library terra is required"))
  
  # Check input files
  if (!all(file.exists(water_files))) stop(add_ts("The following water_files do not exist:\n",
                                                  paste0(water_files[!file.exists(water_files)], collapse = ", ")))
  if (!all(file.exists(landcover_files))) stop(add_ts("The following landcover_files do not exist:\n",
                                                  paste0(landcover_files[!file.exists(landcover_files)], collapse = ", ")))

  # Check output dir
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))

  # Check other parameters
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))

  # Initialize output

  processed_files <- c()
  
  for (wf in water_files) {

    wfn <- basename(wf)
    if (verbose) message_ts("Creating landcover overlays for water file ", wfn)

    # Check output files
    out_files <- file.path(output_dir, paste0(substr(wfn, 0, nchar(wfn) - 4), "_x_", basename(landcover_files)))
    if (all(file.exists(out_files)) & overwrite != TRUE) {

      if (verbose) message_ts("All water x landcover overlays created for this file. Moving to next.")
      processed_files <- c(processed_files, out_files)
      next

    }

    # Load
    wtr_rst <- rast(wf)

    # Loop across passed landcover files
    for (lcf in landcover_files) {

      lcfn <- basename(lcf)
      if (verbose) message_ts("Calculating overlay for ", lcfn, "...")

      # Load
      lc_rst <- rast(lcf)

      # Check file existence and whether or not to overwrite
      out_file <- file.path(output_dir, paste0(substr(wfn, 0, nchar(wfn) - 4), "_x_", lcfn))
      if (file.exists(out_file) & overwrite != TRUE) {
        if (verbose) message_ts("File already processed and overwrite not set to TRUE. Moving to next.")
        processed_files <- c(processed_files, out_file)
        next
      }

      # Create overlay
      #if (verbose) message_ts("Overlaying water and landcover...")
      if (verbose) message_ts("Output file: ", out_file)
      wxl_rst <- lapp(c(wtr_rst, lc_rst), 
                      fun = function(x, y) { x * y })
      if (verbose) message_ts("Exporting...")
      writeRaster(wxl_rst, filename = out_file, overwrite = overwrite)
      
      # Append to output
      processed_files <- c(processed_files, out_file)

    }

  }

  return(processed_files)

}
