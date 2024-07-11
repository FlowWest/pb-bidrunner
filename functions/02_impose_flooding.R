
# Define function to impose flooding
# Takes water and field files as inputs
# Returns a vector of created files
impose_flooding <- function(water_files, field_files, output_dir, 
                            imposed_value = 1, imposed_label = "imposed",
                            mask = FALSE, overwrite = FALSE, 
                            verbose = TRUE) {
  
  # Load required packages
  if (!require(terra)) stop(add_ts("Library terra is required"))

  # Check input files
  if (!all(file.exists(water_files))) stop(add_ts("The following water_files do not exist:\n",
                                                  paste0(water_files[!file.exists(water_files)], collapse = ", ")))
  if (!all(file.exists(field_files))) stop(add_ts("The following field_files do not exist:\n",
                                                  paste0(field_files[!file.exists(field_files)], collapse = ", ")))

  # Check output dir
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))

  # Check other parameters
  if (!is.logical(mask)) stop(add_ts("Argument 'mask' must be TRUE or FALSE"))
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  if (!is.null(imposed_value) & !is.numeric(imposed_value)) stop(add_ts("Argument 'imposed_value' must be null or numeric"))
  if (!is.character(imposed_label)) stop(add_ts("Argument 'imposed_label' must be a character string of length 1"))
  
  # Initialize output
  processed_files <- c()

  for (wf in water_files) {

    wfn <- basename(wf)
    if (verbose) message_ts("Imposing flooding on water file ", wfn)

    # Load
    wtr_rst <- rast(wf)

    # Loop across passed flood files
    for (ff in field_files) {

      ffn <- basename(ff)
      if (verbose) message_ts("Using field file ", ffn, "...")

      # Build export filename and check if has been processed
      out_fn_base <- paste(extract_subelement(strsplit(ffn, "\\."), 1), extract_subelement(strsplit(wfn, "\\."), 1), sep = "_")
      out_file <- file.path(output_dir, paste0(out_fn_base, "_", imposed_label, ".tif"))
      if (file.exists(out_file) & overwrite != TRUE) {
        
        if (verbose) message_ts("Flooding already imposed. Moving to next.")
        processed_files <- c(processed_files, out_file)
        next
        
      }

      # Load flooding area raster
      fld_rst <- rast(ff)
      
      # Stop if projection is not correct
      if (!identical(crs(wtr_rst), crs(fld_rst))) {
        #stop(add_ts("Projection mismatch between wtr_rst and fld_rst.\n\twtr_rst: ", crs(wtr_rst), 
        #            "\n\tfld_rst: ", crs(fld_rst)))
        message_ts("Projection mismatch; assuming fld rst is just improperly documented")
        crs(fld_rst) <- crs(wtr_rst)
      }
      
      # Stop if no intersection
      int_test <- try(intersect(wtr_rst, fld_rst), silent = TRUE)
      if(inherits(int_test, "try-error")) stop(add_ts("Extents of wtr_rst and fld_rst do not intersect."))
      
      # Mask if requested (speeds up subsequent processing)
      wtr_msk_rst <- wtr_rst
      if (mask == TRUE) {

        if (verbose) message_ts("Masking to non-NA areas of field raster...")
        values(wtr_msk_rst)[is.na(values(fld_rst))] <- NA

      }

      # If imposed_value is numeric, impose value 
      if (is.numeric(imposed_value)) {
        
        if (verbose) message_ts("Imposing constant flood value...")
        
        # Find extent of field
        is_field <- !is.na(values(fld_rst)) & values(fld_rst) == 1
        
        # Impose value
        imp_rst <- wtr_msk_rst
        values(imp_rst)[is_field] <- imposed_value
        
      } else if(is.null(imposed_value)) {
        
        if (verbose) message_ts("Imposing no value...")
        imp_rst <- wtr_msk_rst
        
      } else {
        
        if (verbose) message_ts("Unrecognized value for 'imposed_value'")
        next
        
      }
      
      # Export
      if (verbose) message_ts("Output file: ", out_file)
      writeRaster(imp_rst, filename = out_file, overwrite = TRUE)
      if (verbose) message_ts("Complete.")
      
      # Append to output
      processed_files <- c(processed_files, out_file)

    }

  }
  
  return(processed_files)
  
}