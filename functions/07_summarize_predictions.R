# Code to combine all bird prediction stats into a single file and do preliminary analysis
#

# TODO: add parameters for joining data
summarize_predictions <- function(stat_files, field_shapefile, output_dir, overwrite = FALSE) {
  
  # Load required packages
  if (!require(terra)) stop(add_ts("Library terra is required"))
  if (!require(dplyr)) stop(add_ts("Library dplyr is required"))
  if (!require(tidyr)) stop(add_ts("Library tidyr is required"))
  
  # Check input files
  if (!all(file.exists(stat_files))) stop(add_ts("The following stat_files do not exist:\n",
                                                 paste0(stat_files[!file.exists(stat_files)], collapse = ", ")))
  
  if (!(file.exists(field_shapefile))) stop(add_ts("field_shapefile does not exist"))
  
  # Check output dir
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))
  
  # Check other parameters
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  
  # Initialize output
  processed_files <- c()
  
  # Read metadata
  md_shp <- vect(field_shapefile)
  md_df <- as.data.frame(md_shp)
  md_df$BidFieldID <- paste0(md_df$BidID, "-", clean_string(md_df$FieldID))
  write.csv(md_df, file.path(fld_dir, "field_metadata.csv"), row.names = FALSE)
  
  # Read data
  long_file <- file.path(output_dir, "00_prediction_summary_long.csv")
  if (file.exists(long_file) & overwrite != TRUE) {
    
    message_ts("File already exists and overwrite != TRUE. Loading already-combined data...")
    long_df <- read.csv(long_file)
    
  } else {
    
    message_ts("Reading and combining data...")
    long_df <- do.call(rbind, lapply(stat_files, function(x) readRDS(x)))
    
    write.csv(long_df, long_file, row.names = FALSE)
    message_ts("Exported.")
    
    processed_files <- c(processed_files, long_file)
    
  }
  
  # Check missing
  #md_shp$BidFieldID[!(md_shp$BidFieldID %in% long_df$FloodingArea)]
  #fas <- unique(long_df$FloodingArea)
  #md_shp$BidFieldID[!(md_shp$BidFieldID %in% long_df$FloodingArea)]
  
  # Join to metadata
  # Must be unique -- If Flooding area is bid+field, use that alone
  # Otherwise, use bid + field.  If field lost, use bid + acres
  #join_col <- "BidID" #add as argument
  #joined_df <- left_join(long_df, md_df, by = c("FloodingArea" = join_col))
  #joined_df <- left_join(long_df, md_df, by = c("FloodingArea" = "BidID", "FieldAreaAcres" = "AreaAcres"))
  joined_df <- left_join(long_df, md_df, by = c("FloodingArea" = "BidFieldID"))
  
  # Fix date if needed
  # TODO - check format
  joined_df <- joined_df %>%
    mutate(StartDate = as.Date(StartDate, format = "%Y/%m/%d"),
           EndDate = as.Date(EndDate, format = "%Y/%m/%d"))
  
  # Data frame listing the number of days per month by leap year
  # 30 days: Apr Jun Sep Nov
  days_df <- data.frame("Month" = rep(month.abb, each = 2), 
                        "LeapYear" = rep(c(FALSE, TRUE), times = 12),
                        "Days" = c(31, 31, #Jan
                                   28, 29, #Feb
                                   31, 31, #Mar
                                   30, 30, #Apr
                                   31, 31, #May
                                   30, 30, #Jun
                                   31, 31, #Jul
                                   31, 31, #Aug
                                   30, 30, #Sep
                                   31, 31, #Oct
                                   30, 30, #Nov
                                   31, 31)) #Dec
  
  # Get start and end date for prediction
  # Sequence along all dates from the first start date to the last end date
  valid_dates <- seq(min(joined_df$StartDate), max(joined_df$EndDate), by = 1)
  
  # Get distinct year/month pairs and determine number of days in month
  mth_yr_df <- data.frame(Year = format(valid_dates, format = "%Y"),
                          Month = format(valid_dates, format = "%b")) %>%
    distinct() %>%
    # Leap year every four years, excepting three-quarters of centennial years
    #   E.g., 2100, 2200, 2300 are leap years; 2000 and 2400 are not
    #   So, a leap year if and only if year:
    #     is divisible by 4 but not divisible by 100
    #     OR is divisible by 400
    mutate(YearNum = as.integer(Year),
           LeapYear = (YearNum %% 4 == 0 & YearNum %% 100 != 0) | (YearNum %% 400 == 0)) %>%
    left_join(days_df, by = join_by(Month == Month, LeapYear == LeapYear)) %>%
    mutate(FirstDay = as.Date(paste(Year, match(Month, month.abb), 1, sep = "-")),
           LastDay = as.Date(paste(Year, match(Month, month.abb), Days, sep = "-")))
  
  # Calculate overlap
  joined_df <- joined_df %>%
    left_join(select(mth_yr_df, c(Month, FirstDay, LastDay)), by = join_by(PredictionMonth == Month)) %>%
    rename(PredictionDateStart = FirstDay,
           PredictionDateEnd = LastDay) %>%
    mutate(FloodDateStart = as.Date(StartDate, format = "%Y/%m/%d"),
           FloodDateEnd = as.Date(EndDate, format = "%Y/%m/%d")) %>%
    mutate(OverlapMin = pmax(as.Date(FloodDateStart), PredictionDateStart), 
           OverlapMax = pmin(as.Date(FloodDateEnd), PredictionDateEnd),
           DaysOverlap = pmax(0, difftime(OverlapMax, OverlapMin, unit = "days") + 1))
  
  # Fix field name
  #joined_df <- rename(joined_df, FieldName = Field_Name, Property = Prop_Name, ReportedAcres = Report_Ac)
  joined_df <- mutate(joined_df, FieldAreaAcres = AreaAcres)
  
  # Export
  long_file <- file.path(output_dir, "01_prediction_summary_joined.csv")
  if (file.exists(long_file) & overwrite != TRUE) {
    message_ts("File already exists and overwrite != TRUE. Moving to next.")
  } else {
    write.csv(joined_df, long_file, row.names = FALSE)
    message_ts("Exported.")
    processed_files <- c(processed_files, long_file)
  }
  
  # Calculate ensembles
  message_ts("Calculating ensembles...")
  group_cols <- c("FloodingArea", "BidID", "FieldID", "Split", "FieldAreaAcres", "PricePerAc", "PredictionMonth", 
                  "FloodDateStart", "FloodDateEnd", "DaysOverlap", "Species", "Model")
  other_cols <- c("CoverType") #"Region"
  calc_cols <- c("ModelLocation", "PredictionMean", "PredictionSum", "PredictionMean_Landscape", "PredictionSum_Landscape")
  ens_df <- joined_df %>%
    #rename(Bid = BidID) %>%
    #mutate(Bid = FloodingArea) %>% # if bid is the flooding area unit
    select(all_of(c(group_cols, other_cols, calc_cols))) %>%
    group_by(across(all_of(c(group_cols, other_cols)))) %>%
    mutate(EnsembleWeights = ifelse(ModelLocation == "S", 1, 2)) %>%
    summarize(SuitabilityMean = weighted.mean(PredictionMean, EnsembleWeights),
              SuitabilitySum = sum(PredictionSum * EnsembleWeights / 3),
              LandscapeMean = weighted.mean(PredictionMean_Landscape, EnsembleWeights),
              LandscapeSum = sum(PredictionSum_Landscape * EnsembleWeights / 3)) %>%
    mutate(PredictionMonth = factor(PredictionMonth, levels = month.abb, ordered = TRUE)) %>%
    arrange(FloodingArea, BidID, FieldID, PredictionMonth)
  
  ## SUMMARIZE BY FIELD ##
  fld_df <- filter(ens_df, (Split == TRUE | Split == 1 | Split == "Y" | Split == "Yes" ) & DaysOverlap > 0)
  if (nrow(fld_df) == 0) {
    message_ts("WARNING - No splittable fields found; all analysis performed at bid level. ",
               "Check that this is correct!\n\n")
    
  } else {
      
    # Export (ensembles)
    fld_file <- file.path(output_dir, "02_prediction_summary_field.csv")
    if (file.exists(fld_file) & overwrite != TRUE) {
      message_ts("File already exists and overwrite != TRUE. Moving to next...")
    } else {
      write.csv(fld_df, fld_file, row.names = FALSE)
      message_ts("Exported.")
      processed_files <- c(processed_files, fld_file)
    }
    
    # Combine by month
    fld_cmb_df <- fld_df %>%
      group_by(across(all_of(c("FloodingArea", "BidID", "FieldID", "Split", "FieldAreaAcres", "PricePerAc", 
                               "FloodDateStart", "FloodDateEnd", "Species", other_cols)))) %>%
      summarize(PredictionYear = "Combined", PredictionMonth = "Combined", BidLength = sum(DaysOverlap),
                SuitMean = weighted.mean(SuitabilityMean, DaysOverlap),
                SuitSum = sum(SuitabilityMean * DaysOverlap * FieldAreaAcres), 
                LandscapeMean = weighted.mean(LandscapeMean, DaysOverlap))
    
    # Export
    fld_cmb_file <- file.path(output_dir, "02a_prediction_summary_field_across_months.csv")
    if (file.exists(fld_cmb_file) & overwrite != TRUE) {
      message_ts("File already exists and overwrite != TRUE. Moving to next...")
    } else {
      write.csv(fld_cmb_df, fld_cmb_file, row.names = FALSE)
      message_ts("Exported.")
      processed_files <- c(processed_files, fld_cmb_file)
    }
    
    # Calculate totals across species
    fld_wide_df <- fld_cmb_df %>%
      select(-PredictionYear, - PredictionMonth, - LandscapeMean) %>%
      pivot_wider(names_from = Species, values_from = c(SuitMean, SuitSum)) %>%
      mutate(SuitMean_Total = mean(c(SuitMean_AMAV, SuitMean_BNST, SuitMean_DOWI, SuitMean_DUNL)),
             SuitSum_Total = sum(c(SuitSum_AMAV, SuitSum_BNST, SuitSum_DOWI, SuitSum_DUNL)))
    
    # Export
    fld_wide_file <- file.path(output_dir, "02b_prediction_summary_field_totals_wide.csv")
    if (file.exists(fld_wide_file) & overwrite != TRUE) {
      message_ts("File already exists and overwrite != TRUE. Moving to next...")
    } else {
      write.csv(fld_wide_df, fld_wide_file, row.names = FALSE)
      message_ts("Exported.")
      processed_files <- c(processed_files, fld_wide_file)
    }
  }
    
  ## BY BID ##
  
  ## TODO: make it easier to add columns
  
  # Summarize by bid
  # Field-level stats calculated above for splittable fields, so summarize at bid level here
  message_ts("Summarizing by bid...")
  bid_df <- ens_df %>%
    filter(DaysOverlap > 0) %>%
    ungroup() %>%
    group_by(across(all_of(c("BidID", "PredictionMonth", "FloodDateStart", "FloodDateEnd", 
                             "DaysOverlap", "Species", "Model", other_cols)))) %>%
    summarize(FieldsIncluded = paste0(FieldID, collapse = ", "),
              AreaAcres = sum(FieldAreaAcres), 
              PricePerAc = weighted.mean(PricePerAc, FieldAreaAcres),
              SuitabilityMean = weighted.mean(SuitabilityMean, FieldAreaAcres),
              SuitabilitySum = sum(SuitabilitySum),
              LandscapeMean = weighted.mean(LandscapeMean, FieldAreaAcres))
  
  # Export
  bid_file <- file.path(output_dir, "03_prediction_summary_bid.csv")
  if (file.exists(bid_file) & overwrite != TRUE) {
    message_ts("File already exists and overwrite != TRUE. Moving to next...")
  } else {
    write.csv(bid_df, bid_file, row.names = FALSE)
    message_ts("Exported.")
    processed_files <- c(processed_files, bid_file)
  }
  
  # Combine by month
  bid_cmb_df <- bid_df %>%
    group_by(across(all_of(c("BidID", "FieldsIncluded", "AreaAcres", "PricePerAc", 
                             "FloodDateStart", "FloodDateEnd", "Species", other_cols)))) %>%
    summarise(PredictionYear = "Combined", PredictionMonth = "Combined", BidLength = sum(DaysOverlap),
              SuitMean = weighted.mean(SuitabilityMean, DaysOverlap),
              SuitSum = sum(SuitabilityMean * DaysOverlap * AreaAcres), LandscapeMean = weighted.mean(LandscapeMean, DaysOverlap))
  
  # Export
  bid_cmb_file <- file.path(output_dir, "03a_prediction_summary_bid_across_months.csv")
  if (file.exists(bid_cmb_file) & overwrite != TRUE) {
    message_ts("File already exists and overwrite != TRUE. Moving to next...")
  } else {
    write.csv(bid_cmb_df, bid_cmb_file, row.names = FALSE)
    message_ts("Exported.")
    processed_files <- c(processed_files, bid_cmb_file)
  }
  
  # Pivot wider
  bid_wide_df <- bid_cmb_df %>%
    select(-PredictionYear, - PredictionMonth, - LandscapeMean) %>%
    pivot_wider(names_from = Species, values_from = c(SuitMean, SuitSum)) %>%
    mutate(SuitMean_Total = mean(c(SuitMean_AMAV, SuitMean_BNST, SuitMean_DOWI, SuitMean_DUNL)),
           SuitSum_Total = sum(c(SuitSum_AMAV, SuitSum_BNST, SuitSum_DOWI, SuitSum_DUNL)))
  
  # Export
  bid_wide_file <- file.path(output_dir, "03b_prediction_summary_bid_across_months_wide.csv")
  if (file.exists(bid_wide_file) & overwrite != TRUE) {
    message_ts("File already exists and overwrite != TRUE. Moving to next...")
  } else {
    write.csv(bid_wide_df, bid_wide_file, row.names = FALSE)
    message_ts("Exported.")
    processed_files <- c(processed_files, bid_wide_file)
  }
  
  return(processed_files)
  
}
