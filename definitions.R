# Defines auction parameters for running an auction
#   file directories, input file characteristics, and model parameters
#
# Change the following parameters as appropriate
#   axn_file - name of the field shapefile
#   axn_dir - path to the auction
#   extra_cols - any additional columns to require in the shapefile and pass along to final stat files
#   bids_to_remove - optional character strings of bids to exclude from processing (TODO: consider adding as column?)
#   cores_max_global - maximum number of cores to use
#   overwrite_global - whether or not to overwrite existing files
#   temp_dir - path to use for writing temporary files, defaults to tempdir()
#   repo_dir - path to the cloned repository; defaults to base_dir/bid-runner-local 
#  
# Point Blue, California Rice Commission

# Auction ----------------------------------------------------------------------
# Name of auction and auction shapefile
# Used as a folder name, so letters, numbers, underscores, and dashes only
auction_id <- "2024-02-B4B" #example format: year-mth-code

# Path to auction files
# By default, place in same directory as this repo (one level up from getwd())
# base_dir isn't used for anything except setting axn_dir, so could skip or set in UI
base_dir <- dirname(getwd()) #point to new location on remote
axn_dir <- file.path(base_dir, auction_id)

# Name and path of the field shapefile specifying the bids to analyze
# Defaults to being stored in axn_dir; change as needed
shp_fn <- "B4B_spring_24_fields_all.shp" #include extension
axn_file <- file.path(axn_dir, shp_fn)

# Required columns in the shapefile
# Don't remove columns from base_cols(without changing code elsewhere), but extra columns can be added
base_cols <- c("BidID", "FieldID", "StartDate", "EndDate", "Split", "AreaAcres", "PricePerAc", "CoverType")
extra_cols <- c()
required_cols <- c(base_cols, extra_cols)

# Bids to remove
# Unique identifiers for bids to remove from consideration
# Leave blank to include all: bids_to_remove <- c()
bids_to_remove <- c() #e.g., paste0("23-FDF-", c(402, 405)) 

# Spatial extent of the fields to process
# Specifies the landsat scene the fields are part of, or 'valley' if multiple scenes
# Leave as 'valley' by default or if unsure; currently scene-specific files are not in GitHub
# Allowed values: 'p44r33' (Sacramento), 'p44r34' (Suisun), 'p43r34' (Delta), 'p42r35' (Tulare)
#                 'valley' (entire CVJV) if multiple
# See map in documentation for details
axn_extent <- "valley"

# Processing parameters --------------------------------------------------------
# Maximum number of cores to use for processing
# Must be an integer less than the number of cores on your machine
# Processing is memory-intensive & your machine is likely to run out of memory before CPU
#   Monitor and set max cores appropriately
cores_max_global <- 4

# Whether or not previously-created outputs in this auction should be overwritten
# Must be TRUE or FALSE
overwrite_global <- FALSE

# Other directory parameters ---------------------------------------------------
# Temporary directory to use for storing temp files
temp_dir <- tempdir() #change if desired

# Directory containing the GitHub repository with the code and data
# Defaults to assuming 'bid-runner-local' cloned to base_dir; adjust as needed
repo_dir <- file.path(base_dir, "bid-runner-local")
