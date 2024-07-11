# bid-runner-local
Analyze bids that flood wetlands or agricultural fields to provide habitat for migrating shorebirds in California's Central Valley 

## Overview
An updated version of bid runner that can analyze auctions in any part of the Central Valley, is optimized for speed, and can be run either in the cloud, on a remote desktop, or locally

## Installation
### Code
Get code from GitHub (repo bid-runner-local hosted at california-rice-commission). Two options:
- Download zip file of code from GitHub: (TO BE CREATED)
- Install Git or GitHub Desktop and clone repo from GitHub: https://github.com/california-rice-commission/bid-runner-local
### Modelling data
Many of the data files used by this project are too large for regular GitHub storage.  There are three potential ways to obtain these files:  
- LFS (do we want to configure this?)
- download the zipped file from GitHub that includes code and data
- download the zipped 'data' file from https://storage.googleapis.com/pointblue-autowater-pub/projects/bid-runner/data.zip

If using a manual download option, the unzipped data files should be placed in pointblue-bid-runner/data (configurable in definitions.R).

#### Downloading archive from 

### Auction data
Bid shapefile 

TODO: copy medata about required format from report

## Setup
1. Choose / create a directory for your project
2. Place code and data files in said directory
3. Create file for auction in said root directory
4. Locate definitions.R, in in the root of the downloaded code repository
5. Open definitions.R using RStudio or another editor and change the following parameters as appropriate:
	- axn_file: name of the field shapefile
	- axn_dir: path to the auction
	- extra_cols: any additional columns to require in the shapefile and pass along to final stat files
	- bids_to_remove: optional character strings of bids to exclude from processing (TODO: consider adding as column?)
	- cores_max_global: maximum number of cores to use
	- overwrite_global: whether or not to overwrite existing files
	- temp_dir: path to use for writing temporary files, defaults to tempdir()
	- repo_dir: path to the cloned repository; defaults to base_dir/bid-runner-local 
6. Save definitions.R (the scripts read from the saved version of the file)

## Running an auction
Scripts to run the auction are in the scripts folder of the repository
1. Run 01_setup.R
	- Checks passed parameters
	- Checks installed packages
	- Checks that all code files are present
	- Checks that all data files are present
	- Checks auction file for required columns, date formats, other things specified in Auction Data above
	- Cleans bid and field names, removing invalid characters and enforcing uniqueness
	- Parses auction shapefile for dates
	- Creates list of flooding areas
2. Run 02_analyze_bids.R
	- This sources 01_setup.R and runs all but the final analysis step of the auction
	- It uses multiple cores and gives robust progress updates
	- Error logs are written to a 'logs' folder of axn_dir (specified in definitions.R)
