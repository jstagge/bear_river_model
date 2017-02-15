# *------------------------------------------------------------------
# | PROGRAM NAME: 01_process_littlebear_flows
# | FILE NAME: 01_process_littlebear_flows.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  Download daily flow data for gauges on the East Fork and South Fork
# |				of the Little Bear River, UT. Calculate annual mean flow (cfs) and
# |				annual mean based on water year.
# |
# *------------------------------------------------------------------
# | COMMENTS:               
# |
# |  1:  Water Year is assumed to start in Oct (10)
# |  2:  Data is saved for later use.
# |  3: 
# |*------------------------------------------------------------------
# | DATA USED:               
# | USGS Streamflow gauge data downloaded from web
# |
# |*------------------------------------------------------------------
# | CONTENTS:               
# |
# |  PART 1:  
# |  PART 2: 
# |  PART 3: 
# *-----------------------------------------------------------------
# | UPDATES:               
# |
# |
# *------------------------------------------------------------------
 
### Clear any existing data or functions.
rm(list=ls())

###########################################################################
## Set the Paths
###########################################################################
### Path for Data and Output	
data_path <- "../../data"
output_path <- "../../output"
global_path <- "../global_func"
function_path <- "./functions"

### Set download location
data_path <- file.path(data_path,"little_bear_separation")

### Set global output location
output_path <- file.path(output_path,"little_bear_separation")

output_name <- "monthly_flow"
write_output_path <- file.path(output_path,output_name)

###########################################################################
###  Load functions
###########################################################################
### Load these functions for all code
require(colorout)
require(assertthat)

### Load these functions for this unique project
require(zoo)
require(lubridate)
require(data.table)
require(dataRetrieval)

#source(file.path(global_path,"usgs_month_dl.R"))
source(file.path(global_path,"usgs_daily_dl.R"))
source(file.path(global_path,"usgs_readin.R"))
source(file.path(global_path,"usgs_wateryear.R"))
source(file.path(global_path,"unit_conversions.R"))

###########################################################################
## Set Initial Values
###########################################################################
### Set site data
site_id_list <- c("10104900", "10105000", "10104700", "10104600", "10105900", "10106000")

param_cd <- "00060"


###########################################################################
###  Save Daily Data
###########################################################################
### Loop through all site_ids and save time series data
for (n in seq(1,length(site_id_list))) {
	usgs_daily_dl(site_id_list[n], param_cd, destination_folder=data_path)
}


###########################################################################
###  Read in Daily Data and Process to Monthly and Annual
###########################################################################
### Loop through all site_ids and save time series data
for (n in seq(1,length(site_id_list))) {

### Set site id from list
site_id <- site_id_list[n]
site_info <- readNWISsite(site_id)
site_name <- site_info$station_nm

### Read in observed flows
flow_obs <- usgs_readin(site_id, param_cd=param_cd, destination_folder=data_path)

### Rename columns
colnames(flow_obs)[2] <- "site_id"
flow_obs$site_id <- as.factor(flow_obs$site_id)
colnames(flow_obs)[3] <- "date"
flow_obs$date <- as.Date(flow_obs$date)
colnames(flow_obs)[4] <- "flow_cfs"
colnames(flow_obs)[5] <- "cd"

### Apply a short name
flow_obs$short_name <- as.factor(substr(site_name,1,12))

### Calculate date, month and year
flow_obs$month <- month(flow_obs$date)
flow_obs$year <- year(flow_obs$date)

### Calculate water year
flow_obs$water_year <- usgs_wateryear(year=flow_obs$year, month=flow_obs$month, first_month=wy_first_month)

### Create a datatable with keys to allow for Monthly and Annual mean calculations
flow_obs <- data.table(flow_obs)
setkey(flow_obs, site_id, month, year, water_year)

### Calculate mean monthly flow and then re-sort by year and month
### Assigns a date as the mean day of each month (only to be used for plotting)
flow_obs_monthly <- flow_obs[,list(date=mean(date), monthly_mean_cfs=mean(flow_cfs), site_name=site_name, monthly_n=sum(is.finite(flow_cfs))),by=c("year", "month", "site_id", "water_year")]
flow_obs_monthly <- flow_obs_monthly[order(rank(year), month)]
setkey(flow_obs_monthly, water_year)

	### Calculate mean annual flow based on water year and set water_year as key for sorting and merging
	flow_obs_annual <- flow_obs[,list(wy_mean_cfs=mean(flow_cfs)),by="water_year"]
	flow_obs_annual$site_id <- site_id
	flow_obs_annual$site_name <- site_name
	setkey(flow_obs_annual, water_year)

	### Calculate monthly proportion of annual flow and mean proportion
	flow_obs <-  merge(flow_obs_monthly,flow_obs_annual, by=c("site_id", "site_name", "water_year"), all.x = T)

	### Write to data frame
	write_location <- file.path(write_output_path, paste0(site_id,"_",param_cd,"_mon_wy.csv"))
	write.csv(flow_obs, file = write_location,row.names=FALSE)
}



