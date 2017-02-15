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
data_path <- "../../../data"
output_path <- "../../../output"
global_path <- "../../global_func"
function_path <- "../functions"

### Set download location
data_path <- file.path(data_path,"usgs_flow_data")

### Set global output location
output_path <- file.path(output_path,"usgs_flow_data")

output_name <- "monthly"
write_output_path <- file.path(output_path,output_name)

#data_path <- "/run/media/jhstagge/Data/Documents/work/data/usgs_flow_data"

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
require(ggplot2)

#
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source)

#
file.sources = list.files(global_path, pattern="*.R", recursive=TRUE)
sapply(file.path(global_path, file.sources),source)



###########################################################################
## Set Initial Values
###########################################################################
param_cd <- "00060"
wy_first_month <- 10


###########################################################################
## Read in sites
###########################################################################
site <- readNWISdata(bBox=c(-113,41,-110,43), parameterCd="00060",
                     service="site", seriesCatalogOutput=TRUE)

### Subset to HUC
huc_test <- site$huc_cd == "16010201" | site$huc_cd == "16010202" | site$huc_cd == "16010203" | site$huc_cd == "16010204" | site$huc_cd == "16010101" | site$huc_cd == "16010102"
site <- site[huc_test, ]


### Set site data
site_id_list <- unique(site$site_no)
site_id_list <- site_id_list[!is.na(site_id_list)]
### This site did not work properly
#site_id_list <- site_id_list[site_id_list != "10019000"]

#site_id_list <- c("10039500")
#, "10105000", "10104700", "10104600", "10105900", "10106000")

###########################################################################
###  Save Daily Data
###########################################################################
### Loop through all site_ids and save time series data
for (n in seq(1,length(site_id_list))) {
	cat(site_id_list[n])
	cat("\n")
	usgs_daily_dl(site_id=site_id_list[n], parameterCd=param_cd, destination_folder = data_path)
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

cat(site_name)
cat("\n")
	
### Read in observed flows
flow_obs <- try(usgs_readin(site_id, param_cd=param_cd, destination_folder=data_path))

### If it is able to read in and the table is longer than 1 row
if (class(flow_obs)!="try-error") {
if (dim(flow_obs)>1) {
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

### Write to data frame
write_location <- file.path(write_output_path, paste0(site_id,"_",param_cd,"_mon.csv"))
write.csv(flow_obs_monthly, file = write_location,row.names=FALSE)
}
}
}





























usgs_daily_dl <- function(site_id, parameterCd = "00060", dest_folder = getwd()) {
	require(dataRetrieval)	
	### Create url for the data location at the USGS website using dataRetrieval package
#	url <- 	constructNWISURL(site_id, parameterCd = parameterCd, startDate = "",
#endDate = "", service="dv", format="tsv")

	### The constructNWISURL command didn't work
	url <- paste0("https://waterservices.usgs.gov/nwis/dv/?site=",site_id,"&format=rdb,1.0&ParameterCd=00060&StatCd=00003&startDT=1851-01-01")

	### Download file with a catch for failures
	dl_location <- file.path(dest_folder, paste0(site_id,"_",param_cd,"_daily.txt"))
	dl_result <- try(download.file(url, destfile = dl_location, method="curl"), silent = TRUE)
	
	### If dl_result works (equals zero) and the first character is a hash, return result.  If not, delete mistake download
	if (dl_result == 0 & readChar(dl_location, 1)=="#") {
			dl_result <- "Success"
		} else {
			### Delete file	and return failure
			file.remove(dl_location)
			dl_result <- "Failure"
		}
	return(dl_result)
	}
