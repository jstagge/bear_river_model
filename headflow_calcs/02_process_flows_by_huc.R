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
data_path <- file.path(output_path,"usgs_flow_data/monthly")

### Set global output location
output_path <- file.path(output_path,"bear_river")

output_name <- "headflows"
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
huc_list <- c("16010201", "16010202", "16010203", "16010204", "16010101", "16010102")
param_cd <- "00060"

water_year_range <- c(1967,2017)


###########################################################################
###  Expand to all possible months in WEAP time frame
###########################################################################
### Create a list with all years from min to max across all sites
all_years <- seq(water_year_range[1]-2,water_year_range[2]+2)
all_years <- expand.grid(year=all_years, month=seq(1,12))

### Add the water year and date
all_years$water_year <- usgs_wateryear(year=all_years$year, month=all_years$month, first_month=10)
all_years$date <- mid_month(year=all_years$year, month=all_years$month)

### Extract to water years
all_years <- all_years[all_years$water_year >= water_year_range[1],]
all_years <- all_years[all_years$water_year <= water_year_range[2],]

### Re-Sort the dataframe by date
all_years <- all_years[order(all_years$date),]



###########################################################################
## Read in sites
###########################################################################
site <- readNWISdata(bBox=c(-113,41,-110,43), parameterCd="00060",
                     service="site", seriesCatalogOutput=TRUE)


###########################################################################
## Loop through huc list and combine to make a single dataframe
###########################################################################
for (n in seq(1,length(huc_list))) {

### Subset to huc
huc_id <- huc_list[n]
huc_test <- site$huc_cd == huc_id
site_n <- site[huc_test, ]

site_id_list <- unique(site_n$site_no)
site_id_list <- site_id_list[!is.na(site_id_list)]

### Create a dataframe to hold results
monthly_flow_by_huc <- all_years

###########################################################################
## Loop through each site
###########################################################################
for (j in seq(1, length(site_id_list))) {

### Set site id from list
site_id <- site_id_list[j]
site_info <- readNWISsite(site_id)
site_name <- site_info$station_nm

### Read in flow data
read_location <- file.path(data_path, paste0(site_id,"_",param_cd,"_mon.csv"))
flow_df <- try(read.csv(read_location))

if (class(flow_df) != "try-error") {
### Merge with month dataframe
merge_temp <- merge(all_years, flow_df, by=c("year", "month"), all.x=TRUE)

### Add to the results dataframe, calculate width of datafreame to know where to put column
width_results <- dim(monthly_flow_by_huc)[2]
monthly_flow_by_huc <- cbind(monthly_flow_by_huc, merge_temp$monthly_mean_cfs)
names(monthly_flow_by_huc)[(width_results+1)] <- paste0("usgs_",site_id)
}
}

###########################################################################
## Save results to a csv file
###########################################################################
write_location <- file.path(write_output_path, paste0("huc_",huc_id,"_flow_mon.csv"))
write.csv(monthly_flow_by_huc, file = write_location,row.names=FALSE)

}



