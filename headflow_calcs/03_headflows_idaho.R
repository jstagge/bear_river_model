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
data_path <- file.path(data_path,"bear_river")

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

water_year_range <- c(1967,2007)


###########################################################################
## Read in USGS Flows
###########################################################################
huc_16010101 <- read.csv(file.path(output_path, "headflows/huc_16010101_flow_mon.csv"))
huc_16010102 <- read.csv(file.path(output_path, "headflows/huc_16010102_flow_mon.csv"))
huc_16010201 <- read.csv(file.path(output_path, "headflows/huc_16010201_flow_mon.csv"))
huc_16010202 <- read.csv(file.path(output_path, "headflows/huc_16010202_flow_mon.csv"))



###########################################################################
###  Read in Data
###########################################################################
### Read in wra hst file
wra_hst <- read.csv(file.path(data_path, "wra_hst/WRA_HST_Bear_4-6-2016.csv"))

### Rename flow column
names(wra_hst)[names(wra_hst)=="Flow..cfs."] <- "Flow_cfs"

### Convert date column to date format and create month and year columns
wra_hst$date <- as.Date(wra_hst$DataDate, format = "%m/%d/%Y")
wra_hst$month <- month(wra_hst$date)
wra_hst$year <- year(wra_hst$date)

### Replace spaces and punctuation with underscore in site names
wra_hst$FullName  <- gsub('[[:punct:]]', '', wra_hst$FullName)
wra_hst$FullName  <- gsub('[[:blank:]]+', '_', wra_hst$FullName ) 

### Add a column that combines site name and id
wra_hst$site_name_id <- paste0(wra_hst$FullName,"_",wra_hst$SiteID)

### Check format
head(wra_hst)

### Extract site names
site_names <- unique(wra_hst$FullName)
site_name_id <- unique(wra_hst$site_name_id)

sort(site_name_id)

###########################################################################
###  Reformat table to wide format
###########################################################################

### Re-cast dataframe using month and year along the vertical axis, site along horizontal
### Calculate monthly mean if there are at least 7 days of data
wra_hst_wide <- dcast(wra_hst, month + year ~ site_name_id, value.var="Flow_cfs", mean_if_enough, n=7)


###########################################################################
###  Expand to all possible months in WEAP time frame
###########################################################################
### Create a list with all years from min to max across all sites
all_years <- seq(1966,2017)
all_years <- expand.grid(year=all_years, month=seq(1,12))

### Add the water year and date
all_years$water_year <- usgs_wateryear(year=all_years$year, month=all_years$month, first_month=10)
all_years$date <- mid_month(year=all_years$year, month=all_years$month)

### Extract to water years 1967 to 2017
all_years <- all_years[all_years$water_year >= 1967,]

### Re-Sort the dataframe by date
all_years <- all_years[order(all_years$date),]


###########################################################################
###  Merge data with all month year combos to fill in month gaps
###########################################################################

wra_hst_wide <- merge(all_years, wra_hst_wide, by=c("year", "month"), all.x=TRUE)

### Check result
wra_hst_wide[1:10,1:10]



#Montpelier Creek - 10047000, 10047500
#Georgetown creek - 10069000
#Soda Creek - 10076400, 10076500
#Cottonwood Creek - 10084000, 10084500
#Mink Creek - 10087500, 10089500

#Devil Creek - 10122500, 10123000
#Deep Creek - 10125000
#Wright Creek - 10118500
#Little Malad - 10119000


###########################################################################
## Create Bear River at Border
###########################################################################
headflow_name <-"bear_wyoborder"
### subset flow
flow_df <- data.frame(huc_16010102[,1:4], flow_cfs=huc_16010102$usgs_10039500)
flow_df$source <- NA
flow_df$source[is.finite(flow_df$flow_cfs)] <- "usgs"

### Fill gaps over a maximum distance of 6 months with linear interpolation
interp_ts <- linear_interp(ts=flow_df$flow_cfs, max_gap=6)
flow_df$flow_cfs <- interp_ts$interp_vals
flow_df$source[interp_ts$interp_test] <- "interp"

### Save results
write_location <- file.path(write_output_path, paste0("details/",headflow_name,"_ts.csv"))
write.csv(flow_df, file = write_location,row.names=FALSE)

### Reformat and save for WEAP
flow_df_weap <- flow_df[(flow_df$water_year >= water_year_range[1] & flow_df$water_year <= water_year_range[2]), ]
flow_df_weap <- data.frame(flow_df_weap[,c(1,2)], flow_df_weap$flow_cfs)

write_location <- file.path(write_output_path, paste0("weap/flow_",headflow_name,"_weap.csv"))
write.table( flow_df_weap , file = write_location, sep=",", row.names=FALSE, col.names=FALSE)



###########################################################################
## Create Rainbow Inlet
###########################################################################
headflow_name <-"rainbow_inlet"
### subset flow
flow_df <- data.frame(huc_16010201[,1:4], flow_cfs=huc_16010201$usgs_10046000)
flow_df$source <- NA
flow_df$source[is.finite(flow_df$flow_cfs)] <- "usgs"

### Fill gaps over a maximum distance of 6 months with linear interpolation
interp_ts <- linear_interp(ts=flow_df$flow_cfs, max_gap=6)
flow_df$flow_cfs <- interp_ts$interp_vals
flow_df$source[interp_ts$interp_test] <- "interp"

### Save results
write_location <- file.path(write_output_path, paste0("details/",headflow_name,"_ts.csv"))
write.csv(flow_df, file = write_location,row.names=FALSE)

### Reformat and save for WEAP
flow_df_weap <- flow_df[(flow_df$water_year >= water_year_range[1] & flow_df$water_year <= water_year_range[2]), ]
plot(as.Date(flow_df_weap$date), flow_df_weap$flow_cfs, type="l")
flow_df_weap <- data.frame(flow_df_weap[,c(1,2)], flow_df_weap$flow_cfs)

write_location <- file.path(write_output_path, paste0("weap/flow_",headflow_name,"_weap.csv"))
write.table( flow_df_weap , file = write_location, sep=",", row.names=FALSE, col.names=FALSE)


###########################################################################
## Create Bear Lake Outlet
###########################################################################
headflow_name <-"bear_lake_outlet"
### subset flow
flow_df <- data.frame(huc_16010201[,1:4], flow_cfs=huc_16010201$usgs_10059500)
flow_df$source <- NA
flow_df$source[is.finite(flow_df$flow_cfs)] <- "usgs"

### Fill in gap using WRA HST
wra_subset <- data.frame(wra_hst_wide[,c(1,2)], flow_wra=wra_hst_wide$BEAR_LAKE_OUTLET_CANAL_AT_DIKE_10059500)
wra_subset <- merge(flow_df, wra_subset, by=c("year", "month"), all.x=TRUE)
missing_test <- is.na(flow_df$source) & !is.na(wra_subset$flow_wra)
flow_df$flow_cfs[missing_test] <- wra_subset$flow_wra[missing_test]
flow_df$source[missing_test] <- "wra"

### Fill gaps over a maximum distance of 6 months with linear interpolation
interp_ts <- linear_interp(ts=flow_df$flow_cfs, max_gap=6)
flow_df$flow_cfs <- interp_ts$interp_vals
flow_df$source[interp_ts$interp_test] <- "interp"

### Save results
write_location <- file.path(write_output_path, paste0("details/",headflow_name,"_ts.csv"))
write.csv(flow_df, file = write_location,row.names=FALSE)

### Reformat and save for WEAP
flow_df_weap <- flow_df[(flow_df$water_year >= water_year_range[1] & flow_df$water_year <= water_year_range[2]), ]
plot(as.Date(flow_df_weap$date), flow_df_weap$flow_cfs, type="l")
flow_df_weap <- data.frame(flow_df_weap[,c(1,2)], flow_df_weap$flow_cfs)

write_location <- file.path(write_output_path, paste0("weap/flow_",headflow_name,"_weap.csv"))
write.table( flow_df_weap , file = write_location, sep=",", row.names=FALSE, col.names=FALSE)





