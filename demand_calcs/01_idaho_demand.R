# *------------------------------------------------------------------
# | PROGRAM NAME: 
# | FILE NAME: 
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  
# |		Read in data from the Idaho diversions file, reformat, and export		
# |		for use in Bear River model
# |
# *------------------------------------------------------------------
# | COMMENTS:               
# |
# |  1:  
# |  2:  
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

### Set data location
data_path <- file.path(data_path,"bear_river")

### Set global output location
output_path <- file.path(output_path,"bear_river")

output_name <- "demand"
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
require(ggplot2)
require(reshape2)
require(stringi)
require(gam)

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

huc_16010102 <- read.csv(file.path(output_path, "headflows/huc_16010102_flow_mon.csv"))

### HUC regions
#16010102 WY border
#16010201 near Bear Lake
#16010202 Below Alexander

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

###########################################################################
###  Process Last Chance Canal
###########################################################################
### Name to save
demand_name <- "last_chance"

### Create last chance subset
last_chance_sub <- data.frame(wra_hst_wide[,1:4], flow_cfs=wra_hst_wide$LAST_CHANCE_10079600)
head(last_chance_sub)

### Test plot
plot(last_chance_sub$date, last_chance_sub$flow_cfs, type="l")

### Create summary and save
demand_summary <- summary_se(last_chance_sub, measurevar="flow_cfs", groupvars=c("month"), na.rm=TRUE, conf.interval=.95)
write_location <- file.path(write_output_path, paste0(demand_name,"/",demand_name,"_summary.csv"))
write.csv(demand_summary, file = write_location,row.names=FALSE)

### Merge with Wyoming Border flow
wyo_border_df <- data.frame(huc_16010102[,1:2], wyo_border=huc_16010102$usgs_10039500)
last_chance_sub <- merge(last_chance_sub, wyo_border_df, by=c("year", "month"), all.x=TRUE)

### Create a column for results and source
last_chance_sub$estimated <- NA
last_chance_sub$source <- NA
last_chance_sub$source[is.finite(last_chance_sub$flow_cfs)] <- "usgs"

### Loop through Months 4 through 9
for (j in seq(4,9)) {
month_n <- j

### Extract the month
month_test <- last_chance_sub$month == month_n
last_chance_month_n <- last_chance_sub[month_test,]

### Fit GAM model using Wyoming border flows
gam_model <- gam(flow_cfs ~ s(wyo_border),data=last_chance_month_n, na=na.gam.replace)

### Save fit summary
write_location <- file.path(write_output_path, paste0(demand_name,"/fit"))
fit_summary <- summary(gam_model)
capture.output(fit_summary, file = file.path(write_location, paste0(demand_name,"_fit_month_",month_n,".txt")))

### Save fit figure
png(file.path(write_location, paste0(demand_name,"_fit_month_",month_n,".png")), width=6, height=4.5, units="in", res=300)
par(mar=c(5,4,2,2)+0.1)
plot(gam_model, residuals=TRUE)
dev.off()

### Save the predicted values
last_chance_month_n$predicted <- predict(gam_model, newdata=last_chance_month_n)
last_chance_sub$estimated[month_test] <- last_chance_month_n$predicted
}

### Add a note for estimated
estimated_test <- is.na(last_chance_sub$flow_cfs) & !is.na(last_chance_sub$estimated)
last_chance_sub$source[estimated_test] <- "estimated"

### Add zeroes outside withdrawal period
no_withdrawal_test <- last_chance_sub$month <= 3 | last_chance_sub$month >= 10
last_chance_sub$estimated[no_withdrawal_test] <- 0
last_chance_sub$source[no_withdrawal_test] <- "outside season"

### Prevent negative values
last_chance_sub$estimated[last_chance_sub$estimated < 0 ] <- 0


###########################################################################
###  Plot Last Chance Canal Time Series
###########################################################################
### Save time series plot
p <- ggplot(last_chance_sub, aes(x=date))
#p <- p + geom_line(aes(y=flow_cfs), colour="green")
p <- p + geom_line(aes(y=estimated), colour="black")
p <- p + geom_line(aes(y=flow_cfs), colour="red")
p <- p + theme_classic_correct(9)
p <- p + scale_y_continuous(name="Flow (cfs)")
p <- p + scale_x_date(name="date", breaks=seq(as.Date("1950-01-01"), as.Date("2020-01-01"), by="5 years"))
#p

ggsave(p, paste0(plot_file,".png"), 5, 4)

### Save to pdf and png
plot_file <- file.path(write_output_path, paste0(demand_name,"/",demand_name,"_fit_ts"))
ggsave(paste0(plot_file,".png"), p, width=8, height=4, dpi=600)
ggsave(paste0(plot_file,".pdf"), p, width=8, height=4)
ggsave(paste0(plot_file,".svg"), p, width=8, height=4)


###########################################################################
###  Save Results
###########################################################################
### Save details to folder
write_location <- file.path(write_output_path, paste0(demand_name,"/",demand_name,"_ts.csv"))
write.csv(last_chance_sub, file = write_location,row.names=FALSE)

### Reformat and save for WEAP
last_chance_weap <- last_chance_sub[(last_chance_sub$water_year >= water_year_range[1] & last_chance_sub$water_year <= water_year_range[2]), ]
last_chance_weap <- data.frame(last_chance_weap[,c(1,2)], last_chance_weap$estimated)

write_location <- file.path(write_output_path, paste0("demand_",demand_name,"_weap.csv"))
write.table( last_chance_weap , file = write_location, sep=",", row.names=FALSE, col.names=FALSE)
