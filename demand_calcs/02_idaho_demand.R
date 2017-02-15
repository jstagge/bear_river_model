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
p

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









###########################################################################
###  Process Bench B in same way
###########################################################################
### Name to save
demand_name <- "bench_b"

### Create last chance subset
bench_b_sub <- data.frame(wra_hst_wide[,1:4], flow_cfs=wra_hst_wide$BENCH_B_1_10079800)
head(bench_b_sub)

### Test plot
plot(bench_b_sub$date, bench_b_sub$flow_cfs, type="l")

### Create summary and save
demand_summary <- summary_se(bench_b_sub, measurevar="flow_cfs", groupvars=c("month"), na.rm=TRUE, conf.interval=.95)
demand_summary
write_location <- file.path(write_output_path, paste0(demand_name,"/",demand_name,"_summary.csv"))
write.csv(demand_summary, file = write_location,row.names=FALSE)

### Merge with Wyoming Border flow
wyo_border_df <- data.frame(huc_16010102[,1:2], wyo_border=huc_16010102$usgs_10039500)
bench_b_sub <- merge(bench_b_sub, wyo_border_df, by=c("year", "month"), all.x=TRUE)

### Create a column for results and source
bench_b_sub$estimated <- NA
bench_b_sub$source <- NA
bench_b_sub$source[is.finite(bench_b_sub$flow_cfs)] <- "usgs"

### Loop through Months 4 through 9
for (j in seq(4,9)) {
month_n <- j

### Extract the month
month_test <- bench_b_sub$month == month_n
bench_b_month_n <- bench_b_sub[month_test,]

### Fit GAM model using Wyoming border flows
gam_model <- gam(flow_cfs ~ s(wyo_border),data=bench_b_month_n, na=na.gam.replace)

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
bench_b_month_n$predicted <- predict(gam_model, newdata=bench_b_month_n)
bench_b_sub$estimated[month_test] <- bench_b_month_n$predicted
}

### Add a note for estimated
estimated_test <- is.na(bench_b_sub$flow_cfs) & !is.na(bench_b_sub$estimated)
bench_b_sub$source[estimated_test] <- "estimated"

### Add zeroes outside withdrawal period
no_withdrawal_test <- bench_b_sub$month <= 3 | bench_b_sub$month >= 9
bench_b_sub$estimated[no_withdrawal_test] <- 0
bench_b_sub$source[no_withdrawal_test] <- "outside season"

### Prevent negative values
bench_b_sub$estimated[bench_b_sub$estimated < 0 ] <- 0


###########################################################################
###  Plot Last Chance Canal Time Series
###########################################################################
### Save time series plot
p <- ggplot(bench_b_sub, aes(x=date))
#p <- p + geom_line(aes(y=flow_cfs), colour="green")
p <- p + geom_line(aes(y=estimated), colour="black")
p <- p + geom_line(aes(y=flow_cfs), colour="red")
p <- p + theme_classic_correct(9)
p <- p + scale_y_continuous(name="Flow (cfs)")
p <- p + scale_x_date(name="date", breaks=seq(as.Date("1950-01-01"), as.Date("2020-01-01"), by="5 years"))
p

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
write.csv(bench_b_sub, file = write_location,row.names=FALSE)

### Reformat and save for WEAP
bench_b_weap <- bench_b_sub[(bench_b_sub$water_year >= water_year_range[1] & bench_b_sub$water_year <= water_year_range[2]), ]
bench_b_weap <- data.frame(bench_b_weap[,c(1,2)], bench_b_weap$estimated)

write_location <- file.path(write_output_path, paste0("demand_",demand_name,"_weap.csv"))
write.table( bench_b_weap , file = write_location, sep=",", row.names=FALSE, col.names=FALSE)












###########################################################################
###  Process Gentile Valley in same way
###########################################################################
### Name to save
demand_name <- "gentile"

### Create last chance subset
gentile_flows <- wra_hst_wide$GENTILE_VALLEY_10080105 + wra_hst_wide$WHEELER_PUMP_10098835 +wra_hst_wide$MATHEWS_B_10080385 +wra_hst_wide$SKABELAND_1_10081650 +wra_hst_wide$SKABELUND_10082550

gentile_flows_subset <- wra_hst_wide$GENTILE_VALLEY_10080105 + wra_hst_wide$MATHEWS_B_10080385 + wra_hst_wide$SKABELUND_10082550

### See any difference.  Use the subset, nothing really lost
plot(gentile_flows, type="l")                     
lines(gentile_flows_subset, col="red")


gentile_sub <- data.frame(wra_hst_wide[,1:4], flow_cfs=gentile_flows_subset)
head(gentile_sub)

### Test plot
plot(gentile_sub$date, gentile_sub$flow_cfs, type="l")

### Create summary and save
demand_summary <- summary_se(gentile_sub, measurevar="flow_cfs", groupvars=c("month"), na.rm=TRUE, conf.interval=.95)
demand_summary
write_location <- file.path(write_output_path, paste0(demand_name,"/",demand_name,"_summary.csv"))
write.csv(demand_summary, file = write_location,row.names=FALSE)

### Merge with Wyoming Border flow
wyo_border_df <- data.frame(huc_16010102[,1:2], wyo_border=huc_16010102$usgs_10039500)
gentile_sub <- merge(gentile_sub, wyo_border_df, by=c("year", "month"), all.x=TRUE)

### Create a column for results and source
gentile_sub$estimated <- NA
gentile_sub$source <- NA
gentile_sub$source[is.finite(gentile_sub$flow_cfs)] <- "usgs"

### Loop through Months 4 through 9
for (j in seq(4,9)) {
month_n <- j

### Extract the month
month_test <- gentile_sub$month == month_n
gentile_month_n <- gentile_sub[month_test,]

### Fit GAM model using Wyoming border flows
gam_model <- gam(flow_cfs ~ s(wyo_border),data=gentile_month_n, na=na.gam.replace)

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
gentile_month_n$predicted <- predict(gam_model, newdata=gentile_month_n)
gentile_sub$estimated[month_test] <- gentile_month_n$predicted
}

### Add a note for estimated
estimated_test <- is.na(gentile_sub$flow_cfs) & !is.na(gentile_sub$estimated)
gentile_sub$source[estimated_test] <- "estimated"

### Add zeroes outside withdrawal period
no_withdrawal_test <- gentile_sub$month <= 3 | gentile_sub$month >= 9
gentile_sub$estimated[no_withdrawal_test] <- 0
gentile_sub$source[no_withdrawal_test] <- "outside season"

### Prevent negative values
gentile_sub$estimated[gentile_sub$estimated < 0 ] <- 0


###########################################################################
###  Plot Gentile Valley Time Series
###########################################################################
### Save time series plot
p <- ggplot(gentile_sub, aes(x=date))
#p <- p + geom_line(aes(y=flow_cfs), colour="green")
p <- p + geom_line(aes(y=estimated), colour="black")
p <- p + geom_line(aes(y=flow_cfs), colour="red")
p <- p + theme_classic_correct(9)
p <- p + scale_y_continuous(name="Flow (cfs)")
p <- p + scale_x_date(name="date", breaks=seq(as.Date("1950-01-01"), as.Date("2020-01-01"), by="5 years"))
p

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
write.csv(gentile_sub, file = write_location,row.names=FALSE)

### Reformat and save for WEAP
gentile_weap <- gentile_sub[(gentile_sub$water_year >= water_year_range[1] & gentile_sub$water_year <= water_year_range[2]), ]
gentile_weap <- data.frame(gentile_weap[,c(1,2)], gentile_weap$estimated)

write_location <- file.path(write_output_path, paste0("demand_",demand_name,"_weap.csv"))
write.table( gentile_weap , file = write_location, sep=",", row.names=FALSE, col.names=FALSE)







###########################################################################
###  Process west_cache Valley in same way
###########################################################################
### Name to save
demand_name <- "west_cache"

### Create west cache subset
west_cache_other_flows <- rowSums( cbind (wra_hst_wide$WEST_CACHE_4_10098804, wra_hst_wide$WEST_CACHE_NO_2_10092900, wra_hst_wide$RIVERDALE_IRR_10089950, wra_hst_wide$NELSON_10089850), na.rm=TRUE)


west_cache_flows <- wra_hst_wide$WEST_CACHE_10090250 + west_cache_other_flows 

west_cache_sub <- data.frame(wra_hst_wide[,1:4], flow_cfs=west_cache_flows)
head(west_cache_sub)

### Test plot
plot(west_cache_sub$date, west_cache_sub$flow_cfs, type="l")

### Create summary and save
demand_summary <- summary_se(west_cache_sub, measurevar="flow_cfs", groupvars=c("month"), na.rm=TRUE, conf.interval=.95)
demand_summary
write_location <- file.path(write_output_path, paste0(demand_name,"/",demand_name,"_summary.csv"))
write.csv(demand_summary, file = write_location,row.names=FALSE)

### Merge with Wyoming Border flow
wyo_border_df <- data.frame(huc_16010102[,1:2], wyo_border=huc_16010102$usgs_10039500)
west_cache_sub <- merge(west_cache_sub, wyo_border_df, by=c("year", "month"), all.x=TRUE)

### Create a column for results and source
west_cache_sub$estimated <- NA
west_cache_sub$source <- NA
west_cache_sub$source[is.finite(west_cache_sub$flow_cfs)] <- "usgs"

### Loop through Months 4 through 9
for (j in seq(4,9)) {
month_n <- j

### Extract the month
month_test <- west_cache_sub$month == month_n
west_cache_month_n <- west_cache_sub[month_test,]

### Fit GAM model using Wyoming border flows
gam_model <- gam(flow_cfs ~ s(wyo_border),data=west_cache_month_n, na=na.gam.replace)

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
west_cache_month_n$predicted <- predict(gam_model, newdata=west_cache_month_n)
west_cache_sub$estimated[month_test] <- west_cache_month_n$predicted
}

### Add a note for estimated
estimated_test <- is.na(west_cache_sub$flow_cfs) & !is.na(west_cache_sub$estimated)
west_cache_sub$source[estimated_test] <- "estimated"

### Add zeroes outside withdrawal period
no_withdrawal_test <- west_cache_sub$month <= 3 | west_cache_sub$month >= 9
west_cache_sub$estimated[no_withdrawal_test] <- 0
west_cache_sub$source[no_withdrawal_test] <- "outside season"

### Prevent negative values
west_cache_sub$estimated[west_cache_sub$estimated < 0 ] <- 0


###########################################################################
###  Plot west_cache Valley Time Series
###########################################################################
### Save time series plot
p <- ggplot(west_cache_sub, aes(x=date))
#p <- p + geom_line(aes(y=flow_cfs), colour="green")
p <- p + geom_line(aes(y=estimated), colour="black")
p <- p + geom_line(aes(y=flow_cfs), colour="red")
p <- p + theme_classic_correct(9)
p <- p + scale_y_continuous(name="Flow (cfs)")
p <- p + scale_x_date(name="date", breaks=seq(as.Date("1950-01-01"), as.Date("2020-01-01"), by="5 years"))
p

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
write.csv(west_cache_sub, file = write_location,row.names=FALSE)

### Reformat and save for WEAP
west_cache_weap <- west_cache_sub[(west_cache_sub$water_year >= water_year_range[1] & west_cache_sub$water_year <= water_year_range[2]), ]
west_cache_weap <- data.frame(west_cache_weap[,c(1,2)], west_cache_weap$estimated)

write_location <- file.path(write_output_path, paste0("demand_",demand_name,"_weap.csv"))
write.table( west_cache_weap , file = write_location, sep=",", row.names=FALSE, col.names=FALSE)







###########################################################################
###  Process preston in same way
###########################################################################
### Name to save
demand_name <- "preston"

### Create preston subset
preston_flows <- wra_hst_wide$RIVERDALE_IRR_10089950 + wra_hst_wide$NELSON_10089850

preston_sub <- data.frame(wra_hst_wide[,1:4], flow_cfs=preston_flows)
head(preston_sub)

### Test plot
plot(preston_sub$date, preston_sub$flow_cfs, type="l")

### Create summary and save
demand_summary <- summary_se(preston_sub, measurevar="flow_cfs", groupvars=c("month"), na.rm=TRUE, conf.interval=.95)
demand_summary
write_location <- file.path(write_output_path, paste0(demand_name,"/",demand_name,"_summary.csv"))
write.csv(demand_summary, file = write_location,row.names=FALSE)


###########################################################################
###  Manually create demand
###########################################################################
demand_manual <- c(0,0,0,1,10,19,19,19,15,0,0,0)

### Create a column for results and source
preston_sub$estimated <- NA
preston_sub$source <- NA
preston_sub$source[is.finite(preston_sub$flow_cfs)] <- "usgs"

### Loop through Months 4 through 9
for (j in seq(1,12)) {
month_n <- j

### Extract the month
month_test <- preston_sub$month == month_n

### Save the predicted values
preston_sub$estimated[month_test] <- demand_manual[month_n]
}

### Add a note for estimated
preston_sub$source[estimated_test] <- "estimated"

###########################################################################
###  Plot preston Valley Time Series
###########################################################################
### Save time series plot
p <- ggplot(preston_sub, aes(x=date))
#p <- p + geom_line(aes(y=flow_cfs), colour="green")
p <- p + geom_line(aes(y=estimated), colour="black")
p <- p + geom_line(aes(y=flow_cfs), colour="red")
p <- p + theme_classic_correct(9)
p <- p + scale_y_continuous(name="Flow (cfs)")
p <- p + scale_x_date(name="date", breaks=seq(as.Date("1950-01-01"), as.Date("2020-01-01"), by="5 years"))
p

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
write.csv(preston_sub, file = write_location,row.names=FALSE)

### Reformat and save for WEAP
preston_weap <- preston_sub[(preston_sub$water_year >= water_year_range[1] & preston_sub$water_year <= water_year_range[2]), ]
preston_weap <- data.frame(preston_weap[,c(1,2)], preston_weap$estimated)

write_location <- file.path(write_output_path, paste0("demand_",demand_name,"_weap.csv"))
write.table( preston_weap , file = write_location, sep=",", row.names=FALSE, col.names=FALSE)







###########################################################################
###  Process montpelier in same way
###########################################################################
### Name to save
demand_name <- "montpelier"

### Create montpelier subset
montpelier_flows <- wra_hst_wide$PRESTONMONTPELIE_10044450

montpelier_sub <- data.frame(wra_hst_wide[,1:4], flow_cfs=montpelier_flows)
head(montpelier_sub)

### Test plot
plot(montpelier_sub$date, montpelier_sub$flow_cfs, type="l")

### Create summary and save
demand_summary <- summary_se(montpelier_sub, measurevar="flow_cfs", groupvars=c("month"), na.rm=TRUE, conf.interval=.95)
demand_summary
write_location <- file.path(write_output_path, paste0(demand_name,"/",demand_name,"_summary.csv"))
write.csv(demand_summary, file = write_location,row.names=FALSE)

### Merge with Wyoming Border flow
wyo_border_df <- data.frame(huc_16010102[,1:2], wyo_border=huc_16010102$usgs_10039500)
montpelier_sub <- merge(montpelier_sub, wyo_border_df, by=c("year", "month"), all.x=TRUE)

### Create a column for results and source
montpelier_sub$estimated <- NA
montpelier_sub$source <- NA
montpelier_sub$source[is.finite(montpelier_sub$flow_cfs)] <- "usgs"

### Loop through Months 4 through 9
for (j in seq(4,9)) {
month_n <- j

### Extract the month
month_test <- montpelier_sub$month == month_n
montpelier_month_n <- montpelier_sub[month_test,]

### Fit GAM model using Wyoming border flows
gam_model <- gam(flow_cfs ~ s(wyo_border),data=montpelier_month_n, na=na.gam.replace)

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
montpelier_month_n$predicted <- predict(gam_model, newdata=montpelier_month_n)
montpelier_sub$estimated[month_test] <- montpelier_month_n$predicted
}

### Add a note for estimated
estimated_test <- is.na(montpelier_sub$flow_cfs) & !is.na(montpelier_sub$estimated)
montpelier_sub$source[estimated_test] <- "estimated"

### Add zeroes outside withdrawal period
no_withdrawal_test <- montpelier_sub$month <= 3 | montpelier_sub$month >= 9
montpelier_sub$estimated[no_withdrawal_test] <- 0
montpelier_sub$source[no_withdrawal_test] <- "outside season"

### Prevent negative values
montpelier_sub$estimated[montpelier_sub$estimated < 0 ] <- 0


###########################################################################
###  Plot montpelier Valley Time Series
###########################################################################
### Save time series plot
p <- ggplot(montpelier_sub, aes(x=date))
#p <- p + geom_line(aes(y=flow_cfs), colour="green")
p <- p + geom_line(aes(y=estimated), colour="black")
p <- p + geom_line(aes(y=flow_cfs), colour="red")
p <- p + theme_classic_correct(9)
p <- p + scale_y_continuous(name="Flow (cfs)")
p <- p + scale_x_date(name="date", breaks=seq(as.Date("1950-01-01"), as.Date("2020-01-01"), by="5 years"))
p

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
write.csv(montpelier_sub, file = write_location,row.names=FALSE)

### Reformat and save for WEAP
montpelier_weap <- montpelier_sub[(montpelier_sub$water_year >= water_year_range[1] & montpelier_sub$water_year <= water_year_range[2]), ]
montpelier_weap <- data.frame(montpelier_weap[,c(1,2)], montpelier_weap$estimated)

write_location <- file.path(write_output_path, paste0("demand_",demand_name,"_weap.csv"))
write.table( montpelier_weap , file = write_location, sep=",", row.names=FALSE, col.names=FALSE)





###########################################################################
###  Process dingle in same way
###########################################################################
### Name to save
demand_name <- "dingle"

### Create dingle subset
dingle_flows <- wra_hst_wide$BLACK_OTTER_10044200 + wra_hst_wide$DINGLE_10044060 + wra_hst_wide$REAMCROCKETT_10044070 

dingle_sub <- data.frame(wra_hst_wide[,1:4], flow_cfs=dingle_flows)
head(dingle_sub)

### Test plot
plot(dingle_sub$date, dingle_sub$flow_cfs, type="l")

### Create summary and save
demand_summary <- summary_se(dingle_sub, measurevar="flow_cfs", groupvars=c("month"), na.rm=TRUE, conf.interval=.95)
demand_summary
write_location <- file.path(write_output_path, paste0(demand_name,"/",demand_name,"_summary.csv"))
write.csv(demand_summary, file = write_location,row.names=FALSE)

### Merge with Wyoming Border flow
wyo_border_df <- data.frame(huc_16010102[,1:2], wyo_border=huc_16010102$usgs_10039500)
dingle_sub <- merge(dingle_sub, wyo_border_df, by=c("year", "month"), all.x=TRUE)

### Create a column for results and source
dingle_sub$estimated <- NA
dingle_sub$source <- NA
dingle_sub$source[is.finite(dingle_sub$flow_cfs)] <- "usgs"

### Loop through Months 4 through 9
for (j in seq(4,9)) {
month_n <- j

### Extract the month
month_test <- dingle_sub$month == month_n
dingle_month_n <- dingle_sub[month_test,]

### Fit GAM model using Wyoming border flows
gam_model <- gam(flow_cfs ~ s(wyo_border),data=dingle_month_n, na=na.gam.replace)

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
dingle_month_n$predicted <- predict(gam_model, newdata=dingle_month_n)
dingle_sub$estimated[month_test] <- dingle_month_n$predicted
}

### Add a note for estimated
estimated_test <- is.na(dingle_sub$flow_cfs) & !is.na(dingle_sub$estimated)
dingle_sub$source[estimated_test] <- "estimated"

### Add zeroes outside withdrawal period
no_withdrawal_test <- dingle_sub$month <= 3 | dingle_sub$month >= 9
dingle_sub$estimated[no_withdrawal_test] <- 0
dingle_sub$source[no_withdrawal_test] <- "outside season"

### Prevent negative values
dingle_sub$estimated[dingle_sub$estimated < 0 ] <- 0


###########################################################################
###  Plot dingle Time Series
###########################################################################
### Save time series plot
p <- ggplot(dingle_sub, aes(x=date))
#p <- p + geom_line(aes(y=flow_cfs), colour="green")
p <- p + geom_line(aes(y=estimated), colour="black")
p <- p + geom_line(aes(y=flow_cfs), colour="red")
p <- p + theme_classic_correct(9)
p <- p + scale_y_continuous(name="Flow (cfs)")
p <- p + scale_x_date(name="date", breaks=seq(as.Date("1950-01-01"), as.Date("2020-01-01"), by="5 years"))
p

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
write.csv(dingle_sub, file = write_location,row.names=FALSE)

### Reformat and save for WEAP
dingle_weap <- dingle_sub[(dingle_sub$water_year >= water_year_range[1] & dingle_sub$water_year <= water_year_range[2]), ]
dingle_weap <- data.frame(dingle_weap[,c(1,2)], dingle_weap$estimated)

write_location <- file.path(write_output_path, paste0("demand_",demand_name,"_weap.csv"))
write.table( dingle_weap , file = write_location, sep=",", row.names=FALSE, col.names=FALSE)












###########################################################################
###  Process west_fork in same way
###########################################################################
### Name to save
demand_name <- "west_fork"

### Create west_fork subset
west_fork_flows <- wra_hst_wide$WEST_FORK_10045800

west_fork_sub <- data.frame(wra_hst_wide[,1:4], flow_cfs=west_fork_flows)
head(west_fork_sub)

### Test plot
plot(west_fork_sub$date, west_fork_sub$flow_cfs, type="l")

### Create summary and save
demand_summary <- summary_se(west_fork_sub, measurevar="flow_cfs", groupvars=c("month"), na.rm=TRUE, conf.interval=.95)
demand_summary
write_location <- file.path(write_output_path, paste0(demand_name,"/",demand_name,"_summary.csv"))
write.csv(demand_summary, file = write_location,row.names=FALSE)

### Merge with Wyoming Border flow
wyo_border_df <- data.frame(huc_16010102[,1:2], wyo_border=huc_16010102$usgs_10039500)
west_fork_sub <- merge(west_fork_sub, wyo_border_df, by=c("year", "month"), all.x=TRUE)

### Create a column for results and source
west_fork_sub$estimated <- NA
west_fork_sub$source <- NA
west_fork_sub$source[is.finite(west_fork_sub$flow_cfs)] <- "usgs"

### Loop through Months 4 through 9
for (j in seq(4,9)) {
month_n <- j

### Extract the month
month_test <- west_fork_sub$month == month_n
west_fork_month_n <- west_fork_sub[month_test,]

### Fit GAM model using Wyoming border flows
gam_model <- gam(flow_cfs ~ s(wyo_border),data=west_fork_month_n, na=na.gam.replace)

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
west_fork_month_n$predicted <- predict(gam_model, newdata=west_fork_month_n)
west_fork_sub$estimated[month_test] <- west_fork_month_n$predicted
}

### Add a note for estimated
estimated_test <- is.na(west_fork_sub$flow_cfs) & !is.na(west_fork_sub$estimated)
west_fork_sub$source[estimated_test] <- "estimated"

### Add zeroes outside withdrawal period
no_withdrawal_test <- west_fork_sub$month <= 3 | west_fork_sub$month >= 9
west_fork_sub$estimated[no_withdrawal_test] <- 0
west_fork_sub$source[no_withdrawal_test] <- "outside season"

### Prevent negative values
west_fork_sub$estimated[west_fork_sub$estimated < 0 ] <- 0


###########################################################################
###  Plot west_fork Time Series
###########################################################################
### Save time series plot
p <- ggplot(west_fork_sub, aes(x=date))
#p <- p + geom_line(aes(y=flow_cfs), colour="green")
p <- p + geom_line(aes(y=estimated), colour="black")
p <- p + geom_line(aes(y=flow_cfs), colour="red")
p <- p + theme_classic_correct(9)
p <- p + scale_y_continuous(name="Flow (cfs)")
p <- p + scale_x_date(name="date", breaks=seq(as.Date("1950-01-01"), as.Date("2020-01-01"), by="5 years"))
p

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
write.csv(west_fork_sub, file = write_location,row.names=FALSE)

### Reformat and save for WEAP
west_fork_weap <- west_fork_sub[(west_fork_sub$water_year >= water_year_range[1] & west_fork_sub$water_year <= water_year_range[2]), ]
west_fork_weap <- data.frame(west_fork_weap[,c(1,2)], west_fork_weap$estimated)

write_location <- file.path(write_output_path, paste0("demand_",demand_name,"_weap.csv"))
write.table( west_fork_weap , file = write_location, sep=",", row.names=FALSE, col.names=FALSE)


