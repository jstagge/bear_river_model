# *------------------------------------------------------------------
# | PROGRAM NAME: 02_little_bear_separation
# | FILE NAME: 02_little_bear_separation.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  Separate flows in the East Fork, South Fork and main Stem of the
# |				Little Bear River.  Estimate flows during ungaged periods by 
# |				relationships with gaged locations. Fill in remaining gaps.
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

output_name <- "separated_timeseries"
write_output_path <- file.path(output_path,output_name)
write_figure_path <- file.path(output_path,"figures")

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
require(mgcv)

#source(file.path(global_path,"usgs_readin.R"))
#source(file.path(global_path,"usgs_wateryear.R"))
#source(file.path(global_path,"unit_conversions.R"))
#source(file.path(global_path,"theme_classic_correct.R"))
#source(file.path(global_path,"read_table_wheaders.R"))
#source(file.path(global_path,"mid_month.R"))
#source(file.path(global_path,"linear_interp.R"))
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source)

#source(file.path(function_path,"plots.R"))
file.sources = list.files(global_path, pattern="*.R", recursive=TRUE)
sapply(file.path(global_path, file.sources),source)

### To add equation and R2 to facet plots
### To add equation and R2 to facet plots
library(devtools)
source_gist("524eade46135f6348140")
devtools::source_gist("524eade46135f6348140", filename = "ggplot_smooth_func.R")


###########################################################################
## Set Initial Values
###########################################################################
### Set site data



###########################################################################
###  Read in Data
###########################################################################
### Read in monthly flow and fix data type
flow_file_name <- "little_bear_separated_flows.csv"
flow_by_forks <- read.csv(file.path(write_output_path,flow_file_name))
flow_by_forks$date <- as.Date(flow_by_forks$date)  

### Read in UT DWR data
ut_dwr_name <- "ut_dwr_mon.csv"
ut_dwr <- read.csv(file.path(file.path(output_path,"monthly_flow"),ut_dwr_name))
ut_dwr$date <- as.Date(ut_dwr$date)

### Check that data is read in properly
plot(flow_by_forks$date,flow_by_forks$East_Fork, type="l")
lines(ut_dwr$date,ut_dwr$East_fork, col="red")

plot(flow_by_forks$date,flow_by_forks$Little_Bear, type="l")
lines(ut_dwr$date,ut_dwr$Little_bear_river_at_paradise_usgs, col="red")


###########################################################################
###   Totally new attempt
###########################################################################

### Set up a dataframe to hold results
ut_dwr_forks <- ut_dwr[c(3)]

### Add important columns
ut_dwr_forks$EF_porcupine_release <- ut_dwr$Porcupine_reservoir_outlet
ut_dwr_forks$EF_withdrwl_paradise_highline <- ut_dwr$Highline_canal + ut_dwr$Paradise_canal
ut_dwr_forks$SF_withdrwl_hyrum_canal <- ut_dwr$Hyrum_canal

flow_expand <- merge(flow_by_forks, ut_dwr_forks, by="date", all.x=TRUE)

flow_expand$water_bal_SF <- flow_expand$Little_Bear - flow_expand$EF_porcupine_release + flow_expand$EF_withdrwl_paradise_highline + flow_expand$SF_withdrwl_hyrum_canal

flow_expand$data_SF <- as.character(flow_expand$data_SF)
flow_expand$data_EF <- as.character(flow_expand$data_EF)
flow_expand$data_LB <- as.character(flow_expand$data_LB)

###########################################################################
###  Fit May through September using withdrawals and UT DWR
###########################################################################

new_fit <-  fit_wrapper(data = flow_expand, predictors = c("East_Fork", "Little_Bear", "EF_withdrwl_paradise_highline", "SF_withdrwl_hyrum_canal"), month_loop=seq(5,9), y_name="South_Fork")

### Insert data
insert_test <- is.na(flow_expand$South_Fork) & !is.na(new_fit$estimate)
flow_expand$South_Fork[insert_test] <- new_fit$estimate[insert_test]
flow_expand$data_SF[insert_test] <- "estimated with EF LB EF_withd SF_withd"

### Test plot
plot(flow_expand$date, flow_expand$South_Fork, type="l")


new_fit <-  fit_wrapper(data = flow_expand, predictors = c("East_Fork", "Little_Bear"), month_loop=seq(5,9), y_name="South_Fork")

### Insert data
insert_test <- is.na(flow_expand$South_Fork) & !is.na(new_fit$estimate)
flow_expand$South_Fork[insert_test] <- new_fit$estimate[insert_test]
flow_expand$data_SF[insert_test] <- "estimated with EF LB"


###########################################################################
###  Fit October through April using September and May
###########################################################################
### Create columns for previous September and future May
sep_subset <- subset(flow_expand, month==9)
sep_subset <- sep_subset[,c("water_year", "South_Fork")]
colnames(sep_subset) <- c("water_year", "Prev_Sep")
sep_subset$water_year <- sep_subset$water_year+1

may_subset <- subset(flow_expand, month==5)
may_subset <- may_subset[,c("water_year", "South_Fork")]
colnames(may_subset) <- c("water_year", "Future_May")

flow_expand <- merge(flow_expand, sep_subset, by="water_year", all.x=TRUE)
flow_expand <- merge(flow_expand, may_subset, by="water_year", all.x=TRUE)
flow_expand <- flow_expand[order(flow_expand$date),]

### Fit using other branches and Prev/Future
new_fit <-  fit_wrapper(data = flow_expand, predictors = c("East_Fork", "Little_Bear", "Prev_Sep", "Future_May"), month_loop=c(seq(10,12), seq(1,4)), y_name="South_Fork")

### Insert data
insert_test <- is.na(flow_expand$South_Fork) & !is.na(new_fit$estimate)
flow_expand$South_Fork[insert_test] <- new_fit$estimate[insert_test]
flow_expand$data_SF[insert_test] <- "estimated with EF LB Prev_Sep Future_May"


###########################################################################
###  Fill in, making sure that flow is greater than hyrum canal
###########################################################################
### Use a catch to make sure that South Fork is always greater than Hyrum canal
less_than_test <- !is.na(flow_expand$South_Fork) & !is.na(flow_expand$SF_withdrwl_hyrum_canal) & flow_expand$South_Fork < flow_expand$SF_withdrwl_hyrum_canal
flow_expand$South_Fork[less_than_test] <- flow_expand$SF_withdrwl_hyrum_canal[less_than_test]

plot(flow_expand$date, flow_expand$South_Fork, type="l")
lines(flow_expand$date, flow_expand$South_Fork, col="red")
lines(flow_by_forks$date,flow_by_forks$South_Fork, col="black")
lines(flow_expand$date,flow_expand$SF_withdrwl_hyrum_canal, col="blue")
lines(flow_expand$date,less_than_test*100, col="purple")


###########################################################################
###  Fill gaps by linear interpolation
###########################################################################
### Fill gaps over a maximum distance of 6 months with linear interpolation
interp_ts <- linear_interp(ts=flow_expand$South_Fork, max_gap=6)
### Save results back to dataframe
flow_expand$South_Fork <- interp_ts$interp_vals
flow_expand$data_SF[interp_ts$interp_test] <- "interpolated"

###########################################################################
###  Fill gaps by linear interpolation
###########################################################################

#Problem is transition from 1 to 2 in 2002, 2005
#month_sub <- subset(flow_expand, month==7)
### Check for flow_diff
flow_expand$SF_diff <- c(NA, diff(flow_expand$South_Fork))
### Raise values with a difference less than -6
month_sub <- subset(flow_expand, month==2)
quantile(month_sub$SF_diff[month_sub$data_SF=="gauge"], 0.1, na.rm=TRUE)
drop_test <- flow_expand$month==2 &  flow_expand$SF_diff < -6 & flow_expand$data_SF!="gauge" & !is.na(flow_expand$data_SF)
flow_expand$South_Fork[drop_test] <- flow_expand$South_Fork[drop_test] - flow_expand$SF_diff[drop_test] -6

###########################################################################
###  Re-insert data to original dataframe
###########################################################################
flow_by_forks$South_Fork <- flow_expand$South_Fork
flow_by_forks$data_SF <- flow_expand$data_SF


###########################################################################
###  Save results
###########################################################################
### Write model to RDS file for later
#saveRDS(single_model, file.path(write_output_path, "SF_model_fit.rds"))

### Save full time series
write.csv(flow_by_forks, file = file.path(write_output_path, "little_bear_separated_flows.csv"),row.names=FALSE)


###########################################################################
###  Plot results as a reference
###########################################################################

### Melt back so that all data is vertical
flow_df <- melt(flow_by_forks, id.vars = c("year", "month", "date"), measure.vars = c("East_Fork", "Little_Bear", "South_Fork"))
colnames(flow_df)[4] <- "fork"
colnames(flow_df)[5] <- "monthly_mean_cfs"

### Melt the source column as well
data_df <- melt(flow_by_forks, id.vars = c("year", "month", "date"), measure.vars = c("data_EF", "data_LB", "data_SF"))
flow_df$source <- data_df$value

## Test plot to check all inserted data
p <- ts_plot(data=flow_df, x_col="date", x_name="Date", y_col="monthly_mean_cfs", y_name="Mean Monthly Flow (cfs)", colour_var="fork", group="fork", plot_pal=scale_colour_manual(name=NULL,  values=c("red", "black", "blue")))
p

### Save to pdf and png
ggsave(file.path(write_figure_path, "Little_Bear_ts_by_fork_SF_fixed.png"), p, width=8, height=4, dpi=600)
ggsave(file.path(write_figure_path, "Little_Bear_ts_by_fork_SF_fixed.pdf"), p, width=8, height=4)


