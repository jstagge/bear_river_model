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
###   Calculate withdrawals
###########################################################################
### Set up a dataframe to hold results
ut_dwr_forks <- ut_dwr[c(3)]

### Add important columns
ut_dwr_forks$EF_porcupine_release <- ut_dwr$Porcupine_reservoir_outlet
ut_dwr_forks$EF_withdrwl_paradise_highline <- ut_dwr$Highline_canal + ut_dwr$Paradise_canal
ut_dwr_forks$SF_withdrwl_hyrum_canal <- ut_dwr$Hyrum_canal


### Merge these columns with original
flow_final <- merge(flow_by_forks, ut_dwr_forks, by="date", all.x=TRUE)
flow_final  <- flow_final[order(flow_final$date),] 



###########################################################################
###  Save results
###########################################################################
### Save full time series
write.csv(flow_final, file = file.path(write_output_path, "little_bear_separated_flows.csv"),row.names=FALSE)


###########################################################################
###  Plot results as a reference
###########################################################################

### Melt back so that all data is vertical
flow_df <- melt(flow_final, id.vars = c("year", "month", "date"), measure.vars = c("East_Fork", "Little_Bear", "South_Fork", "EF_withdrwl_paradise_highline", "SF_withdrwl_hyrum_canal"))
colnames(flow_df)[4] <- "fork"
colnames(flow_df)[5] <- "monthly_mean_cfs"

## Test plot to check all inserted data
p <- ts_plot(data=flow_df, x_col="date", x_name="Date", y_col="monthly_mean_cfs", y_name="Mean Monthly Flow (cfs)", colour_var="fork", group="fork", plot_pal=scale_colour_manual(name=NULL,  values=c("red", "black", "blue", "green", "purple")))
p <- p + coord_cartesian(xlim=c(as.Date("1960-01-01"), as.Date("2017-01-01")))
p

### Save to pdf and png
ggsave(file.path(write_figure_path, "Little_Bear_ts_by_fork_all.png"), p, width=8, height=4, dpi=600)
ggsave(file.path(write_figure_path, "Little_Bear_ts_by_fork_all.pdf"), p, width=8, height=4)

