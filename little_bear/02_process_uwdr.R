# *------------------------------------------------------------------
# | PROGRAM NAME: 02_process_uwdr
# | FILE NAME: 02_process_uwdr.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  Prepare data downloaded from Utah DWR 
# |
# |
# *------------------------------------------------------------------
# | COMMENTS:               
# |
# |  1:  Water Year is assumed to start in Oct (10)
# |  2:  There appears to be a problem with the Pump Canal site
# |  3: 
# |*------------------------------------------------------------------
# | DATA USED:               
# | DWR data retrieved from http://www.waterrights.utah.gov/cgi-bin/dvrtview.exe?Modinfo=SysStations&SYSTEM_NAME=LITTLE+BEAR+RIVER&RECORD_YEAR=2016
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
write_figure_path <- file.path(output_path,"figures/ut_dwr")

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
require(reshape2)

### Load functions
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source)

#source(file.path(function_path,"plots.R"))
file.sources = list.files(global_path, pattern="*.R", recursive=TRUE)
sapply(file.path(global_path, file.sources),source)

###########################################################################
## Set Initial Values
###########################################################################
wy_first_month <- 10


###########################################################################
###  Read in Daily Utah DWR Data
###########################################################################
### Read in DWR data
ut_dwr_name <- "UT_DWR_flow.csv"
ut_dwr <- read.csv(file.path(data_path,ut_dwr_name), check.names=FALSE)

### Fix column names (deal with spaces and all uppercase)
ut_colnames <- colnames(ut_dwr)

ut_colnames <- gsub(" ", "_", ut_colnames)
ut_colnames <- capwords (ut_colnames, strict=TRUE)
colnames(ut_dwr) <- ut_colnames
colnames(ut_dwr)[[1]] <- "date"

### Convert date to proper date format
ut_dwr$date <- as.Date(ut_dwr$date, format="%m/%d/%Y")

### Plot to verify data is correct
head(ut_dwr)
plot(ut_dwr$date,ut_dwr$East_fork, type="l")


###########################################################################
###  Calculate mean monthly flows
###########################################################################
### Melt dataframe to vertical format
ut_dwr_melt <- melt(data=ut_dwr, id.vars="date", variable.name="site_name")
### Check melt result
head(ut_dwr_melt)

### Calculate date, month and year
ut_dwr_melt$month <- month(ut_dwr_melt$date)
ut_dwr_melt$year <- year(ut_dwr_melt$date)

### Calculate water year
ut_dwr_melt$water_year <- usgs_wateryear(year=ut_dwr_melt$year, month=ut_dwr_melt$month, first_month=wy_first_month)

### Create a datatable with keys to allow for Monthly and Annual mean calculations
ut_dwr_melt <- data.table(ut_dwr_melt)
setkey(ut_dwr_melt, site_name, month, year, water_year)

### Calculate mean monthly flow and then re-sort by year and month
### Assigns a date as the mean day of each month (only to be used for plotting)
ut_dwr_monthly <- ut_dwr_melt[,list(date=mean(date), monthly_mean_cfs=mean(value, na.rm=TRUE), monthly_n=sum(is.finite(value))),by=c("year", "month", "site_name", "water_year")]
ut_dwr_monthly <- ut_dwr_monthly[order(site_name, rank(year), month)]

### Recast to obtain a table with sites along the top
ut_dwr_monthly_table <- recast(ut_dwr_monthly, formula = year + month + date ~ site_name, measure.var="monthly_mean_cfs")

### Save results
write_location <- file.path(write_output_path, "ut_dwr_mon.csv")
write.csv(ut_dwr_monthly_table, file = write_location,row.names=FALSE)
   
###########################################################################
###  Plot results as a check
###########################################################################

### Replot to validate
p <- ts_plot(data=ut_dwr_monthly, x_col="date", x_name="Date", y_col="monthly_mean_cfs", y_name="Mean Monthly Flow (cfs)", colour_var="site_name", group="site_name", plot_pal=scale_colour_manual(name=NULL,  values=iwanthue_13))
p <- p + guides(col = guide_legend(ncol = 3))
p_facet <- p + facet_wrap(~site_name, scales="free_x") +  theme(legend.position="none") + scale_colour_manual(values=rep("black", 13))
p_free <- p_facet + facet_wrap(~site_name, scales="free")
  

### Save plots
ggsave(file.path(write_figure_path, "ut_dwr_ts.png"), p, width=8, height=5, dpi=600)
ggsave(file.path(write_figure_path, "ut_dwr_ts.pdf"), p, width=8, height=5)

ggsave(file.path(write_figure_path, "ut_dwr_ts_zoom.png"), p + coord_cartesian(ylim=c(0,450)), width=8, height=5, dpi=600)
ggsave(file.path(write_figure_path, "ut_dwr_ts_zoom.pdf"), p + coord_cartesian(ylim=c(0,450)), width=8, height=5)

### Save plots
ggsave(file.path(write_figure_path, "ut_dwr_bysite_fixed.png"), p_facet, width=8, height=6, dpi=600)
ggsave(file.path(write_figure_path, "ut_dwr_bysite_fixed.pdf"), p_facet, width=8, height=6)

### Save plots
ggsave(file.path(write_figure_path, "ut_dwr_bysite_free.png"), p_free, width=8, height=6, dpi=600)
ggsave(file.path(write_figure_path, "ut_dwr_bysite_free.pdf"), p_free, width=8, height=6)




