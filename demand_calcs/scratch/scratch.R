




write_location, paste0(demand_name,"_fit_ts"))



, width=6, height=4.5, units="in", res=300)

ggsave(p, )





plot(last_chance_sub$date, last_chance_sub$flow_cfs, type="l")
lines(last_chance_sub$date, last_chance_sub$estimated, col="red")





month_n <- 7
wra_hst_wide_month_n <- subset(wra_hst_wide, month==month_n)

attempt <- lm(BEAR_RIVER_AT_ALEXANDER_10079500 ~ LAST_CHANCE_10079600 + year,data= wra_hst_wide_month_n)

summary(lm(BEAR_RIVER_AT_ALEXANDER_10079500 ~ LAST_CHANCE_10079600,data= wra_hst_wide_month_n))


plot(wra_hst_wide_month_n$BEAR_RIVER_AT_ALEXANDER_10079500 ~ wra_hst_wide_month_n$LAST_CHANCE_10079600)

plot(wra_hst_wide_month_n$BEAR_LAKE_OUTLET_CANAL_AT_DIKE_10059500 ~ wra_hst_wide_month_n$LAST_CHANCE_10079600)


bear_list <- c( "BEAR_LAKE_10055500" 
, "BEAR_LAKE_OUTLET_CANAL_AT_DIKE_10059500"
, "BEAR_RIVER_AT_ALEXANDER_10079500"
, "BEAR_RIVER_AT_BORDER_WY_10039500"              
, "BEAR_RIVER_AT_IDUT_STATE_LINE_10092700"       
, "BEAR_RIVER_AT_SODA_SPRINGS_10075000"          
, "BEAR_RIVER_BL_STEWART_DAM_10046500"           
, "BEAR_RIVER_BL_UPL_TAILRACE_AT_ONEIDA_10086500"
)



plot(wra_hst_wide_month_n$BEAR_RIVER_AT_BORDER_WY_10039500, wra_hst_wide_month_n$LAST_CHANCE_10079600)

require(mgcv)
require(gam)

yup <- gam(LAST_CHANCE_10079600 ~ s(BEAR_RIVER_AT_BORDER_WY_10039500), data=wra_hst_wide_month_n, na.action=na.omit)

uhhuh <- gam(wra_hst_wide_month_n$LAST_CHANCE_10079600 ~ s(wra_hst_wide_month_n$BEAR_RIVER_AT_BORDER_WY_10039500), trace=TRUE)
     
     
## see also examples in ?gam.models (e.g. 'by' variables, 
## random effects and tricks for large binary datasets)

library(gam)

month_n <- 8
wra_hst_wide_month_n <- subset(wra_hst_wide, month==month_n)
b <- gam(LAST_CHANCE_10079600 ~ s(BEAR_RIVER_AT_BORDER_WY_10039500),data=wra_hst_wide_month_n, na=na.gam.replace)

summary(b)
plot(b, residuals=TRUE)  ## show partial residuals



wra_hst_wide_month_n <- subset(wra_hst_wide, month==month_n)
b <- gam(LAST_CHANCE_10079600 ~ s(BEAR_RIVER_AT_ALEXANDER_10079500),data=wra_hst_wide_month_n, na=na.gam.replace)

summary(b)
plot(b, residuals=TRUE)  ## show partial residuals




linear_fit_facet_plot(wra_hst_wide_month_n, x_col="BEAR_RIVER_AT_BORDER_WY_10039500", x_name="Border", y_col="LAST_CHANCE_10079600", y_name="Last Chance", x_pos=100, y_pos=100)
     
     
     
#, "BEAR_RIVER_AT_COLLINSTON_10118000"        
#, "BEAR_RIVER_AT_PESCADERO_ID_10068500"   
#, "BEAR_RIVER_BEL_GRACE_DAM_NR_GRACE_ID_10080000"
#, "BEAR_RIVER_NR_CORINNE_10126000" 
#apply(bear_data_only,2,function(x){sum(!is.na(x))})

yup <- list()

for (k in c(1,length(bear_list))) {

f <- paste0("LAST_CHANCE_10079600 ~ ", bear_list[k])

fit_eq <- lm(as.formula(f), data=wra_hst_wide_month_n)

yup[[k]] <- summary(fit_eq)

}






require(leaps)

month_n <- 10 
wra_hst_wide_month_n <- subset(wra_hst_wide, month==month_n)


bear_data_test <- names(wra_hst_wide_month_n) %in% bear_list
bear_data_only <- wra_hst_wide_month_n[,bear_data_test]

complete_df <- data.frame(bear_data_only, wra_hst_wide_month_n$LAST_CHANCE_10079600)
complete_test <- complete.cases(complete_df)


leapmodels <- leaps( x=bear_data_only[complete_test,] , y=wra_hst_wide_month_n$LAST_CHANCE_10079600[complete_test],  method="Cp")#, names=names(bear_data_only))#nvmax=2)


cbind(leapmodels$size,leapmodels$which, leapmodels$Cp)

### Goal is small Mallows Cp
plot(leapmodels$size, leapmodels$Cp)
abline(0,1)
 
4: 5, 1
5: 5, 4
6: 2, 4, 5
7: 4, 2, 8
8: 4, 1, 3
9: 4, 1, 8


Crazy - Bear river at Wyoming!





















plot(wra_hst_wide_month_n$BEAR_RIVER_AT_ALEXANDER_10079500 ~ wra_hst_wide_month_n$LAST_CHANCE_10079600)

plot(wra_hst_wide_month_n$BEAR_LAKE_OUTLET_CANAL_AT_DIKE_10059500 ~ wra_hst_wide_month_n$LAST_CHANCE_10079600)


bear_list <- c( "BEAR_LAKE_10055500" 
, "BEAR_LAKE_OUTLET_CANAL_AT_DIKE_10059500"
, "BEAR_RIVER_AT_ALEXANDER_10079500"
, "BEAR_RIVER_AT_BORDER_WY_10039500"              
, "BEAR_RIVER_AT_IDUT_STATE_LINE_10092700"       
, "BEAR_RIVER_AT_SODA_SPRINGS_10075000"          
, "BEAR_RIVER_BL_STEWART_DAM_10046500"           
, "BEAR_RIVER_BL_UPL_TAILRACE_AT_ONEIDA_10086500"
)



plot(wra_hst_wide_month_n$BEAR_RIVER_AT_BORDER_WY_10039500, wra_hst_wide_month_n$LAST_CHANCE_10079600)

require(mgcv)
require(gam)

yup <- gam(LAST_CHANCE_10079600 ~ s(BEAR_RIVER_AT_BORDER_WY_10039500), data=wra_hst_wide_month_n, na.action=na.omit)

uhhuh <- gam(wra_hst_wide_month_n$LAST_CHANCE_10079600 ~ s(wra_hst_wide_month_n$BEAR_RIVER_AT_BORDER_WY_10039500), trace=TRUE)
     
     
## see also examples in ?gam.models (e.g. 'by' variables, 
## random effects and tricks for large binary datasets)

library(gam)

month_n <- 8
wra_hst_wide_month_n <- subset(wra_hst_wide, month==month_n)
b <- gam(LAST_CHANCE_10079600 ~ s(BEAR_RIVER_AT_BORDER_WY_10039500),data=wra_hst_wide_month_n, na=na.gam.replace)

summary(b)
plot(b, residuals=TRUE)  ## show partial residuals



wra_hst_wide_month_n <- subset(wra_hst_wide, month==month_n)
b <- gam(LAST_CHANCE_10079600 ~ s(BEAR_RIVER_AT_ALEXANDER_10079500),data=wra_hst_wide_month_n, na=na.gam.replace)

summary(b)
plot(b, residuals=TRUE)  ## show partial residuals




linear_fit_facet_plot(wra_hst_wide_month_n, x_col="BEAR_RIVER_AT_BORDER_WY_10039500", x_name="Border", y_col="LAST_CHANCE_10079600", y_name="Last Chance", x_pos=100, y_pos=100)
     
     
     
#, "BEAR_RIVER_AT_COLLINSTON_10118000"        
#, "BEAR_RIVER_AT_PESCADERO_ID_10068500"   
#, "BEAR_RIVER_BEL_GRACE_DAM_NR_GRACE_ID_10080000"
#, "BEAR_RIVER_NR_CORINNE_10126000" 
#apply(bear_data_only,2,function(x){sum(!is.na(x))})

yup <- list()

for (k in c(1,length(bear_list))) {

f <- paste0("LAST_CHANCE_10079600 ~ ", bear_list[k])

fit_eq <- lm(as.formula(f), data=wra_hst_wide_month_n)

yup[[k]] <- summary(fit_eq)

}




























###########################################################################
###  Export
###########################################################################

### Format should be actual year, actual month, flow in cfs
### With no header





###########################################################################
## Set Initial Values
###########################################################################
### Set site data
site_id_list <- c("10104900", "10105000", "10104700", "10104600", "10105900", "10106000")

param_cd <- "00060"


###########################################################################
###  Set up a loop to run through all site_ides and combine into a dataframe
###########################################################################
for (n in seq(1,length(site_id_list))) {

site_id <- site_id_list[n]

###########################################################################
###  Read in Data
###########################################################################

### Read in monthly flow and fix data type
obs_file_name <- paste0(site_id,"_",param_cd,"_mon_wy.csv")
flow_obs <- read.csv(file.path(output_path,paste0("monthly_flow/",obs_file_name)))
flow_obs$date <- as.Date(flow_obs$date)  
flow_obs$site_id <- as.factor(flow_obs$site_id)  
flow_obs$site_name <- as.factor(flow_obs$site_name) 
#head(flow_obs) # Review data frame

###########################################################################
###  Save to dataframe by looping and adding to bottom of data
###########################################################################
if (n ==1) {
	flow_all_sites <- data.frame(flow_obs)
} else {
	flow_all_sites <- rbind(flow_all_sites, data.frame(flow_obs))
}

}


###########################################################################
###  Expand to all possible months
###########################################################################
### Create a list with all years from min to max across all sites
all_poss_years <- seq(min(flow_all_sites$year, na.rm=TRUE), max(flow_all_sites$year, na.rm=TRUE))
all_poss_years <- expand.grid(year=all_poss_years, month=seq(1,12))
### Add the water year and date
all_poss_years$water_year <- usgs_wateryear(year=all_poss_years$year, month=all_poss_years$month, first_month=10)
all_poss_years$date <- mid_month(year=all_poss_years$year, month=all_poss_years$month)
### Re-Sort the dataframe by date
all_poss_years <- all_poss_years[order(all_poss_years$date),]

### Loop through all site ids, expand to months/years
for (n in seq(1,length(site_id_list))) {

site_id <- site_id_list[n]
### Subset to each site ID
flow_obs <- flow_all_sites[flow_all_sites$site_id == site_id, ]
site_id <- flow_obs$site_id[1]
site_name <- unlist(lapply(flow_obs$site_name[1], function(x) paste(strwrap(x,30), collapse="\n")))

### Merge all months and years with the observed flows
flow_expand_temp <- merge(all_poss_years, flow_obs, all.x=TRUE)
### Fill in missing data
flow_expand_temp$site_id <- site_id
flow_expand_temp$site_name <- site_name

###########################################################################
###  Save back to dataframe by looping and adding to bottom of data
###########################################################################
if (n ==1) {
	flow_all_sites_expand <- data.frame(flow_expand_temp)
} else {
	flow_all_sites_expand <- rbind(flow_all_sites_expand, data.frame(flow_expand_temp))
}
}

flow_all_sites <- flow_all_sites_expand

###########################################################################
###  Test plot to make sure everything was read properly
###########################################################################
p <- ts_plot(data=flow_all_sites, x_col="date", x_name="Date", y_col="monthly_mean_cfs", y_name="Mean Monthly Flow (cfs)", colour_var="site_name", group="site_name", plot_pal=scale_colour_brewer(name=NULL, type="qual",  palette="Dark2"))

### Save to pdf and png
ggsave(file.path(write_figure_path, "Little_Bear_ts_by_site.png"), p, width=8, height=4, dpi=600)
ggsave(file.path(write_figure_path, "Little_Bear_ts_by_site.pdf"), p, width=8, height=4)


###########################################################################
###  Create a column that represents the fork of the Little Bear river
###########################################################################
### Create an empty column to hold data
flow_all_sites$fork <- NA

flow_all_sites$fork[flow_all_sites$site_id == "10104900" | flow_all_sites$site_id == "10105000"] <- "East_Fork"
flow_all_sites$fork[flow_all_sites$site_id == "10104700" | flow_all_sites$site_id == "10104600"] <- "South_Fork"
flow_all_sites$fork[flow_all_sites$site_id == "10105900" | flow_all_sites$site_id == "10106000"] <- "Little_Bear"

### Test plot to check forks
p <- ts_plot(data=flow_all_sites, x_col="date", x_name="Date", y_col="monthly_mean_cfs", y_name="Mean Monthly Flow (cfs)", colour_var="fork", group="site_name", plot_pal=scale_colour_manual(name=NULL,  values=c("red", "black", "blue")))

### Save to pdf and png
ggsave(file.path(write_figure_path, "Little_Bear_ts_by_fork.png"), p, width=8, height=4, dpi=600)
ggsave(file.path(write_figure_path, "Little_Bear_ts_by_fork.pdf"), p, width=8, height=4)



###########################################################################
###  Remove Gauge 10104600 completely from the dataframe
###########################################################################
### Create a subset for all rows that don't equal this gauge id
flow_all_sites <- subset(flow_all_sites, site_id != "10104600")
### Re-factor to remove this level from site_id and site_name
flow_all_sites$site_id <- factor(flow_all_sites$site_id)
flow_all_sites$site_name <- factor(flow_all_sites$site_name)

### Replot to ensure
p <- ts_plot(data=flow_all_sites, x_col="date", x_name="Date", y_col="monthly_mean_cfs", y_name="Mean Monthly Flow (cfs)", colour_var="fork", group="site_name", plot_pal=scale_colour_manual(name=NULL,  values=c("red", "black", "blue")))

### Save to pdf and png
ggsave(file.path(write_figure_path, "Little_Bear_ts_by_fork_droppedgauge.png"), p, width=8, height=4, dpi=600)
ggsave(file.path(write_figure_path, "Little_Bear_ts_by_fork_droppedgauge.pdf"), p, width=8, height=4)



###########################################################################
###  Recast so that each fork has a separate column
###########################################################################
### Reorganize into columns
### We know that there are no overlaps, but to be careful, use the mean of each combination
flow_by_forks <- recast(flow_all_sites, year + month + date + water_year ~ fork, measure.var = "monthly_mean_cfs", fun.aggregate=mean, na.rm=TRUE)

### Replot to ensure
p <- ggplot(flow_by_forks, aes(x=date))
p <- p + geom_line(aes(y=Little_Bear), colour="black")
p <- p + geom_line(aes(y=East_Fork), colour="red")
p <- p + geom_line(aes(y=South_Fork), colour="blue")
p <- p + theme_classic_correct()
p

################################################
###  Add a tag to dataframe to explain where data came from
#################################################
### Create empty columns
flow_by_forks$data_EF <- NA
flow_by_forks$data_LB <- NA
flow_by_forks$data_SF <- NA

flow_by_forks$data_EF[is.finite(flow_by_forks$East_Fork)] <- "gauge"
flow_by_forks$data_LB[is.finite(flow_by_forks$Little_Bear)] <- "gauge"
flow_by_forks$data_SF[is.finite(flow_by_forks$South_Fork)] <- "gauge"


################################################
###  Fill in data from UT DWR
#################################################
### Assume that USGS is correct, only fill in with UT DWR data if USGS is not available.

### Read in UT DWR data
ut_dwr_name <- "ut_dwr_mon.csv"
ut_dwr <- read.csv(file.path(file.path(output_path,"monthly_flow"),ut_dwr_name))
ut_dwr$date <- as.Date(ut_dwr$date)

### Fill in East Fork using the UT DWR East Fork site
### Plot to check data
plot(flow_by_forks$date,flow_by_forks$East_Fork, type="l")
lines(ut_dwr$date,ut_dwr$East_fork, col="red")

flow_by_forks <- insert_missing(original_data= flow_by_forks,original_col ="East_Fork", replace_data = ut_dwr, replace_col = "East_fork", source_col = "data_EF", replace_source <- "UT DWR")

### Fill in Little Bear using the UT DWR Little Bear at Paradise
### Plot to check data
plot(flow_by_forks$date,flow_by_forks$Little_Bear, type="l")
lines(ut_dwr$date,ut_dwr$Little_bear_river_at_paradise_usgs, col="red")
### Only place needed is in 2015, 2016
plot(flow_by_forks$date[850:948],flow_by_forks$Little_Bear[850:948], type="l")
lines(ut_dwr$date,ut_dwr$Little_bear_river_at_paradise_usgs, col="red")
lines(flow_by_forks$date,flow_by_forks$Little_Bear, col="black")
### Replace data
flow_by_forks <- insert_missing(original_data= flow_by_forks,original_col ="Little_Bear", replace_data = ut_dwr, replace_col = "Little_bear_river_at_paradise_usgs", source_col = "data_LB", replace_source <- "UT DWR")


### Replot to verify
p <- ggplot(flow_by_forks, aes(x=date))
p <- p + geom_line(aes(y=Little_Bear), colour="black")
p <- p + geom_line(aes(y=East_Fork), colour="red")
p <- p + geom_line(aes(y=South_Fork), colour="blue")
p <- p + theme_classic_correct()
p

### Save to pdf and png
ggsave(file.path(write_figure_path, "Little_Bear_ts_by_fork_ut_dwr_added.png"), p, width=8, height=4, dpi=600)
ggsave(file.path(write_figure_path, "Little_Bear_ts_by_fork_ut_dwr_added.pdf"), p, width=8, height=4)



### Save results to CSV 
write.csv(flow_by_forks, file = file.path(write_output_path, "little_bear_base_flows_by_forks.csv"),row.names=FALSE)

