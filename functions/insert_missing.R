# *------------------------------------------------------------------
# | FUNCTION NAME: insert_missing
# | FILE NAME: insert_missing.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        original_data - dataframe of original time series
# |                original_col - name of the column in original_data with values to be replaced
# |                replace_data - dataframe of replacement time series
# |                original_col - name of the column in replace_data with values to be inserted
# |                source_col - name of the column that holds the key for source
# |                replace_source - signifier for replacement data source
# |     Out:       original_data - the resulting dataframe
# | 
# |     Desc:      This function inserts new time series data from replace_data
# | 				everywhere that data in original_data is a NA. 
# *------------------------------------------------------------------

insert_missing <- function(original_data, original_col, replace_data, replace_col, source_col, replace_source) {

### Extract data to be merged based on date
to_be_merged <- replace_data[,c("date", replace_col)]
colnames(to_be_merged) <- c("date", "merge_col")

### Merge data
merged_data <- merge(original_data, to_be_merged, by="date", all.x=TRUE)

### Create a T/F test everywhere there is an NA in original_col
original_ts <- original_data[,c(original_col)]
merge_ts <- merged_data[,c("merge_col")]
source_ts <- original_data[,c(source_col)]

insert_test <- is.na(original_ts) & !is.na(merge_ts)

### Insert new data and source information
original_ts[insert_test] <- merge_ts[insert_test]
source_ts[insert_test] <- replace_source

### Reinsert new time series to original data and insert note about data source
original_data[,c(original_col)] <- original_ts
original_data[,c(source_col)] <- source_ts


return(original_data)
}

