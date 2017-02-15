# *------------------------------------------------------------------
# | FUNCTION NAME: mean_if_enough
# | FILE NAME: mean_if_enough.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        x - original data as a vector
# |                n - minimum number of values that are not NA, defaults to 7
# |
# |     Out:       returns mean or NA if not met
# | 
# |     Desc:      This function is meant to be used with cast, it calcultes the mean
# | 				of a vector only if there are more than n non-NA values
# *------------------------------------------------------------------


mean_if_enough <- function(x, n=7) {

### If then statement to check if non-NA values are more than n
### If true
if(sum(is.finite(x)) >= n){
	return(mean(x, na.rm=TRUE))
### If false
} else {
	return(NA_real_)
}
}

