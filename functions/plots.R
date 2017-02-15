# *------------------------------------------------------------------
# | FUNCTION NAME: ts_plot
# | FILE NAME: plots.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        data - a dataframe
# |                x_col - name of the column in data with values for x
# |     Out:       p - the resulting plot
# | 
# |     Desc:      This function provides a standard time series plot
# |					using ggplot's geom_line. 
# *------------------------------------------------------------------

ts_plot <- function(data, x_col, x_name, y_col, y_name, colour_var, group_var, plot_pal) {
require(ggplot2)

p <- ggplot(data, aes(x=get(x_col), y=get(y_col), colour=get(colour_var), group=get(group_var)))
p <- p + geom_line(size=0.25)
#if (is.null(linetype_var)) {
#p <- p + geom_line(size=0.25)
#} else {
#p <- p + geom_line(size=0.25, aes(linetype=get(linetype_var)))
#}
p <- p + theme_classic_correct()
p <- p + plot_pal
p <- p + scale_x_date(name=x_name, breaks=seq(as.Date("1900-01-01"), as.Date("2020-01-01"), by="10 years"), date_labels = "%Y")
p <- p + scale_y_continuous(name=y_name)
p <- p +  theme(legend.position="bottom")

return(p)
}





# *------------------------------------------------------------------
# | FUNCTION NAME: linear_fit_facet_plot
# | FILE NAME: plots.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *------------------------------------------------------------------
# | Parameter:
# |     In:        data - a dataframe
# |                x_col - name of the column in data with values for x
# |     Out:       p - the resulting plot
# | 
# |     Desc:      This function provides a standard time series plot
# |					using ggplot's geom_line. 
# *------------------------------------------------------------------

linear_fit_facet_plot <- function(data, x_col, x_name, y_col, y_name, x_pos, y_pos) {

require(ggplot2)

p <- ggplot(data, aes(x=get(x_col), y=get(y_col)))
p <- p + geom_point()
p <- p + geom_smooth(method=lm)
p <- p + theme_bw(9)
p <- p + scale_x_continuous(name=x_name) + scale_y_continuous(name=y_name)

### Create a plot with same scales
p_fixed <- p + facet_wrap(~ month, ncol = 4)
p_fixed <- p_fixed + stat_smooth_func(geom="text",method="lm",xpos = x_pos, ypos = y_pos, hjust=0,parse=TRUE, size=2.5)

### Create a plot with free scales
p_free <- p + facet_wrap(~ month, ncol = 4, scale="free")
p_free <- p_free + stat_smooth_func(geom="text",method="lm", hjust=0,parse=TRUE, size=2.5)

return(list(fixed_scale=p_fixed , free_scale=p_free))
}




