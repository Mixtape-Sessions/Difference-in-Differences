library(readstata13)
library(ggplot2)
library(fixest) # Sun & Abraham 

castle <- data.frame(read.dta13('https://github.com/scunning1975/mixtape/raw/master/castle.dta'))
castle$treated <- ifelse(is.na(castle$eff), 0, 1) # create a treatment variable

# TWFE Event Study 
res_naive = feols(l_homicide ~ i(treated, time_til, ref = -1, ref=-4) | sid + year, castle)
summary(res_naive)

# Sun and Abraham
res_cohort = feols(l_homicide ~ i(time_til, f2=effyear, drop = -1) | sid + year, castle)
summary(res_cohort)

# get S&A aggregate estimate
agg_coef = aggregate(res_cohort, "(time_til)::(-?[[:digit:]])") # get the aggregate estimate

# Plot the two types of estimates
{coefplot(res_naive, ylim = c(-3, 2), main = 'Effect on log(homicide)', xlab = 'Year since Treatment')
x = c(-9:-2, 0:5) + .1
points(x, agg_coef[, 1], pch = 17, col = 4)
ci_low = agg_coef[, 1] - 1.96 * agg_coef[, 2]
ci_up = agg_coef[, 1] + 1.96 * agg_coef[, 2]
segments(x0 = x, y0 = ci_low, x1 = x, y1 = ci_up, col = 4)
legend("topleft", col = c(1, 4), pch = c(20, 17), cex = 0.8, legend = c("TWFE", "Sun & Abraham"))}

