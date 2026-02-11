#bootstraps a confidence level for the given statistic
#using the given number of samples.
#can be used with add_interaction_error to add error
#bars to an interaction plot.
#we bootstrap a statistic that should have a normal distribution
#to calculate the confidence intervals for a normally distribued
#variable (I think).

conf_int <- function(vals, conf.level = 95, n.samp = 1000, fun = mean){
    
    mfun <- match.fun(fun)    
    lp <- (100-(conf.level))/200
    up = 1-lp
    boot_stat <- replicate(n.samp, mfun(sample(vals, replace = TRUE)))
    ci <- quantile(boot_stat, c(lp, up))
    return(ci)

}