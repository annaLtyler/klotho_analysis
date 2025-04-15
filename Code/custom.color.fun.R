# This is an attempt to make a color distribution function
# That works with imageWithText.R. We want a vector that
# goes from the minimum of the distribution to the maximum
# of the distribution with steps that reflect the density
# of the distribution, so if you have a distribution with
# a long tail, the bulk of the values will have light colors
# and the tail will be more strongly colored
# We do this by using the cumulative distribution of the inverse
# of the density of the custom distribution. 
# custom.dist is a vector of values 
# If you want to accentuate the tails of the distribution, put
#in a heavy-tailed distribution. 
#custom.dist <- jitter(rpois(1000, 1), factor = 5); hist(custom.dist)
#custom.dist <- rnorm(1000); hist(custom.dist)

custom.color.fun <- function(custom.dist, num.cols = 256){

    
    min.val <- min(custom.dist, na.rm = TRUE)
    max.val <- max(custom.dist, na.rm = TRUE)

    dist.fun <- ecdf(custom.dist)
    #plot(dist.fun)

	#map the 0-1 interval onto the interval from x.min to x.max
    x <- seq(min.val, max.val, length.out = num.cols)
    emp.y  <- sapply(x, dist.fun)

    target.max <- max(custom.dist, na.rm = TRUE)
    target.min <- min(custom.dist, na.rm = TRUE)
    V <- emp.y
    r.min <- min(V)
    r.max <- max(V)

    #scale to run between min and max of the original distribution
    new.vals <- ((target.max - target.min) * (V - r.min)/(r.max - r.min)) + target.min
    #plot(new.vals)
	return(new.vals)
    
}
