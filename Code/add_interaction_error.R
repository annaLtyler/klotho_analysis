#This function takes the same arguments as interaction.plot
#and plots error bars over your interaction plot.
#col will be applied to each level of x.factor
#make sure to use the same function as in interaction.plot 
#if error function returns a confidence interval, this function
#uses the max and min as upper and lower values.

add_interaction_error <- function(x.factor, trace.factor, response,
    fun = mean, error.fun = sd, lwd = 1, cols = "black", 
    horiz.segments = TRUE, horiz.width = 0.02, plot.results = TRUE){

        levels.x <- levels(x.factor)
        levels.trace <- levels(trace.factor)
        mfun <- match.fun(fun)
        efun <- match.fun(error.fun)
        if(length(cols) < length(levels.trace)){
            cols <- rep(cols, length(levels.trace))
        }

        min.mat <- max.mat <- matrix(NA, nrow = length(levels.x), ncol = length(levels.trace))
        colnames(min.mat) <- colnames(max.mat) <- levels.trace
        rownames(min.mat) <- rownames(max.mat) <- levels.x

        for(i in 1:length(levels.x)){
            for(j in 1:length(levels.trace)){
                group.int <- intersect(which(x.factor == levels.x[i]), 
                    which(trace.factor == levels.trace[j]))

                y.val <- mfun(response[group.int])
                if(!is.finite(y.val)){next()}

                error.val <- efun(response[group.int])
                
                #hist(response[group.int])
                #abline(v = error.val)
                #abline(v = median(response[group.int]), col = "red")

                if(length(error.val) == 2){ #assume a confidence interval
                    upper.y <- max(error.val)
                    lower.y <- min(error.val)
                }else{
                    upper.y <- y.val+error.val
                    lower.y <- y.val-error.val
                }
                min.mat[i,j] <- lower.y
                max.mat[i,j] <- upper.y

                if(plot.results){
                    segments(x0 = i, y0 = upper.y, y1 = lower.y,
                        col = cols[j], lwd = lwd)
                
                    #add horizontal bars if requested
                    if(horiz.segments){
                        #upper bar    
                        segments(x0 = i-horiz.width, y0 = upper.y, 
                            x1 = i+horiz.width, y1 = upper.y, 
                            col = cols[j], lwd = lwd)

                        segments(x0 = i-horiz.width, y0 = lower.y, 
                            x1 = i+horiz.width, y1 = lower.y, 
                            col = cols[j], lwd = lwd)
                    }
                }
            }
        }

    result <- list("lower_bounds" = min.mat, "upper_bounds" = max.mat)
    invisible(result)

}
