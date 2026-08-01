#This function groups together values of one vector
#based on bins in another vector. 
#breaks is the same as in the function hist(). It
#can either be a positive integer or a vector of 
#values defining break points. This function draws
#barplots showing the mean of the binned values
#with standard errors. 

bin_by_hist <- function(x, y, breaks = 25, plot.result = TRUE,
    plot.type = c("bars", "boxes"),
    axes = TRUE, col = "gray", ylim = NULL){

    xhist <- hist(x, breaks = breaks, plot = FALSE)

    xmids <- xhist$mids
    xbins <- xhist$breaks
    
    yvals <- vector(mode = "list", length = length(xmids))
    for(i in 1:(length(xbins)-1)){
        bin.min <- xbins[i]
        bin.max <- xbins[(i+1)]
        x.idx <- intersect(which(x >= bin.min), which(x < bin.max))
        yvals[[i]] <- y[x.idx]
    }

    if(plot.result){
        ymeans <- sapply(yvals, function(x) mean(x, na.rm = TRUE))
        yse <- sapply(yvals, function(x) sd(x, na.rm = TRUE)/sqrt(length(x)))
        ymax <- max(c(ymeans, max(ymeans+yse, na.rm = TRUE)), na.rm = TRUE)
        if(is.null(ylim)){
            ylim = c(0, ymax)
        }
        if(plot.type == "bars"){
            a <- barplot(ymeans, ylim = ylim, col = col)
            segments(x0 = a, y0 = ymeans - yse, y1 = ymeans + yse)
        if(axes){
            axis(1, at = a-0.5, labels = c(signif(xbins[1:length(xmids)], 2)))
        }
        result <- list("xmids" = xmids, "yvals" = yvals, "xcoord" = a)
        }
        if(plot.type == "boxes"){
            boxplot(yvals, ylim = ylim, col = col)
            result <- list("xmids" = xmids, "yvals" = yvals)
        }
    
    }else{
        result <- list("xmids" = xmids, "yvals" = yvals)
    }


    invisible(result)
}
