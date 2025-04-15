#This function plots a polygon from x and y 
#values. This is useful for drawing standard
#error clouds around lines
#There is no need to reverse vectors. That is
#all handled in this function.

#poly.top.x <- 1:10
#poly.bottom.x <- 1:10
#poly.top.y <- 1:10
#poly.bottom.y <- 0:9

plot.poly.xy <- function(poly.top.x, poly.top.y, poly.bottom.x, poly.bottom.y,
border = NULL, col = "black", lwd = 1, new.plot = FALSE){

    #remove missing and infinite entries
    finite.entries <- Reduce("intersect", 
        list(which(is.finite(poly.top.x)),
             which(is.finite(poly.top.y)),
             which(is.finite(poly.bottom.x)),
             which(is.finite(poly.bottom.y))))
    
    poly.top.x <- poly.top.x[finite.entries]
    poly.top.y <- poly.top.y[finite.entries]
    poly.bottom.x <- poly.bottom.x[finite.entries]
    poly.bottom.y <- poly.bottom.y[finite.entries]

    min.x <- min(c(poly.top.x, poly.bottom.x))
    max.x <- max(c(poly.top.x, poly.bottom.x))
    min.y <- min(c(poly.top.y, poly.bottom.y))
    max.y <- max(c(poly.top.y, poly.bottom.y))

    poly.x <- c(poly.top.x, rev(poly.bottom.x))
    poly.y <- c(poly.top.y, rev(poly.bottom.y))

    if(new.plot){
        plot.new()
        plot.window(xlim = c(min.x, max.x), ylim = c(min.y, max.y))
    }
    polygon(poly.x,poly.y, border = border, col = col, lwd = lwd)

}