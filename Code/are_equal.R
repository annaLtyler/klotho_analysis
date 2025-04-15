#This function uses values and errors to calculate 
#whether values are sufficiently different from y = x
#to highlight. It returns the indices of the points
#whose error bars do not cross the y = x line

are_equal <- function(x, x_error, y, y_error){

    min.x <- x - x_error
    max.x <- x + x_error

    min.y <- y - y_error
    max.y <- y + y_error

    x.mat = cbind(min.x, max.x)
    y.mat = cbind(min.y, max.y)

    x.overlap <- y.overlap <- rep(NA, nrow(x.mat))
    for(i in 1:nrow(x.mat)){
        y.overlap[i] <- segments.overlap(x.mat[i,1], x.mat[i,2], y[i], y[i])
        x.overlap[i] <- segments.overlap(y.mat[i,1], y.mat[i,2], x[i], x[i])
    }


    no.x.overlap <- which(!x.overlap)
    no.y.overlap <- which(!y.overlap)
    neither.overlap = intersect(no.x.overlap, no.y.overlap)
    result <- list("no_x_overlap" = no.x.overlap, "no_y_overlap" = no.y.overlap, 
        "neither_overlap" = neither.overlap)
    return(result)
}
