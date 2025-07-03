#This function returns a position that is a fraction
#of the way between the min and max determined by frac.

fractional_pos <- function(min.pos, max.pos, frac = 0.1){
    total.dist <- max.pos - min.pos
    pos.shift = total.dist*frac
    frac.pos <- min.pos + pos.shift
    return(frac.pos)
}