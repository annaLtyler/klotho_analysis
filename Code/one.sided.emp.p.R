one.sided.emp.p <- function(obs, null.dist){
    side1 <- length(which(null.dist >= obs))/length(null.dist)
    side2 <- length(which(null.dist <= obs))/length(null.dist)
    return(c("greater" = side1, "less" = side2))
}