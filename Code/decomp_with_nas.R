# This function does a decomposition on a matrix with NAs
#It clusters the covariance matrix instead.

decomp_with_nas <- function(mat, pc = 2, add.zeros = FALSE, cols = rep("black", nrow(mat)),
  label.points = FALSE){  
  
  if(add.zeros){
    no.na.mat <- mat
    no.na.mat[which(is.na(mat))] <- 0
  }else{
    cov.mat <- cov(t(mat), use = "pairwise.complete.obs")
    no.na.mat <- cov.mat
  }
  no.na.decomp <- plot.decomp(no.na.mat, cols = cols, pc = pc, label.points = label.points)
  invisible(no.na.decomp)
  
}
