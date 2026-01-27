#use t-test to test differences between all
#pairs of a matrix.

pairwise_test <- function(mat){
  col.pairs <- pair.matrix(1:ncol(mat))
  all.tests <- vector(mode = "list", length = nrow(col.pairs))
  for(i in 1:nrow(col.pairs)){
    group1 <- col.pairs[i,1]
    group2 <- col.pairs[i,2]
    all.tests[[i]] <- try(t.test(mat[,group1], mat[,group2]), silent = TRUE)
  }
  result <- list("pairs" = col.pairs, "tests" = all.tests)
  return(result)
}
