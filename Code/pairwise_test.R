#use t-test to test differences between all
#pairs of a matrix.

pairwise_test <- function(mat.or.list){

  if(class(mat.or.list)[1] == "list"){
    the.list <- mat.or.list
    el.mat <- pair.matrix(1:length(the.list))
    all.tests <- vector(mode = "list", length = nrow(el.mat))
    for(i in 1:nrow(el.mat)){
      group1 <- el.mat[i,1]
      group2 <- el.mat[i,2]
      all.tests[[i]] <- try(t.test(the.list[[group1]], the.list[[group2]]), silent = TRUE)
    }
    result <- list("pairs" = el.mat, "tests" = all.tests)
  }

  if(class(mat.or.list)[1] == "matrix"){
    mat <- mat.or.list
    col.pairs <- pair.matrix(1:ncol(mat))
    all.tests <- vector(mode = "list", length = nrow(col.pairs))
    for(i in 1:nrow(col.pairs)){
      group1 <- col.pairs[i,1]
      group2 <- col.pairs[i,2]
      all.tests[[i]] <- try(t.test(mat[,group1], mat[,group2]), silent = TRUE)
    }
    result <- list("pairs" = col.pairs, "tests" = all.tests)
  }


  return(result)
}
