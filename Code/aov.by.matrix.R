#This function assumes that each column of a matrix has
#its own factor. It creates a factor 
#based on the number of columns in the matrix.

aov.by.matrix <- function(matX){

    all.num <- as.vector(matX)
    fact.mat <- matrix(rep(1:ncol(matX), each = nrow(matX)), nrow = nrow(matX), ncol = ncol(matX))
    fact <- as.factor(as.vector(fact.mat))
    #boxplot(all.num~fact)
    test <- aov(all.num~fact)
    return(test)
}