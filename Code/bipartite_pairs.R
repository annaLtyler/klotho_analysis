#This function pairs every element of vector 1 (V1)
#with every element of vector 2 (V2). It returns a
#two-column matrix with the pairs and column names
#matching the labels for each vector.

bipartite_pairs <- function(V1, V2, V1.label = NULL, V2.label = NULL){
    
    V.pairs <- cbind(rep(V1, length(V2)), rep(V2, each = length(V1)))
    colnames(V.pairs) <- c(V1.label, V2.label)
    return(V.pairs)
}
