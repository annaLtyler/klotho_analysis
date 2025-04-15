#This function takes a matrix of categorical variables
#and creates binary dummy variables for n-1 of the 
#categories. 
#It returns a matrix of 1's and 0's with column names
#specifying the category used.

dummy_covar <- function(covar.mat){

    u_factors <- lapply(1:ncol(covar.mat), function(x) sort(unique(covar.mat[,x])))
    num.factors <- sapply(u_factors, length)
    total.columns <- sum(num.factors) - length(num.factors)
    new.names <- lapply(1:length(u_factors), function(x) paste(colnames(covar.mat)[x], u_factors[[x]][2:length(u_factors[[x]])], sep = "_"))
    
    new.covar <- vector(mode = "list", length = length(new.names))
    for(i in 1:length(new.names)){
        factor.locale <- lapply(u_factors[[i]], function(x) which(covar.mat[,i] == x))
        new.mat <- matrix(0, ncol = length(new.names[[i]]), nrow = nrow(covar.mat))
        colnames(new.mat) <- new.names[[i]]
        for(j in 1:ncol(new.mat)){
            new.mat[factor.locale[[j]],j] <- 1
        }
        new.covar[[i]] <- new.mat
    }

    new.covar.mat <- Reduce("cbind", new.covar)
    rownames(new.covar.mat) <- rownames(covar.mat)
    return(new.covar.mat)
}