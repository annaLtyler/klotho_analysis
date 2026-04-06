#This function converts row and column numbers to
#an index in a matrix. 

row_col_to_idx <- function(row.idx, col.idx, n.row){

    max.idx <- n.row*max(col.idx)
    idx.mat <- matrix(1:max.idx, nrow = n.row, ncol = max(col.idx))
    idx <- sapply(1:length(row.idx), function(x) idx.mat[row.idx[x], col.idx[x]])
    return(idx)
}
