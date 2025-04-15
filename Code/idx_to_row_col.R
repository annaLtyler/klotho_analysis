#This function converts matrix indices to rows and 
#columns given the number of rows in the original matrix

idx_to_row_col <- function(idx, n.row){

    row.idx <- idx %% n.row
    row.idx[which(row.idx == 0)] <- n.row

    col.idx <- ceiling(idx/n.row)
    result <- cbind(row.idx, col.idx)
    colnames(result) <- c("row", "column")
    return(result)
}
