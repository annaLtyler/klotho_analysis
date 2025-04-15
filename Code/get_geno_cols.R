get_geno_cols <- function(file_path){
    geno.col.table <- read.csv(file_path, comment.char = "!", header = FALSE)
    geno.cols <- geno.col.table[,2]
    names(geno.cols) <- geno.col.table[,1]
    return(geno.cols)
}