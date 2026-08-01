#adds log normalized matrix to the sample_data object
log_norm_data <- function(sample_data){
    gene.data <- sample_data$data
    log.data <- log2(gene.data+1)
    sample_data$log_data <- log.data
    return(sample_data)
}