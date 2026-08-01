#scales the mean abundance matrix
#adds this new matrix to the sample_data object
scale_abundance <- function(sample_data){
    gene.data <- sample_data$mean_abundance
    scaled.data <- t(apply(gene.data, 1, scale))
    dimnames(scaled.data) <- dimnames(gene.data)
    sample_data$scaled_abundance <- scaled.data
    return(sample_data)
}