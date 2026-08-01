#get peptide labels for a given gene name

peptide_labels <- function(gene.name, sample_data, data.type = c("raw", "log", "mean", "scaled")){

    gene.id.col <- sample_data$gene.id.col
    gene.name.col <- sample_data$gene.name.col

    dat.mat <- get_data(sample_data, data.type)
    gene.idx <- which(sample_data$pr_info[,gene.name.col] == gene.name)
    peptide.labels <- sample_data$pr_info[gene.idx,id.col]
    return(peptide.labels)
}
