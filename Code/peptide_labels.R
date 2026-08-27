#get peptide labels for a given gene name

peptide_labels <- function(gene.name, sample_data){

    gene.id.col <- sample_data$gene.id.col
    gene.name.col <- sample_data$gene.name.col

    pep.idx <- which(sample_data$pr_info[,gene.name.col] == gene.name)
    peptide.labels <- sample_data$pr_info[gene.idx,gene.id.col]
    return(peptide.labels)
}
