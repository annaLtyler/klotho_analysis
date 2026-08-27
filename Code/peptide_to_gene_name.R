#get peptide labels for a given gene name

peptide_to_gene_name <- function(peptide_label, sample_data){

    gene.id.col <- sample_data$gene.id.col
    gene.name.col <- sample_data$gene.name.col

    pep.idx <- which(sample_data$pr_info[,gene.id.col] == peptide_label)
    gene.name <- sample_data$pr_info[pep.idx,gene.name.col]
    return(gene.name)
}
