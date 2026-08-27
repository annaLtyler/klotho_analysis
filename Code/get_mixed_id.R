#translate a vector of mixed transcript and protein IDs to gene names
get_mixed_id <- function(ids, sample_data, rna_data){
    
    id.col <- sample_data$gene.id.col
    name.col <- sample_data$gene.name.col

    tx.idx <- which(ids %in% rna_data$tx_info[,"ensembl_gene_id"])
    pr.idx <- which(ids %in% sample_data$pr_info[,id.col])
    id.names <- rep(NA, length(ids))
    if(length(tx.idx) > 0){
        id.names[tx.idx] <- rna_data$tx_info[match(ids[tx.idx], rna_data$tx_info[,"ensembl_gene_id"]), "external_gene_name"]
    }
    if(length(pr.idx) > 0){
        id.names[pr.idx] <- sample_data$pr_info[match(ids[pr.idx], sample_data$pr_info[,id.col]),gene.col]
    }
    no.name <- which(id.names == "")
    id.names[no.name] <- ids[no.name]
    
    names(id.names) <- ids
    return(id.names)
}