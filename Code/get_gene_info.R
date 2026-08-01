#get information with gene name

get_gene_info <- function(gene.name, sample_data){
    gene.idx <- grep(gene.name, sample_data$pr_info[,gene.col])
    if(length(gene.idx) > 0){
        id.info <- sample_data$pr_info[gene.idx,]
        return(id.info)
    }
}
