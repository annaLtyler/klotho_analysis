#get information with gene name

get_gene_info <- function(gene.name, sample_data){
    gene.col <- sample_data$gene.name.col
    id.col <- sample_data$gene.id.col

    gene.idx <- grep(gene.name, sample_data$pr_info[,gene.col])
    if(length(gene.idx) == 0){
        #check in the IDs
        gene.idx <- grep(gene.name, sample_data$pr_info[,id.col])
    }
    
    if(length(gene.idx) > 0){
        id.info <- sample_data$pr_info[gene.idx,]
        return(id.info)
    }
}
