#get protein group information

get_prg_info <- function(pr_group_name, sample_data){
    gene.id.col <- sample_data$gene.id.col
    pr.idx <- which(sample_data$pr_info[,gene.id.col] == pr_group_name)
    if(length(pr.idx) > 0){
        id.info <- sample_data$pr_info[pr.idx,]
        return(id.info)
    }
}
