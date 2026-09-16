#get protein group information

get_prg_info <- function(pr_group_name, sample_data){
    gene.id.col <- sample_data$gene.id.col

    pr.idx <- lapply(pr_group_name, function(x) sample_data$pr_info[which(sample_data$pr_info[,gene.id.col] == x),])
    has.vals <- which(sapply(pr.idx, length) > 0)

    if(length(has.vals) > 0){
        pr_info_table <- Reduce("rbind", pr.idx[has.vals])
        rownames(pr_info_table) <- pr_group_name[has.vals]
        return(pr_info_table)
    }
}
