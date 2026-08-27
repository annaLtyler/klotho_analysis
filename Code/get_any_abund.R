#get abundance for any 

get_any_abund <- function(gene.id, aligned_data){
    
    aligned_pr <- aligned_data$aligned_proteins
    aligned_tx <- aligned_data$aligned_transcripts

    id.idx <- which(rownames(aligned_pr) == gene.id)
    tx.idx <- which(rownames(aligned_tx) == gene.id)
    if(length(id.idx) > 0){
        gene.abund <- t(aligned_pr[id.idx,,drop=FALSE])
    }
    if(length(tx.idx) > 0){
        gene.abund <- t(aligned_tx[tx.idx,,drop=FALSE])
    }
    if(length(tx.idx) == 0 && length(id.idx) == 0){
        gene.abund <- rep(NA, ncol(aligned_tx))
    }
    return(gene.abund)
}