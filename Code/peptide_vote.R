#tally the proteins assigned to different peptides grouped under a 
#given gene.

peptide_vote <- function(gene.name, sample_data, data.type = c("raw", "log", "mean", "scaled")){

    gene.id.col <- sample_data$gene.id.col
    gene.name.col <- sample_data$gene.name.col

    dat.mat <- get_data(sample_data, data.type)

    gene.idx <- grep(gene.name, sample_data$pr_info[,gene.name.col])
    if(length(gene.idx) == 0){
        return(paste(gene.name, "not found"))
    }
    pep.info <- sample_data$pr_info[gene.idx,,drop=FALSE]
    gene.prg <- pep.info[,1]
    common.prg <- intersect(rownames(dat.mat), gene.prg)

    if(length(common.prg) == 0){
        return(paste("No abundance data for", gene.name))
    }

    pep.info <- pep.info[match(common.prg, pep.info[,gene.id.col]),]

    pep.strings <- pep.info[,"protein_ids"]
    split.pep <- lapply(pep.strings, function(x) strsplit(x, ";")[[1]])
    split.gene <- vector(mode = "list", length = length(split.pep))
    all.pep.id <- unique(unlist(split.pep))    
    pep.genes <- t(sapply(all.pep.id, pep2gene))
    for(i in 1:length(split.pep)){
        split.gene[[i]] <- pep.genes[match(split.pep[[i]], pep.genes[,"accession"]),gene.col]
    }
    all.gene.count <- table(unlist(split.gene))
    #barplot_with_num(all.gene.count)
    gene.vote.mat <- matrix(0, nrow = nrow(pep.info), ncol = length(all.gene.count))
    rownames(gene.vote.mat) <- pep.info[,1]
    colnames(gene.vote.mat) <- names(all.gene.count)
    for(i in 1:length(split.gene)){
        pep.gene.count <- table(split.gene[[i]])
        gene.vote.mat[i,names(pep.gene.count)] <- as.numeric(pep.gene.count)
    }
    result <- list("pep_info" = pep.info, "votes" = gene.vote.mat)
    return(result)
}
