#get expression values for peptides/proteins
#there is an option to adjust for covariates
#before returning values. If unique.peptides.only
#is set to TRUE, the function only returns data
#for genes with a single representative peptide.

peptide_vals <- function(gene.names, sample_data, data.type = c("raw", "log", "mean", "scaled"),
    adjust.for = NULL, unique.peptides.only = FALSE, add.gene.names = FALSE){
    
    gene.id.col <- sample_data$gene.id.col
    gene.name.col <- sample_data$gene.name.col

    dat.mat <- get_data(sample_data, data.type, adjust.for = adjust.for)
        
    pr.id <- lapply(gene.names, function(x) sample_data$pr_info[which(sample_data$pr_info[,gene.name.col] == x),gene.id.col])
    names(pr.id) <- gene.names
    
    #check for peptide IDs if some protein names are not found
    not.found <- which(sapply(pr.id, length) == 0)
    if(length(not.found) > 0){
        id.check <- lapply(gene.names[not.found], function(x) sample_data$pr_info[which(sample_data$pr_info[,gene.id.col] == x),gene.id.col])
        pr.id[not.found] <- id.check
    }
    
    pr.labels <- sapply(1:length(pr.id), function(x) paste(gene.names[x], pr.id[[x]], sep = ": "))

    if(unique.peptides.only){
        has.vals <- which(sapply(pr.id, length) == 1)
    }else{
        has.vals <- which(sapply(pr.id, length) > 0)
    }

    if(length(has.vals) == 0){
        return("genes not found")
    }
    
    final.id <- unlist(pr.id[has.vals])
    names(final.id) <- unlist(pr.labels[has.vals])

    #pull out the abundance of all gene groups for the named gene
    common.prg <- which(final.id %in% rownames(dat.mat))
    pull.id <- final.id[common.prg]
    pr.table <- as.matrix(dat.mat[pull.id,,drop=FALSE])

    if(add.gene.names){
        rownames(pr.table) <- names(pull.id)
    }

    #set NAs to 0
    #pr.table[which(is.na(pr.table))] <- 0
    return(pr.table)
}
