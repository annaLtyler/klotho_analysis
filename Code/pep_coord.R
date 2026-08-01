#get protein coordinates for peptides in a given gene

pep_coord <- function(gene.name, sample_data, pep.fasta, data.type = c("log", "raw", "mean", "scaled")){

    gene.id.col <- sample_data$gene.id.col
    gene.name.col <- sample_data$gene.name.col

    dat.mat <- get_data(sample_data, data.type)

    gene.idx <- which(sample_data$pr_info[,gene.name.col] == gene.name)
    all.peps <- sample_data$pr_info[gene.idx,"precursor_id"]
    common.peps <- intersect(rownames(dat.mat), all.peps)
    stripped.seq <- sample_data$pr_info[match(common.peps, sample_data$pr_info[,gene.id.col]), 
        "stripped_sequence"]

    idx <- grep(paste0("GN=", gene.name), names(pep.fasta))
    #pep.fasta[idx]

    #the first protein listed is always the longest. Let's use that for now.
    prot <- fasta[[idx[1]]]
    prot.mat <- matrix(NA, nrow = length(common.peps), ncol = 2)
    rownames(prot.mat) <- common.peps
    colnames(prot.mat) <- c("start", "end")
    for(pe in 1:length(common.peps)){
        hits <- matchPattern(stripped.seq[pe], prot)
        pep.start <- start(hits)
        pep.end <- end(hits)
        if(length(pep.start) > 0){
            prot.mat[pe,] <- c(pep.start, pep.end)
        }
    }
    start.order <- order(prot.mat[,1])
    ordered.pep <- prot.mat[start.order,,drop=FALSE]
    return(ordered.pep)
}