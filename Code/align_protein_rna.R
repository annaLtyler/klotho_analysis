#align RNA and protein data based on individuals in the 
#provided sample_data and optional protein names
#If align.genes is TRUE, the proteins and transcripts
#will be aligned 1:1.

align_protein_rna <- function(pr_data, rna_data, 
    data.type = c("raw", "log", "mean", "scaled"), gene.names = NULL, 
    adjust.for = NULL, align.genes = FALSE){

    gene.name.col <- pr_data$gene.name.col
    gene.id.col <- pr_data$gene.id.col

    if(length(data.type) > 1){data.type = "mean"}

    #if unspecified, get protein abundance for all genes
    if(is.null(gene.names)){
        gene.names <- unique(c(pr_data$pr_info[,gene.name.col], rna_data$tx_info[,"external_gene_name"]))
    }
    
    #if we are going to align genes later, only request proteins with single entries
    if(align.genes){
        unique.peptides.only = TRUE
        add.gene.names = TRUE
    }else{
        unique.peptides.only = FALSE #we don't need unique peptides
        add.gene.names = FALSE #we don't need to label with gene names
    }
    
    #don't adjust until we have the final list of individuals
    pr.mat <- peptide_vals(gene.names, pr_data, data.type,
            adjust.for = NULL, add.gene.names = add.gene.names, 
            unique.peptides.only = unique.peptides.only)


    #first align individuals in the two data sets.
    pr.ind <- colnames(pr.mat)
    rna.ind <- colnames(rna_data$data)
    common.ind <- intersect(pr.ind, rna.ind)
    common.pr <- pr.mat[,common.ind]
    common.rna <- rna_data$data[,common.ind]

    #pull out the right genes for the RNA
    gene.id <- rna_data$tx_info[match(gene.names, rna_data$tx_info[,"external_gene_name"]),"ensembl_gene_id"]
    common.id <- intersect(rownames(common.rna), gene.id)    
    
    aligned.rna <- common.rna[common.id,]

    if(align.genes){
        gene.names <- rna_data$tx_info[match(common.id, rna_data$tx_info[,"ensembl_gene_id"]), "external_gene_name"]
        rna.labels <- paste(gene.names, common.id, sep = ": ")

        pr_genes <- sapply(strsplit(rownames(common.pr), ": "), function(x) x[1])
        common.gene <- intersect(gene.names, pr_genes)

        common.rna.idx <- match(common.gene, gene.names)
        common.pr.idx <- match(common.gene, pr_genes)

        gene.rna <- aligned.rna[common.rna.idx,,drop=FALSE]
        rownames(gene.rna) <- rna.labels[common.rna.idx]
        
        gene.pr <- common.pr[common.pr.idx,,drop=FALSE]
    }else{
        gene.rna <- aligned.rna
        gene.pr <- common.pr
    }

    #adjust the rna for batch effects and Ide expression
    rna.covar <- dummy_covar(as.matrix(rna_data$mouse_info[common.ind,"sequencingBatch",drop=FALSE]))
    rna.covar <- as.matrix(cbind(rna.covar, rna_data$mouse_info[common.ind,"scaled_Ide_expression",drop=FALSE]))
    adj.rna <- t(adjust(t(gene.rna), rna.covar))

    #if there are additional adjustments to be made, make those here.
    if(!is.null(adjust.for)){
        mouse.info.var <- get_factor_var(pr_data, data.type = data.type)
        add.covar <- dummy_covar(mouse.info.var[,adjust.for,drop=FALSE])
        final.rna <- t(adjust(t(adj.rna), add.covar))
        final.pr <- t(adjust(t(gene.pr), add.covar))
    }else{
        final.rna <- adj.rna
        final.pr <- gene.pr
    }

    result <- list("aligned_proteins" = final.pr, "aligned_transcripts" = final.rna)
    return(result)
}
