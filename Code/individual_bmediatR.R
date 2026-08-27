#plots probabilities for an individual mediation
#using gene ane protein names. This uses flat 
#priors for all models

individual_bmediatR <- function(tx.name, pr.name, aligned_data, sample_data, rna_data,
    plot.results = TRUE){
    
    gene.name.col <- sample_data$gene.name.col
    gene.id.col <- sample_data$gene.id.col
    aligned_pr <- aligned_data$aligned_proteins
    aligned_tx <- aligned_data$aligned_transcripts

    tx.id <- rna_data$tx_info[which(rna_data$tx_info[,"external_gene_name"] == tx.name), "ensembl_gene_id"]
    pr.id <- sample_data$pr_info[which(sample_data$pr_info[,gene.name.col] == pr.name), gene.id.col]
    
    out.var <- t(aligned_pr[pr.id,,drop=FALSE])
    med.var <- t(aligned_tx[tx.id,,drop=FALSE])
    
    mouse.var.table <- get_factor_var(sample_data, data.type = "scaled")[colnames(aligned_tx),]
    mouse.geno <- mouse.var.table[,"genotype"]

    geno.design <- model.matrix(~mouse.geno-1)
    rownames(geno.design) <- rownames(med.var)

    #for prior assignment see: page(bmediatR:::return_ln_prior_c_from_presets)
    #reactive has flat priors, but I'm not sure why
    #partial allows only panel b from Fig 1. Assumes there is a direct effect of X on Y
    #complete makes impossible all options from panel d in Figure 1, which all have a reactive effect from Y to M
    #I think having flat priors probably makes the most sense.
    #priors <- list("complete", "partial", "reactive")
    #I also tested several reverse pairs, and the number are always exactly the
    #same, but swapped positions, so we only need to fit once.
    med.mat <- matrix(NA, nrow = length(pr.id), ncol = 12)
    
    for(i in 1:length(pr.id)){
        med.check <- bmediatR(y = out.var, M = med.var, 
            X = geno.design, ln_prior_c = "reactive", 
            options_X = list(sum_to_zero = TRUE, 
            center = FALSE, scale = FALSE))

        post.probs.forward <- get_posterior(med.check)
        prob.bars <- as.matrix(post.probs.forward[2:length(post.probs)])
    
        med.mat[i,] <- prob.bars
    }
    colnames(med.mat) <- names(post.probs)[2:length(post.probs)]
    row.id <- paste(pr.name, pr.id, sep = ":")
    rownames(med.mat) <- paste(tx.name, ">", row.id)

    if(plot.results){
        if(length(pr.id) == 1){
            par(mar = c(10, 4, 4, 2))
            barplot(med.mat, ylim = c(0, 1), main = paste(tx.name, "->", pr.name), las = 2)
        }else{
        cols <- colors.from.values(seq(0, 1, 0.01), use.pheatmap.colors = TRUE)
        pheatmap(med.mat, cluster_rows = FALSE, cluster_cols = FALSE,
            color = cols, main = paste(tx.name, "and", pr.name), display_numbers = TRUE, 
            breaks = seq(0, 1, length.out = (length(cols)+1)))
        }
    }

    invisible(med.mat)
}
