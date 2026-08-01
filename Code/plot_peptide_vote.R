
plot_peptide_vote <- function(gene.name, sample_data, data.type = c("raw", "log", "mean", "scaled")){

    gene.id.col <- sample_data$gene.id.col
    gene.name.col <- sample_data$gene.name.col

    vote.info <- peptide_vote(gene.name, sample_data, data.type = data.type, 
        gene.name.col, gene.id.col)
    if(length(vote.info) == 1){
        return(paste(gene.name, "not found"))
    }
    
    gene.info <- vote.info$pep_info
    gene.votes <- vote.info$votes
    vote.result <- colnames(gene.votes)[apply(gene.votes, 1, which.max)]

    #get abundance values for the query peptide
    pep_vals <- peptide_vals(gene.name, sample_data, data.type = data.type,
        gene.name.col, gene.id.col)


    #other possible peptides
    other_peps <- setdiff(colnames(gene.votes), gene.name)

    #mark the peptides that were voted best for this gene
    vote.annot <- rep("no", length(vote.result))
    vote.annot[which(vote.result == gene.name)] <- "yes"
    vote.df <- data.frame("match" = factor(vote.annot))
    colnames(vote.df) <- paste("Match to", gene.name)
    rownames(vote.df) <- rownames(pep_vals)
    #add a fixed color for the voting
    color_list <- factor.cols
    color_list$vote <- c("no" = "#9D9D9D", "yes" = "#EEC946")
    names(color_list)[length(color_list)] <- paste("Match to", gene.name)

    #plot of gene votes for each peptide
    if(ncol(gene.votes) == 1){
        cluster_cols = FALSE; cluster_rows = FALSE
    }else{
        cluster_cols = TRUE; cluster_rows = TRUE
    }
    row.order <- order(gene.votes[,1])

    breaks = seq(0, max(c(4, max(gene.votes))), 1)
    color = colors.from.values(breaks, use.pheatmap.colors = TRUE)

    pheatmap(gene.votes[row.order,,drop=FALSE], main = "Gene Votes per Peptide", annotation_row = vote.df,
        annotation_colors = color_list, cluster_cols = cluster_cols, cluster_rows = cluster_rows,
        color = color, breaks = breaks)
  
    if(length(other_peps) > 0){
        #get the same information for all genes mentioned in voting
        other.gene.info <- lapply(other_peps, function(x) peptide_vote(x, sample_data, data.type,
            gene.name.col, gene.id.col))

        #get abundances for peptides listed for other genes
        other_pep_vals <- lapply(other_peps, function(x) peptide_vals(x, sample_data, data.type,
            gene.name.col, gene.id.col))
        names(other_pep_vals) <- other_peps
    }


    #plot of gene votes for each peptide
    if(nrow(pep_vals) == 1){
        cluster_cols = FALSE; cluster_rows = FALSE
    }else{
        cluster_cols = TRUE; cluster_rows = TRUE
    }
    col.order <- order(pep_vals[1,])
    
    annot.df <- get_factor_var(sample_data, data.type = data.type)
    
    #heat map of abundance of all peptides assigned to the gene
    pheatmap(pep_vals[,col.order,drop=FALSE], show_colnames = FALSE, 
        annotation_col = annot.df, annotation_colors = color_list, annotation_row = vote.df,
        cluster_rows = cluster_rows, cluster_cols = cluster_cols)

    
    breaks = seq(-1, 1, 0.2)
    color = colors.from.values(breaks, use.pheatmap.colors = TRUE)

    if(nrow(pep_vals) > 1){
        #heat map of correlation matrix for all peptides assigned to the gene
        in_gene_cor <- cor(t(pep_vals), use = "pairwise.complete.obs")
        pheatmap(in_gene_cor, 
            annotation_row = vote.df, annotation_colors = color_list,
            main = paste("Peptide Correlations for", gene.name),
            color = color, breaks = breaks, legend.color = color, legend.breaks = breaks)
    }else{
        in_gene_cor <- NULL
    }

    if(length(other_peps)){
        #look at correlations of these peptides to peptides in the other listed genes
        cross_pep_cor <- vector(mode = "list", length = length(other_pep_vals))
        names(cross_pep_cor) <- names(other_pep_vals)
        for(i in 1:length(other_pep_vals)){
            if(length(other_pep_vals[[i]]) > 1){
                pep_cor <- matrix(NA, nrow = nrow(pep_vals), ncol = nrow(other_pep_vals[[i]]))
                rownames(pep_cor) <- rownames(pep_vals)
                colnames(pep_cor) <- rownames(other_pep_vals[[i]])
                for(p1 in 1:nrow(pep_vals)){
                    for(p2 in 1:nrow(other_pep_vals[[i]])){
                        pep_cor[p1,p2] <- cor(pep_vals[p1,], other_pep_vals[[i]][p2,], use = "pairwise.complete.obs")
                    }
                }
                cross_pep_cor[[i]] <- pep_cor
                if(ncol(pep_cor) > 1){
                    pheatmap(pep_cor, annotation_row = vote.df,
                        main = paste("Comparison of", gene.name, "(rows) to", 
                        names(other_pep_vals)[i], "(columns)"), annotation_colors = color_list,
                        breaks = breaks, color = color)
                }else{
                    par(mar = c(16, 4, 4, 2))
                    cor.order <- order(pep_cor[,1])
                    barplot(pep_cor[cor.order,1], ylim = c(-1,1), las = 2,
                        main = paste("Correlation of", gene.name, "peptides to", 
                        names(other_pep_vals)[i], "peptides"), cex.names = 0.8,
                        col = as.numeric(vote.df[cor.order,1])*-1+9)
                    abline(h = seq(-1, 1, 0.25), lty = 2, col = "gray")
                    abline(h = 0)
                    legend("topleft", fill = c(7, 8), title = paste("Match to", gene.name), 
                        legend = c("yes", "no"), bg = "white")
                }
            }
        }
    }else{
        other.gene.info <- NULL
        other_pep_vals <- NULL
        cross_pep_cor <- NULL
    }
    
    result <- list("gene_info" = gene.info, "peptide_abundance" = pep_vals,
        "vote_matrix" = gene.votes, "gene_vote" = vote.result, 
        "other_possible_genes" = other_peps,
        "other_gene_info" = other.gene.info, 
        "other_gene_peptide_abundance" = other_pep_vals,
        "in_gene_correlations" = in_gene_cor,
        "cross_gene_correlations" = cross_pep_cor)
    invisible(result)
}