

plot_multi_effects <- function(gene.names, sample_data, data.type = c("raw", "log", "mean", "scaled"), 
   effect_type = c("effect_size", "R2", "p"), plot.label = ""){

    gene.id.col <- sample_data$gene.id.col
    gene.name.col <- sample_data$gene.name.col
 
    effect_type <- effect_type[1] #default to effect size
    
    gene.abund <- lapply(gene.names, function(x) peptide_vals(x, sample_data, data.type,
        gene.name.col, gene.id.col))
    genes.found <- which(sapply(gene.abund, length) > 1)
    gene.vals <- Reduce("rbind", gene.abund[genes.found])
    gene.labels <- unlist(sapply(genes.found, 
        function(x) paste(gene.names[x], rownames(gene.abund[[x]]), sep = ": ")))
    rownames(gene.vals) <- gene.labels

    mean.abund <- colMeans(gene.vals, na.rm = TRUE)
    
    gene.tests <- lapply(gene.names[genes.found], 
        function(x) plot_gene(x, sample_data, data.type = data.type,
            gene.name.col = gene.col, gene.id.col = id.col, plot.results = FALSE))


    if(effect_type == "R2"){
        plot.mat <- t(Reduce("cbind", lapply(gene.tests, function(x) sapply(x, function(y) as.numeric(y$stats[,"r2"])))))
        colnames(plot.mat) <- rownames(gene.tests[[1]][[1]]$stats)
        rownames(plot.mat) <- gene.labels
        ylab <- "R2"
    }

    if(effect_type == "effect_size"){
        plot.mat <- t(Reduce("cbind", lapply(gene.tests, function(x) sapply(x, function(y) as.numeric(y$stats[,"linear.effect.size"])))))
        colnames(plot.mat) <- rownames(gene.tests[[1]][[1]]$stats)
        rownames(plot.mat) <- gene.labels
        ylab <- "effect size"
   }

    if(effect_type == "p"){
        plot.mat <- -log10(t(Reduce("cbind", lapply(gene.tests, function(x) sapply(x, function(y) as.numeric(y$stats[,"linear.effect.p"]))))))
        colnames(plot.mat) <- rownames(gene.tests[[1]][[1]]$stats)
        rownames(plot.mat) <- gene.labels
        ylab <- "-log10(p)"
    }


    layout.mat <- matrix(c(1,2,6,3,4,5), nrow = 2, byrow = TRUE)
    layout(layout.mat)
    par(mar = c(8, 4, 4, 2))
    ymin <- floor(min(plot.mat, na.rm = TRUE)*100)/100
    ymax <- ceiling(max(plot.mat, na.rm = TRUE)*100)/100
    for(i in 1:ncol(plot.mat)){
        barplot(plot.mat[,i], main = colnames(plot.mat)[i], ylim = c(ymin, ymax), las = 2,
            ylab = ylab)
        add_zero_line()
    }
    plot.text(plot.label, font = 2, cex = 1.2)

}
