
#This function compares expression in groups from the scaled abundance 
#matrix by default. Set scaled.data = FALSE to draw from log normalized
#means.
#data.type = "scaled"; plot.label = ""; plot.results = TRUE; stat.x = 0.1; stat.y = 0.9; stat.y.spread = 0.15; cex.lab = 1; return.text = TRUE; ylab = "Abundance (A.U.)"; autoflip.stat.y = FALSE; autoplace.text = FALSE; n.samples = 25; min.contig = 5; jitter.factor = 1; ylim = NULL; plot.label.cex = 1
plot_pr_abund <- function(gene.name, sample_data, data.type = c("raw", "log", "mean", "scaled"), 
    plot.label = "", plot.results = TRUE, stat.x = 0.1, stat.y = 0.9, stat.y.spread = 0.15, 
    cex.lab = 1, return.text = TRUE, ylab = "Abundance (A.U.)", autoflip.stat.y = FALSE, 
    autoplace.text = FALSE, n.samples = 25, min.contig = 5, jitter.factor = 1, ylim = NULL, 
    plot.label.cex = 1){

    gene.id.col <- sample_data$gene.id.col
    gene.name.col <- sample_data$gene.name.col

    if(length(data.type) > 1){data.type = "scaled"} #default to scaled

    id.info <- get_gene_info(gene.name, sample_data)
    if(is.null(id.info)){
        message(paste("I can't find", gene.name))
        return(NULL)
    }

    gene.data <- peptide_vals(gene.name, sample_data, data.type)
    if(nrow(gene.data) == 0 || length(gene.data) == 1){
        message(paste("No data for", gene.name))
        return(NULL)
    }

    factor_df <- get_factor_var(sample_data, data.type)
    
    all_stats <- vector(mode = "list", length = nrow(gene.data))
    names(all_stats) <- rownames(gene.data)

    for(i in 1:length(all_stats)){
        gene.label <- paste(gene.name, rownames(gene.data)[i], sep = ": ")

        if(plot.results){
            layout.mat <- matrix(c(1,2,6,3,4,5), byrow = TRUE, nrow = 2)
            layout(layout.mat)
        }

        #plot one: main effect of sex
        if(length(unique(factor_df[,"sex"])) > 1){
            adj.vals <- adjust(t(gene.data[i,,drop=FALSE]), dummy_covar(factor_df[,c("age", "genotype")]), 
                retain.intercept = FALSE)
            sex.result <- test_effect(values = adj.vals, plot.factor = factor_df$sex, 
                return.text = return.text,
                plot.results = plot.results, stat.x = stat.x, stat.y = stat.y, 
                stat.y.spread = stat.y.spread, cex.lab = cex.lab, plot.label = 
                paste("Effect of sex on", gene.label), 
                ylab = ylab, autoflip.stat.y = autoflip.stat.y, autoplace.text = autoplace.text,
                n.samples = n.samples, min.contig = min.contig, jitter.factor = jitter.factor, ylim = ylim)
        }else{
            if(plot.results){
                plot.text("Only one sex represented.")
            }
            sex.result <- rep(NA, 4)
        }
        
        if(length(unique(factor_df[,"age"])) > 1){
            #plot two: main effect of age
            adj.vals <- adjust(t(gene.data[i,,drop=FALSE]), dummy_covar(factor_df[,c("sex", "genotype")]), 
                retain.intercept = FALSE)
            age.result <- test_effect(adj.vals, factor_df$age, return.text = return.text,
                plot.results = plot.results, stat.x = stat.x, stat.y = stat.y, 
                stat.y.spread = stat.y.spread, cex.lab = cex.lab, plot.label = 
                paste("Effect of age on", gene.label), 
                ylab = ylab, autoflip.stat.y = autoflip.stat.y, autoplace.text = autoplace.text,
                n.samples = n.samples, min.contig = min.contig, jitter.factor = jitter.factor, ylim = ylim)
        }else{
            if(plot.results){
                plot.text("Only one age represented")
            }
            age.result <- rep(NA, 4)
            
        }

        if(length(unique(factor_df[,"genotype"])) > 1){
            #plot three: main effect of genotype
            adj.vals <- adjust(t(gene.data[i,,drop=FALSE]), dummy_covar(factor_df[,c("age", "sex")]), 
                retain.intercept = FALSE)
            geno.result <- test_effect(values = adj.vals, plot.factor = factor_df$genotype, 
                return.text = return.text, plot.results = plot.results, stat.x = stat.x, 
                stat.y = stat.y, stat.y.spread = stat.y.spread, cex.lab = cex.lab, plot.label = 
                paste("Effect of genotype on", gene.label), 
                ylab = ylab, autoflip.stat.y = autoflip.stat.y, autoplace.text = autoplace.text,
                n.samples = n.samples, min.contig = min.contig, jitter.factor = jitter.factor, ylim = ylim)
        }else{
            if(plot.results){
                plot.text("Only one genotype represented")
            }
            geno.result <- rep(NA, 4)
            
        }

        #plot four to look at interaction effect: main effect of genotype in 4 month old mice
        four.idx <- which(factor_df[,"age"] == "4")
        if(length(four.idx) > 0){
            four.result <- test_effect(values = adj.vals[four.idx], 
                plot.factor = factor_df$genotype[four.idx], 
                return.text = return.text, plot.results = plot.results, stat.x = stat.x, 
                stat.y = stat.y, stat.y.spread = stat.y.spread, cex.lab = cex.lab, 
                plot.label = paste("Effect of genotype on", gene.label, "\nin 4-month old mice"), 
                ylab = ylab, autoflip.stat.y = autoflip.stat.y, 
                autoplace.text = autoplace.text, n.samples = n.samples, min.contig = min.contig, 
                jitter.factor = jitter.factor, ylim = ylim)
        }else{
            if(plot.results){
                plot.text("No four month old mice in the data set")
            }
            four.result <- rep(NA, 4)
            
        }

        #plot five to look at interaction effect: main effect of genotype in 12 month old mice
        twelve.idx <- which(factor_df[,"age"] == "12")
        if(length(twelve.idx) > 0){
            twelve.result <- test_effect(values = adj.vals[twelve.idx], 
                plot.factor = factor_df$genotype[twelve.idx], 
                return.text = return.text, plot.results = plot.results, stat.x = stat.x, 
                stat.y = stat.y, stat.y.spread = stat.y.spread, cex.lab = cex.lab, 
                plot.label = paste("Effect of genotype on", gene.label, "\nin 12-month old mice"), 
                ylab = ylab, autoflip.stat.y = autoflip.stat.y, 
                autoplace.text = autoplace.text, n.samples = n.samples, min.contig = min.contig, 
                jitter.factor = jitter.factor, ylim = ylim)
        }else{
            if(plot.results){
                plot.text("No twelve month old mice in data set")
            }
            twelve.result <- rep(NA, 4)
        }
        
        if(plot.results){
            #add a label in the black spot
            plot.text(plot.label, cex = plot.label.cex)
        }
    
    stat.mat <- rbind("sex" = sex.result, "age" = age.result, 
        "genotype" = geno.result, "geno_four" = four.result,
        "geno_twelve" = twelve.result)

    all_stats[[i]] <- list("expr_mat" = gene.data[i,,drop=FALSE], "stats" = stat.mat)
    }

    invisible(all_stats)
}
