plot_rna_abund <- function(gene.name, rna_data){
    gene.idx <- which(rna_data$tx_info[,"external_gene_name"] == gene.name)
    
    if(length(gene.idx) == 0){
        stop(paste("Can't find", gene.name))
    }

    gene.id <- rna_data$tx_info[gene.idx,"ensembl_gene_id"]
    mouse.info <- rna_data$mouse_info

    #adjust for batch right off the bad
    batch <- matrix(factor(mouse.info[,"sequencingBatch"]))
    rownames(batch) <- rownames(mouse.info)
    gene.data <- adjust(t(rna_data$data[gene.id,,drop=FALSE]), batch)

    #because of the way our color list is namee, we need to change sex to M/F
    sex <- mouse.info[,"sex_ge"]
    sex[which(sex == "Male")] <- "M"
    sex[which(sex == "Female")] <- "F"
    factor_df <- data.frame("sex" = ordered(sex, levels = c("F", "M")), 
        "age" = ordered(mouse.info[,"age_batch"], levels = c("4", "12")),
        "genotype" = ordered(mouse.info[,"ordered_geno"], levels = c("FC", "WT", "VS")))


    all_stats <- vector(mode = "list", length = nrow(gene.data))
    names(all_stats) <- rownames(gene.data)

    for(i in 1:length(all_stats)){
        gene.label <- paste(gene.name, colnames(gene.data)[i], sep = ": ")

        if(plot.results){
            layout.mat <- matrix(c(1,2,6,3,4,5), byrow = TRUE, nrow = 2)
            layout(layout.mat)
        }

        #plot one: main effect of sex
        if(length(unique(factor_df[,"sex"])) > 1){
            adj.vals <- adjust(gene.data[,i,drop=FALSE], dummy_covar(factor_df[,c("age", "genotype")]), 
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
            adj.vals <- adjust(gene.data[,i,drop=FALSE], dummy_covar(factor_df[,c("sex", "genotype")]), 
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
            adj.vals <- adjust(gene.data[,i,drop=FALSE], dummy_covar(factor_df[,c("age", "sex")]), 
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