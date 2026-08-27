

plot_genotype_mediation <- function(aligned_data, sample_data, rna_data, 
    source.gene, target.gene, source.gene.type = "transcript", target.gene.type = "peptide",
    stat.x = 0.1, stat.y = 0.9, stat.y.spread = 0.15, autoplace.text = FALSE, reverse.mediation = FALSE,
    nsims = 1000){

    id.col <- sample_data$gene.id.col
    name.col <- sample_data$gene.name.col

    mouse.var <- get_factor_var(sample_data, data.type = "mean")[colnames(aligned_data[[1]]),]
    genotype <- mouse.var[,"genotype"]
    geno.cols <- sapply(genotype, function(x) factor.cols$genotype[which(names(factor.cols$genotype) == x)])

    if(source.gene.type == "transcript"){
        source.id <- rna_data$tx_info[which(rna_data$tx_info[,"external_gene_name"] == source.gene), "ensembl_gene_id"]
    }
    if(source.gene.type == "peptide"){
        source.id <- sample_data$pr_info[which(sample_data$pr_info[,name.col] == source.gene), id.col]
    }
    source.abund <- get_any_abund(source.id, aligned_data)
    

    if(target.gene.type == "transcript"){
        target.id <- rna_data$tx_info[which(rna_data$tx_info[,"external_gene_name"] == target.gene), "ensembl_gene_id"]
    }
    if(target.gene.type == "peptide"){
        target.id <- sample_data$pr_info[which(sample_data$pr_info[,name.col] == target.gene), id.col]
    }
    target.abund <- get_any_abund(target.id, aligned_data)


    for(i in 1:ncol(target.abund)){
        #plot the effect of genotype on the transcript
        
        no.na <- which(!is.na(target.abund))
       
        if(reverse.mediation){
            mediator = target.abund[no.na,1]; mediator.gene <- target.gene; mediator.label = target.gene.type
            outcome <- source.abund[no.na,1]; outcome.gene <- source.gene; outcome.label = source.gene.type
        }else{
            mediator = source.abund[no.na,1]; mediator.gene <- source.gene; mediator.label = source.gene.type
            outcome <- target.abund[no.na,1]; outcome.gene <- target.gene; outcome.label = target.gene.type
        }

        layout.mat <- matrix(c(1,5,6,2,3,4), nrow = 2, byrow = TRUE)
        layout(layout.mat)
        
        plot.with.model(mediator, outcome, xlab = paste(mediator.gene, mediator.label),
            ylab = paste(outcome.gene, outcome.label), col = geno.cols)

        med.effect <- test_effect(mediator, genotype[no.na], return.text = FALSE,
                plot.results = TRUE, stat.x = stat.x, stat.y = stat.y, stat.y.spread = stat.y.spread, 
            cex.lab = 1, plot.label = paste(mediator.gene, mediator.label), 
            ylab = "Abundance (A.U.)", autoflip.stat.y = FALSE,
            autoplace.text = autoplace.text, n.samples = 25, min.contig = 5, jitter.factor = 1, ylim = NULL)

        out.effect <- test_effect(outcome, genotype[no.na], return.text = FALSE,
            plot.results = TRUE, stat.x = stat.x, stat.y = stat.y, stat.y.spread = stat.y.spread, 
            cex.lab = 1, plot.label = paste(outcome.gene, outcome.label), 
            ylab = "Abundance (A.U.)", autoflip.stat.y = FALSE,
            autoplace.text = autoplace.text, n.samples = 25, min.contig = 5, jitter.factor = 1, ylim = NULL)

        #adjust the peptide values for the transcript values and plot again
        adj.out <- adjust(as.matrix(outcome), as.matrix(mediator))
        med.effect <- test_effect(adj.out, genotype[no.na], return.text = FALSE,
            plot.results = TRUE, stat.x = stat.x, stat.y = stat.y, stat.y.spread = stat.y.spread, 
            cex.lab = 1, plot.label = paste(outcome.gene, "adjusted for", mediator.label), 
            ylab = "Abundance (A.U.)", autoflip.stat.y = FALSE,
            autoplace.text = autoplace.text, n.samples = 25, min.contig = 5, jitter.factor = 1, ylim = NULL)

        non.med.effect <- out.effect$linear.effect.size
        after.med.effect <- med.effect$linear.effect.size


        barplot(c(non.med.effect, after.med.effect),
            ylab = "Linear Effect Size", main = "",
            names = c(paste("Genotype Effect on\n", outcome.gene, outcome.label), 
                paste("Effect accounting for\n", mediator.gene, mediator.label)))
        abline(h = 0)
    
        #test the mediation
        gd <- c("FC" = 0, "WT" = 1, "VS" = 2) #allele dosage
        Gdosage <- sapply(genotype[no.na], function(x) gd[which(names(gd) == x)])

        df <- data.frame("out" = outcome, "med" = mediator, "Gdosage" = Gdosage)
        #test the mediation
        med.fit <- lm(med ~ Gdosage, data = df)
        out.fit <- lm(out ~ Gdosage + med, data = df)
    
        med.out <- suppress_messages(mediate(med.fit, out.fit, 
            treat = "Gdosage", mediator = "med", boot = TRUE, sims = nsims))
        med.effect <- med.out$n1
        med.p <- med.out$n1.p

        if(med.p == 0){
            p.text <- paste("p <", 1/nsims)
            }else{
            p.text <- paste("p =",med.p)
        }

        plot.new()
        plot.window(xlim = c(0, 1), ylim = c(0,1))
        arrow.offset <- 0.04
        model.x <- 0.2
        text(x = model.x, y = 0.1, labels = "genotype", font = 2, cex = 1.2)
        arrows(x0 = model.x, y0 = 0.1+arrow.offset, y1 = 0.5-arrow.offset, lwd = 2, length = 0.1)
        text(x = model.x, y = 0.5, labels = paste(mediator.gene, mediator.label), font = 2, cex = 1.2)
        arrows(x0 = model.x, y0 = 0.5+arrow.offset, y1 = 0.9-arrow.offset, lwd = 2, length = 0.1)
        text(x = model.x, y = 0.9, labels = paste(outcome.gene, outcome.label), font = 2, cex = 1.2)

        text(x = 0.9, y = 0.75, labels = paste0("Proportion Mediated: ", signif(med.effect, 2)*100, "%"), 
            font = 1, cex = 1.2, adj = 1) 
        text(x = 0.9, y = 0.65, labels = paste("Mediation", p.text), font = 1, cex = 1.2, adj = 1) 
    }
    result <- c("Proportion_Mediated" = med.effect, "p" = med.p)
    invisible(result)
}
