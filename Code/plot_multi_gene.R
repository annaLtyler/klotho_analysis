#mouse_order_by determines how the mice will be ordered in columns
#pr_order_by determines how the proteins will be ordered in rows
#show_r2 shows which protein test statistic will be shown in the
#bar plot. If pr_order_by and show_r2 are the same, the bars should
#be in order of decreasing value

plot_multi_gene <- function(gene.names, sample_data, mouse_order_by = "clust", 
    pr_order_by = "genotype", show_r2 = "genotype", adjust.for = "sex",
    data.type = c("raw", "log", "mean", "scaled"),
    test_factor = c("eigengene", "mean"), plot.label = "",
    stat.x = 0.1, stat.y = 0.9, autoplace.text = FALSE){

    gene.id.col <- sample_data$gene.id.col
    gene.name.col <- sample_data$gene.name.col

    if(length(data.type) > 1){data.type = "scaled"} #default to scaled
    test_factor = test_factor[1]

    mouse.factors <- get_factor_var(sample_data, data.type = data.type)
    gene.abund <- lapply(gene.names, 
        function(x) as.matrix(peptide_vals(x, sample_data, data.type, adjust.for)))
    genes.found <- which(sapply(gene.abund, length) > 1)
    gene.vals <- Reduce("rbind", gene.abund[genes.found])

    mean.abund <- colMeans(gene.vals, na.rm = TRUE)
    gene.labels <- unlist(sapply(genes.found, 
        function(x) paste(gene.names[x], rownames(gene.abund[[x]]), sep = ": ")))
    rownames(gene.vals) <- gene.labels

    gene.tests <- lapply(gene.names[genes.found], 
        function(x) plot_gene(x, sample_data, data.type = data.type, plot.results = FALSE))
    all.r2 <- t(Reduce("cbind", lapply(gene.tests, function(x) sapply(x, function(y) as.numeric(y$stats[,"r2"])))))
    colnames(all.r2) <- rownames(gene.tests[[1]][[1]]$stats)
    rownames(all.r2) <- gene.labels

    #establish some order based on clustering
    mouse.order <- hclust(dist(t(gene.vals)))$order

    #check to see if we wanted to order the mice by their genotype, age, or sex
    order.idx <- which(colnames(mouse.factors) %in% mouse_order_by)
    if(length(order.idx) > 0){
        mouse.order <- order(mouse.factors[,order.idx])
    }
    #otherwise, we can order by the abundance mean
    if(mouse_order_by == "mean"){
        mouse.order <- order(mean.abund)
    }
    #or median
    if(mouse_order_by == "median"){
        col.med <- apply(gene.vals, 2, function(x) median(x, na.rm = TRUE))
        mouse.order <- order(col.med)
    }
    
    #establish an order for the proteins based on clustering
    pr.order <- hclust(dist(gene.vals))$order
    
    #check to see if we want to order the 
    order_by <- which(colnames(all.r2) %in% pr_order_by)

    if(length(order_by) > 0){
        if(all(is.na(all.r2[,pr_order_by]))){
            requested.order <- pr_order_by
            max.ind <- idx_to_row_col(which.max(all.r2), nrow(all.r2))
            pr_order_by <- colnames(all.r2)[max.ind[,"column"]]
            warning(paste0("No variation in ", requested.order, ". Ordering by ", pr_order_by, " instead."))
        }
        pr.order <- order(all.r2[,pr_order_by], decreasing = TRUE)
    }
    
    if(data.type == "scaled"){
        #layout.matrix <- matrix(c(1,1,3,2,2,0,4,5,5), nrow = 3, byrow = FALSE)
        layout.matrix <- matrix(c(1,1,1,2,2,2,3,4,5,6,7,8), nrow = 3, byrow = FALSE)
    }else{
        #otherwise add another panel for the heat map color bar
        #layout.matrix <- matrix(c(1,1,3,2,2,6,4,5,5), nrow = 3, byrow = FALSE)
        layout.matrix <- matrix(c(1,1,1,2,2,2,3,4,5,6,7,8), nrow = 3, byrow = FALSE)
    }
    #layout(layout.matrix, widths = c(1,0.4, 0.8, 0.8), heights = c(0.7, 0.3, 0.5))
    layout(layout.matrix, widths = c(1,0.4, 0.7, 0.7))

    #pheatmap(gene.vals, annotation_col = mouse.factors, annotation_colors = factor.cols, show_colnames = FALSE)
    par(mar = c(4,12,8,0), xpd = NA)
    if(data.type == "scaled"){
        imageWithText(gene.vals[pr.order,mouse.order], show.text = FALSE, split.at.vals = TRUE,
            col.scale = c("purple", "brown"), col.names = NULL, grad.dir = "ends", row.text.shift = 0.01)
    }else{
        imageWithText(gene.vals[pr.order,mouse], show.text = FALSE, split.at.vals = FALSE,
            use.pheatmap.colors = TRUE, row.text.shift = 0.01, col.names = NULL)
    }
    
    plot.dim <- par("usr")
    plot.height <- plot.dim[4] - plot.dim[3]
    #add colored bars for individual data
    yvals <- segment_region(plot.dim[4], plot.dim[4]+(plot.height*0.1), 4, "ends")

    sex <- mouse.factors[,"sex"]
    sex.col <- sapply(sex, function(x) factor.cols$sex[which(names(factor.cols$sex) == x)])
    segments(x0 = 1:ncol(gene.vals), y0 = rep(yvals[1], ncol(gene.vals)), 
        y1 = rep(yvals[2], ncol(gene.vals)), col = sex.col[mouse.order], lwd = 3)
    text("sex", x = plot.dim[3],y = mean(yvals[c(1,2)]), adj = 1)

    age <- mouse.factors[,"age"]
    age.col <- sapply(age, function(x) factor.cols$age[which(names(factor.cols$age) == x)])
    segments(x0 = 1:ncol(gene.vals), y0 = rep(yvals[2], ncol(gene.vals)), 
        y1 = rep(yvals[3], ncol(gene.vals)), col = age.col[mouse.order], lwd = 3)
    text("age", x = plot.dim[3],y = mean(c(yvals[c(2,3)])), adj = 1)

    geno <- mouse.factors[,"genotype"]
    geno.col <- sapply(geno, function(x) factor.cols$genotype[which(names(factor.cols$genotype) == x)])
    segments(x0 = 1:ncol(gene.vals), y0 = rep(yvals[3], ncol(gene.vals)), 
        y1 = rep(yvals[4], ncol(gene.vals)), col = geno.col[mouse.order], lwd = 3)
    text("genotype", x = plot.dim[3],y = mean(yvals[c(3,4)]), adj = 1)

    #barplots plot backwards, reverse the order of the r2 here
    par(mar = c(4,0,8,4))

    xmax = ceiling(max(all.r2[pr.order,show_r2], na.rm = TRUE)*10)/10
    barplot(all.r2[rev(pr.order),show_r2], horiz = TRUE, names = NA, xlim = c(0, xmax))
    mtext(paste("R2 by", show_r2), side = 1, line = 2.5)
    plot.dim <- par("usr")
    vert.lines <- bin.vector(segment_region(0.02, xmax, 5, "ends"), seq(0, xmax, 0.05))
    segments(x0 = vert.lines, y0 = 0, y1 = plot.dim[4], lty = 2, col = "darkgray")

    par(mar = c(4,4,4,4))

    #need these later
    adj.gene.vals <- adjust(t(gene.vals), dummy_covar(mouse.factors[,c("age", "genotype")]))
    gene.decomp.sex <- decomp_with_nas(adj.gene.vals, plot.results = FALSE)

    adj.gene.vals <- adjust(t(gene.vals), dummy_covar(mouse.factors[,c("sex", "genotype")]))
    gene.decomp.age <- decomp_with_nas(adj.gene.vals, plot.results = FALSE)

    adj.gene.vals <- adjust(t(gene.vals), dummy_covar(mouse.factors[,c("age", "sex")]))
    gene.decomp.geno <- decomp_with_nas(adj.gene.vals, plot.results = FALSE)

    if(test_factor == "eigengene"){
        if(!all(is.na(all.r2[,"sex"]))){
            adj.eig.sex <- gene.decomp.sex$u[,1,drop=FALSE]
            rownames(adj.eig.sex) <- rownames(mouse.factors)

            test_effect(adj.eig.sex, sex, plot.results = TRUE, 
                autoplace.text = autoplace.text, ylab = "Eigengene", 
                stat.x = stat.x, stat.y = stat.y)
        }else{
            plot.text("No variation in sex")
        }

        if(!all(is.na(all.r2[,"age"]))){
            adj.eig.age <- gene.decomp.age$u[,1,drop=FALSE]
            rownames(adj.eig.age) <- rownames(mouse.factors)

            test_effect(adj.eig.age, age, plot.results = TRUE, 
                autoplace.text = autoplace.text, ylab = "Eigengene", 
                stat.x = stat.x, stat.y = stat.y)
        }else{
            plot.text("No variation in age")
        }

        if(!all(is.na(all.r2[,"genotype"]))){
            adj.eig.geno <- gene.decomp.geno$u[,1,drop=FALSE]
            rownames(adj.eig.geno) <- rownames(mouse.factors)

            test_effect(adj.eig.geno, geno, plot.results = TRUE, 
                autoplace.text = autoplace.text, ylab = "Eigengene", 
                stat.x = stat.x, stat.y = stat.y)
        }else{
            plot.text("No variation in gennotype")
        }
    }
    if(test_factor == "mean"){
        imp.mat <- gene.vals
        for(i in 1:nrow(imp.mat)){
            imp.mat[i, which(is.na(imp.mat[i,]))] <- min(imp.mat[i,], na.rm = TRUE)
        }
        imp.mean <- matrix(colMeans(imp.mat), ncol = 1)
        rownames(imp.mean) <- rownames(mouse.factors)

        if(!all(is.na(all.r2[,"sex"]))){
            adj.mean <- adjust(imp.mean, dummy_covar(mouse.factors[,c("age", "genotype")]))        
            test_effect(adj.mean, sex, plot.results = TRUE, 
                autoplace.text = autoplace.text, ylab = "Mean Abundance (Imputed)", 
                stat.x = stat.x, stat.y = stat.y)
        }else{
            plot.text("No variation in sex.")
        }

        if(!all(is.na(all.r2[,"age"]))){
            adj.mean <- adjust(imp.mean, dummy_covar(mouse.factors[,c("sex", "genotype")]))
            test_effect(adj.mean, age, plot.results = TRUE, 
                autoplace.text = autoplace.text, ylab = "Mean Abundance (Imputed)", 
                stat.x = stat.x, stat.y = stat.y)
        }else{
            plot.text("No variation in age.")
        }

        if(!all(is.na(all.r2[,"genotype"]))){
            adj.mean <- adjust(imp.mean, dummy_covar(mouse.factors[,c("age", "sex")]))
            test_effect(adj.mean, geno, plot.results = TRUE, 
                autoplace.text = autoplace.text, ylab = "Mean Abundance (Imputed)", 
                stat.x = stat.x, stat.y = stat.y)
        }else{
            plot.text("No variation in genotype")
        }
    }


    if(!all(is.na(all.r2[,"sex"]))){
        var1 <- round(gene.decomp.sex$var.exp[1]*100)
        var2 <- round(gene.decomp.sex$var.exp[2]*100)

        plot(gene.decomp.sex$u[,1:2], pch = 16, col = sex.col, 
            xlab = paste0("PC1 (", var1, "%)"), ylab = paste0("PC2 (", var2, "%)"), 
            main = "Decomposition")
    }else{
        plot.text("No variation in sex.")
    }

    if(!all(is.na(all.r2[,"age"]))){
        var1 <- round(gene.decomp.age$var.exp[1]*100)
        var2 <- round(gene.decomp.age$var.exp[2]*100)

        plot(gene.decomp.age$u[,1:2], pch = 16, col = age.col, 
            xlab = paste0("PC1 (", var1, "%)"), ylab = paste0("PC2 (", var2, "%)"), 
            main = "Decomposition")
    }else{
        plot.text("No variation in age.")
    }

    if(!all(is.na(all.r2[,"genotype"]))){
        var1 <- round(gene.decomp.geno$var.exp[1]*100)
        var2 <- round(gene.decomp.geno$var.exp[2]*100)

        plot(gene.decomp.geno$u[,1:2], pch = 16, col = geno.col, 
            xlab = paste0("PC1 (", var1, "%)"), ylab = paste0("PC2 (", var2, "%)"), 
            main = "Decomposition")
    }else{
        plot.text("No variation in genotype.")
    }

    if(data.type != "scaled"){
        par(mar = c(2, 6, 6, 6))
        imageWithTextColorbar(gene.vals[row.order,col.order], split.at.vals = FALSE,
            use.pheatmap.colors = TRUE, cex = 1.5)
    }

    mtext(plot.label, side = 3, line = -2.5, font = 2, cex = 1.2, outer = TRUE)

    invisible(all.r2)
}
