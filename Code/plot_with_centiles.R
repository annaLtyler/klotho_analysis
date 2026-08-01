plot_with_centiles <- function(x, y, main = "", xlab = "", 
    ylab = "", xlim = NULL, ylim = NULL, 
    col.ramp = brewer.pal(9, "BrBG")[c(3,2,2,2)], 
    centile.seq = seq(0, 1, 0.01), lwd = 2){

    plot.hexbin.as.plot(x = x, y = y, 
    ylab = "", xlab = "",
    main = "", n.bins = 4, use.pheatmap.colors = FALSE, 
    custom.colors = col.ramp, with.model = FALSE, 
    legend.pos = NA, write.results = FALSE, xlim = xlim, 
    ylim = ylim)

    mtext(main, side = 3, line = 0.5, font = 2, cex = 1.5)
    mtext(xlab, side = 1, line = 2.5)
    mtext(ylab, side = 2, line = 2.5)

    #add line for mean of deciles
    #break genes into deciles based on variance explained
    x.seq <- centile.seq
    seq.mids <- rowMeans(consec_pairs(x.seq))

    #find the deciles of variance explained
    x.deciles <- quantile(x, probs = x.seq, na.rm = TRUE)

    #there may be many transcripts with 0 variance explained.
    #the centiles will split these into different groups, which
    #causes problems with plotting. Remove any centiles with 
    #duplicate values to group all those genes into a single
    #bin.
    dups <- which(duplicated(x.deciles))
    if(length(dups) > 0){
        x.deciles <- x.deciles[-dups]
    }

    #use these deciles as break points and find the trait correlations
    #of the genes that fit into the variance explained deciles
    y.vals <- bin_by_hist(x = x, y = y, plot.result = FALSE,
        breaks = x.deciles)
    #sapply(trait.cor.vals$yvals, length)
    decile.mids <- rowMeans(consec_pairs(x.deciles))
    
    #find the mean trait correlation for these genes
    decile.means <- sapply(y.vals$yvals, 
        function(x) mean(x, na.rm = TRUE))    
    
    #points(seq.mids, decile.means, type = "b", pch = 16)
    smoothed.mean <- DescTools::SmoothSpline(decile.mids, decile.means)
    points(smoothed.mean, type = "l", lwd = lwd, col = "gray30")

    #find the 95th percentile of trait correlations for these genes
    upper.val <- sapply(y.vals$yvals, 
        function(x) get.percentile(x, 95))
    smoothed.upper <- DescTools::SmoothSpline(decile.mids, upper.val)
    points(smoothed.upper, type = "l", lwd = lwd, col = "gray30", lty = 2)

    lower.val <- sapply(y.vals$yvals, 
        function(x) get.percentile(x, 5))
    smoothed.lower <- DescTools::SmoothSpline(decile.mids, lower.val)
    points(smoothed.lower, type = "l", lwd = lwd, col = "gray30", lty = 2)

    spear.cor <- cor.test(x, y, use = "pairwise.complete.obs", method = "spearman")
    spear.p <- threshold_p(spear.cor$p.value, return.text = TRUE)
    spear.rho <- spear.cor$estimate
    plot.text(bquote("Spearman" ~ rho == .(signif(spear.rho, 2))), rel.x = 0.95, rel.y = 0.95, add = TRUE, adj = 1)
    #plot.text(paste("rho =", signif(spear.rho, 2)), rel.x = 0.95, rel.y = 0.95, add = TRUE, adj = 1)
    plot.text(spear.p, rel.x = 0.95, rel.y = 0.85, add = TRUE, adj = 1)
}