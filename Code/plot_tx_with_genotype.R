#like plot_tx_with_factor, but specifically for genotype
#It handles all the colors, and splits out VS and FC separately

plot_tx_with_genotype <- function(expr.mat, covar.table, tx_name, 
  ylab = "Count", tx_label = "Transcript", pt_col = "#c51b8a",
  plot.results = TRUE, cex.labels = 1, label.shift = -0.5,
  order.by.mean = FALSE, stat.x = 0.5, stat.y = -2, stat.y.spread = 0.15,
  autoplace.text = FALSE){

  not.genotype <- setdiff(1:ncol(covar.table), grep("geno", colnames(covar.table)))
  is_numeric <- which(sapply(1:ncol(covar.table), function(x) length(levels(covar.table[,x]))) == 0)
  #dummy_covar will convert anything that is a factor to a dummy
  dummy.mat <- dummy_covar(covar.table[,not.genotype])
  #tack on any numeric covariates
  if(length(is_numeric) > 0){
    dummy.mat <- as.matrix(cbind(dummy.mat, covar.table[,is_numeric,drop=FALSE]))
  }

  tx_idx <- which(rownames(expr.mat) %in% tx_name)
  if(length(tx_idx) == 0){return(NULL)}

  u_genotype <- levels(covar.table[,"ordered_geno"])
  geno.idx <- lapply(u_genotype, function(x) which(covar.table[,"ordered_geno"] == x))
  names(geno.idx) <- u_genotype

  if(length(u_genotype) == 5){
    just.fc <- c("WT/WT", "WT/FC", "FC/FC")
    just.vs <- c("WT/WT", "WT/VS", "VS/VS")
    geno.cols = c("WT/WT" = "darkgray", "WT/VS" = "#a6bddb", "VS/VS" = "#2b8cbe", "WT/FC" = "#a1d99b", "FC/FC" = "#31a354")
  }else{
    just.fc <- c("FC", "WT")
    just.vs <- c("WT", "VS")
    geno.cols = c("FC" = "#31a354", "WT" = "darkgray", "VS" = "#2b8cbe")
  }
  
  grouped.vals <- vector(mode = "list", length = length(tx_name))
  names(grouped.vals) <- tx_name

  if(plot.results){
    layout(matrix(c(1,1,2,3), nrow = 2, byrow = TRUE))
  }

    for(i in 1:length(tx_name)){
      #plot genotypes in order of
      adj.tx <- adjust(t(expr.mat[tx_name[i],,drop=FALSE]), dummy.mat)
      tx.vals <- lapply(geno.idx, function(x) adj.tx[x,1]) 
      grouped.vals[[i]] <- tx.vals
      ylim <- c(min(unlist(tx.vals)), max(unlist(tx.vals)))
      val.mean <- sapply(tx.vals, function(x) mean(x, na.rm = TRUE))
      if(order.by.mean){
        mean.order <- order(val.mean)
      }else{
        #otherwise, order by genotype
        #c("FC/FC", "WT/FC", "WT/WT", "WT/VS", "VS/VS")
        mean.order <- match(u_genotype, names(val.mean))
      }

      all.test <- lm(expr.mat[tx_name[i],]~dummy.mat+covar.table[,"ordered_geno"])
      int.idx <- which(rownames(coef(summary(all.test))) == "covar.table[, \"ordered_geno\"].L")
      all.r2 <- signif(summary(all.test)$adj.r.squared, 2)
      all.coef <- signif(coef(summary(all.test))[int.idx,"Estimate"], 2)
      all.p <- threshold_p(coef(summary(all.test))[int.idx,"Pr(>|t|)"])
      all.n <- sapply(levels(covar.table[,"ordered_geno"]), function(x) length(which(covar.table[,"ordered_geno"] == x)))
      names(all.n) <- paste0(names(all.n), ".n")
      all.stat <- c("r2" = all.r2, "beta" = all.coef, "p" = all.p, all.n)

      vs.idx <- which(covar.table[,"ordered_geno"] %in% just.vs)
      vs.test <- lm(expr.mat[tx_name[i],vs.idx]~dummy.mat[vs.idx,]+covar.table[vs.idx,"ordered_geno"])
      vs.r2 <- signif(summary(vs.test)$adj.r.squared, 2)
      vs.coef <- signif(coef(summary(vs.test))[int.idx,"Estimate"], 2)
      vs.p <- threshold_p(coef(summary(vs.test))[int.idx,"Pr(>|t|)"])
      vs.n <- sapply(levels(covar.table[,"ordered_geno"]), function(x) length(which(covar.table[vs.idx,"ordered_geno"] == x)))
      names(vs.n) <- paste0(names(vs.n), ".n")
      vs.stat <- c("r2" = vs.r2, "beta" = vs.coef, "p" = vs.p, vs.n)

      fc.idx <- which(covar.table[,"ordered_geno"] %in% just.fc)
      fc.test <- lm(expr.mat[tx_name[i],fc.idx]~dummy.mat[fc.idx,]+covar.table[fc.idx,"ordered_geno"])
      fc.r2 <- signif(summary(fc.test)$adj.r.squared, 2)
      fc.coef <- signif(coef(summary(fc.test))[int.idx,"Estimate"], 2)
      fc.p <- threshold_p(coef(summary(fc.test))[int.idx,"Pr(>|t|)"])
      fc.n <- sapply(levels(covar.table[,"ordered_geno"]), function(x) length(which(covar.table[fc.idx,"ordered_geno"] == x)))
      names(fc.n) <- paste0(names(fc.n), ".n")
      fc.stat <- c("r2" = fc.r2, "beta" = fc.coef, "p" = fc.p, fc.n)

      if(plot.results){
        #all genotypes in order
        vioplot(tx.vals[mean.order], col = geno.cols[names(tx.vals)[mean.order]], 
          main = paste0("\nR2 = ", all.r2, "; beta = ", all.coef, "; p = ", all.p), 
          ylim = ylim, names = rep("", length(tx.vals)))
        mtext(ylab, side = 2, line = 2.5, cex = cex.labels)
        stripchart(tx.vals[mean.order], vertical = TRUE, add = TRUE,
          col = "#c51b8a", pch = 16, method = "jitter")
        abline(h = 0)
        plot.dim <- par("usr")
        par(xpd = NA)
        text(x = c(1:length(tx.vals)), y = plot.dim[3] + label.shift,
          labels = paste0(names(tx.vals), "\n(", all.n, ")"), cex = cex.labels)

        #Just VS
        vioplot(tx.vals[just.vs], col = geno.cols[just.vs],
          main = paste0("VS genotypes\nR2 = ", vs.r2, "; beta = ", vs.coef, "; p = ", vs.p), 
          ylim = ylim, names = rep("", length(just.vs)))
        mtext(ylab, side = 2, line = 2.5, cex = cex.labels)
        stripchart(tx.vals[just.vs], vertical = TRUE, add = TRUE,
          col = "#c51b8a", pch = 16, method = "jitter")
        abline(h = 0)
        plot.dim <- par("usr")
        par(xpd = NA)
        vs.idx <- sapply(just.vs, function(x) grep(x, names(vs.n)))
        text(x = c(1:length(tx.vals[just.vs])), y = plot.dim[3] + label.shift,
          labels = paste0(names(tx.vals[just.vs]), "\n(", vs.n[vs.idx], ")"), cex = cex.labels)

        #Just FC
        vioplot(tx.vals[just.fc], col = geno.cols[just.fc], cex.labels = cex.labels,
          main = paste0("FC genotypes\nR2 = ", fc.r2, "; beta = ", fc.coef, "; p = ", fc.p), 
          ylim = ylim, names = rep("", length(just.fc)))
        mtext(ylab, side = 2, line = 2.5, cex = cex.labels)
        stripchart(tx.vals[just.fc], vertical = TRUE, add = TRUE,
          col = "#c51b8a", pch = 16, method = "jitter")
        abline(h = 0)
        plot.dim <- par("usr")
        par(xpd = NA)
        fc.idx <- sapply(just.fc, function(x) grep(x, names(fc.n)))
        text(x = c(1:length(tx.vals[just.fc])), y = plot.dim[3] + label.shift,
          labels = paste0(names(tx.vals[just.fc]), "\n(", fc.n[fc.idx], ")"), cex = cex.labels)

        mtext(tx_label[i], side = 3, outer = TRUE, line = -1.5, font = 2)
    }
  }

  stat.mat <- rbind(all.stat, vs.stat, fc.stat)
  result <- list("grouped_vals" = grouped.vals, "stats" = stat.mat)
  invisible(result)
}
