#like above, but specifically for genotype
plot_tx_with_genotype <- function(expr.mat, covar.table, tx_name, 
  ylab = "Count", tx_label = "Transcript", pt_col = "#c51b8a",
  plot.results = TRUE, cex.labels = 1,
  order.by.mean = TRUE){

  not.genotype <- setdiff(1:ncol(covar.table), grep("geno", colnames(covar.table)))
  is_numeric <- which(sapply(1:ncol(covar.table), function(x) length(levels(covar.table[,x]))) == 0)
  #dummy_covar will convert anything that is a factor to a dummy
  dummy.mat <- dummy_covar(covar.table[,not.genotype])
  #tack on any numeric covariates
  if(length(is_numeric) > 0){
    dummy.mat <- as.matrix(cbind(dummy.mat, covar.mat[,is_numeric,drop=FALSE]))
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

      vs.idx <- which(covar.table[,"ordered_geno"] %in% just.vs)
      vs.test <- lm(expr.mat[tx_name[i],vs.idx]~dummy.mat[vs.idx,]+covar.table[vs.idx,"ordered_geno"])
      vs.r2 <- signif(summary(vs.test)$adj.r.squared, 2)
      vs.coef <- signif(coef(summary(vs.test))[int.idx,"Estimate"], 2)
      vs.p <- threshold_p(coef(summary(vs.test))[int.idx,"Pr(>|t|)"])

      fc.idx <- which(covar.table[,"ordered_geno"] %in% just.fc)
      fc.test <- lm(expr.mat[tx_name[i],fc.idx]~dummy.mat[fc.idx,]+covar.table[fc.idx,"ordered_geno"])
      fc.r2 <- signif(summary(fc.test)$adj.r.squared, 2)
      fc.coef <- signif(coef(summary(fc.test))[int.idx,"Estimate"], 2)
      fc.p <- threshold_p(coef(summary(fc.test))[int.idx,"Pr(>|t|)"])


      if(plot.results){
        #all genotypes in order
        vioplot(tx.vals[mean.order], col = geno.cols[names(tx.vals)[mean.order]], 
          main = paste0("\nR2 = ", all.r2, "; beta = ", all.coef, "; p = ", all.p), 
          ylim = ylim, cex.axis = cex.labels)
        mtext(ylab, side = 2, line = 2.5)
        stripchart(tx.vals[mean.order], vertical = TRUE, add = TRUE,
          col = "#c51b8a", pch = 16, method = "jitter")
        abline(h = 0)

        #Just VS
        vioplot(tx.vals[just.vs], col = geno.cols[just.vs], cex.axis = cex.labels,
          main = paste0("VS genotypes\nR2 = ", vs.r2, "; beta = ", vs.coef, "; p = ", vs.p), 
          ylim = ylim)
        mtext(ylab, side = 2, line = 2.5)
        stripchart(tx.vals[just.vs], vertical = TRUE, add = TRUE,
          col = "#c51b8a", pch = 16, method = "jitter")
        abline(h = 0)

        vioplot(tx.vals[just.fc], col = geno.cols[just.fc], cex.axis = cex.labels,
          main = paste0("FC genotypes\nR2 = ", fc.r2, "; beta = ", fc.coef, "; p = ", fc.p), 
          ylim = ylim)
        mtext(ylab, side = 2, line = 2.5)
        stripchart(tx.vals[just.fc], vertical = TRUE, add = TRUE,
          col = "#c51b8a", pch = 16, method = "jitter")
        abline(h = 0)

        mtext(tx_label[i], side = 3, outer = TRUE, line = -1.5, font = 2)
    }
  }
  result <- list("grouped_vals" = grouped.vals, "p" = c("all" = all.p, "FC" = fc.p, "VS" = vs.p))
  invisible(result)
}
