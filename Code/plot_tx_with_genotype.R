#like above, but specifically for genotype
plot_tx_with_genotype <- function(expr.mat, covar.table, tx_name, 
  ylab = "Count", tx_label = "Transcript", pt_col = "#c51b8a",
  plot.results = TRUE, geno.cols = c("WT/WT" = "darkgray", 
  "WT/VS" = "#a6bddb", "VS/VS" = "#2b8cbe", 
  "WT/FC" = "#a1d99b", "FC/FC" = "#31a354"), cex.labels = 1,
  order.by.mean = TRUE){


  not.genotype <- which(colnames(covar.table) != "climb_geno")
  dummy.mat <- dummy_covar(covar.table[,not.genotype])

  tx_idx <- which(rownames(expr.mat) == tx_name)
  if(length(tx_idx) == 0){return(NULL)}

  u_genotype <- levels(as.factor(covar.table[,"climb_geno"]))
  geno.idx <- lapply(u_genotype, function(x) which(covar.table[,"climb_geno"] == x))
  names(geno.idx) <- u_genotype

  just.fc <- c("WT/WT", "WT/FC", "FC/FC")
  just.vs <- c("WT/WT", "WT/VS", "VS/VS")
  
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
        mean.order <- match(c("FC/FC", "WT/FC", "WT/WT", "WT/VS", "VS/VS"), names(val.mean))
      }

      all.test <- aov.by.list(tx.vals, return.aov = TRUE)
      all.p <- anova(all.test)$"Pr(>F)"[1]
      #pairwise.p <- TukeyHSD(all.test)
      vs.test <- aov.by.list(tx.vals[just.vs])
      vs.p <- vs.test$"Pr(>F)"[1]
      fc.test <- aov.by.list(tx.vals[just.fc])
      fc.p <- fc.test$"Pr(>F)"[1]


      if(plot.results){
        #all genotypes in order
        vioplot(tx.vals[mean.order], col = geno.cols[names(tx.vals)[mean.order]], 
          main = paste("\np =", signif(all.p,2)), ylim = ylim, cex.axis = cex.labels)
        mtext(ylab, side = 2, line = 2.5)
        stripchart(tx.vals[mean.order], vertical = TRUE, add = TRUE,
          col = "#c51b8a", pch = 16, method = "jitter")
        abline(h = 0)

        #Just VS
        vioplot(tx.vals[just.vs], col = geno.cols[just.vs], cex.axis = cex.labels,
          main = paste("VS genotypes\np =", signif(vs.p, 2)), ylim = ylim)
        mtext(ylab, side = 2, line = 2.5)
        stripchart(tx.vals[just.vs], vertical = TRUE, add = TRUE,
          col = "#c51b8a", pch = 16, method = "jitter")
        abline(h = 0)

        vioplot(tx.vals[just.fc], col = geno.cols[just.fc], cex.axis = cex.labels,
          main = paste("FC genotypes\np =", signif(fc.p, 2)), ylim = ylim)
        mtext(ylab, side = 2, line = 2.5)
        stripchart(tx.vals[just.fc], vertical = TRUE, add = TRUE,
          col = "#c51b8a", pch = 16, method = "jitter")
        abline(h = 0)

        mtext(tx_label[i], side = 3, outer = TRUE, line = -1.5)
    }
  }
  result <- list("grouped_vals" = grouped.vals, "p" = c("all" = all.p, "FC" = fc.p, "VS" = vs.p))
  invisible(result)
}
