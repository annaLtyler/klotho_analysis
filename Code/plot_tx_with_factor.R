#I'm turning this into a more general plotting function that will 
#handle the plot_tx_with_genotype functions as well. stat.x and 
#stat.y are fractional positions from 0 to 1

plot_tx_with_factor <- function(expr.mat, covar.table, tx_name, factor_name, 
  ylab = "Count", tx_label = "Transcript", pt_col = "#c51b8a", 
  group.cols = "lightgray", cex.labels = 1, cex.axis = 1,
  label.shift = -0.5, order.by.mean = FALSE, stat.x = 0.1, stat.y = 0.1, 
  stat.y.spread = 0.15, autoflip.stat.y = FALSE, autoplace.text = FALSE, 
  x.samples = 5, y.samples = 20, text.window.x = 2, text.window.y = 4, 
  ylim = NULL){
    
    #make a dummy matrix to adjust expression 
    factor.idx <- which(colnames(covar.table) == factor_name)
    is_numeric <- which(sapply(1:ncol(covar.table), function(x) length(levels(covar.table[,x]))) == 0)

    #dummy_covar will convert anything that is a factor to a dummy
    dummy.mat <- dummy_covar(covar.table[,-c(factor.idx)])
    #tack on any numeric covariates
    if(length(is_numeric) > 0){
      dummy.mat <- as.matrix(cbind(dummy.mat, covar.mat[,is_numeric,drop=FALSE]))
    }

    model.stats <- vector(mode = "list", length = length(tx_name))
    names(model.stats) <- tx_name

    plot.factor <- covar.table[,factor_name]

    for(i in 1:length(tx_name)){
        values <- expr.mat[tx_name[i],]

        model <- lm(values~plot.factor)
        model.n <- sapply(levels(plot.factor), function(x) length(which(plot.factor == x)))
        n.groups <- length(levels(plot.factor))
        if(length(group.cols) < n.groups){
            group.cols <- rep(group.cols, n.groups)
        }

        model.r2 <- signif(summary(model)$adj.r.squared, 2)
        model.coef <- signif(coef(summary(model))[2,"Estimate"], 2)
        model.p <- signif(anova(model)$"Pr(>F)"[1], 2)
        text.p <- threshold_p(model.p, return.text = TRUE)
        tx.stats <- c("r2" = model.r2, "beta" = model.coef, "p" = model.p, "n" = model.n)
        model.stats[[i]] <- tx.stats

        vioplot(values~plot.factor, xlab = "", xaxt = "n", tick = FALSE,
        main = paste(tx_label[i], "\n", paste(names(tx.stats)[1:3], "=", tx.stats[1:3], collapse = "; ")), 
        col = group.cols, ylab = "", 
        names = rep("", length(levels(plot.factor))))
        
        plot.dim <- par("usr")
        par(xpd = NA)
        text(x = c(1:length(levels(plot.factor))), y = plot.dim[3] + label.shift,
            labels = paste0(levels(plot.factor), "\n(", model.n, ")"), cex = cex.labels)
        mtext(ylab, side = 2, line = 2.5, cex = cex.axis)
        stripchart(values~plot.factor, 
            col = pt_col, vertical = TRUE, pch = 16, method = "jitter", add = TRUE)

        #add text for stats
        if(autoplace.text){
            possible.coord <- autoplace_text(x = jitter(as.numeric(plot.factor), factor = 2), 
                y = values, x.samples = x.samples, y.samples = y.samples, 
                text.window.x = text.window.x, text.window.y = text.window.y)
            if(length(possible.coord) > 0){
                #convert from absolute coordinates back to relative coordinates
                #to fit with our original method of placing stats
                stat.x <- (possible.coord[1,1]-plot.dim[1])/(plot.dim[2]-plot.dim[1])
                stat.y <- (possible.coord[1,2]-plot.dim[3])/(plot.dim[4]-plot.dim[3])
            }
        }
    
        text.x <- fractional_pos(plot.dim[1], plot.dim[2], stat.x)
        if(autoflip.stat.y){
            if(model.coef < 0){
                stat.y <- 1.1-stat.y #automatically flip if the effect size is negative. not sure if this is a good idea
            }
        }
        text.y <- segment_region(stat.y, stat.y - stat.y.spread, 3, "ends")
        effect.y <- fractional_pos(plot.dim[3], plot.dim[4], text.y[1])
        r2.y <- fractional_pos(plot.dim[3], plot.dim[4], text.y[2])
        p.y <- fractional_pos(plot.dim[3], plot.dim[4], text.y[3])
        text(text.x, r2.y, bquote(italic(R)^2==.(signif(model.r2, 2))), adj = 0)
        text(text.x, p.y, labels = text.p, adj = 0)
        text(text.x, effect.y, labels = bquote(beta==.(signif(model.coef, 2))), adj = 0)
    }


  stat.mat <- Reduce("rbind", model.stats)
  invisible(stat.mat)

}