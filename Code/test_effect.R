
test_effect <- function(values, plot.factor, return.text = FALSE,
    plot.results = FALSE, stat.x = 0.1, stat.y = 0.9, stat.y.spread = 0.15, 
    cex.lab = 1, plot.label = "", ylab = "Abundance (A.U.)", autoflip.stat.y = FALSE,
    autoplace.text = FALSE, n.samples = 25, min.contig = 5, jitter.factor = 1, ylim = NULL){
    
    model <- lm(values~plot.factor)
    #summary(model)

    r2 <- summary(model)$adj.r.squared
    p <- anova(model)$"Pr(>F)"[1]
    text.p <- threshold_p(p, sig.dig = 2, return.text = TRUE)
    linear.effect.size <- coefficients(summary(model))["plot.factor.L","Estimate"]
    linear.effect.p <- coefficients(summary(model))["plot.factor.L","Pr(>|t|)"]
    
        if(plot.results){
            factor.idx <- which(sapply(factor.cols, function(x) length(which(names(x) %in% levels(plot.factor)))) > 0)
            these.cols <- factor.cols[[factor.idx]]
            box.cols <- sapply(levels(plot.factor), function(x) these.cols[which(names(these.cols) == x)])
            vioplot(values~plot.factor, col = box.cols, xlab = "", main = plot.label, 
                ylab = "", cex.names = cex.lab, ylim = ylim)
            mtext(ylab, side = 2, cex = cex.lab, line = 2)
            stripchart(values~plot.factor, vertical = TRUE, pch = 16, method = "jitter",
                add = TRUE, col = dot.col)
            plot.dim <- par("usr")
            segments(x0 = plot.dim[1], x1 = plot.dim[2], y0 = 0)
            
            if(autoplace.text){
  
                possible.coord <- autoplace_text2(x = jitter(as.numeric(plot.factor), factor = jitter.factor), 
                    y = values, n.samples = n.samples, min.contig = min.contig, plot.result = FALSE) 

                #place in region relative to specified stat.x and stat.y
                text.x <- fractional_pos(min(possible.coord[,1]), max(possible.coord[,1]), stat.x)
                
                plot.dim <- par("usr")
                plot.height <- plot.dim[4] - plot.dim[3]
                min.visible <- plot.dim[3] + (plot.height*0.1) #the minimum visible is above the minimum
                #define the maximum y position based on stat.y and the box we found
                max.y <- fractional_pos(min(possible.coord[,2]), max(possible.coord[,2]), stat.y)
                #define min.y based on max.y and stat.y.spread
                min.y <- max.y - (plot.height*stat.y.spread)
                
                if(min.y < min.visible){ #don't let the text go off the bottom of the plot
                    #move the y position up the amount that it is going off the bottom
                    off.bottom <- min.visible - min.y
                    max.y <- max.y + off.bottom
                    min.y <- min.visible
                }

                #y positions are evenly distributed from max.y to min.y
                text.y <- segment_region(max.y, min.y, 3, "ends")
                r2.y <- text.y[1]
                effect.y <- text.y[2]
                p.y <- text.y[3]
            }else{
                text.x <- fractional_pos(plot.dim[1], plot.dim[2], stat.x)
                if(autoflip.stat.y){
                    if(linear.effect.size < 0){
                        stat.y <- 1.1-stat.y #automatically flip if the effect size is negative. not sure if this is a good idea
                    }
                }
                text.y <- segment_region(stat.y, stat.y - stat.y.spread, 3, "ends")
                r2.y <- fractional_pos(plot.dim[3], plot.dim[4], text.y[1])
                effect.y <- fractional_pos(plot.dim[3], plot.dim[4], text.y[2])
                p.y <- fractional_pos(plot.dim[3], plot.dim[4], text.y[3])
                
            }
            text(text.x, r2.y, bquote(italic(R)^2==.(signif(r2, 2))), adj = 0)
            text(text.x, p.y, labels = text.p, adj = 0)
            text(text.x, effect.y, labels = bquote(beta==.(signif(linear.effect.size, 2))), adj = 0)
        }

    result <- list("r2" = r2, "model.p" = p, 
        "linear.effect.size" = linear.effect.size, "linear.effect.p" = linear.effect.p)
    invisible(result)
}

