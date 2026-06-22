
qqunif.plot<-function(pvalues, plot.label = "P Value qq plot",
xlab=expression(paste("Expected (",-log[10], " p-value)")),
ylab=expression(paste("Observed (",-log[10], " p-value)")),
line.col = "black", label.above = NULL, label.pos = 4, 
label.cex = 1, xlim = NULL){
	
	exp.dist <- runif(10000)
	log.exp <- -log10(exp.dist)
	log.obs <- -log10(pvalues)

	coords <- qqplot(log.exp, log.obs, xlab = xlab, ylab = ylab, main = plot.label, xlim = xlim)
	abline(0,1, col = line.col)

	if(!is.null(label.above)){
		label.idx <- which(coords$y >= label.above)
		text(coords$x[label.idx], coords$y[label.idx], labels = names(label.idx), 
			pos = label.pos, cex = label.cex)
	}

}
