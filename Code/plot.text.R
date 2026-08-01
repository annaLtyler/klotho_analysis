#makes a plot with text. Can be a new plot (the default)
#or added to an existing plot (if add == TRUE)
#to use x and y as relative positions, use rel.x and rel.y
#These give relative positions between 0 and 1 to deal with 
#varying plot dimensions.

plot.text <- function(the.message, srt = 0, cex = 1, x = 0.5, y = 0.5, 
	rel.x = NULL, rel.y = NULL, font = 1, adj = 0.5, add = FALSE){
	
	if(!add){
		plot.new()
		plot.window(xlim = c(0,1), ylim = c(0,1))
	}

	if(!is.null(rel.x)){
		plot.dim <- par("usr")
		plot.width = plot.dim[2] - plot.dim[1]
		x <- plot.dim[1] + (plot.width*rel.x)
	}

	if(!is.null(rel.y)){
		plot.dim <- par("usr")
		plot.height = plot.dim[4] - plot.dim[3]
		y <- plot.dim[3] + (plot.height*rel.y)
	}

	text(x, y, the.message, srt = srt, cex = cex, font = font, adj = adj)
	invisible(c("x" = x, "y" = y))
	}