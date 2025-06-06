#This function takes in an enrichment table returned from gProfileR
#and *plots* the results in an easier-to-read table
#the "gprofiler" option of order.by uses the default ordering from
#gprofiler

plot.enrichment <- function(enrichment, num.terms = 10, text.size = 1, 
order.by = c("p_value", "gprofiler", "overlap_size", "term_size"), 
decreasing = FALSE, plot.label = "Enrichment", max.term.size = NULL,
max.char = 40, title.color = "black"){

if(class(enrichment) == "list"){
		enrichment <- enrichment[[1]]
	}

	if(is.null(enrichment) || nrow(enrichment) == 0){
		plot.new()
		plot.window(xlim = c(0, 1), ylim = c(0, 1))
		text(x = 0.5, y = 0.5, "No Significant Enrichment")
		text(x = 0.5, y = 0.75, plot.label, col = title.color)
		return()
		}
		
	if(!is.null(max.term.size)){
		small.terms <- which(enrichment[,"term_size"] <= max.term.size)
		enrichment <- enrichment[small.terms,,drop=FALSE]
	}

	order.by <- order.by[1]
	
	if(order.by != "gprofiler"){
		enrichment <- enrichment[order(enrichment[,order.by], decreasing = decreasing),]
		}	
	
	total.lines = 20

	trim.name <- function(term.name, max.char){
		split.name <- strsplit(term.name, "")
		name.len <- length(split.name[[1]])
		if(name.len < max.char){
			return(term.name)
			}else{
			paste.name <- paste0(paste0(split.name[[1]][1:max.char], collapse = ""), "...", collapse = "")
			return(paste.name)
			}	
	}

		
	#par(mar = c(0,4,4,4))
	split.text <- unlist(strsplit(enrichment[,"term_name"], ";"))
	split.text <- split.text[which(split.text != "")]
	num.terms <- min(c(num.terms, nrow(enrichment)))
	if(num.terms < 2){num.terms = 2}
	
	columns.to.write <- c("term_name", "term_size", "query_size", "intersection_size", "p_value", "source")

	sub.table <- enrichment[1:num.terms,columns.to.write]
	colnames(sub.table) <- c("term", "N-term", "N-query", "overlap", "p value", "domain")

	if(!is.null(max.char)){
	trimmed.names <- sapply(sub.table[,"term"], function(x) trim.name(x, max.char))
	sub.table[,"term"] <- trimmed.names
	}


	x.pts <- c(0.4, segment_region(0.55, 1, (ncol(sub.table) - 1), alignment = "ends"))	
	if(nrow(sub.table) > 1){
		y.start = 1
		y.end = max(c(1 - (num.terms/total.lines)), 0)
		}else{
		y.start = 0.51	
		y.end = 0.49
		}
	y.pts <- segment_region(y.start,y.end, (num.terms+1), "ends")
	# min.gap = 0.03; maj.gap = 0.08
	y.pts.dist <- mean(apply(consec_pairs(y.pts), 1, function(x) x[1] - x[2]))

	plot.new()
	plot.window(xlim = c(0, 1), ylim = c(0, 1))
	#write the column and row names of the matrix
	par(xpd = TRUE)
	text(x = 0.5, y = 1.1, plot.label, col = title.color)
	text(x = x.pts, y = rep(y.start, (length(x.pts)-1)), colnames(sub.table), adj = 1, cex = text.size)
	# y.pt <- y.start - maj.gap
	y.pt <- y.pts[1]
	row.idx <- 1
	for(d in 1:num.terms){
		split.text <- unique(strsplit(sub.table[d,1], ";")[[1]])
		for(s in 1:length(split.text)){
			if(s == 1){
				# if(d != 1){y.pt = y.pt - maj.gap}
				#on the first line, print the whole sub.table row with the first phenotype
				plot.vector = c(tolower(split.text[s]), sub.table[d,2:ncol(sub.table)])
				x.vals <- x.pts
				y.vals <- rep(y.pts[(d+1)], length(plot.vector))
				}else{
				split.y <- segment_region(y.pts[(d+1)], y.pts[(d+1)]+(y.pts.dist/5), length(split.text))
				#on subsequent phenotype rows, only print phenotype
				# y.pt = y.pt - min.gap
				plot.vector = tolower(split.text[s])
				x.vals <- x.pts[1]
				y.vals <- split.y[s]
				}
				plot.vector <- lapply(plot.vector, function(x) if(is.numeric(x)){signif(x, 2)}else{x})
				text(x = x.vals, y = y.vals, plot.vector, col = "black", adj = 1, cex = text.size)
				row.idx <- row.idx + 1
			} #end looping through split text terms
		}
	par(xpd = FALSE)
	invisible(sub.table)
	}	

