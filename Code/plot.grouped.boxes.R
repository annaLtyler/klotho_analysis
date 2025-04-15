plot.grouped.boxes <- function(group.list, group.labels = names(group.list), 
group.cols = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3"),
main = "", type = c("list", "matrix"), plot.grouping = c("outer", "inner"),
plot.type = c("box", "stripchart", "vioplot"), strip.method = "jitter", strip.offset = 0.1, 
print.vals = c("mean", "median"), ylab = "", las = 1,
stats.cex = 0.7, label.srt = 0, legend.x = NULL, legend.y = NULL, notch = FALSE,
cex = 1, cex.names = 1, pch = 16, within.group.sep = 0.7, between.group.sep = 1.3){

	oldPar <- par(no.readonly = TRUE)
	on.exit(oldPar)

	plot.type = plot.type[1]
	print.vals <- print.vals[1]
	type <- type[1]
	plot.grouping = plot.grouping[1]

	if(plot.grouping == "inner"){
		new.list <- lapply(1:length(group.list[[1]]), function(x) lapply(group.list, function(y) y[[x]]))
		names(new.list) <- names(group.list[[1]])
		group.list <- new.list
	}

	ymin <- min(unlist(group.list), na.rm = TRUE)*0.9
	ymax <- max(unlist(group.list), na.rm = TRUE)*1.1
	xmin = 0

	if(type == "list"){
		max.iter <- length(group.list[[1]]) #number of elements in each list
		}else{
		max.iter <- dim(group.list[[1]])[2]
		}

	#use the iteration scheme below to calculate the maximum position of plotted elements
	box.pos = 1
	for(i in 1:max.iter){ #for each element of an individual group
		for(l in 1:length(group.list)){ #plot the ith element from group l
		if(l < length(group.list)){
			box.pos = box.pos + within.group.sep
			}else{
			box.pos = box.pos + between.group.sep
			}			
		}
	}

	xmax <- box.pos
	
	#reset box.pos to 1
	box.pos <- 1
	plot.height <- ymax - ymin


	plot.new()
	plot.window(xlim = c(xmin, xmax), ylim = c(ymin, ymax))

	for(i in 1:max.iter){ #for each element of an individual group
		group.pos <- rep(NA, length(group.list))
		for(l in 1:length(group.list)){ #plot the ith element from group l
			
			if(type == "list"){
				data.vals <- group.list[[l]][[i]];label <- names(group.list[[l]])[i]
				}else{
				data.vals <- group.list[[l]][,i]; label <- colnames(group.list[[l]])[i]
				}

			if(length(data.vals) > 0){
				col.idx <- l%%length(group.cols)
				if(col.idx == 0){col.idx <- length(group.cols)}
				if(plot.type == "box"){
					boxplot(as.vector(data.vals), at = box.pos, add = TRUE, 
						col = group.cols[col.idx], 
						axes = FALSE, main = "", notch = notch, cex = cex, pch = pch, las = las)
				}
				if(plot.type == "stripchart"){
					stripchart(as.vector(data.vals), at = box.pos, add = TRUE, 
						col = group.cols[col.idx], axes = FALSE, 
						main = "", method = strip.method, offset = strip.offset, 
						vertical = TRUE, pch = pch, las = las)
				}
				if(plot.type == "vioplot"){
					vioplot(as.vector(data.vals), at = box.pos, add = TRUE, 
						col = group.cols[col.idx], axes = FALSE, 
						main = "", las = las)
				}

			}
			mtext(main, side = 3, line = 0)
			mtext(ylab, side = 2, line = 2.5)

			if(!is.na(print.vals)){
			par(xpd = TRUE)
			if(length(data.vals) > 0){
				if(print.vals == "mean"){
					print.val1 <- signif(mean(data.vals, na.rm = TRUE), 2)
					}else{
					print.val1 <- signif(median(data.vals, na.rm = TRUE), 2)
					}
				print.val2 <- signif(sd(data.vals, na.rm = TRUE), 2)
				text(x = box.pos, y = (ymin - (plot.height*0.05)), labels = print.val1, cex = stats.cex)
				text(x = box.pos, y = (ymin - (plot.height*0.075)), labels = "+/-", cex = stats.cex)
				text(x = box.pos, y = (ymin - (plot.height*0.1)), labels = print.val2, cex = stats.cex)
				par(xpd = FALSE)
				}

			}
			group.pos[l] <- box.pos
			if(l < length(group.list)){
				box.pos = box.pos + within.group.sep
				}else{
				box.pos = box.pos + between.group.sep
				}			
			} #end looping through groups
		#write a label for the group
		par(xpd = TRUE)
		text(x = mean(group.pos), y = (ymin - (plot.height*0.15)), labels = label, srt = label.srt, 
			cex = cex.names)
		par(xpd = FALSE)
		} #end looping through group elements

	axis(2)

	par(xpd = NA)
	if(is.null(legend.x) || is.null(legend.y)){
		legend("topleft", fill = group.cols, legend = group.labels)
	}else{
		legend(legend.x, legend.y, fill = group.cols, legend = group.labels)
	}
	par(xpd = TRUE)	

	}

