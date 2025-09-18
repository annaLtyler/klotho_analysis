#xlab = ""; ylab = ""; main = NULL; main.shift = 0.12; col.names = NULL; row.names = NULL; row.text.adj = 1; row.text.shift = 0; row.text.rotation = 0; col.text.rotation = 90; col.text.adj = 1; col.text.shift = 0; show.text = TRUE; cex = 0.5; col.text.cex = 1; row.text.cex = 1; main.cex = 1; split.at.vals = FALSE; split.points = 0; col.scale = c("green", "purple", "orange", "blue", "brown", "gray"); light.dark = "f"; class.mat = NULL; grad.dir = c("high", "low", "middle", "ends"); color.fun = c("linear", "exponential"); exp.steepness = 1; global.color.scale = FALSE; global.min = NULL; global.max = NULL; sig.digs = 3;use.pheatmap.colors = FALSE; color.dist = NULL
#The custom color distribution automatically sets a global color scale
#col.scale can be a single color, one of c("green", "purple", "orange", "blue", "brown", "gray")
#or it can be a list the same length as the number of 
#classes in the class matrix. Each element should be the colors
#you want a gradient of in that class.
#if you use the list, you need to pay more attention to the order
#that you enter the ramps.
#testing: 
#display.brewer.all()
#mat <- matrix((1:100), 10, 10); imageWithText(mat, col.scale = list(brewer.pal(8, "Blues")), grad.dir = "low")
#mat <- matrix(sort(rnorm(25)), 5, 5); imageWithText(mat, split.at.vals = TRUE, col.scale = list(brewer.pal(8, "Blues"), brewer.pal(8, "Reds")), grad.dir = "ends")
#mat <- matrix(sort(rnorm(25)), 5, 5); imageWithText(mat, split.at.vals = TRUE, col.scale = c("blue", "brown"), grad.dir = "ends")

imageWithText <- function(mat, xlab = "", ylab = "", main = NULL, main.shift = 0.12, 
col.names = colnames(mat), row.names = rownames(mat), row.text.adj = 1, row.text.shift = 0, 
row.text.rotation = 0, col.text.rotation = 90, col.text.adj = 1, 
col.text.shift = 0, show.text = TRUE, cex = 0.5, col.text.cex = 1, 
row.text.cex = 1, main.cex = 1, split.at.vals = FALSE, split.points = 0, 
col.scale = c("green", "purple", "red", "orange", "blue", "brown", "yellow", "gray"), 
light.dark = "f", n.col = 4, class.mat = NULL, 
grad.dir = c("high", "low", "middle", "ends"), color.fun = c("linear", "exponential", "custom"), 
color.dist = NULL, exp.steepness = 1, global.color.scale = FALSE, global.min = NULL, 
global.max = NULL, sig.digs = 3, use.pheatmap.colors = FALSE, na.col = "lightgray", 
gridlines = FALSE){

		require(grid)
		
		#make sure Inf and -Inf are coded as NA
		mat[which(!is.finite(mat))] <- NA
		
		if(length(which(is.na(mat))) == length(mat)){
			return()
			}
		# if(length(light.dark) < length(col.scale)){light.dark <- rep(light.dark, length(col.scale))}
		
		color.fun <- color.fun[1]
		
		end.fudge.factor = 10^-10

		if(is.null(class.mat)){
			class.mat <- matrix(1, dim(mat)[1], dim(mat)[2])
			if(!is.null(color.dist)){
				dist.class <- rep(1, length(color.dist))
			}
		}

		if(split.at.vals){
			for(p in 1:length(split.points)){
				class.mat[which(mat >= split.points[p])] <- class.mat[which(mat >= split.points[p])] + 1
				if(!is.null(color.dist)){ #if we have specified a color distribution
					dist.class[which(color.dist >= split.points[p])] <- dist.class[which(color.dist >= split.points[p])] + 1
				}
			}
		# if(length(grad.dir) == 2){grad.dir <- "ends"}else{grad.dir <- "high"}		
		}else{
			split.points <- NULL	
		}
		class.mat[which(is.na(mat))] <- NA
		

		if(is.null(color.dist) && !global.color.scale){
			#If there is no class 1, subtract 1 until there is a class 1
			while(min(class.mat, na.rm = TRUE) > 1){class.mat <- class.mat - 1}
		}
		
		if(is.null(color.dist)){
			classes <- sort(unique(as.vector(class.mat)))
		}else{
			classes <- sort(unique(dist.class))
		}
		num.classes <- length(classes)

		if(num.classes == 1 && !global.color.scale){
			class.mat <- matrix(1, nrow(mat), ncol(mat))
			}
	
		if(length(col.scale) == (length(split.points)+1)){
			class.cols <- col.scale
			}else{
			class.cols <- col.scale[classes]
			}
		if(length(col.scale) < num.classes){
			extra.cols <- num.classes - length(col.scale)
			col.scale <- c(col.scale, rep(col.scale, ceiling(extra.cols/length(col.scale))))
			}
			
		get.default <- grep("h", grad.dir)
		if(length(get.default) > 0){
			grad.dir <- "high"
			}
		
		# if(light.dark == "f"){max.col = 4}else{max.col = 8}
		max.col = 4
		dir.list <- vector(mode = "list", length = num.classes)
		names(dir.list) <- classes
		if(grad.dir == "high"){
			for(i in 1:length(dir.list)){
				dir.list[[i]] <- 1:max.col
				}
			}
		if(grad.dir == "low"){
			for(i in 1:length(dir.list)){
				dir.list[[i]] <- max.col:1
				}
			}
		if(grad.dir == "middle"){
			if(length(dir.list) != 2){stop("I can only color the middle if there are exactly two classes")}
			dir.list[[1]] <- 1:max.col
			dir.list[[2]] <- max.col:1
			}
			
			
		if(grad.dir == "ends"){
			if(length(dir.list) != 2){stop("I can only color the ends if there are exactly two classes")}
			dir.list[[1]] <- max.col:1
			dir.list[[2]] <- 1:max.col
			}



		#============================================================================
		#internal functions
		#============================================================================
		#This function takes in a matrix of values matched with colors, and 
		#a vector of values. It matches up the appropriate color for each
		#value in the vector
		bin.cols <- function(color.mat, V){
			color.v <- nearest.vals <- rep(NA, length(V))
			for(i in 1:length(V)){
				if(!is.na(V[i])){
					diff.v  <- V[i] - as.numeric(color.mat[,1])
					closest.val <- get.nearest.pt(as.numeric(color.mat[,1]), V[i])
					nearest.vals[i] <- closest.val
					#closest.val <- which(abs(diff.v) == min(abs(diff.v)))[1]
					color.v[i] <- color.mat[closest.val,2]
				}
			}
			#plot(V, nearest.vals)
			return(color.v)
			#plot(V, pch = 16, col = color.v)
		}

		#This function generates the matrix of colors to use in 
		#the raster function
		fill.color.ramp <- function(mat, class.mat, global){

			#make color scales for each class or globally as defined
			color.scales <- vector(mode = "list", length = num.classes)
			names(color.scales) <- classes

			ColorRamp <- matrix(NA, dim(mat)[1], dim(mat)[2])
			num.classes = length(unique(as.vector(class.mat[which(!is.na(class.mat))])))
			
			if(global.color.scale){
					
				if(is.null(global.min)){
					overall.min = min(mat, na.rm = TRUE)
				}else{
					overall.min = global.min
				}
				if(is.null(global.max)){
					overall.max = max(mat, na.rm = TRUE)
				}else{
					overall.max = global.max
				}
					class.list <- c(overall.min, split.points, overall.max)
			}

			for(cl in 1:length(classes)){
				
				if(global.color.scale){
					min.cl <- class.list[classes[cl]]
					max.cl <- class.list[(classes[cl]+1)]
				}else{
					if(length(which(class.mat == classes[cl])) > 0 && !all(is.na(mat[which(class.mat == classes[cl])]))){
						min.cl <- min(mat[which(class.mat == classes[cl])], na.rm = TRUE)
						max.cl <- max(mat[which(class.mat == classes[cl])], na.rm = TRUE)
					}else{
						min.cl <- NA	
						}						
				}
					
				if(!is.na(min.cl)){
					if(color.fun == "linear"){
						ColorLevels <- seq(min.cl, max.cl, length=256)
						#plot(ColorLevels)
					}
					if(color.fun == "exponential"){
						ColorLevels <- exp.color.fun(min.cl, max.cl, steepness = exp.steepness, num.cols=256)	
						#plot(ColorLevels)
					}
					if(color.fun == "custom"){
						#use the class min and max from the full distribution
						min.cl <- min(color.dist[which(dist.class == classes[cl])], na.rm = TRUE)
						max.cl <- max(color.dist[which(dist.class == classes[cl])], na.rm = TRUE)
						class.dist <- color.dist[intersect(which(color.dist >= min.cl), which(color.dist <= max.cl))]
						#plot(x = sort(color.dist))
						#points(1:length(class.dist), sort(class.dist), col = "red")
						#if(global.color.scale){
						#	ColorLevels <- custom.color.fun(color.dist)
						#}else{
						ColorLevels <- custom.color.fun(class.dist)
						#}
						#plot(ColorLevels, 1:length(ColorLevels))
						}
	
					#plot(ColorLevels)

					col.vals <- get.color(col.scale[[cl]], light.dark = light.dark, n.colors = n.col)
					#barplot(rep(1:length(col.vals)), col = col.vals)

					color.locale <- which(names(color.scales) == classes[cl])
					color.scales[[color.locale]] <- colorRampPalette(col.vals[dir.list[[color.locale]]])
					
					#find the entries in each class
					entry.locale <- which(class.mat == classes[cl])
					if(length(entry.locale) > 0){
						entry.vals <- mat[entry.locale]
						#plot(entry.vals)
						color.key <- cbind(ColorLevels, do.call(color.scales[[color.locale]], list(256)))
						entry.cols <- bin.cols(color.mat = color.key, V = entry.vals)
						#par(mfrow = c(1,2))
						#plot(x = as.numeric(color.key[,1]), y = 1:nrow(color.key), col = color.key[,2], pch = 16)
						#plot(x = entry.vals, 1:length(entry.vals), col = entry.cols, pch = 16, xlim = c(min(as.numeric(color.key[,1])), max(as.numeric(color.key[,1]))))
						entry.order <- order(entry.locale)
						ColorRamp[entry.locale[entry.order]] <- entry.cols
						}
					}
				}
			return(ColorRamp)
			
			}
		
		#============================================================================


		if(use.pheatmap.colors){
			pal <- colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(100)
			if(global.color.scale){
				bks <- pheatmap:::generate_breaks(c(global.min, global.max), length(pal), 
				center = F)
			}else{
				bks <- pheatmap:::generate_breaks(mat, length(pal), center = F)
			}
			ColorRamp <- pheatmap:::scale_colours(mat, col=pal, breaks=bks, na_col = na.col)
		}else{
			ColorRamp = fill.color.ramp(mat, class.mat, global.color.scale)
		}
		 	

		zmin <- min(mat, na.rm = TRUE); zmax <- max(mat, na.rm = TRUE)

		na.locale <- which(is.na(mat))
		if(length(na.locale) > 0){
			mat[na.locale] <- 0
			}
		
		all.ind <- which(!is.na(mat), arr.ind = TRUE)
		if(length(na.locale) > 0){
			mat[na.locale] <- NA
			}
		
		#translate the ColorRamp matrix to rgb matrices so we can use grid.
		rgb.mat <- col2rgb(ColorRamp)
		col <- matrix(rgb(rgb.mat["red",]/256,rgb.mat["green",]/256,rgb.mat["blue",]/256), dim(mat)[1], dim(mat)[2])

		max.dim <- max(c(dim(mat)[1], dim(mat)[2]))

		plot(c(1, dim(mat)[2]), c(1, dim(mat)[1]), type = "n", axes = FALSE, xlab = xlab, ylab = ylab, xlim = c(0.7, dim(mat)[2]+0.2), ylim = c(0.7, dim(mat)[1]+0.2), bg = "transparent")

		rasterImage(col, xleft = 0.5, ybottom = 0.5, xright = dim(mat)[2]+0.5, ytop = dim(mat)[1]+0.5, interpolate = FALSE, bg = "transparent")
		x.coord <- matrix(segment_region(0.5, dim(mat)[2]+0.5, dim(mat)[2], "center"), nrow = dim(mat)[1], ncol = dim(mat)[2], byrow = TRUE)
		y.coord <- matrix(segment_region(0.5, dim(mat)[1]+0.5, dim(mat)[1], "center"), nrow = dim(mat)[1], ncol = dim(mat)[2])
		
		if(show.text){
			text(x.coord, rev(y.coord), labels = signif(as.vector(mat), sig.digs), cex = cex)
		}
		
		if(!is.null(main)){
			par(xpd = TRUE)
			plot.range = max(y.coord) - min(y.coord)
			main.y <- max(y.coord) + (plot.range*(main.shift)) #positive numbers move out of plotting area
			text(mean(x.coord[1,]), main.y, labels = main, cex = main.cex)
			par(xpd = FALSE)
			}
			
		if(!is.null(col.names)){
			if(length(col.names) != dim(mat)[2]){
				stop("There is a different number of column names than columns.")
				}
			par(xpd = TRUE)
			plot.range <- max(y.coord) - min(y.coord)
			col.text.y <- min(y.coord) - (plot.range*(col.text.shift)) #positive numbers move out of plotting area
			text((x.coord[1,]), col.text.y, labels = col.names, srt = col.text.rotation, adj = col.text.adj, cex = col.text.cex)
			}

		if(!is.null(row.names)){
			if(length(row.names) != dim(mat)[1]){
				stop("There is a different number of row names than rows.")
				}
			par(xpd = TRUE)
			plot.range <- (max(x.coord) - min(x.coord))
			row.text.x <- min(x.coord) - (plot.range*(row.text.shift)) #positive numbers move out of plotting area
			text(row.text.x, y.coord[,1], labels = rev(row.names), adj = row.text.adj, cex = row.text.cex, srt = row.text.rotation)
			}

	}