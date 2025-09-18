#This internal function stores colors for image 
#plotting. Given a color name it returns the hex 
#colors used to make color ramps
#test.col <- get.color("blue", plot.results = TRUE)
#test.col <- get.color("red", n.colors = 10, plot.results = TRUE)
#test.col <- get.color(c("blue", "white", "red"), n.colors = 20, plot.results = TRUE)

get.color <- function(col.name, light.dark = c("f", "l", "d"), n.colors = 4, 
	plot.results = FALSE, verbose = FALSE){
	
	light.dark.check <- grep("f", light.dark)
	if(length(light.dark.check) > 0){light.dark = "f"}
	
	possible.light.dark <- c("f", "l", "d")
	light.dark.check2 <- match(light.dark, possible.light.dark)
	if(is.na(light.dark.check2)){
		if(verbose){
			cat("Possible specifications of light.dark are:", possible.light.dark, sep = "\n")
		}
		stop()
		}
	
	possible.cols <- c("green", "purple", "red", "orange", "blue", "brown", "yellow", "gray")		
	col.check <- match(col.name, possible.cols)
	use.custom <- is.na(col.check[1]) || length(col.name) > 1 #check for a single color name on our list

	if(use.custom){ #If we have provided a color not on the list...
		if(length(col.name) == 1){
			if(light.dark == "l" || light.dark == "f"){
				if(verbose){
					message("Detected custom color. Mixing with white.\n")
				}
				col.fun <- colorRampPalette(c("white", col.name)) #ramp gets darker
				col.vals <- col.fun(n.colors)
			}
			if(light.dark == "d"){
				if(verbose){
					message("Detected custom color. Mixing with black\n")
				}
				col.fun <- colorRampPalette(c(col.name, "black")) #ramp gets darker
				col.vals <- col.fun(n.colors)
			}
		}else{ #if we have more than one color, make a ramp out of the provieded colors
				if(verbose){
					message("Detected custom colors for ramp. Make sure list has increasing darkness.\n")
				}
				col.fun <- colorRampPalette(col.name) #ramp gets darker
				col.vals <- col.fun(n.colors)
		}
	}else{ #if the color provided is on our list, pull from our pre-specified color ramps

		light.mat <- matrix(
			c("#edf8fb", "#ccece6", "#99d8c9", "#66c2a4",
			"#f2f0f7", "#dadaeb", "#bcbddc", "#9e9ac8",
			"#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a",
			"#feedde", "#fdd0a2", "#fdae6b", "#fd8d3c",
			"#eff3ff", "#c6dbef", "#9ecae1", "#6baed6",
			"#f5f5f5", "#f6e8c3", "#dfc27d", "#bf812d",
			"#ffffe5", "#fff7bc", "#fee391", "#fec44f",
			"#ffffff", "#f0f0f0", "#d9d9d9", "#bdbdbd"), nrow = 4, byrow = FALSE)


			dark.mat <- matrix(
			c("#66c2a4", "#41ae76", "#238b45", "#005824", #green
			"#9e9ac8", "#807dba", "#6a51a3", "#4a1486", #purple
			"#fb6a4a", "#ef3b2c", "#cb181d", "#99000d", #red
			"#fd8d3c", "#f16913", "#d94801", "#8c2d04",
			"#6baed6", "#4292c6", "#2171b5", "#084594",
			"#dfc27d", "#bf812d", "#8c510a", "#543005",
			"#fec44f", "#fe9929", "#ec7014", "#cc4c02",
			"#bdbdbd", "#969696", "#737373", "#525252"), nrow = 4, byrow = FALSE)


		full.mat <- rbind(light.mat[c(1,3),], dark.mat[c(2,4),])
		
		if(light.dark == "l"){
			# color order: same as in arguments, light to dark 
			all.col.ref <- light.mat
			}
		if(light.dark == "d"){
			#color order: same as in arguments, light to dark 
			all.col.ref <- dark.mat
			}

		if(light.dark == "f"){
			all.col.ref <- 	full.mat
			}
		colnames(all.col.ref) <- possible.cols

		col.locale <- which(colnames(all.col.ref) == col.name)
		col.ramp <- all.col.ref[,col.locale]

		col.fun <- colorRampPalette(col.ramp) #ramp gets darker
		col.vals <- col.fun(n.colors)
	}	

	if(plot.results){
		barplot(rep(1, length(col.vals)), col = col.vals)
	}
	return(col.vals)
}
