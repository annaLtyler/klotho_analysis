



lighten <- function(color, factor=1.4){
    col <- col2rgb(color)
    light.col <- col*factor
    while(max(light.col) > 256){
    		factor <- max(light.col)/256
    		light.col <- col*factor
	    	}
    col <- rgb(t(light.col), maxColorValue=255)
    col
}

