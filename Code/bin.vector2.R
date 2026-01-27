#This function groups a vector into user-defined 
#breaks. The result is a list in which each element
#resides within the sequential breaks.

bin.vector2 <- function(vectorX, breaks = seq(0,1,0.5)){
	
    bin.boundaries <- consec_pairs(breaks)
    bin.labels <- apply(bin.boundaries, 1, function(x) paste(x, collapse = "-"))
    binned.vals <- lapply(1:nrow(bin.boundaries), 
        function(x) vectorX[intersect(which(vectorX >= bin.boundaries[x,1]), which(vectorX <= bin.boundaries[x,2]))])
    names(binned.vals) <- bin.labels
	return(binned.vals)	
	
}
