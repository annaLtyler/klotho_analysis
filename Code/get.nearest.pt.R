get.nearest.pt <- function(v, pt, strictly.greater = FALSE, strictly.less = FALSE){
	# plot(abs(pt - v))

	if(strictly.greater && strictly.less){
		stop("Only one of strictly.greater or strictly.less can be TRUE")
	}

	if(strictly.greater){
		greater.idx <- which(v > pt)
		nearest.greater <- which.min(abs(v[greater.idx] - pt))
		return(greater.idx[nearest.greater])
	}

	if(strictly.less){
		less.idx <- which(v < pt)
		nearest.less <- which.min(abs(v[less.idx] - pt))
		return(less.idx[nearest.less])
	}


	return(which.min(abs(v - pt)))
	}