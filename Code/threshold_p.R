#This function thresholds p values
#at a given minimum. If desired, it
#returns a text statement with either "p = "
#or "p <".

threshold_p <- function(p, thresh = 2.2e-16, return.text = FALSE, sig.dig = 2){
	if(p < thresh){
		if(return.text){
			return(paste("p <", thresh))
            return(bquote(italic(p)<.(thresh)))
        }else{
            return(thresh)
        }
	}else{
        if(return.text){
            return(bquote(italic(p)==.(signif(p, sig.dig))))
            }else{
			return(signif(p, sig.dig))
            }
	}
}
