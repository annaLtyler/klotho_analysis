#This function thresholds p values
#at a given minimum. If desired, it
#returns a text statement with either "p = "
#or "p <".
#scientific notation requires the package corto

threshold_p <- function(p, thresh = 2.2e-16, return.text = FALSE, sig.dig = 2,
    sci.not = FALSE, sci.thresh = 0.001){
	if(p < thresh){
		if(return.text){
			if(sci.not && p < sci.thresh){
                if(p < thresh){
                    return(c(bquote(italic(p)<.), scinot(signif(thresh, sig.dig))))    
                }else{
                    return(c(bquote(italic(p)<.), scinot(signif(p, sig.dig))))
                }
            }else{
                return(bquote(italic(p)<.(thresh)))
            }
        }else{
            return(thresh)
        }
	}else{
        if(return.text){
            if(sci.not && p < sci.thresh){
                return(c(bquote(italic(p)==.), scinot(signif(p, sig.dig))))
                #return(bquote(italic(p)==.(scinot(signif(p, sig.dig)))))
            }else{
                return(bquote(italic(p)==.(signif(p, sig.dig))))
            }
            }else{
			return(signif(p, sig.dig))
            }
	}
}
