#This function uses a bag-of-words approach to summarize
#an enrichment result from gprofiler. We split each GO
#term into a bag of words. We remove punctuation marks
#and common English words (which can be modified in remove.words.
#Each remaining word is weighted by the -log10 of the GO term 
#p value. We find all unique words, and sum up their weights 
#across all terms. We return the num.words words with the
#highest weights.

summarize_enrichment <- function(enrichment, num.words = 5, num.terms = 10, max.term.size = NULL,
    order.by = c("p_value", "gprofiler", "overlap_size", "term_size"), decreasing = FALSE, 
    remove.words = c(data(PubMed_stopwords), stopwords(), "process")){

    if(class(enrichment) == "list"){
		enrichment <- enrichment[[1]]
	}

	if(is.null(enrichment) || nrow(enrichment) == 0){
		return("No enriched terms")
		}
		
	if(!is.null(max.term.size)){
		small.terms <- which(enrichment[,"term_size"] <= max.term.size)
		enrichment <- enrichment[small.terms,,drop=FALSE]
	}

	order.by <- order.by[1]
	
	if(order.by != "gprofiler"){
		enrichment <- enrichment[order(enrichment[,order.by], decreasing = decreasing),]
	}	
			
	#par(mar = c(0,4,4,4))
	split.text <- unlist(strsplit(enrichment[,"term_name"], ";"))
	split.text <- split.text[which(split.text != "")]
	num.terms <- min(c(num.terms, nrow(enrichment)))
	if(num.terms < 2){num.terms = 2}

    sub.text <- split.text[1:num.terms]

    #give weights to the words based on the terms p values
    term.words <- lapply(sub.text, function(x) strsplit(x, " ")[[1]])
    
    #remove punctuation marks and strip out common words
    remove.punct <- c(".", ",")
    for(i in 1:length(term.words)){
        for(j in 1:length(remove.punct)){
            term.words[[i]] <- gsub(remove.punct[j], "", term.words[[i]], fixed = TRUE)
            term.words[[i]] <- setdiff(term.words[[i]], remove.words)
        }
    }
    
    term.weights <- lapply(1:length(term.words), function(x) rep(-log10(enrichment[x,"p_value"]), length(term.words[[x]])))
    word.weights <- unlist(term.weights)
    names(word.weights) <- unlist(term.words)

    u_words <- unique(unlist(term.words))
    final.weights <- sapply(u_words, function(x) sum(word.weights[which(names(word.weights) == x)]))
    sorted.weights <- sort(final.weights, decreasing = TRUE)
    
    #return the num.words with the highest weights
    summary.words <- names(sorted.weights)[1:num.words]
    return(summary.words)
}
