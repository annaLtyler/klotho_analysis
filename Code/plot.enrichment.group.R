#This function plots a group of enrichments in a heatmap
#of -log p values
#max.char limits the number of characters shown for a term's name
#which helps keep the heatmap visible even when term names are long.
#if max.char is NULL, names are not trimmed.
#transformation is a function applied to the final matrix to help with
#visualization, such as log or sqrt

plot.enrichment.group <- function(enrichment.list, n.terms = 10, max.char = 40, 
cluster_cols = TRUE, cluster_rows = TRUE, transformation = NULL, plot.results = TRUE,
plot.label = NULL, sort.by = c("p_value", "default"), max.term.size = NULL, 
pval.thresh = 0.05, return.empty.columns = FALSE){
	
	if(is.null(enrichment.list)){
		plot.text("No enrichment")
		return(NULL)
	}

	#save any names coming in on the original list
	group.names <- names(enrichment.list)

	#threshold on the specified p value and check again for results
	sig.enrich <- lapply(enrichment.list, function(x) x$result[which(x$result[,"p_value"] <= pval.thresh),])

	list.len <- sapply(sig.enrich, length)

	if(!return.empty.columns){
		has.results <- which(list.len != 0)
	}else{
		has.results <- 1:length(sig.enrich)
	}

	enrichment.list <- sig.enrich[has.results]

	if(class(enrichment.list[[1]]) == "list"){
		enrich.list <- lapply(enrichment.list, function(x) x[[1]])
	}else{
		enrich.list <- enrichment.list
	}
	
	if(length(enrich.list) == 1){
		term.mat <- plot.enrichment(enrich.list[[1]], num.terms = n.terms, 
		order.by = sort.by, 
		plot.label = plot.label, max.term.size = max.term.size)
		return()
		}else{

		if(!is.null(max.term.size)){
			trimmed.list <- vector(mode = "list")
			trimmed.names <- NULL
			idx <- 1
			for(i in 1:length(enrich.list)){
				small.terms <- which(enrich.list[[i]][,"term_size"] <= max.term.size)
				if(length(small.terms) > 0){
					trimmed.list[[idx]] <- enrich.list[[i]][small.terms,,drop=FALSE]
					trimmed.names <- c(trimmed.names, names(enrich.list)[i])
					idx <- idx + 1
				}
			}
			names(trimmed.list) <- trimmed.names
			
			#revise has results to those that passed the filter
			has.results <- trimmed.names
		}else{
			trimmed.list <- enrich.list
		}

		sort.by <- sort.by[1]

		if(sort.by != "default"){
			sorted.enrich <- lapply(trimmed.list, function(x) if(length(x) > 0){x[order(x[,sort.by], decreasing = FALSE),]})
		}else{
			sorted.enrich <- trimmed.list
		}
		
		get.terms <- function(enrich, n.terms){
			n.row <- nrow(enrich)
			if(length(n.row) == 0){return(NULL)}
			keep <- min(c(n.row, n.terms))
			term.vals <- enrich[1:keep,c("term_id", "term_name", "p_value")]
			term.vals[,"p_value"] <- -log10(term.vals[,"p_value"])
			return(term.vals)
			}
			
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

		all.terms <- lapply(sorted.enrich, function(x) get.terms(x, n.terms))		
		u_term.id <- unique(unlist(lapply(all.terms, function(x) x[,1])))
		u_term.name <- unique(unlist(lapply(all.terms, function(x) x[,2])))
		no.na <- which(!is.na(u_term.id))
		u_term.id <- u_term.id[no.na]
		u_term.name <- u_term.name[no.na]

		term.mat <- matrix(0, nrow = length(u_term.name), ncol = length(sorted.enrich))
		rownames(term.mat) <- u_term.name
		colnames(term.mat) <- names(sorted.enrich)
		for(i in 1:length(sorted.enrich)){
			term.idx <- match(all.terms[[i]][,1], u_term.id)
			term.mat[term.idx,i] <- all.terms[[i]][,3]
			}
			
		if(!is.null(max.char)){
			trimmed.names <- sapply(rownames(term.mat), function(x) trim.name(x, max.char))
			rownames(term.mat) <- trimmed.names
			}
			
		if(!is.null(transformation)){
			trans.fun <- match.fun(transformation)
			term.mat <- trans.fun(term.mat)	
			}

		#add columns for entries with no enrichments
		#this can be helpful in indexing sometimes
		if(return.empty.columns){
			full.term.mat <- matrix(0, nrow = nrow(term.mat), ncol = length(enrichment.list))
			colnames(full.term.mat) <- group.names
			rownames(full.term.mat) <- rownames(term.mat)
			full.term.mat[,has.results] <- term.mat[,has.results]			
			term.mat <- full.term.mat
		}

		if(plot.results){
			if(!is.null(plot.label)){
				pheatmap(term.mat, cluster_cols = cluster_cols, 
				cluster_rows = cluster_rows, main = plot.label)
			}else{
				pheatmap(term.mat, cluster_cols = cluster_cols, 
				cluster_rows = cluster_rows)
			}
		}

		}
		names(rownames(term.mat)) <- u_term.id
		invisible(term.mat)
	}