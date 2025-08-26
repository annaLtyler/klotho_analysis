#enrichment.result is an object returned from gost
#with evcodes set to TRUE
#if color.all.categories is TRUE, all different types of categories
#GO:CC, GO:BP, etc will get its own color. Otherwise, genes and terms
#each get assigned one color
#filter.source is a vector of strings used to filter by sources.
#To plot only GO:BP and GO:MF, for example, use filter.source = c("GO:BP", "GO:MF")

go_gene_net <- function(enrichment.result, max.pval = 0.05, max.term.size = 3000,
    filter.source = NULL, plot.results = FALSE, min.vertex.size = 1, 
    max.vertex.size = 20, color.all.categories = TRUE){
    
    #filter by term size
	if(!is.null(max.term.size)){
		small.terms <- which(enrichment.result[[1]][,"term_size"] <= max.term.size)
		enrichment.result[[1]] <- enrichment.result[[1]][small.terms,,drop=FALSE]
	}
    if(nrow(enrichment.result[[1]]) == 0){
        plot.text("No results to show.")
        return(NULL)
    }

    #filter by source
    if(!is.null(filter.source)){
        source.idx <- which(enrichment.result[[1]][,"source"] %in% filter.source)
        enrichment.result[[1]] <- enrichment.result[[1]][source.idx,,drop=FALSE]
    }
    if(nrow(enrichment.result[[1]]) == 0){
        plot.text("No results to show.")
        return(NULL)
    }

    #filter by p value
    term.p <- enrichment.result[[1]]$p_value
    names(term.p) <- enrichment.result[[1]]$term_name
    sig.vals <- which(term.p <= max.pval)
    if(length(sig.vals) == 0){
        plot.text("No results to show.")
        return(NULL)
    }

    go.terms <- enrichment.result[[1]]$term_name[sig.vals]
    go.cat <- enrichment.result[[1]]$source[sig.vals]
    #cat(go.terms, sep = "\n")
    gene.list <- enrichment.result[[1]]$intersection[sig.vals]
    if(is.numeric(gene.list[1])){
        stop("evcodes must be set to TRUE to run this function.")
    }
    
    u_term <- unique(go.terms)
    #assign each term a category
    term.cat <- sapply(u_term, function(x) go.cat[which(go.terms == x)])
    u_cat <- unique(term.cat)
    split.gene  <- strsplit(gene.list, ",")
    u_gene <- unique(unlist(split.gene))
    
    term_mat <- matrix(0, nrow = length(u_term), ncol = length(u_gene))
    rownames(term_mat) <- u_term
    colnames(term_mat) <- u_gene

    for(i in 1:length(u_term)){
        term_mat[i,split.gene[[i]]] <- term_mat[i,split.gene[[i]]] + 1
    }

    #barplot(sort(colSums(term_mat)), las = 2)
    #barplot(sort(rowSums(term_mat)), las = 2)
    term_net <- graph_from_biadjacency_matrix(term_mat)
    node.type <- rep(NA, vcount(term_net))
    
    if(color.all.categories){
        node.class <- sapply(V(term_net)$name, function(x) term.cat[which(u_term == x)])
        node.type <- sapply(node.class, function(x) if(length(x) > 0){x}else{"Gene"})
    }else{
        node.type[which(V(term_net)$name %in% u_term)] <- "Term"
        node.type[which(V(term_net)$name %in% u_gene)] <- "Gene"
    }
    
    V(term_net)$term_type <- node.type

    if(plot.results){
        factor.node <- as.factor(node.type)
        node.cat <- levels(factor.node)
        term.col <- as.numeric(factor.node)
        #gene node sizes are based on degree
        node.size <- scale.between.vals(degree(term_net), min.vertex.size, max.vertex.size)
        
        factor.node.idx <- which(factor.node != "Gene")
        #term node sizes are based on -log10 p value
        node.size[factor.node.idx] <- scale.between.vals(-log10(term.p[factor.node.idx]), min.vertex.size, max.vertex.size)

        layout.mat <- matrix(c(1,2), nrow = 1)
        layout(layout.mat, widths = c(1,0.2))
        par(mar = c(0,0,0,0))
        plot(term_net, vertex.color = term.col, vertex.size = node.size)
        
        plot.new()
        plot.window(xlim = c(0, 1), ylim = c(0,1)) 
        
        #legend for sources
        legend(x = 0, y = 1, fill = categorical_pal(8)[1:max(term.col)], legend = node.cat)
        #legend for gene node sizes
        to.show <- round(segment_region(min.vertex.size, max.vertex.size, 5, "ends"))
        scaled <- scale.between.vals(to.show, 1, 2) #max cex = 2, otherwise, the dots are too big
        legend(x = 0, y = 0.7, pch = 16, pt.cex = scaled, col = categorical_pal(8)[1], 
            legend = to.show, title = "Gene Degree")

        #legend for term p value
        to.show <- segment_region(min(-log10(term.p)), max(-log10(term.p)), min(c(length(term.p), 5)))
        scaled <- scale.between.vals(to.show, 1, 2) #max cex = 2, otherwise, the dots are too big
        legend.col <- categorical_pal(8)[2]
        if(length(u_cat) > 1){
            legend.col = "grey"
        }
        legend(x = 0, y = 0.4, pch = 16, pt.cex = scaled, 
            col = legend.col, legend = signif(10^-to.show, 2), 
            title = "Term p value")
    }

    return(term_net)
}