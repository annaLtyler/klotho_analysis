#This function attempts to summarize enrichment results
#from gprofiler. It chops up the term names into words. 
#It then generates a matrix of -log10(p) values
#identifying the position that each word term appears
#in the results list, and the p value of the term it
#appears in. It then converts this matrix into a 
#bipartite graph and projects onto the terms. 
#This function does not take into account the ontology structure,
#but it has the benefit of being able to summarize terms
#from a mixture of ontologies.

plot.enrichment.wordcloud <- function(enrichment, num.terms = 10, plot.label = "", 
    max.term.size = NULL, max.p.val = 0.05, order.by = c("p_value", "default"), 
    decreasing = FALSE, plot.results = TRUE, vertex.label.cex = 1, vertex.label.dist = 1, 
    max.vertex.cex = 30, max.edge.lwd = 5, colors = "black", net_layout = layout_nicely,
    just.wordcloud = FALSE, min.cex = 0.5, max.cex = 5, title.cex = 1, title.font = 2){
    
    if(is.null(enrichment)){
        plot.text("No Enrichment")
        mtext(plot.label, side = 3, outer = FALSE, line = 0)
        return(NULL)
    }

    #require(igraph)
    #require(wordcloud)


    order.by = order.by[1]
    result <- enrichment$result
    if(is.null(max.term.size)){
        max.term.size <- max(result[,"term_size"])
        }
    use.locale <- intersect(which(result[,"term_size"] <= max.term.size), 
        which(result[,"p_value"] <= max.p.val))

    if(length(use.locale) == 0){
        plot.text("No Enrichment")
        return(NULL)
    }

    sub.result <- result[use.locale,,drop=FALSE]

    num.terms <- min(c(num.terms, nrow(sub.result)))
    if(order.by == "default"){
        result.order <- order(1:nrow(sub.result))[1:num.terms]
    }else{
        result.order <- order(sub.result[,order.by], decreasing = decreasing)[1:num.terms]
    }

    sub.result <- sub.result[result.order,,drop=FALSE]
    term.names <- sub.result[,"term_name"]

    term.groups <- strsplit(term.names, " ")
    term.words <- unlist(term.groups)
    clean.words <- clean_words(term.words)

    word.count <- sort(table(clean.words), decreasing = TRUE)
    word.pos <- matrix(0, nrow = nrow(sub.result), ncol = length(word.count))
    colnames(word.pos) <- names(word.count)
    rownames(word.pos) <- 1:nrow(word.pos)
    
    for(i in 1:length(word.count)){
        word.idx <- grep(names(word.count)[i], term.groups, ignore.case = TRUE)
        word.pos[word.idx,i] <- -log10(sub.result[word.idx,"p_value"])
    }


    #barplot(sort(colSums(word.pos), decreasing = TRUE), las = 2)
    #we have high-frequency words and highly-ranked words (strong p value)
    #quartz()
    #pheatmap(word.pos, cluster_rows = FALSE, cluster_cols = FALSE)    
    #pheatmap(t(word.pos[1:10,1:10]), cluster_rows = FALSE, cluster_cols = FALSE)    

    pair.mat <- which(word.pos > 0, arr.ind = TRUE)
    colnames(pair.mat) <- c("position", "word")
    pair.mat[,2] <- colnames(word.pos)[pair.mat[,2]]
    pvals <- word.pos[which(word.pos > 0)]

    inc.mat <- incidence.matrix(pair.mat)
    inc.net <- graph_from_incidence_matrix(inc.mat)
    E(inc.net)$weight <- pvals
    proj <- bipartite_projection(inc.net)

    if(plot.results){
        
        total.word.p <- colSums(word.pos)  
        vertex.scale.factor <- max.vertex.cex/max(total.word.p)
        scaled.p <- total.word.p*vertex.scale.factor
        edge.strength <- E(proj[[2]])$weight
        edge.scale.factor <- max.edge.lwd/max(edge.strength)
        edge.lwd <- edge.strength*edge.scale.factor
        #sum.order <- order(colSums(word.pos), decreasing = FALSE)
        #par(mar = c(4,10,4,4))
        #barplot(word.pos[,sum.order], las = 2, horiz = TRUE)

        #quartz()
        if(length(unique(pvals)) > 1){
            edge.col <- colors.from.values(pvals, use.pheatmap.colors = TRUE)
        }else{
            edge.col <- "lightblue"
            }

        if(!just.wordcloud){
            par(mar = c(0,0,0,0))
            plot(proj[[2]], layout = net_layout, vertex.size = scaled.p, 
            vertex.color = "lightblue", edge.color = edge.col, 
            vertex.label.cex = vertex.label.cex, edge.width = edge.lwd,
            vertex.label.dist = vertex.label.dist)
        }

        par(mar = c(2,2,2,2))
        wordcloud(names(word.count), scaled.p/max(scaled.p), colors = colors,
            scale = c(max.cex, min.cex))

        if(!just.wordcloud){
            mtext(plot.label, side = 3, outer = TRUE, line = -1.5, 
                cex = title.cex, font = title.font)
        }else{
            mtext(plot.label, side = 3, outer = FALSE, line = 0, 
                cex = title.cex, font = title.font)
        }
    }

    final.result <- list("word_position_matrix" = word.pos,
        "word_network" = proj[[2]])
    invisible(final.result)
}