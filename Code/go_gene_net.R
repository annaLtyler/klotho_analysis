#enrichment.result is an object returned from gost
#with evcodes set to TRUE
go_gene_net <- function(enrichment.result, max.pval = 0.05){
    
    term.p <- enrichment.result[[1]]$p_value

    sig.vals <- which(term.p <= max.pval)
    #length(sig.vals)

    go.terms <- enrichment.result[[1]]$term_name[sig.vals]
    #cat(go.terms, sep = "\n")
    gene.list <- enrichment.result[[1]]$intersection[sig.vals]
    
    u_term <- unique(go.terms)
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
    node.type[which(V(term_net)$name %in% u_term)] <- "Term"
    node.type[which(V(term_net)$name %in% u_gene)] <- "Gene"
    V(term_net)$term_type <- node.type

    #plot(term_net, vertex.color = as.numeric(as.factor(node.type)), 
    #    vertex.size = scale.between.vals(degree(term_net), 1, 20),
    #    vertex.label = NA)

    #proj <- bipartite_projection(term_net)
    #plot(proj[[1]])
    #term.mat <- as_adjacency_matrix(proj[[1]])
    #plot(proj[[2]])

    #net.decomp <- plot.decomp(term_mat)
    #abline(h = c(-0.02, 0.025))
    #cl1 <- rownames(term_mat)[which(net.decomp$u[,2] < -0.02)]
    #cl2 <- rownames(term_mat)[intersect(which(net.decomp$u[,2] > -0.02), which(net.decomp$u[,2] < 0.025))]
    #cl3 <- rownames(term_mat)[which(net.decomp$u[,2] > 0.025)]

    return(term_net)
}