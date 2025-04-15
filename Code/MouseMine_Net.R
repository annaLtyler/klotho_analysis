#mouse.table <- read.delim("~/Downloads/MFeature_Synapse_Immune.tsv",  header = FALSE)
#pdf("~/Desktop/net.pdf", width = 30, height = 30)
#mousemine_network(mouse.table, gene.col = 2, min.cex = 2, min.v.size = 3, max.v.size = 10)
#dev.off()
mousemine_network <- function(mouse.table, gene.col = 2, go.col = 8,
    gene.color = "#7fc97f", go.color = "#beaed4", min.v.size = 3, max.v.size = 10,
    min.cex = 1, max.cex = 4){

    library(igraph)
    u_gene <- unique(mouse.table[,gene.col])
    u_go <- unique(mouse.table[,go.col])
    inc.mat <- matrix(0, nrow = length(u_gene), ncol = length(u_go))
    rownames(inc.mat) <- u_gene
    colnames(inc.mat) <- u_go
    
    for(i in 1:nrow(mouse.table)){
        gene.name <- mouse.table[i,gene.col]
        go.name <- mouse.table[i,go.col]
        inc.mat[gene.name, go.name] <- 1
    }
    #pheatmap(t(inc.mat))
    
    net <- graph_from_biadjacency_matrix(inc.mat, directed = FALSE)
    vcol <- rep(gene.color, vcount(net))
    vcol[which(V(net)$name %in% mouse.table[,go.col])] <- go.color
    deg <- degree(net)
    vsize <- scale.between.vals(deg, min.v.size, max.v.size)
    label.size <- scale.between.vals(deg, min.cex, max.cex)

    plot(net, vertex.color = vcol, vertex.size = vsize, vertex.label.cex = label.size)

    invisible(net)

}
