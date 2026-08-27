#edges will be weighted by mediation probability and 
#colored by 

plot_mediation_network <- function(edge.list, sample_data, rna_data, 
    mediation.prob = NULL, abund.cor = NULL,
    main = "", min.vertex.size = 3, max.vertex.size = 10, min.edge.width = 1, 
    max.edge.width = 10, label.cex = 1, vertex.label.dist = 1.2, edge.arrow.size = 0.5){
    
    source.name <- get_mixed_id(ids = edge.list[,1], sample_data, rna_data)
    target.name <- get_mixed_id(edge.list[,2], sample_data, rna_data)
    source.tx <- grep("ENSMUS", edge.list[,1])
    target.tx <- grep("ENSMUS", edge.list[,2])
    tx.names <- unique(c(source.name[source.tx], target.name[target.tx]))

    edge.names <- cbind(source.name, target.name)
    pr.tx.net <- graph_from_edgelist(edge.names)
    
    vcol <- rep("#bcbddc", vcount(pr.tx.net)) #color the transcripts and proteins differently
    vcol[which(V(pr.tx.net)$name %in% tx.names)] <- "#99d8c9" 
    
    if(is.null(mediation.prob)){
        mediation.prob <- rep(1, ecount(pr.tx.net))
        edge.width = 1
    }else{
        edge.width <- scale.between.vals(mediation.prob, min.edge.width, max.edge.width)
    }

    if(is.null(abund.cor)){
        abund.cor <- rep(1, ecount(pr.tx.net))
        edge.col <- "#a6bddb"
    }else{
        edge.col <- colors.from.values(abund.cor, use.pheatmap.colors = TRUE, 
            global.color.scale = TRUE, global.min = -1, global.max = 1)
    }


    E(pr.tx.net)$mediation_prob <- mediation.prob
    E(pr.tx.net)$correlation <- abund.cor
    
    vert.size <- scale.between.vals(degree(pr.tx.net), min.vertex.size, max.vertex.size)
    plot(pr.tx.net, vertex.color = vcol, layout = layout_with_kk, 
        vertex.size = vert.size,
        vertex.label.dist = vertex.label.dist, edge.arrow.size = edge.arrow.size, 
        edge.width = edge.width,
        edge.color = edge.col, main = main, vertex.label.cex = label.cex)
    invisible(pr.tx.net)    
}