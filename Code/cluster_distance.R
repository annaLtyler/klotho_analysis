#Given x and y coordinates and cluster membership
#This function calculates euclidean distances between
#all cluster pairs
cluster_distance <- function(x, y, membership, plot.results = FALSE, 
    xlab = "x", ylab = "y"){

  # Calculate centroids
  coord.mat <- cbind(x, y)
  u_cl <- sort(unique(membership))
  cl_coord <- lapply(u_cl, function(x) coord.mat[which(membership == x),,drop=FALSE])
  centroids <- lapply(cl_coord, colMeans)
  names(centroids) <- u_cl


  cent.pairs <- pair.matrix(u_cl)
  cl.dist <- rep(NA, nrow(cent.pairs))
  for(i in 1:nrow(cent.pairs)){
    # Calculate Euclidean distance between centroids
    cl1 <- cent.pairs[i,1]
    cl2 <- cent.pairs[i,2]
    cl1.idx <- which(names(centroids) == cl1)
    cl2.idx <- which(names(centroids) == cl2)
    cent.dist <- euclidean(centroids[[cl1.idx]], centroids[[cl2.idx]])
    cl.dist[i] <- cent.dist
  }

    #for each point, calculate the distance to it's cluster middle
    pt.dist <- vector(mode = "list", length = length(centroids))
    names(pt.dist) <- u_cl
    for(cl in 1:length(cl_coord)){
        pt.dist[[cl]] <- sapply(1:nrow(cl_coord[[cl]]), function(x) euclidean(cl_coord[[cl]][x,], centroids[[cl]]))
    }

    if(plot.results){
        plot(x, y, col = as.numeric(as.factor(membership)), pch = 16,
            xlab = xlab, ylab = ylab)
        for(cl in 1:length(centroids)){
            points(centroids[[cl]][1], centroids[[cl]][2], pch = "*", cex = 3, col = cl)
        }

        ymin <- min(c(unlist(pt.dist), cl.dist))
        ymax <- max(c(unlist(pt.dist), cl.dist))
        for(i in 1:nrow(cent.pairs)){
            cl1 <- cent.pairs[i,1]
            cl2 <- cent.pairs[i,2]
            cl1.idx <- which(names(centroids) == cl1)
            cl2.idx <- which(names(centroids) == cl2)
            boxplot(pt.dist[c(cl1.idx, cl2.idx)], ylim = c(ymin, ymax),
                main = paste(cl1, "vs.", cl2))
            abline(h = cl.dist[i])
        }
    }

  distance.table <- cbind(cent.pairs, cl.dist)
  colnames(distance.table) <- c("Cl1", "Cl2", "Distance")
  result <- list("Cluster_Pair_Distances" = distance.table, "Cluster_Point_Distances" = pt.dist)
  
  return(result)

}