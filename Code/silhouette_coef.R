#This function calculated silhouette coefficients
#for a set of points in 2D space. The clusters 
#must be provided.

silhouette_coef <- function(pt.coords, membership){

    u_cluster <- unique(membership)
    pt.dist <- as.matrix(dist(pt.coords))

    #silhouette coefficients are calculated for all points
    #in a space

    within.clust.coord <- lapply(u_cluster, function(x) which(membership == x))

    #for each individual point i calculate a,
    #the average distance a to all other points in its own cluster.
    get_within_clust_dist <- function(pt.idx){
        pt.group <- membership[pt.idx]
        all.mem <- which(membership == pt.group)
        other.mem <- setdiff(all.mem, pt.idx)
        dist.to.others <- pt.dist[pt.idx,other.mem]
        mean.dist <- mean(dist.to.others)
        return(mean.dist)
    }
    all.pt.within.dist <- sapply(1:length(membership), get_within_clust_dist)
    #barplot(all.pt.within.dist, names = membership)

    #for each individual point i calculate b,
    #the minimum of the average distance to points in other clusters.
    get_out_clust_dist <- function(pt.idx){
        pt.group <- membership[pt.idx]
        all.mem <- which(membership == pt.group)
        out.mem <- setdiff(1:length(membership), all.mem)
        all.out.dist <- pt.dist[pt.idx,out.mem]
        mean.out.group.dist <- sapply(unique(membership[out.mem]), function(x) mean(all.out.dist[which(membership[out.mem] == x)]))
        min.out.group.mean.dist <- min(mean.out.group.dist)
        return(min.out.group.mean.dist)
    }

    
    min.out.clust.dist <- sapply(1:length(membership), get_out_clust_dist)
    #barplot(min.out.clust.dist, names = membership)

    dist.case1 <- which(all.pt.within.dist < min.out.clust.dist)
    dist.case2 <- which(all.pt.within.dist > min.out.clust.dist)

    s <- rep(NA, length(membership))
    if(length(dist.case1) > 0){
        s[dist.case1] <- 1 - all.pt.within.dist[dist.case1]/min.out.clust.dist[dist.case1]
    }
    if(length(dist.case2) > 0){
        s[dist.case2] <- min.out.clust.dist[dist.case2]/all.pt.within.dist[dist.case2] - 1
    }
    names(s) <- membership
    mean.cl.width <- sapply(u_cluster, function(x) mean(s[which(membership == x)]))
    #barplot(mean.cl.width)

    return(mean.cl.width)
}
