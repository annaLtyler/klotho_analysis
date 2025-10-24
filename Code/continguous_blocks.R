#given a set of x and y indices as a two-column
#matrix, this function identifies which indices
#are contiguous - i.e. in the same column and one
#row apart, or in the same row and one column apart
#or diagonally adjacent, one rown and one column 
#apart

contiguous_blocks <- function(idx.mat, node.dist = 1){
    
    #each entry in the matrix is a node in a graph
    n.vert <- nrow(idx.mat)

    #for each node, find all other nodes that are one away,
    #either by row or column. Also include the index itself
    #in the group of adjacent elements.
    one.away <- vector(mode = "list", length = nrow(idx.mat))
    names(one.away) <- 1:nrow(idx.mat)
    for(i in 1:nrow(idx.mat)){
        test.node <- idx.mat[i,]
        edist <- apply(idx.mat, 1, function(x) euclidean(test.node, x))
        one.away[[i]] <- which(edist <= node.dist)
    }

    #cluster these one-away elements into groups if they 
    #share an element
    #start with the first element and collect all connected
    #groups. When we run out of connected groups, go to the
    #next unconnected group.

    get.all.connected <- function(one.away, test.idx){
        contains.idx <- which(sapply(one.away, function(x) length(which(x == test.idx))) > 0)
        adj.idx <- sort(unique(unlist(one.away[contains.idx])))
        return(adj.idx)
    }

    adj.groups <- list()
    accounted.for <- unlist(adj.groups)
    unaccounted.for <- setdiff(1:nrow(idx.mat), accounted.for)
    all.accounted.for <- length(unaccounted.for) == 0
    current.idx <- unaccounted.for[1]
    group.counter <- 1
    while(!all.accounted.for){
        current.idx <- unaccounted.for[1]        
        contains.idx <- get.all.connected(one.away, current.idx)
        current.group.size <- length(contains.idx)
        check.within <- sort(unique(unlist(lapply(contains.idx, function(x) get.all.connected(one.away, x)))))
        expanded.group.size <- length(check.within)
        while(current.group.size < expanded.group.size){
            current.group.size <- expanded.group.size
            check.within <- sort(unique(unlist(lapply(check.within, function(x) get.all.connected(one.away, x)))))
            expanded.group.size <- length(check.within)
        }
        adj.groups[[group.counter]] <- check.within
        group.counter = group.counter + 1
        accounted.for <- unlist(adj.groups)
        unaccounted.for <- setdiff(1:nrow(idx.mat), accounted.for)
        all.accounted.for <- length(unaccounted.for) == 0
    }

    is.group <- which(sapply(adj.groups, length) > 1)
    group.idx <- lapply(adj.groups[is.group], function(x) idx.mat[x,])
    return(group.idx)
}
