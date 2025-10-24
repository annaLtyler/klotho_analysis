#I'm trying in this function to find, not just the 
#first position where text can be added, but the 
#middle of the largest empty block.
#x <- jitter(c(rep(1, 100), rep(2, 100)), factor = 1)
#y <- x+rnorm(100)
#plot(x,y)

autoplace_text2 <- function(x, y, n.samples = 25, min.contig = 5, plot.result = FALSE){
    
    plot.test <- FALSE
    #plot.test <- TRUE
    
    if(plot.test){
        col.mat <- matrix(rep(1:n.samples, n.samples), nrow = n.samples, byrow = TRUE)
        row.mat <- matrix(rep(1:n.samples, each = n.samples), nrow = n.samples, byrow = TRUE)
        #plot(row.mat, col.mat)
    }

    #plot(x,y)
    xbins <- segment_region(min(x, na.rm = TRUE), max(x, na.rm = TRUE), n.samples, "ends")
    ybins <- segment_region(min(y, na.rm = TRUE), max(y, na.rm = TRUE), n.samples, "ends")

    empty.adj <- matrix(1, nrow = n.samples, ncol = n.samples)
    for(i in 1:(length(xbins)-1)){
        above.xmin <- which(x >= xbins[i])
        below.xmax <- which(x <= xbins[(i+1)])
        inx <- intersect(above.xmin, below.xmax)

        for(j in 1:(length(ybins)-1)){
            above.ymin <- which(y >= ybins[j])
            below.ymax <- which(y <= ybins[(j+1)])
            iny <- intersect(above.ymin, below.ymax)

            inboth <- intersect(inx, iny)
            if(length(inboth) > 0){
                empty.adj[(n.samples-j),i] <- 0
            }
        }
    }
    
    #plot(x,y)
    #pheatmap(empty.adj, cluster_rows = FALSE, cluster_cols = FALSE)
    #find contiguous clumps by row and column
    coord.by.row <- lapply(1:nrow(empty.adj), function(x) which(empty.adj[x,] == 1))
    contig.by.row <- lapply(1:length(coord.by.row), function(x) contiguous_blocks(cbind(rep(x, length(coord.by.row[[x]])), coord.by.row[[x]])))
    
    coord.by.col <- lapply(1:ncol(empty.adj), function(x) which(empty.adj[,x] == 1))
    contig.by.col <- lapply(1:length(coord.by.col), function(x) contiguous_blocks(cbind(coord.by.col[[x]], rep(x, length(coord.by.col[[x]])))))

    row.blocks <- list()
    idx <- 1
    is_new_block = TRUE
    for(i in 1:length(contig.by.row)){
        contig.size <- sapply(contig.by.row[[i]], nrow)
        above.min <- which(contig.size > min.contig)
        max.idx <- which.max(contig.size)
        use.idx <- intersect(above.min, max.idx)
        if(length(use.idx) > 0){
            if(is_new_block){
                row.blocks[[idx]] <- contig.by.row[[i]][[use.idx]]
                is_new_block <- FALSE
            }else{
                #compare column fill to the previous row.
                prev.col <- row.blocks[[idx]][,2]
                if(length(contig.by.row[[i]][[use.idx]]) < 2){next()}
                new.col <- contig.by.row[[i]][[use.idx]][,2]
                common.col <- intersect(prev.col, new.col)
                if(length(common.col) > 0){
                    comb.block <- unique(rbind(row.blocks[[idx]], contig.by.row[[i]][[use.idx]]))
                    keep.idx <- which(comb.block[,2] %in% common.col)
                    merged.block <- comb.block[keep.idx,]
                    row.blocks[[idx]] <- merged.block
                }else{
                    idx <- idx + 1
                    is_new_block <- TRUE
                }
            }
        }else{
            idx <- idx + 1 #if no contiguous blocks above min size
            is_new_block = TRUE
        }
        if(length(row.blocks) == idx && plot.test){
            pdf(paste0("~/Desktop/test_blocks/test_row", i, ".pdf"))
            plot(row.mat, col.mat)
            #plot.new()
            #plot.window(xlim = c(1,25), ylim = c(1,25))
            text(row.blocks[[idx]][,2], row.blocks[[idx]][,1], 
                labels = row.blocks[[idx]][,1], cex = 0.4, col = "red")
            dev.off()
        }
    }    

    col.blocks <- list()
    idx <- 1
    is_new_block <- TRUE
    for(i in 1:length(contig.by.col)){
        contig.size <- sapply(contig.by.col[[i]], nrow)
        above.min <- which(contig.size > min.contig)
        max.idx <- which.max(contig.size)
        use.idx <- intersect(above.min, max.idx)
        if(length(use.idx) > 0){
            if(is_new_block){
                col.blocks[[idx]] <- contig.by.col[[i]][[use.idx]]
                is_new_block <- FALSE
            }else{
                #compare column fill to the previous row.
                prev.row <- col.blocks[[idx]][,1]
                if(length(contig.by.col[[i]][[use.idx]]) < 2){next()}
                new.row <- contig.by.col[[i]][[use.idx]][,1]
                common.row <- intersect(prev.row, new.row)
                if(length(common.row) > 0){
                    comb.block <- unique(rbind(col.blocks[[idx]], contig.by.col[[i]][[use.idx]]))
                    keep.idx <- which(comb.block[,1] %in% common.row)
                    merged.block <- comb.block[keep.idx,]
                    col.blocks[[idx]] <- merged.block
                }else{
                    idx <- idx + 1
                    is_new_block <- TRUE
                }
            }
        }else{
            idx <- idx + 1 #if no contiguous blocks above min size
            is_new_block = TRUE
        }
        if(length(col.blocks) == idx && plot.test){
            pdf(paste0("~/Desktop/test_blocks/test_col", i, ".pdf"))
            #plot.new()
            #plot.window(xlim = c(1,25), ylim = c(1,25))
            plot(row.mat, col.mat)
            text(col.blocks[[idx]][,2], col.blocks[[idx]][,1], labels = col.blocks[[idx]][,1], cex = 0.4, col = "red")
            dev.off()
        }
    }

    row.block.size <- sapply(row.blocks, function(x) if(is.null(x)){0}else{nrow(x)})
    max.row.block <- max(row.block.size)

    col.block.size <- sapply(col.blocks, function(x) if(is.null(x)){0}else{nrow(x)})
    max.col.block <- max(col.block.size)

    if(max.row.block > max.col.block){
        possible.coords <- row.blocks[[which.max(row.block.size)]]
    }else{
        possible.coords <- col.blocks[[which.max(col.block.size)]]
    }

    #par(mfrow = c(1,2))
    #imageWithText(empty.adj)
    #test.adj <- empty.adj
    #test.adj[possible.coords[,1], possible.coords[,2]] <- 3
    #imageWithText(test.adj)

    #translate to the coordinates used in the input
    #the top left of the matrix is the origin. higher
    #numbers to to the right and down.
    x.range <- segment_region(min(x, na.rm = TRUE), max(x, na.rm = TRUE), 
        n.samples, "ends")
    y.range <- segment_region(min(y, na.rm = TRUE), 
        max(y, na.rm = TRUE), n.samples, "ends")
    
    #the rows are the y coordinates, and the columns are the x coordinates
    possible.x <- x.range[possible.coords[,2]]
    possible.y <- rev(y.range)[possible.coords[,1]]
    
    if(plot.result){
        plot(x,y)
        points(possible.x, possible.y, col = "red")
    }
    return(cbind(possible.x, possible.y))

}
