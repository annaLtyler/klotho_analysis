
autoplace_text <- function(x, y, x.samples = 10, y.samples = 25, text.window.x = 4,
    text.window.y = 4){
    
    plot.test = FALSE
    #plot.test = TRUE

    #plot(x,y)
    xbins <- segment_region(min(x, na.rm = TRUE), max(x, na.rm = TRUE), x.samples, "ends")
    ybins <- segment_region(min(y, na.rm = TRUE), max(y, na.rm = TRUE), y.samples, "ends")

    empty.adj <- matrix(1, nrow = y.samples, ncol = x.samples)
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
                empty.adj[(y.samples-j),i] <- 0
            }
        }
    }
    #plot(x,y)
    #pheatmap(empty.adj, cluster_rows = FALSE, cluster_cols = FALSE)

    #scan the matrix to find empty blocks of the requested size
    idx <- 1
    possible.coord <- NULL
    for(i in 1:(ncol(empty.adj)-text.window.x)){
        test.x <- i:(i+text.window.x)
        for(j in 1:(nrow(empty.adj)-text.window.y)){
            test.mat <- empty.adj
            test.y <- j:(j+text.window.y)
            test.block <- empty.adj[test.y, test.x]
            if(plot.test){
                test.mat[test.y, test.x] <- 3
                pdf(paste0("~/Desktop/scan_test/scan", idx, ".pdf"))
                pheatmap(test.mat, cluster_rows = FALSE, cluster_cols = FALSE)
                dev.off()
                idx <- idx + 1
            }
            if(all(test.block == 1)){
                possible.coord <- rbind(possible.coord, c(xbins[i],rev(ybins)[j]))
            }
        }
    }

    
    if(plot.test){
        pdf(paste("~/Desktop/scan_test/", "test_check.pdf"))
        for(i in 1:nrow(possible.coord)){
            label.x <- possible.coord[i,1]; label.y <- possible.coord[i,2]
            plot(x,y)
            text(label.x, label.y, labels = "test")
        }
        dev.off()
    }
    
    if(length(possible.coord) == 2){
        possible.coord  <- matrix(possible.coord, nrow = 1)
    }
    if(length(possible.coord) > 0){
        colnames(possible.coord) <- c("x","y")
    }
    return(possible.coord)
}
