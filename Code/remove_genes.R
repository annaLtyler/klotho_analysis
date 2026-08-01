#remove specified genes from the sample_data_object
remove_genes <- function(sample_data, genes.to.remove = NULL){

    if(is.null(genes.to.remove)){
        return(sample_data)
    }

    mouse.info <- sample_data$mouse_info
    sample.info <- sample_data$sample_info
    gene.data <- sample_data$data

    #here we don't need to differentiate between mouse-level and sample-level data.
    keep.genes <- setdiff(rownames(gene.data), genes.to.remove)
    keep.idx <- which(rownames(gene.data)%in% keep.genes)
    new.data <- gene.data[keep.idx,]

  if(!is.null(sample_data$log_data)){
        new.log <- sample_data$log_data[keep.idx,]        
    }else{
        new.log <- NULL
    }

    if(!is.null(sample_data$mean_abundance)){
        new.mean <- sample_data$mean_abundance[keep.idx,]        
    }else{
        new.mean <- NULL
    }

    new.sample.data <- list("mouse_info" = mouse.info, "sample_info" = sample.info, 
        "pr_info" = sample_data$pr_info, "data" = new.data, "log_data" = new.log, 
        "mean_abundance" = new.mean, "gene.id.col" = sample_data$gene.id.col,
        "gene.name.col" = sample_data$gene.name.col)

    if(!is.null(sample_data$scaled_abundance)){
        #recalculate scaled data from new mean matrix
        new.sample.data <- scale_abundance(new.sample.data)
    }

    return(new.sample.data)
}