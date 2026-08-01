#removes specified mice from the sample_data object

remove_mice <- function(sample_data, mice.to.remove = NULL){

    if(is.null(mice.to.remove)){
        return(sample_data)
    }

    mouse.info <- sample_data$mouse_info
    sample.info <- sample_data$sample_info
    gene.data <- sample_data$data

    #dim(mouse.info); dim(sample.info); dim(gene.data)
    
    #remove mice from mouse info table
    keep.id <- setdiff(mouse.info[,"animalID"], mice.to.remove)
    
    #indices of mice in mouse-level data
    mouse.keep.idx <- which(mouse.info[,"animalID"] %in% keep.id) 
    new.mouse.info <- mouse.info[mouse.keep.idx,]

    #indices of mice in sample-level data
    sample.keep.idx <- which(sample.info[,"animalID"] %in% keep.id)

   if(!is.null(sample_data$log_data)){
        #log data are indexed by sample
        new.log <- sample_data$log_data[,sample.keep.idx]        
    }else{
        new.log <- NULL
    }

    if(!is.null(sample_data$mean_abundance)){
        #mean data are indexed by mice
        new.mean <- sample_data$mean_abundance[,mouse.keep.idx]        
    }else{
        new.mean <- NULL
    }

    #remove all samples associated with these mice from the sample information 
    #table and the raw data table
    new.sample.info <- sample.info[sample.keep.idx,]
    new.data <- gene.data[,sample.keep.idx]

    new.sample.data <- list("mouse_info" = new.mouse.info, "sample_info" = new.sample.info, 
        pr_info = sample_data$pr_info, "data" = new.data, "log_data" = new.log, 
        "mean_abundance" = new.mean, "gene.id.col" = sample_data$gene.id.col,
        "gene.name.col" = sample_data$gene.name.col)

    if(!is.null(sample_data$scaled_abundance)){
        #recalculate scaled data from new mean matrix
        new.sample.data <- scale_abundance(new.sample.data)
    }

    return(new.sample.data)
}
