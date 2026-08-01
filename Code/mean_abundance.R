#this function calculates mean log expression it works on 
#min reps determines how many replicates need to have data
#before the mean is taken. If it is two, any protein group
#with data in only one replicate will be returned as NA
mean_abundance <- function(sample_data, min.rep = 2){
    
    mouse.info <- sample_data$mouse_info
    sample.info <- sample_data$sample_info
    gene.data <- sample_data$log_data

    if(is.null(gene.data)){
        stop("log_norm_data must be performed prior to taking means.")
    }

    u_id <- mouse.info[,"animalID"]
    u_idx <- lapply(u_id, function(x) which(sample.info[,"animalID"] == x))
    split.dat <- lapply(u_idx, function(x) gene.data[,x])
    num.reps <- sapply(split.dat, function(x) apply(x, 1, function(x) length(which(!is.na(x)))))
    for(i in 1:length(split.dat)){
        below.min <- which(num.reps[,i] < min.rep)
        if(length(below.min) > 0){
            split.dat[[i]][below.min,] <- NA
        }
    }
    mean_data <- sapply(split.dat, function(x) rowMeans(x, na.rm = TRUE))
    colnames(mean_data) <- u_id

    sample_data$mean_abundance <- mean_data
    return(sample_data)
}