#pull out an entire data matrix 
#optional adjustment for covariates.
get_data <- function(sample_data, data.type = c("raw", "log", "mean", "scaled"),
    adjust.for = NULL){
    
    #if we've requested adjusted scaled data, change data.type to mean
    #and then rescale after.
    rescale = FALSE
    if(!is.null(adjust.for) && data.type == "scaled"){
        rescale <- TRUE
        data.type = "mean"
    }

    #use the log data by default
    if(length(data.type) > 1){data.type = "log"}

    if(data.type == "raw"){
        dat.mat <- sample_data$data
    }
    if(data.type == "log"){
        dat.mat <- sample_data$log_data
    }
    if(data.type == "mean"){
        dat.mat <- sample_data$mean_abundance
    }
    if(is.null(adjust.for) && data.type == "scaled"){
        dat.mat <- sample_data$scaled_abundance
    }

    #only adjust if the data type is mean or log
    if(rescale){
        factor.var <- get_factor_var(sample_data, data.type)
        dummy.var <- dummy_covar(factor.var[,adjust.for,drop=FALSE])
        adj.dat <- t(adjust(t(dat.mat), dummy.var))
        scaled.data <- t(apply(adj.dat, 1, scale))
        dimnames(scaled.data) <- dimnames(adj.dat)
    }else{
        scaled.data <- dat.mat
    }
    
    return(scaled.data)
}
