#pull out an entire data matrix 
#optional adjustment for covariates.
get_data <- function(sample_data, data.type = c("raw", "log", "mean", "scaled"),
    adjust.for = NULL){
    
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
    if(data.type == "scaled"){
        dat.mat <- sample_data$scaled_abundance
    }

    #only adjust if the data type is mean or log
    if(!is.null(adjust.for) && (data.type == "mean" || data.type == "log")){
        factor.var <- get_factor_var(sample_data, data.type)
        dummy.var <- dummy_covar(factor.var[,adjust.for,drop=FALSE])
        adj.dat <- t(adjust(t(dat.mat), dummy.var))
    }else{
        adj.dat <- dat.mat
    }

    return(adj.dat)
}
