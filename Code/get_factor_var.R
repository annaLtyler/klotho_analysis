#get data frame of age, sex and genotype as ordered factors

get_factor_var <- function(sample_data, data.type = c("raw", "log", "mean", "scaled")){
    
    if(length(data.type) > 1){ #default to log
        data.type = "log"
        }

    if(data.type == "mean" || data.type == "scaled"){
        info.table <- sample_data$mouse_info
        row.names <- info.table[,1]
    }else{
        info.table <- sample_data$sample_info
        row.names <- rownames(info.table)
    }
    
    age <- ordered(info.table[,"age_months"], levels = c("4", "12"))
    sex <- ordered(info.table[,"sex"], levels = c("F", "M"))
    genotype <- ordered(info.table[,"genotype"], levels = c("FC", "WT", "VS"))
    factor.df <- data.frame(age, sex, genotype)
    rownames(factor.df) <- row.names
    return(factor.df)

}