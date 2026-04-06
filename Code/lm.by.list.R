#This function assumes that each level of a list has
#its own factor. It unlists the list, and creates a factor 
#based on the number of elements in the list.

lm.by.list <- function(listX){

    all.num <- unlist(listX)
    fact <- as.factor(unlist(lapply(1:length(listX), function(x) rep(x, length(listX[[x]])))))
    model <- lm(all.num~fact)
    #boxplot(all.num~fact)
    return(model)
}
