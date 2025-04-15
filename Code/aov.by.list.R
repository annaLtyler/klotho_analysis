#This function assumes that each level of a list has
#its own factor. It unlists the list, and creates a factor 
#based on the number of elements in the list.

aov.by.list <- function(listX, return.aov = FALSE){

    all.num <- unlist(listX)
    fact <- as.factor(unlist(lapply(1:length(listX), function(x) rep(x, length(listX[[x]])))))
    #boxplot(all.num~fact)
    if(return.aov){
        test <- aov(all.num~fact)
    }else{
        test <- anova(aov(all.num~fact))
    }
    return(test)
}
