#get information about specific mice.
get_mouse_info <- function(sample_data, mouse.id = NULL){
   if(is.null(mouse.id)){
        return(NULL)
    }
    mouse.info <- sample_data$mouse_info
    mouse.idx <- which(mouse.info[,"animalID"] %in% mouse.id)
    sub.info <- mouse.info[mouse.idx,]
    return(sub.info)
}
