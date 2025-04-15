#This function cleans words for word clouds
#It removes common words and punctuation.
#if remove.truncated words is true, words ending
#with ... will be removed. These occur in results
#from plot.enrichment.group

clean_words <- function(words, additional.words.to.remove = NULL,
    remove.truncated.words = TRUE){

    #make all lowercase
    words <- str_to_lower(words)

    if(remove.truncated.words){
        trunc.words <- grep("...", words, fixed = TRUE)
        to.keep <- setdiff(1:length(words), trunc.words)
        words <- words[to.keep]
    }

    #remove punctuation
    punct <- c(";", ",", ".")
    for(i in 1:length(punct)){
        words <- gsub(punct[i], "", words, fixed = TRUE)
    }

    #remove common words
    remove.words <- c("by", "in", "and", "to", "or", "on", "a", "as", "with", 
        "into", "via", "other", "the", "between", "process", "of")

    remove.words <- c(remove.words, additional.words.to.remove)
    to.remove <- which(words %in% remove.words)
    to.keep <- setdiff(1:length(words), to.remove)

    words <- words[to.keep]

    return(words)
}
