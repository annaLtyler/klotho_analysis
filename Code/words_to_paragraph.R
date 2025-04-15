#This function converts a vector of words
#to a string that is separated by the given
#character string, and grouped into lines
#with carriage returns at the end. This is
#for plotting large sets of words in plot.text())

words_to_paragraph <- function(words, sep = ", ", line.len = 5){
    if(length(words) == 1){return(words)}
    word.list <- sliding.window.el(words, window.size = line.len, gap.size = line.len)
    pasted.words <- sapply(word.list, function(x) paste(x, collapse = sep))
    paragraph <- paste(pasted.words, collapse = "\n")
    return(paragraph)
}
