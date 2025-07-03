#adds a commented out header to a data table.
#The header is processed to make it easier to 
#type it in the first place. First, all new lines
#are replaced with spaces. Then the header is broken
#into words and turned back into a paragraph with a 
#line length specified by line.len using
#words_to_paragraph. Then a # is placed at the beginning
#of each line before writing out to the file. This should
#make typing the header into the function easier. You don't
#need to worry about formatting it when typing it in.

write_table_with_header <- function(data.table, file.name, header = "",
  sep = "\t", col.names = TRUE, row.names = FALSE, line.len = 10){

  write.table(data.table, file.name, quote = FALSE, sep = sep, 
    col.names = col.names, row.names = row.names)
  
  no.newline <- gsub("\n", " ", header)
  no.space <- str_squish(no.newline)
  split.words <- strsplit(no.space, " ")[[1]]
  plines <- words_to_paragraph(split.words, line.len = line.len, sep = " ")
  #plot.text(plines)
  comment.lines <- gsub("\n", "\n#", plines)
  #plot.text(comment.lines)

  fConn <- file(file.name, 'r+')
  Lines <- readLines(fConn)
  writeLines(c(paste0("#", comment.lines), Lines), fConn)
  close(fConn)
}
