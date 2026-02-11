#adds zero line to grouped box plot, which sets xpd to NA

add_zero_line <- function(){
  plot.dim <- par("usr")
  segments(plot.dim[1], 0, plot.dim[2], 0)
}
