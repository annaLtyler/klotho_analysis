#adds zero line to grouped box plot, which sets xpd to NA

add_zero_line <- function(h = 0, lty = 1, col = "black"){
  plot.dim <- par("usr")
  segments(plot.dim[1], h, plot.dim[2], h, lty = lty, col = col)
}
