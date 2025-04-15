#mix colors together to make an intermediate color
#a mixing parameter below 0.5 pushes the result more torward
#col1, while a mixing parameter above 0.5 pushes the result
#more toward col2
mix_colors <- function(col1, col2, mixing.parameter = 0.5, plot.result = FALSE){

    if(mixing.parameter < 0 || mixing.parameter > 1){stop("the mixing parameter must be between 0 and 1.")}

    color.ramp <- colorRampPalette(c(col1, col2))
    mixed.color <- color.ramp(100)[round(mixing.parameter*100)]

    if(plot.result){
    par(mar = c(0,0,0,0))
    plot.new()
    plot.window(xlim = c(0,3), ylim = c(0,1))
    draw.rectangle(0.1, 0.9, 0.3, 0.6, fill = col1)
    draw.rectangle(1.1, 1.9, 0.3, 0.6, fill = mixed.color)
    draw.rectangle(2.1, 2.9, 0.3, 0.6, fill = col2)
    }

    return(mixed.color)

}