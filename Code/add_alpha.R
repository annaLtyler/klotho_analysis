#This function takes in a color and adds an alpha (transparency).
#It returns the hex value of the color with the alpha added.

add_alpha <- function(color, alpha = 0.5){
    rgb.vals <- col2rgb(color)
    new.col <- rgb(rgb.vals[1,1]/256, rgb.vals[2,1]/256, rgb.vals[3,1]/256, alpha = alpha)
    return(new.col)
}