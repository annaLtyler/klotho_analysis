#This function provides a wrapper for the interactions 
#interface


plot.continuous.interaction <- function(x.factor, trace.factor, response, 
    x.factor.label = "x.factor", trace.factor.label = "trace.factor", 
    response.label = "response", int.width = 0.9){

    df <- data.frame(cbind(x.factor, trace.factor, response))
    fml <- as.formula(paste("response ~ x.factor * trace.factor")) 
    model <- lm(fml, data = df)
    #summary(model)
    interactions::interact_plot(model, x.factor, trace.factor, interval = TRUE, int.width = int.width, 
        x.label = x.factor.label, y.label = trace.factor.label, legend.main = response.label)
}