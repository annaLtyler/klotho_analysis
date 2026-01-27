#plot pairwise t-test p values using results from pairwise_test()
#the y nudges are fractions of the total range of y.
#x nudges are absolute, assuming that we will have few
#comparisons, like three.
#y.nudge = 0.005; x.nudge = 0.05; end.width = 0.01; text.nudge = 0.02; min.beam.dist = 0.02

add_pairwise_results <- function(data.mat, test.results, y.nudge = 0.005, 
  x.nudge = 0.05, end.width = 0.01, text.nudge = 0.02, min.beam.dist = 0.08){
  
  cols.tested <- test.results[[1]]
  p.vals <- lapply(test.results[[2]], 
    function(x) if(class(x)=="try-error"){NA}else{threshold_p(x$p.value, return.text = TRUE)})

  data.range <- max(data.mat, na.rm = TRUE) - min(data.mat, na.rm = TRUE) #full range of the data in the y direction
  old.y.pos <- y.pos <- max(data.mat[,cols.tested[1,]]) #the position of the first beam
  
  for(i in 1:nrow(cols.tested)){
    if(length(p.vals[[i]]) == 1){ #assuming text return from threshold_p()
      next()
    }
    y.pos <- max(data.mat[,cols.tested[i,]], na.rm = TRUE) #the maximum value in the groups being compared
    
    #if this is not the first bar, check it's position with the last placed bar
    #If it is too close as defined by min.beam.dist, move it higher.
    if(i > 1){
      if((y.pos - old.y.pos) <= (data.range*min.beam.dist)){
        y.pos <- y.pos + (data.range*min.beam.dist)
      }
    }
    
    text.x <- mean(cols.tested[i,])

    #nudge should increase for each test so they aren't overplotted
    #increase by 
    beam.y <- y.pos+(data.range*y.nudge) #the position of the beam
    text.y <- beam.y + (data.range*text.nudge) #the text is a little higher than the beam
    
    beam.start <- cols.tested[i,1]+x.nudge #begin the beam a little in from the first group
    beam.end <- cols.tested[i,2]-x.nudge #end the beam a little in from the second group
    vertical.top <- beam.y + (data.range*end.width)
    vertical.bottom <- beam.y - (data.range*end.width)

    text(text.x, text.y, labels = p.vals[[i]], adj = 0.5) #add the text for the p value
    
    #horizontal bar
    segments(beam.start, beam.y, beam.end)
    
    #vertical bars for the beam to show ends
    segments(beam.start, vertical.top, beam.start, vertical.bottom)
    segments(beam.end, vertical.top, beam.end, vertical.bottom)

    old.y.pos <- y.pos #the current y position is now the old y position
  }
}
