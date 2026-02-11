#plot pairwise t-test p values using results from pairwise_test()
#the y nudges are fractions of the total range of y.
#x nudges are absolute, assuming that we will have few
#comparisons, like three.
#if force.higher is TRUE, subsequent beams will not be
#plotted below beams already plotted. This is useful if the
#middle group has the highest values. It makes sure the 
#spanning comparison doesn't cut through the middle. 

#y.nudge = 0.005; x.nudge = 0.05; end.width = 0.01; text.nudge = 0.02; min.beam.dist = 0.02

add_pairwise_results <- function(data.mat.or.list, test.results, y.nudge = 0.005, 
  x.nudge = 0.05, end.width = 0.01, text.nudge = 0.02, min.beam.dist = 0.08,
  force.higher = FALSE){
  
    cols.tested <- test.results[[1]]
    p.vals <- lapply(test.results[[2]], 
      function(x) if(class(x)=="try-error"){NA}else{threshold_p(x$p.value, return.text = TRUE)})
  
    data.range <- max(unlist(data.mat.or.list), na.rm = TRUE) - min(unlist(data.mat.or.list), na.rm = TRUE) #full range of the data in the y direction
    
    #initialize old.y.pos and y.pos
    if(class(data.mat.or.list)[1] == "matrix"){
      old.y.pos <- y.pos <- max(data.mat.or.list[,cols.tested[1,]], na.rm = TRUE) #the position of the first beam
    }else{
      old.y.pos <- y.pos <- max(unlist(data.mat.or.list[cols.tested[1,]]), na.rm = TRUE) #the position of the first beam
    }
    
    #initialize x position matrix to check x overlaps
    old.x.pos <- NULL

    check_increase <- function(too.close, x.overlap, force.higher){
      #initialize as FALSE
      need.to.increase = FALSE
      #if the bar is too close to previous bars
      if(any(too.close) && any(x.overlap)){need.to.increase = TRUE}
      #if we are requesting each bar to be higher than the next
      if(force.higher){need.to.increase = TRUE}
      return(need.to.increase)
    }

    #go through comparisons and plot beams and p values
    for(i in 1:nrow(cols.tested)){
      
      if(length(p.vals[[i]]) == 1){ #assuming text return from threshold_p()
        next()
      }

      #start with the y position at the maximum position of the data in the comparison
      if(class(data.mat.or.list)[1] == "matrix"){
        y.pos <- max(data.mat.or.list[,cols.tested[i,]], na.rm = TRUE) #the maximum value in the groups being compared
      }else{
        y.pos <- max(unlist(data.mat.or.list[cols.tested[i,]]), na.rm = TRUE) #the maximum value in the groups being compared
      }

      #set beam start and stop points
      beam.start <- cols.tested[i,1]+x.nudge #begin the beam a little in from the first group
      beam.end <- cols.tested[i,2]-x.nudge #end the beam a little in from the second group

      #if the next beam is going to be plotted below the previous one,
      #but we don't want that to happen, 
      #or if the x positions overlap and the beam is too close to the
      #previous one,
      #set the new beam position to the maximum old position plus 
      #the minimum beam distance.
      if(i > 1){
        #check x overlap with previous beams
        x.overlap <- apply(old.x.pos, 1, 
          function(x) segments.overlap(x[1], x[2], beam.start, beam.end))

        #check closeness to previous beams
        too.close <- any(abs(y.pos - old.y.pos) <= (data.range*min.beam.dist))

        need.increase <- check_increase(too.close, x.overlap, force.higher)

        if(need.increase){
          increase.from <- max(old.y.pos, y.pos)
          y.pos <- increase.from + (data.range*min.beam.dist)
        }
      }

      #position for the p value text
      text.x <- mean(cols.tested[i,])

      #y.nudge puts the beam a little higher than the highest data point
      #in the comparison
      beam.y <- y.pos+(data.range*y.nudge) #the position of the beam
      text.y <- beam.y + (data.range*text.nudge) #the text is a little higher than the beam
            
      #set the beam caps max and min positions
      vertical.top <- beam.y + (data.range*end.width)
      vertical.bottom <- beam.y - (data.range*end.width)

      text(text.x, text.y, labels = p.vals[[i]], adj = 0.5) #add the text for the p value
      
      #add horizontal bar
      segments(beam.start, beam.y, beam.end)
      
      #add vertical bars for the beam to show ends
      segments(beam.start, vertical.top, beam.start, vertical.bottom)
      segments(beam.end, vertical.top, beam.end, vertical.bottom)

      #keep track of all beam positions so they don't overlap each other
      old.y.pos <- c(old.y.pos, y.pos)
      old.x.pos <- rbind(old.x.pos, c(beam.start, beam.end))
    }
}

