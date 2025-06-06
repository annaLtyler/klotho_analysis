sort.by.then.by <-
function(tableX, sort.cols = c(1,2), col.type = c("c", "n"), decreasing = c(FALSE, FALSE), return.order = FALSE){
	

	
	if(length(sort.cols) > 2){
		stop("This script can't handle more than 2 columns")
		}
	
	if(length(sort.cols) != length(col.type)){
		stop("The col.type vector must be the same length as sort.cols")
		}
	
	if(length(sort.cols) == 1){
		
		if(col.type == "n"){
			sorted.col <- sort.int(as.numeric(tableX[,sort.cols]), index.return = TRUE, decreasing = decreasing[1], na.last = TRUE, method = "radix")
			}else{
				sorted.col <- sort.int(tableX[,sort.cols], index.return = TRUE, decreasing = decreasing[1], na.last = TRUE, method = "radix")
				}
		
		new.table <- tableX[sorted.col$ix,]
		return(new.table)

		}else{
			
			#start with the table ordered by the first sort column. We will change
			#chunks of this as we go
			if(col.type[1] == "n"){
				new.order <- sort.int(as.numeric(tableX[,sort.cols[1]]), index.return = TRUE, na.last = TRUE, decreasing = decreasing[1], method = "radix")$ix	
				}else{
					new.order <- sort.int(tableX[,sort.cols[1]], index.return = TRUE, decreasing = decreasing[1], na.last = TRUE, method = "radix")$ix
					}
			sorted.table <- tableX[new.order, ]
			
			if(return.order){
				final.order <- new.order
				}
				
			if(col.type[1] == "n"){
				u_col_el <- sort(as.numeric(unique(sorted.table[,sort.cols[1]])), na.last = TRUE, method = "radix") #find the unique column elements for column 1
			}else{
				u_col_el <- sort(unique(sorted.table[,sort.cols[1]]), na.last = TRUE, method = "radix") #find the unique column elements for column 1
				}
				
			
			new.order <- NULL
				
			for(i in 1:length(u_col_el)){ #go through each of these elements, and sort the second column within the category of the first column
				if(is.na(u_col_el[i])){
				el.locale <- which(is.na(sorted.table[,sort.cols[1]])) #find the entries for element i in column 1
				}else{
					if(col.type[1] == "n"){
						el.locale <- which(as.numeric(sorted.table[,sort.cols[1]]) == u_col_el[i])
					}else{
						el.locale <- which(as.numeric(sorted.table[,sort.cols[1]]) == u_col_el[i])
					}
				}
				if(col.type[2] == "n"){
					subset.order <- sort.int(as.numeric(sorted.table[el.locale, sort.cols[2]]), index.return = TRUE, decreasing = decreasing[2], na.last = TRUE, method = "radix")$ix
					}else{
					subset.order <- sort.int(sorted.table[el.locale, sort.cols[2]], index.return = TRUE, decreasing = decreasing[2], na.last = TRUE, method = "radix")$ix
					}
				new.order <- c(new.order, el.locale[subset.order])
				}
					
			sorted.table <- sorted.table[new.order,]
			if(return.order){
				final.order <- cbind(final.order, new.order)
				return(final.order)
				}	
			
			return(sorted.table)
		} #end case for if sort.cols has more than one element
		
}
