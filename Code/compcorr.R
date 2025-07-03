#Taken from DiffCorr package, which I couldn't install for some reason
#https://cran.r-project.org/web/packages/DiffCorr/index.html
compcorr <- function(n1, r1, n2, r2){
  # Fisher's Z-transformation
  # ad hoc process
  num1a <- which(r1 >= 0.99)
  num2a <- which(r2 >= 0.99)
  r1[num1a] <- 0.99
  r2[num2a] <- 0.99
  num1b <- which(r1 <= -0.99)
  num2b <- which(r2 <= -0.99)
  r1[num1b] <- -0.99
  r2[num2b] <- -0.99
  # z
  z1 <- atanh(r1)
  z2 <- atanh(r2)
  # difference
  dz <- (z1 - z2)/sqrt(1/(n1 - 3) + (1/(n2 - 3)))
  # p-value
  pv <- 2*( 1 - pnorm(abs(dz)) )
  return(list(diff=dz, pval=pv))
}