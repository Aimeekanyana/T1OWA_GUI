# from JMG:	R Fuzzy Toolbox: v0.5: JMG: 03/02/05
#
trimf <- function(x, mfParams) {
  a <- mfParams[1]
  b <- mfParams[2]
  c <- mfParams[3]
  
  y= pmax(pmin( (x-a)/(b-a), (c-x)/(c-b) ), 0)
  y[is.na(y)]= 1; y
}