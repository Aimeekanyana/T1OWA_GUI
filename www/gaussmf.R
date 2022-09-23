# from JMG:	R Fuzzy Toolbox: v0.5: JMG: 03/02/05
#
gaussmf <- function(x, mfParams) {
  sig <- mfParams[1]
  c <- mfParams[2]
  
  exp(-(x - c)^2/(2 * sig^2))
}