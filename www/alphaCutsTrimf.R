
#This function is to calculate the end point value of the interval values of alpha-cut of a triangular MF
#
#	Inputs:
#			mfParams[1]	: the left point of MF
#			mfParams[2]	: the center point of MF
#			mfParams[3]	: the right point of MF
#			alpha		: the confidence level value in [0,1]
#
#	Outputs:
#			$L			:	the left end value of the alpha-cut result
#			$R			:  	the right end value of the alpha-cut result
#
#	Shang-Ming Zhou, De Montfort University, 20/11/2006
#
#

alphaCutsTrimf <- function(mfParams,alpha){
  a <- mfParams[1]
  b <- mfParams[2]
  c <- mfParams[3]
  
  left=a+alpha*(b-a)
  right=c-alpha*(c-b)
  
  return(list(itvL=left,itvR=right,alpha=alpha));
}