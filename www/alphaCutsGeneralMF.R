
#This function is to calculate the end point value of the interval values of alpha-cut of a general MF
#
#Inputs: 
#			A[[i]]		:	Fuzzy set	-- 	A=vector("list",1);
#							A$mfValue	---	membership degrees;
#							A$domain	--- domain X;
#
#			alpha		: the confidence level value in [0,1]
#
#	Outputs:
#			$itvL			:	the left end value of the alpha-cut result
#			$itvR			:  	the right end value of the alpha-cut result
#
#	Shang-Ming Zhou, De Montfort University, 10/11/2008
#

alphaCutsGeneralMF <- function(A,alpha){
  
	mvalues = A$mfValue
	domainX = A$domain
	
	if(alpha <= max(mvalues))
	{
		ind=(mvalues>=alpha)
		
		left=min(domainX[ind]);
		right=max(domainX[ind]);
  }
  else
  {
  	left=NA;
  	right=NA;
  }
	
  return(list(itvL=left,itvR=right,alpha=alpha));
}
