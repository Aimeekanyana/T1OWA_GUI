
#This function is to calculate membership degrees based on the alpha-cut sets
#
#	Inputs:
#			alphaCuts[[i]]	: 	list structure of the ith alpha-cuting set interval 
#		
#								alphaCuts[[i]]$itvL		--	left value of the interval
#								alphaCuts[[i]]$itvR		--	Right value of the interval
#								alphaCuts[[i]]$alpha	--	corresponding alpha value
#		
#								note: creating alphaCutsby using alphaCuts=vector("list",1);
#
#			domainX			:	1*n vector-- the domain of discourse
#
#	Outputs:
#			$mfValue		:	MF values of the aggregated FS
#			$domain			:  	the domain of the aggregated FS
#
#	Shang-Ming Zhou, De Montfort University, 23/11/2006
#
#  To check if the A[[i]] are null sets: 7/11/2008
#	version	: 2
#

memDegByAlphaCuts_withNullCheck <- function(domainX, alphaCuts){

	numX=length(domainX);
	numA=length(alphaCuts);	
	
	mfValue=vector("numeric",1);
	
	for(k in 1:numX)
	 {
	 	tmpX=domainX[k];
	 	s=0;
	 	
	 	listAlpha=vector("numeric",1);
	 	
		for(i in 1:numA)
		 {
		 	if (!is.na(alphaCuts[[i]]$itvL))		 	
		 	{
			 	tmpL=alphaCuts[[i]]$itvL;
			 	tmpR=alphaCuts[[i]]$itvR;
			 	
				if((tmpL<=tmpX)&(tmpX<=tmpR))
				 {
				 	s <- s+1;
				 	listAlpha[s]=alphaCuts[[i]]$alpha;
				 }	
			}		
		 }
		 		 
		 mfValue[k]=max(listAlpha);		 
	 }
	
	print("LLLLLLMMMMMMMMMM")
	print(mfValue)
	print('PAAAAAAAAAAAAAAAAAAAAAPPPPPPPPPPP')
	
  return(list(mfValue=mfValue, domain=domainX));
}