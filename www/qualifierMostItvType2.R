#
#	Proportional qualifier "most" defined by interval type-2 fuzzy set
#	
#	Inputs:		
#			parA	: the first parameter
#			parB	: the second parameter
#			parC	: the third parameter
#			0<=r<=1	: the relative position 
#
#	Outputs: 
#			[itvL, itvR]	: interval
#
#	"most"					:	parA=0.3,parB=0.4,parC=0.8.
#
#	Shang-Ming Zhou, De Montfort University, 19/11/2006
#
qualifierMostItvType2 <- function(parA=0.3,parB=0.4,parC=0.8,r){

#print(parA)
#print(parB)
#print(parC)

	if (r<parA)
		{
			intervalLeft=0;
			intervalRight=0;
		}
	else if(r<parB)
		{
			intervalLeft=0;	
			intervalRight=(r-parA)/(parC-parA);
		 }
	else if(r<=parC){
			intervalLeft=(r-parB)/(parC-parB);
			intervalRight=(r-parA)/(parC-parA);
		}
	else{
			intervalLeft=1;
			intervalRight=1;
		}
	
	return(list(itvL=intervalLeft,itvR=intervalRight));

}
