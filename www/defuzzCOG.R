# To defuzzify fuzzy set by calcuting its cener of gravity
#
#Inputs: 
#			A[[i]]		:	Fuzzy set	-- 	A=vector("list",1);
#							A$mfValue	---	membership degrees;
#							A$domain	--- domain X;
##	Outputs:
#			cog			:	the cener of gravity of A[[i]]
#
#	Author	:	Shang-Ming Zhou, De Montfort University, 10/11/2008
#

defuzzCOG <- function(A){
  
	mvalues=A$mfValue
	domainX=A$domain
	
	cog=sum(mvalues*domainX/sum(mvalues));
	cog	
 # return(list(itvL=left,itvR=right,alpha=alpha));
}