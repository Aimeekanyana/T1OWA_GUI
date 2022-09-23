
#This function is to calculate the end point value of the interval values of alpha-cut of a general MF
#
#Inputs: 
#			A[[i]]		:	Fuzzy set	-- 	A=vector("list",1);
#							A$para		---	parameters of A;
#							A$domain	--- domain X;
#							A$label		--- class label that A is representing
#							A$type		---	type of membership functions: "gauss", "trimf", "genTrapesoid"
#
#			samples		: 	point to be classified
#
#	Outputs:
#			labels		:	class label assigned to x
#
#	Shang-Ming Zhou, De Montfort University, 10/11/2008
#

maxMembershipPrinciple2 <- function(samples, A){
  
	numSets=length(A);
	numSample=length(samples);
	
	labels=vector("numeric",numSample)
	
	mfValue=matrix(0,nrow=numSets,ncol=numSample);
			
	# to compute the MF values
	for (i in 1:numSets){
		
		mfType=A[[i]]$type;
		
		if(mfType=="gauss"){
			mfValue[i,]=gaussmf(samples,A[[i]]$para);		
		}	
		
		if(mfType=="trimf"){
			mfValue[i,]=trimf2(samples,A[[i]]$para);		
		}	
			
		if(mfType=="trapesoid"){
			mfValue[i,]=trapezoidalMFs(samples,A[[i]]$para);		
		}		
	
		if(mfType=="genTrapesoid"){
			mfValue[i,]=genTrapezoidalMFs(samples,A[[i]]$para);		
		}	
	}
	
	# transpose: each sample corresponds to a row
	
	mfValue2=t(mfValue);	
	maxIndex=max.col(mfValue2);
	
	for (s in 1:numSample){
		ind=maxIndex[s]
		if(is.na(ind)){
			labels[s]=NA;
		} else {
			labels[s]=A[[ind]]$label;	
		}
	}
	
	labels
     
  #return(list(itvL=left,itvR=right,alpha=alpha));
}