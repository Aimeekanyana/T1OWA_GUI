# Set up the weight in fuzzy sets
#
#	Inputs:
#		numObj	:	number of aggregated objects
#	
#		qty		:	type of quantifiers
#						"t2most"  # type-2 quantifier--"most"	
#			 		
#	Outputs:
#		W[[k]]		$domain
#					$mfValue
#
#		WmfParams	: parameters for triangular fuzzy set weights
#
#	Author:
#			Shang-Ming Zhou, De Montfort University, 26/11/2008
#
#

#fuzzySetWeightsByQuantifier <- function(numObj,qty){
source("./www/qualifierMostItvType2.R")
source("./www/trimf.R")
source("./www/t1fsDiff.R")

#fuzzySetWeightsByQuantifier <- function(numObj,qty){
fuzzySetWeightsByQuantifier2 <- function(numObj){

	#if(qty=="t2most")	
	#{		
		stepU=0.001;
		domainU=seq(0,1,by=stepU);
		
		#To calculate the weight intervals by type-2 linguistic quantifier "most"
		
		quanty="most"#"atLeast"
		
		q=0:numObj;
		q=q/numObj;
		
		noJ=numObj+1;
		
		FOU=vector("list",noJ);
		MF2nd=vector("list",noJ);
		W=vector("list",numObj);
		
		# To calculate the FOU at each point u
		if(quanty=="most")
		 {
			for(k in 1:(numObj+1))
			{
				#FOU[[k]]=qualifierMostItvType2(parA=0.3,parB=0.4,parC=0.8,r=q[k]);	
				FOU[[k]]=qualifierMostItvType2(parA=0.3,parB=0.5,parC=0.8,r=q[k]);	
			}
		 }	
		# To set up the domains based on the FOUs for the secondary MFs as triangular functions
			for(k in 1:(numObj+1))
			{		
				st=FOU[[k]]$itvL;
				en=FOU[[k]]$itvR;
				midP=(st+en)/2;
				
		#		st=as.integer(st/stepU)*stepU;
		#		en=as.integer(en/stepU)*stepU;
				
				MF2ndPara=c(st, midP, en);
				#MF2ndPara=c(st, midP, en,1.0);
				
				ind=(st<=domainU)&(domainU<=en)		
		
				MF2nd[[k]]$domain=domainU[ind]; #seq(st,en,by=stepU);
				
			    MF2nd[[k]]$mfValue=trimf(MF2nd[[k]]$domain, MF2ndPara);		
			    #MF2nd[[k]]$mfValue=trimf2(MF2nd[[k]]$domain, MF2ndPara);	
			}		
		 
		
		#To induce the linguistic weights by perfoming the differences of the secondary MFs
		 	for(k in 1:numObj)
			{		
				W[[k]]=t1fsDiff(MF2nd[[k+1]],MF2nd[[k]],domainU,"min");				
			}

		########################plot the secondary MFs######################
		plot(seq(0,1,by=stepU), seq(0,1,length=length(seq(0,1,by=stepU))), xlab="U",ylab="Grade of membership degree",font.lab=1,type ="n")
		for(k in 1:(numObj+1))
		  {
		  if(length(MF2nd[[k]]$domain)==1)
		  	{
		  	 lines(c(MF2nd[[k]]$domain,MF2nd[[k]]$domain),c(0,MF2nd[[k]]$mfValue),lty="dashed",lwd=2)
		  	 points(MF2nd[[k]]$domain,MF2nd[[k]]$mfValue,pch=19)
		  	}
		   else
		    {	lines(MF2nd[[k]]$domain,MF2nd[[k]]$mfValue,lwd=2) }
		  } 
		  						
		########################plot linguistic weights######################
		#To simplify the domains of weight fuzzy sets
		
		W2=vector("list",numObj);
		
		for(k in 1:numObj)
		  {
			ind=(W[[k]]$mfValue>0);
			
			W2[[k]]$domain=W[[k]]$domain[ind];
			W2[[k]]$mfValue=W[[k]]$mfValue[ind];
		  }
		  
		
		plot(seq(0,1,by=stepU), seq(0,1,length=length(seq(0,1,by=stepU))), xlab="U",ylab="Grade of membership degree",font.lab=1,type ="n")
		for(k in 1:numObj)
		  {
		  	  if(length(W2[[k]]$domain)==1)
		  	{
		  	 lines(c(W2[[k]]$domain,W2[[k]]$domain),c(0,W2[[k]]$mfValue),lty="dashed",lwd=2)
		  	 points(W2[[k]]$domain,W2[[k]]$mfValue,pch=19)
		  	}
		   else
		    {
			 lines(W2[[k]]$domain,W2[[k]]$mfValue,lwd=2) }
		  } 
		 	
		
		plot(seq(0,1,by=stepU), seq(0,1,length=length(seq(0,1,by=stepU))), xlab="U",ylab="Grade of membership degree",font.lab=1,type ="n")
		for(k in 1:numObj)
		  { 
			 lines(W[[k]]$domain,W[[k]]$mfValue,lwd=2)
		  }  
		  
		# To estimate the parameters of triangular linguistic weight functions
		
		WmfParams=vector("list",numObj);
		
		for(k in 1:numObj)
		  {
			ind=(W[[k]]$mfValue>0);
			par1=min(W[[k]]$domain[ind]);
			par3=max(W[[k]]$domain[ind]);
			
			indM=(W[[k]]$mfValue==max(W[[k]]$mfValue));	
			par2=mean(W[[k]]$domain[indM]);
			
			WmfParams[[k]]=c(par1, par2, par3);
		  }  
	
	#}	 			 
				 
	return(list(W=W,WmfParams=WmfParams));			 
}			 

 