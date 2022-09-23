# Set up the weight in fuzzy sets
#
#	Inputs:
#		numObj	:	number of aggregated objects
#	
#		wty== 1 # "trimf_unEqualPartition")# manual design:
#			  specificWty=	1: "MeetLike"
#							2: "JoinLike"
#							3: "AvgLike"					  
#		
#	 	wty==2 # "trimf_equalPartition":
#		  	specificWty=	1:	"left2right"
#							2:	"right2left" 
#
#		wty== 3# "crisp"	# sigleton fuzzy sets
#		  	specificWty=	1:	"singletonMeet" 
#							2:	"singletonJoin"
#							3:	"singletonAvg" 
#
#		partitionX ="equal"
#		domainL  =  number of sampling points in the domain of W in meet-like and join-like operators
#
#	Outputs:
#		LW[[k]]		$domain
#					$mfValue
#
#		WmfParams	: parameters for triangular fuzzy set weights
#
#	Author:
#			@Shang-Ming Zhou, De Montfort University, 10/11/2008
#			Rev: adding labels, 14/06/2022
#
#
#fuzzySetWeights <- function(numObj,wty,specificWty,partitionX){	
#fuzzySetWeights_likeOpDomainLen <- function(numObj,wty,specificWty,partitionX,domainL){	
fuzzySetWeightsByTrimf <- function(numObj,wty,specificWty){	
	
	if(wty==1) #"trimf_unEqualPartition" # manual design
	{
				source("trimf.r")
										
				if(specificWty==1)#"MeetLike"
						{
						  ## create  meet-like operator
						  
						  LW=vector("list",numObj);
						  WmfParams=vector("list",numObj);
						  tmpWplot=vector("list",numObj);
						  
						  LW[[numObj]]$domain=seq(0.8,1,length=30);#seq(0.8,1,length=4); #for plot the joinness
						  WmfParams[[numObj]]=c(0.8,1,1);
						  LW[[numObj]]$mfValue=trimf(LW[[numObj]]$domain, WmfParams[[numObj]]);
								
						  tmpWplot[[numObj]]$domain=seq(0,1,0.01);
						  tmpWplot[[numObj]]$mfValue=trimf(tmpWplot[[numObj]]$domain, WmfParams[[numObj]]);				  
						  
						  for(k in 1:(numObj-1))
							{
								#LW[[k]]$domain=seq(0,0.1,length=30);#seq(0,0.1,length=4);
								LW[[k]]$domain=seq(0,0.2,length=30);#seq(0,0.1,length=4);
								WmfParams[[k]]=c(0,0,0.2);
								LW[[k]]$mfValue=trimf(LW[[k]]$domain, WmfParams[[k]]);
								
								tmpWplot[[k]]$domain=seq(0,1,0.01);
								tmpWplot[[k]]$mfValue=trimf(tmpWplot[[k]]$domain, WmfParams[[k]]);
							}
						}
										
					if(specificWty==2) #"JoinLike"
						{
						  ## create  Join-like operator
						  
						  LW=vector("list",numObj);
						  WmfParams=vector("list",numObj);
						  tmpWplot=vector("list",numObj);
						  
						  LW[[1]]$domain=seq(0.8,1,length=30);#seq(0.8,1,length=4);
						  WmfParams[[1]]=c(0.8,1,1);
						  LW[[1]]$mfValue=trimf(LW[[1]]$domain, WmfParams[[1]]);
								
						  tmpWplot[[1]]$domain=seq(0,1,0.01);
						  tmpWplot[[1]]$mfValue=trimf(tmpWplot[[1]]$domain, WmfParams[[1]]);				  
						  
						  for(k in 2:numObj)
							{
								LW[[k]]$domain=seq(0,0.2,length=30);#seq(0,0.2,length=4);
								WmfParams[[k]]=c(0,0,0.2);
								LW[[k]]$mfValue=trimf(LW[[k]]$domain, WmfParams[[k]]);
								
								tmpWplot[[k]]$domain=seq(0,1,0.01);
								tmpWplot[[k]]$mfValue=trimf(tmpWplot[[k]]$domain, WmfParams[[k]]);
							}
						}	

					if(specificWty== 3) #"AvgLike"
						{
						  q=1/numObj; ## create  like-avg operator
						  
						  LW=vector("list",numObj);
						  WmfParams=vector("list",numObj);
						  tmpWplot=vector("list",numObj);
						  
						  for(k in 1:numObj)
							{
								LW[[k]]$domain=seq(0,2*q,length=5);
								WmfParams[[k]]=c(0,q,2*q);
								LW[[k]]$mfValue=trimf(LW[[k]]$domain, WmfParams[[k]]);
								
								tmpWplot[[k]]$domain=seq(0,1,0.01);
								tmpWplot[[k]]$mfValue=trimf(tmpWplot[[k]]$domain, WmfParams[[k]]);
							}
						}						
						
				}		

	if(wty==2) #"trimf_equalPartition"
		{
			q=0:(numObj-1);
			q=q/(numObj-1);
			
			Wini=vector("list",numObj);
			LW=vector("list",numObj);
			WmfParams0=vector("list",numObj);
			WmfParams=vector("list",numObj);		
			
			source("trimf.r")
			
			for(k in 1:numObj)
			{	
				if(k==1)
					{
						Wini[[k]]$domain=seq(q[1],q[k+1],length=5);
						WmfParams0[[k]]=c(q[k],q[k],q[k+1]);
					}
				else if(k==numObj)
					{
						Wini[[k]]$domain=seq(q[k-1],q[k],length=5);
						WmfParams0[[k]]=c(q[k-1],q[k],q[k]);
					}
				else
					{
						Wini[[k]]$domain=seq(q[k-1],q[k+1],length=5);
						WmfParams0[[k]]=c(q[k-1],q[k],q[k+1]);
					}
					
				Wini[[k]]$mfValue=trimf(Wini[[k]]$domain, WmfParams0[[k]]);	
			}
			
			for(k in 1:numObj)
			{
				if (specificWty==1)#"left2right"
				{
					LW[[k]]=Wini[[k]];
					WmfParams[[k]]=WmfParams0[[k]];
				}
				
				if (specificWty==2) #"right2left"
				{	
					LW[[k]]=Wini[[(numObj-(k-1))]];
					WmfParams[[k]]=WmfParams0[[(numObj-(k-1))]];
				}
			}
		}
	
	if(wty==3) #"crisp" # sigleton fuzzy sets
		{
		
			if(specificWty==1) #"singletonMeet"
				{		
			
					LW=vector("list",numObj);
					#WmfParams=vector("list",numObj);
					
					LW[[numObj]]$domain=1;				
					LW[[numObj]]$mfValue=1;
					
					for(k in 1:(numObj-1))
					{	
						LW[[k]]$domain=0;				
						LW[[k]]$mfValue=1;	
					}
				}
				
			if(specificWty==2) #"singletonJoin"
				{		
			
					LW=vector("list",numObj);
					#WmfParams=vector("list",numObj);
					
					LW[[1]]$domain=1;				
					LW[[1]]$mfValue=1;
					
					for(k in 2:numObj)
					{	
						LW[[k]]$domain=0;				
						LW[[k]]$mfValue=1;	
					}
				}
				
			if(specificWty== 3) #"singletonAvg"
				{
					q=1/numObj;
			
					LW=vector("list",numObj);
					#WmfParams=vector("list",numObj);
					
					for(k in 1:numObj)
					{	
						LW[[k]]$domain=q;				
						LW[[k]]$mfValue=1;	
					}
				}
				
		}
		
	########################plot linguistic weights######################
	domainLW=seq(0,1,0.1);
		
	if(wty==1) #"trimf_unEqualPartition"
	{	 		
	 		for(k in 1:numObj)
				{	x11()
					plot(seq(0,1,0.1), seq(0,1,length=length(seq(0,1,0.1))), xlab="U",ylab="Grade of membership degree",font.lab=1,type ="n")
					lines(LW[[k]]$domain,LW[[k]]$mfValue,lwd=2)
					
					txtX=WmfParams[[k]][2]-0.02
					text(txtX,0.8,eval(substitute(expression(italic(W^k)))),adj=c(0,0))					
			 	}
	 }
	
	if(wty==2) #"trimf_equalPartition"
		{
				for(k in 1:numObj)
			 		{
						x11()
						plot(seq(0,1,0.1), seq(0,1,length=length(seq(0,1,0.1))), xlab="U",ylab="Grade of membership degree",font.lab=1,type ="n")

						lines(LW[[k]]$domain,LW[[k]]$mfValue,lwd=2)
						if(k==1)
						  {
						  	mxDom=max(LW[[k]]$domain);
						  	lines(c(mxDom,1),c(0,0),lwd=2)							  	
						  }
						else if(k==numObj)
						 {
						 	mnDom=min(LW[[k]]$domain);
						  	lines(c(0,mnDom),c(0,0),lwd=2)	
						 }
						 else
						 {
						 	mnDom=min(LW[[k]]$domain);
						 	mxDom=max(LW[[k]]$domain);
						 	lines(c(0,mnDom),c(0,0),lwd=2)
						 	lines(c(mxDom,1),c(0,0),lwd=2)								 
						 }
					}
					
				x11()
				plot(seq(0,1,0.1), seq(0,1,length=length(seq(0,1,0.1))), xlab="U",ylab="Grade of membership degree",font.lab=1,type ="n")
				for(k in 1:numObj)
			 		{			
						lines(LW[[k]]$domain,LW[[k]]$mfValue,lwd=2)
					}
				
				#######################################
				# Showing labels of weights
				#######################################
				x11()
				plot(seq(0,1,0.1), seq(0,1,length=length(seq(0,1,0.1))), xlab="U",ylab="Grade of membership degree",font.lab=1,type ="n")
				for(k in 1:numObj)
			 		{						
						lines(LW[[k]]$domain,LW[[k]]$mfValue,lwd=2)
						
						if (specificWty==1) #"left2right"		
						 {
							if(k==1){ 
								txtX=WmfParams[[k]][2]									
							 }else{
								txtX=WmfParams[[k]][2]-0.02
						   	 }							
						}															
						
						if (specificWty==2) #"right2left"		
						 {
							if(k<numObj){
								txtX=WmfParams[[k]][2]-0.02
						 	 } else {
								txtX=WmfParams[[k]][2]
							 }
						 }								
						 
					  text(txtX,0.8,eval(substitute(expression(italic(W^k)))),adj=c(0,0))									
					}		
					lines(c(0,1),c(0,0),lwd=2)					
		}
		
	if(wty==3) #"crisp"
		{
		  for(k in 1:numObj)
			{
		    	x11()
				plot(seq(0,1,0.1), seq(0,1,length=length(seq(0,1,0.1))), xlab="U",ylab="Grade of membership degree",font.lab=1,type ="n")
				points(LW[[k]]$domain,LW[[k]]$mfValue,pch=20)						
				lines(c(LW[[k]]$domain,LW[[k]]$domain),c(LW[[k]]$mfValue,0),lty=2)
				
				if(LW[[k]]$domain==0)
					txtX=LW[[k]]$domain+0.02
				else if(LW[[k]]$domain==1)
					txtX=LW[[k]]$domain-0.06
				else
					txtX=LW[[k]]$domain+0.02				
				
				text(txtX,0.8,eval(substitute(expression(italic(W^k)))),adj=c(0,0))
			}
		}
				 
	  return(list(W=LW,WmfParams=WmfParams));			 
}			 

 