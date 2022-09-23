# Generate the fuzzy set weights to define a type-1 OWA operator
#
#	Inputs:
#		numObj	:	number of aggregated objects
#	
#		swOpt   : 	singleton weight option
#		lwOpt	:	linguistic weight option
#					lwOpt$lwCat 		-- 	linguistic weight category for $swOpt==4
#					lwOpt$specificWt	--	specific weight chosen for a given 
#											linguistic weight category (lwCat) 
#	Outputs:
#		LW[[k]]		$domain
#					$mfValue
#
#		WmfParams	: parameters for triangular fuzzy set weights
#
# usage fsW=fuzzySetWeightsGen(numObj,swOpt,lwOpt)
#
#	Author:
#			@Shang-Ming Zhou, 14/06/2022
#
#	rev: adding "type", "para" in each weight
#
#######################################################
#	swOpt		 -		1 = 'Meet'/'singletonMeet'/'fuzzfiedMinimum'
#						2 = 'Join'/'singletonJoin'/'fuzzifiedMaximum'
#						3 = 'Mean'/'singletonAvg'/'fuzzifiedAverage'
#						4 =	Not use crisp weights
#	For swOpt==4:
#	 lwOpt$lwCat   -	1 =	'MeetLike'
#						2 =	'JoinLike'
#						3 =	'AvgLike'
#
# 	lwOpt$specificWt - 1)For lwOpt$lwCat ==1 # 'MeetLike'
#							1 = 'unEqualDomainPartition'
#							2 = 'equalDomainPartition'	# 'left2right' 
#							3 = 'type-2 quantifier "most"'
#
#	lwOpt$specificWt - 2)For lwOpt$lwCat==2 # 'JoinLike'
#							1 = 'unEqualDomainPartition'
#							2 = 'equalDomainPartition' # 'right2left' 
#
#	lwOpt$specificWt - 3)For weightType==3 #'AvgLike'
#							1 = 'unEqualDomainPartition'
#
#############################################################

source("./www/fuzzySetWeightsByQuantifier2.r")
source("./www/trimf.r")

fuzzySetWeightsGen <- function(numObj, swOpt, lwOpt, specificWt){	

	####################################################
	###
	#### sigleton fuzzy sets
	###
	######################################################
	LW = vector("list", numObj);
  if(swOpt == 1) #"singletonMeet"
	{	
    #WmfParams=vector("list",numObj);
    
    LW[[numObj]]$domain=1;				
    LW[[numObj]]$mfValue=1;
    LW[[numObj]]$type="triangular"
    
    for(k in 1:(numObj-1))
    {	
      LW[[k]]$domain=0;				
      LW[[k]]$mfValue=1;	
      LW[[k]]$type="triangular"
    }
	}
			 
	if(swOpt == 2) #"singletonJoin"
	{				
		#WmfParams=vec      tor("list",numObj);
			
		LW[[1]]$domain=1;				
		LW[[1]]$mfValue=1;
		LW[[1]]$type="triangular"
				
		for(k in 2:numObj)
			{	
				LW[[k]]$domain=0;				
				LW[[k]]$mfValue=1;	
				LW[[k]]$type="triangular"
			}
	}
			
	if(swOpt == 3) #"singletonAvg"
	{
			q=1/numObj;
			#WmfParams=vector("list",numObj);
					
			for(k in 1:numObj)
			{	
				LW[[k]]$domain=q;				
				LW[[k]]$mfValue=1;	
				LW[[k]]$type="singletonMF"
			}
		}
				
	####################################################
	###
	#### Linguistic fuzzy sets
	###
	######################################################	
  if(swOpt == 4) # not singleton weights
	{	
		###################################
		############ "meetLike"
		###################################
		if(lwOpt == 1) # "meetLike"
		{
		  
			if(specificWt == 1)#'unEqualDomainPartition'
			{
				## create  meet-like operator
						  
				LW=vector("list",numObj);
				WmfParams=vector("list",numObj);
				tmpWplot=vector("list",numObj);
						  
				LW[[numObj]]$domain=seq(0.8,1,length=30);#seq(0.8,1,length=4); #for plot the joinness
				WmfParams[[numObj]]=c(0.8,1,1);
				LW[[numObj]]$mfValue=trimf(LW[[numObj]]$domain, WmfParams[[numObj]]);
				LW[[numObj]]$type="triMF"
				LW[[numObj]]$para=WmfParams[[numObj]]
				
				tmpWplot[[numObj]]$domain=seq(0,1,0.01);
				tmpWplot[[numObj]]$mfValue=trimf(tmpWplot[[numObj]]$domain, WmfParams[[numObj]]);				  
						  
				for(k in 1:(numObj-1))
					{								#LW[[k]]$domain=seq(0,0.1,length=30);#seq(0,0.1,length=4);
						LW[[k]]$domain=seq(0,0.2,length=30);#seq(0,0.1,length=4);
						WmfParams[[k]]=c(0,0,0.2);
						LW[[k]]$mfValue=trimf(LW[[k]]$domain, WmfParams[[k]]);
						LW[[k]]$type="triMF"
						LW[[k]]$para=WmfParams[[k]]
				
						tmpWplot[[k]]$domain=seq(0,1,0.01);
						tmpWplot[[k]]$mfValue=trimf(tmpWplot[[k]]$domain, WmfParams[[k]]);
					}	
			}
			
			if(specificWt == 2)#'equalDomainPartition'	# 'left2right' 
			{
				q=0:(numObj-1);
				q=q/(numObj-1);
					
				Wini=vector("list",numObj);
				LW=vector("list",numObj);
				WmfParams0=vector("list",numObj);
				WmfParams=vector("list",numObj);		
					
				for(k in 1:numObj)
				{	
					if(k==1)
					{
						Wini[[k]]$domain=seq(q[1],q[k+1],length=5);
						WmfParams0[[k]]=c(q[k],q[k],q[k+1]);
					} else if(k==numObj)
					{
						Wini[[k]]$domain=seq(q[k-1],q[k],length=5);
						WmfParams0[[k]]=c(q[k-1],q[k],q[k]);
					} else
					{
						Wini[[k]]$domain=seq(q[k-1],q[k+1],length=5);
						WmfParams0[[k]]=c(q[k-1],q[k],q[k+1]);
					}
							
						Wini[[k]]$mfValue=trimf(Wini[[k]]$domain, WmfParams0[[k]]);	
				}
					
				for(k in 1:numObj)
				{
					LW[[k]]=Wini[[k]];
					WmfParams[[k]]=WmfParams0[[k]];
					LW[[k]]$type="triMF"
					LW[[k]]$para=WmfParams[[k]]
				}											
			}
			
			if(specificWt == 3)#'type-2 quantifier "most"'	
			{
				fsW=fuzzySetWeightsByQuantifier2(numObj);
				LW=fsW$W;
				WmfParams = fsW$WmfParams
				
				for(k in 1:numObj)
				{
					LW[[k]]$type="triMF"
					LW[[k]]$para=WmfParams[[k]]
				}								
			}	
			
		} # end "if(lwOpt$lwCat==1)" # "meetLike"
	
		###################################
		############ "joinLike"
		###################################
		if(lwOpt == 2) # "joinLike"
		{
			if(specificWt==1)#'unEqualDomainPartition'
			{
			  ## create  Join-like operator
				  
			  LW=vector("list",numObj);
			  WmfParams=vector("list",numObj);
			  tmpWplot=vector("list",numObj);
						  
			  LW[[1]]$domain=seq(0.8,1,length=30);#seq(0.8,1,length=4);
			  WmfParams[[1]]=c(0.8,1,1);
			  LW[[1]]$mfValue=trimf(LW[[1]]$domain, WmfParams[[1]]);
			  LW[[1]]$type="triMF"
	 		  LW[[1]]$para=WmfParams[[1]]						
								
			  tmpWplot[[1]]$domain=seq(0,1,0.01);
			  tmpWplot[[1]]$mfValue=trimf(tmpWplot[[1]]$domain, WmfParams[[1]]);				  
			  
			  for(k in 2:numObj)
				{
					LW[[k]]$domain=seq(0,0.2,length=30);#seq(0,0.2,length=4);
					WmfParams[[k]]=c(0,0,0.2);
					LW[[k]]$mfValue=trimf(LW[[k]]$domain, WmfParams[[k]]);
					LW[[k]]$type="triMF"
					LW[[k]]$para=WmfParams[[k]]
						
					tmpWplot[[k]]$domain=seq(0,1,0.01);
					tmpWplot[[k]]$mfValue=trimf(tmpWplot[[k]]$domain, WmfParams[[k]]);
				}					
			}
			
			if(specificWt==2)#'equalDomainPartition' # 'right2left' 
			{			
				q=0:(numObj-1);
				q=q/(numObj-1);
				
				Wini=vector("list",numObj);
				LW=vector("list",numObj);
				WmfParams0=vector("list",numObj);
				WmfParams=vector("list",numObj);		
				
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
					LW[[k]]=Wini[[(numObj-(k-1))]];
					WmfParams[[k]]=WmfParams0[[(numObj-(k-1))]];
					LW[[k]]$type="triMF"
					LW[[k]]$para=WmfParams[[k]]					
				}					
			} # end "if(lwOpt$specificWt==2)" #'equalDomainPartition'

		}# end "if(lwOpt$lwCat==2)" # "joinLike"
		
		###################################
		############ "averageLike"
		###################################
		if(lwOpt == 3) # "averageLike" only by 'unEqualDomainPartition' #lwOpt$specificWt
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
				LW[[k]]$type="triMF"
				LW[[k]]$para=WmfParams[[k]]
						
				tmpWplot[[k]]$domain=seq(0,1,0.01);
				tmpWplot[[k]]$mfValue=trimf(tmpWplot[[k]]$domain, WmfParams[[k]]);
			}			
		 } # end "if(lwOpt$lwCat==3)" # "averageLike"
				
	} #end "if(swOpt==4)" # not singleton weights
	#######################################################
		
	########################plot linguistic weights######################
	domainLW = seq(0, 1, 0.1);
	
	if(swOpt == 4 & specificWt == 1) # 'unEqualDomainPartition'
	#if(wty==1) #"trimf_unEqualPartition"
	{	 		
			plot(seq(0,1,0.1), seq(0,1,length=length(seq(0,1,0.1))), xlab="U",ylab="Grade of membership degree",font.lab=1,type ="n")
	 		for(k in 1:numObj)
			{
				lines(LW[[k]]$domain,LW[[k]]$mfValue,lwd=2)
				
				txtX=WmfParams[[k]][2]-0.02
				text(txtX,0.8,eval(substitute(expression(italic(W^k)))),adj=c(0,0))					
			 }
	 }
	 
	if(swOpt == 4 & specificWt == 2) # 'trimf_equalPartition'
	#if(wty==2) #"trimf_equalPartition"
	{
				plot(seq(0,1,0.1), seq(0,1,length=length(seq(0,1,0.1))), xlab="U",ylab="Grade of membership degree",font.lab=1,type ="n")
				for(k in 1:numObj)
			 	{

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
					
	  
				plot(seq(0,1,0.1), seq(0,1,length=length(seq(0,1,0.1))), xlab="U",ylab="Grade of membership degree",font.lab=1,type ="n")
				for(k in 1:numObj)
			 	{			
					lines(LW[[k]]$domain,LW[[k]]$mfValue,lwd=2)
				}
				
				#######################################
				# Showing labels of weights
				#######################################
				
				plot(seq(0,1,0.1), seq(0,1,length=length(seq(0,1,0.1))), xlab="U",ylab="Grade of membership degree",font.lab=1,type ="n")
				for(k in 1:numObj)
			 	{						
					lines(LW[[k]]$domain,LW[[k]]$mfValue,lwd=2)
					
					if(lwOpt == 1 & specificWt == 2)##"left2right"	
					#if (specificWty==1) #"left2right"		
					{
						if(k == 1){ 
							txtX=WmfParams[[k]][2]									
						}else{
					  	txtX=WmfParams[[k]][2]-0.02
						}	
					  text(txtX,0.8,eval(substitute(expression(italic(W^k)))),adj=c(0,0))									
					}							
					
					if(lwOpt == 2 & specificWt == 2)##"right2left"	
					#if (specificWty==2) #"right2left"		
					{
					  if(k<numObj){
						  txtX=WmfParams[[k]][2]-0.02
						} else {
						  txtX=WmfParams[[k]][2]
					  }
				    text(txtX,0.8,eval(substitute(expression(italic(W^k)))),adj=c(0,0))									
					}								
					 
				}		
				lines(c(0,1),c(0,0),lwd=2)					
		}
		
	if(swOpt < 4)	##"crisp"
	#if(wty==3) #"crisp"
	{
	  plot(seq(0,1,0.1), seq(0,1,length=length(seq(0,1,0.1))), xlab="U",ylab="Grade of membership degree",font.lab=1,type ="n")
	   
	  for(k in 1:numObj){
	    points(LW[[k]]$domain,LW[[k]]$mfValue,pch=20)						
	    lines(c(LW[[k]]$domain, LW[[k]]$domain), c(LW[[k]]$mfValue, 0), lty=2)
	    
	    if(LW[[k]]$domain==0)
	      txtX=LW[[k]]$domain+0.02
	    else if(LW[[k]]$domain==1)
	      txtX=LW[[k]]$domain-0.06
	    else
	      txtX=LW[[k]]$domain+0.02				
	    
	    text(txtX,0.8,eval(substitute(expression(italic(W^k)))),adj=c(0,0))
	  }
	}
	  
	return(LW)
	  #if(swOpt<4) # singleton weights
	  #{
	  #	  return(list(W=LW));			 
	  #}
	  #	
	  #if(swOpt==4) # linguistic weights
	  #{
	  #	  return(list(W=LW,WmfParams=WmfParams));			 
	  #}
}			 

 