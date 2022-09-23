##
## to implement T1OWA by triangle fuzzy set weights
##
## Inputs :
##		fsW[[k]] - K fuzzy set weights
#			fsW[[k]]$domain
#			fsW[[k]]$mfValue

# 		WmfParams[[k]] -	parameters for triangular fuzzy set weights
#
#		A[[k]] - K fuzzy sets to be aggregated
#			A[[k]]$domain
#			A[[k]]$mfValue
#
# Output :
#	aggResult  - aggregation result:
#		 	$mfValue 
#	 		$domain
#
#	@Shang-Ming Zhou, 14 June 2022
#

t1owa_byTriWeights <- function(fsW,WmfParams,A)
{	
	source("alphaCutsGeneralMF.r")
	source("alphaCutsTrimf.r")
	source("itvOWA_withNullSets.r")
	source("memDegByAlphaCuts_withNullCheck.r")
	
	alpha=seq(0,1,length=100);
	numAlp=length(alpha);
	
	numObj=length(A);
	domainX=A[[1]]$domain;

	alphaCutA=vector("list",numObj);
	alphaCutW=vector("list",numObj);
	alphaCutAgg=vector("list",numAlp);
			
	for(i in 1:numAlp)
	{
		# To get the alpha-cuts of fuzzy sets
		for(k in 1:numObj)
			{
				if(alpha[i]==0)
				{
					alphaCutA[[k]]$itvL=min(domainX);
					alphaCutA[[k]]$itvR=max(domainX);	
					alphaCutW[[k]]$itvL=min(fsW[[k]]$domain);
					alphaCutW[[k]]$itvR=max(fsW[[k]]$domain);				
				} else 
					{
						alphaCutA[[k]]=alphaCutsGeneralMF(A[[k]],alpha[i]);	
						#if(wty=="crisp")
						if(partitionType==3)					
						{
							alphaCutW[[k]]=alphaCutsGeneralMF(fsW[[k]],alpha[i]);
						} else 
						{			
							alphaCutW[[k]]=alphaCutsTrimf(WmfParams[[k]],alpha[i]);
						  }
					}
			}
						
					##############	Perform alpha-cuts OWA  	###########
					
					#alphaCutAgg[[i]]=itvOWA(alphaCutW,alphaCutA);
					alphaCutAgg[[i]]=itvOWA_withNullSets(alphaCutW,alphaCutA);	
				
				alphaCutAgg[[i]]$alpha=alpha[i];
	 }	## end alphaCutAgg

	############################################################################
	###
	### Step 4: To calculate the grades of membership degrees by alpha-cuts
	###
	############################################################################

	aggResult=memDegByAlphaCuts_withNullCheck(domainX,alphaCutAgg);
	
	aggResult
}