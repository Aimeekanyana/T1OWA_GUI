##
## to implement T1OWA by alpha-cuts(AC)
##
## S.-M. Zhou, F. Chiclana, R. I. John and J. M. Garibaldi, “Alpha-level ##aggregation: a practical approach to type-1 OWA operation for aggregating ##uncertain information with applications to breast cancer treatments,” IEEE ##Transactions on Knowledge and Data Engineering, Vol.23, No.10, pp. 1455-1468, ##2011. (http://dx.doi.org/10.1109/TKDE.2010.191)
##
## Inputs :
##		fsW[[k]] - K fuzzy set weights
#			fsW[[k]]$domain
#			fsW[[k]]$mfValue
#			fsW[[k]]$type : "gaussMF", "genTrapMF", "trapMF", "triMF","singtletonMF"
#			fsW[[k]]$para -	parameters for triangular fuzzy set weights

# 		WmfParams[[k]] -	parameters for triangular fuzzy set weights
#
#		swOpt	-	1= "meet"; 2= "join"; 3="mean"; 4="not singleton weights"
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
#	usage: aggResult=t1owaAC(W,A)
#
#
source("./www/alphaCutsGeneralMF.r")
source("./www/alphaCutsTrimf.r")
source("./www/itvOWA_withNullSets.r")
source("./www/memDegByAlphaCuts_withNullCheck.r")
#
#t1owa_byTriWeights2 <- function(fsW,WmfParams,A)
t1owaAC <- function(numObj, fsW, A)
{	
	
	alpha = seq(0, 5, length=100);
	numAlp = length(alpha);
	
	#domainX = A[[3]]$domain;
	domainX = seq(0, 5, length=100);

	alphaCutA = vector("list", numObj);
	alphaCutW = vector("list", numObj);
	alphaCutAgg = vector("list", numAlp);
	
	#print("legnth A")
	#print(length(A))
	#print(length(A[[1]]))
	#print(length(A[[1]][[1]]))
  #for(a in 1:length(A)){
  #  for(b in 1:length(A[[a]])){
  #    print("LMLLLLLLLLLLLLLLLLLLLL")	
  #    print("LMLLLLLLLLLLLLLLLLLLLL")
  #  }
  #}
	
	
	for(i in 1:numAlp)
	{
		# To get the alpha-cuts of fuzzy sets
		for(k in 1:numObj)
			{		
				if(alpha[i] == 0)
				{
					alphaCutA[[k]]$itvL=min(domainX);
					alphaCutA[[k]]$itvR=max(domainX);	
					alphaCutW[[k]]$itvL=min(fsW[[k]]$domain);
					alphaCutW[[k]]$itvR=max(fsW[[k]]$domain);				
				} else 
					{
					  #alphaCutA[[k]] = alphaCutsGeneralMF(A[[k]], alpha[i]);	
					  alphaCutA[[k]] = alphaCutsGeneralMF(A[[k]], alpha[i]);
					  
						if(identical(fsW[[k]]$type,'singletonMF'))			
						{											
							alphaCutW[[k]] = alphaCutsGeneralMF(fsW[[k]], alpha[i]);
						} else if(identical(fsW[[k]]$type,'triMF'))
						{								
							alphaCutW[[k]] = alphaCutsTrimf(fsW[[k]]$para, alpha[i]);
						} else
						{
							alphaCutW[[k]] = alphaCutsGeneralMF(fsW[[k]], alpha[i]);
						}
					}
			} # end "for(k in 1:numObj)"
						
					##############	Perform alpha-cuts OWA  	###########
					
					#alphaCutAgg[[i]]=itvOWA(alphaCutW,alphaCutA);
	        #print("alphacuta")
	        #print(alphaCutA)
	        #print("fsw")
	        #print(fsW)
					alphaCutAgg[[i]] = itvOWA_withNullSets(alphaCutW, alphaCutA);	
				
				  alphaCutAgg[[i]]$alpha = alpha[i];
	 }	## end alphaCutAgg

	############################################################################
	###
	### Step 4: To calculate the grades of membership degrees by alpha-cuts
	###
	############################################################################

	aggResult = memDegByAlphaCuts_withNullCheck(domainX, alphaCutAgg);
	aggResult
}