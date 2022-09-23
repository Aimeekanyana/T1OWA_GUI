
#	after aggregating, the initial result MF usually possesses too many very near points with different MF degrees. 
#	This function is to refine it further.
#
#	Inputs:
#			mfValueIni	:	1*n MF values of the resulted FS
#			domainIni	:	1*n domain of the resulted FS
#			precision	:	the precision expected
#
#	Outputs:
#			mfValue		:	MF values of the resulted FS after refining
#			domain		:	domain of the resulted FS after refining

#
#	Shang-Ming Zhou, De Montfort University, 23/11/2006
#	version	: 1
#
#

postRefiningMF <- function(domainIni,mfValueIni,precision){

	n=length(mfValueIni);
	
	mfValue=vector("logical",1);
	domain=vector("logical",1);
		
	ind=order(domainIni);
	domainIni=domainIni[ind];
	mfValueIni=mfValueIni[ind];
	
	i=1;
	k=1;
	sn=1:length(domainIni);
	
	#while(k<=(n-1))
	while(k<=n)
	{			
		 ind=(domainIni >= (domainIni[k]-precision) & domainIni < (domainIni[k]+precision));
		#ind=(domainIni >= (domainIni[k]-precision));
		j=length(sn[ind]);
		lastK=sn[ind][j];
		
		domain[i]=domainIni[k];
		mfValue[i]=max(mfValueIni[ind]);
		
		k=lastK+1;
		i <- i+1;		
	}
	
	#mfValue[n]=mfValueIni[n];
	#domain[n]=domainIni[n];
	
	return(list(mfValue=mfValue,domain=domain))
}