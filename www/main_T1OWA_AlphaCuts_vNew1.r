# Main program to perform T1OWA based on alpha-cuts approach with weights derived from different settings
#
# Author: Shang-Ming Zhou, 1 June 2010
#   
# Ref:
#	S. -M. Zhou, F. Chiclana, R. I. John and J. M. Garibaldi, "Alpha-level aggregation: a practical approach to 
#				type-1 OWA operation for aggregating uncertain information with applications to breast cancer 
#				treatments," IEEE Transactions on Knowledge and Data Engineering, 2010 (forthcoming) 
#	S. -M. Zhou, F. Chiclana, R. I. John and J. M. Garibaldi, "Type-1 OWA operators for aggregating uncertain 
#				information with uncertain weights induced by type-2 linguistic quantifiers," Fuzzy Sets and Systems, 
#				Vol.159, No.24, pp.3281-3296, 2008 
#
rm(list=ls())
graphics.off()

#### set up working directory
	rootDir="C:\\Users\\smzhou\\Dropbox\\MScProject_T1OWA\\SourceT1OWA\\"
	
	srcDir=paste0(rootDir,"srcR") 
	dataDir=paste0(rootDir,"Data") 
	resultDir=paste0(rootDir,"Results") 
	funcDir=paste0(rootDir,"Func") 

setwd(srcDir)

#### including functions

source("gaussmf.R")
source("trimf.R")
source("t1owa_byTriWeights.r")
source("fuzzySetWeightsByTrimf.R")

source("trimf2.R")
source("postRefiningMF.R")
source("joinnessOfOWA.R")
source("alphaCutsTrapezoidMF.R")
source("trapezoidalMFs.R")
source("genTrapezoidalMFs.R")
source("alphaCutsGenTrapezoidMF.R")

#######################################################################
###
### Step 1: Set up the aggregated objects in the LIST format
###
###########################################################################
		numObj=4;#3;#5;

		parFS=vector("list",numObj);
		A=vector("list",numObj);

		domainX=seq(0,5,length=100);

		fsSty="gauss" #"genTrapesoid" #"trapesoid" #"triangular"

		if(fsSty=="gauss")
		{
			#parFS[[1]]=c(1.5,2);#c(1.2,4.5);
			#parFS[[2]]=c(1.1,4);#c(1.1,5.5);
			#parFS[[3]]=c(1.5,6);#c(1.5,7.5);

			#parFS[[1]]=c(0.25,0.65);
			#parFS[[1]]=c(0.52,1.35);
			#parFS[[2]]=c(0.35,2.1);
			#parFS[[3]]=c(0.4,2.8);
			#parFS[[5]]=c(0.35,3.1);
			
			parFS[[1]]=c(0.3,0.85);
			parFS[[2]]=c(0.6,1.85);
			parFS[[3]]=c(0.5,2.8);
			parFS[[4]]=c(0.4,3.8);
			
			for(i in 1:numObj)
				{
					A[[i]]$mfValue=gaussmf(domainX,parFS[[i]]);
					A[[i]]$domain=domainX;	
				}
		}	

		if(fsSty=="genTrapesoid")
		{
			parFS[[1]]=c(0.25,0.25,0.25,1.5,2.0,2.0,2.0,2.4, NA,NA, 0.9);
			parFS[[2]]=c(0.95,0.95,1.6,1.6,2.5,2.5,2.5,3.6, 0.6,NA, 1.0);
			
			for(i in 1:numObj)
				{
					A[[i]]$mfValue=genTrapezoidalMFs(domainX,parFS[[i]]);
					A[[i]]$domain=domainX;				
				}
		}

		if(fsSty=="trapesoid")
		{
			parFS[[1]]=c(0.25,0.75,1.5,1.9,0.9);
			parFS[[2]]=c(1.25,1.65,1.9,2.2,0.8);
			parFS[[3]]=c(1.9,2.9,3.5,4.0,0.4);
			
			
			for(i in 1:numObj)
				{
					A[[i]]$mfValue=trapezoidalMFs(domainX,parFS[[i]]);
					A[[i]]$domain=domainX;	
				}
		}

		if(fsSty=="triangular")
		{
			parFS[[1]]=c(1.25,1.65,1.9, 0.9);
			
			for(i in 1:numObj)
				{
					A[[i]]$mfValue=trimf2(domainX,parFS[[i]]);
					A[[i]]$domain=domainX;				
				}
		}
	
#print(Sys.time())

#plot aggregated objects
	X11();
	plot(domainX, seq(0,1,length=length(domainX)), xlab="X",ylab="Grade of membership degree",font.lab=1,type ="n") 

	for(i in 1:numObj)
		{
			 lines(domainX,A[[i]]$mfValue,lwd=2)
		}
	 
########################################################################
###
### Step 2: Set up the fuzzy set weights in the LIST format
###
#######################################################################

	fsW=vector("list",numObj);
	WmfParams=vector("list",numObj);
	
## guidelines for selecting types of weights
#
#	quantifierType	- 	1 = using type-2 quantifier "most"
#					  	0 = not using quantifier
#
#	For quantifierType==0:
#	partitionType 	- 	1= 'unEqualPartition'
#						2='equalPartition'
#						3='crisp'

#	specificWty		- 1)For partitionType==1
#						specificWty=1: 	'MeetLike'
#						specificWty=2:	'JoinLike'
#						specificWty=3:	'AvgLike'	
#					 2) For partitionType==2		
#						specificWty=1: 	'left2right' 
#						specificWty=2:	'right2left
#					 3) For partitionType==3
#						specificWty=1	:'singletonMeet' 
#						specificWty=2	:'singletonJoin'
#						specificWty=3	:'singletonAvg'
	
	quantifierType <- readline(prompt="Do you want to use quantifier - 1='yes'; 0='no':")
	
	if(quantifierType==1)
		fsWeights=fuzzySetWeightsByQuantifier2(numObj);
		
	if(quantifierType==0)
	{
		partitionType <- readline(prompt="Choose partition type of linguistic weights - 1= 'unEqualPartition';2='equalPartition'; 3='crisp':")	
		specificWty <- readline(prompt="Choose type of T1OWA operator:")
		
		# fuzzy sets to be generated by triangular membership functions
		fsWeights=fuzzySetWeightsByTrimf(numObj,partitionType,specificWty);
	}
		
	fsW=fsWeights$W;
	WmfParams= fsWeights$WmfParams;
	# fsW[[k]] : a membership function	
	#		fsW[[k]]$domain
	#		fsW[[k]]$mfValue
	# WmfParams[[k]] :	parameters for triangular fuzzy set weights
	#
 
####################################################################
###
###	Step 3:	To perform T1OWA aggregtation
###
######################################################################
print("The starting time and ending time :")
timeSt=Sys.time();
print(timeSt)

aggResult=t1owa_byTriWeights(fsW,WmfParams,A)

timeEn=Sys.time();
print(timeEn)

print("Time used :")
timeUsed=timeEn-timeSt;
print(timeUsed)

#####################################################################
###
###	Step 5:	Plot the initial result  
###
#######################################################################

### Aggregatation result with aggregated fuzzy sets 
X11()
plot(domainX, seq(0,1,length=length(domainX)), xlab="X",ylab="Grade of membership degree",font.lab=1,type ="n") 
for(i in 1:numObj)
	{
		 lines(domainX,A[[i]]$mfValue,lwd=2)
 	}
lines(aggResult$domain,aggResult$mfValue,col="red",lty="dashed",lwd=2)

### Aggregatation result in a single graph 
X11()
plot(aggResult$domain, seq(0,1,length=length(aggResult$domain)), xlab="X",ylab="Grade of membership degree",font.lab=1,type ="n") 
lines(aggResult$domain,aggResult$mfValue,lwd=2)

##############################################################################
###
###	Step 6:	To compute the T1OWA joinness
###
#############################################################################

joinnessRun=0;#1
if(joinnessRun==1)
{
	### To discretise the linguistic weights in more fine points
	# to see "refineJointnessOfOWA.R"
	
	## To compute the joinnes##########
	 OWA_jointness=joinnessOfOWA(fsW,"min")
	
	  precision=0.02
	 finalOWA_jointness=postRefiningMF(OWA_jointness$domain,OWA_jointness$mfValue,precision)
	 
	 #precision=0.1
	 #finalOWA_jointness=postRefiningMF2(OWA_jointness$domain,OWA_jointness$mfValue,precision)
	 
	 X11()
	plot(seq(0,1,0.1), seq(0,1,0.1), xlab="v",ylab="Grade of membership degree",font.lab=1,type ="n") 
	lines(finalOWA_jointness$domain,finalOWA_jointness$mfValue,lwd=2)
	lines(c(0,min(finalOWA_jointness$domain)),c(0,0),lwd=2)
	lines(c(max(finalOWA_jointness$domain),1),c(0,0),lwd=2)
}