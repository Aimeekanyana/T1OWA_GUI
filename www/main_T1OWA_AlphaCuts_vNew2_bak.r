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
source("t1owaAC.r")
source("fuzzySetWeightsByTrimf.R")
source("genWeightChoice.r")
source("fuzzySetWeightsGen.r")

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
###				A[[i]]$mfValue  -- membership values of A[[i]]
###				A[[i]]$domain  	-- domain of A[[i]]
###				A[[i]]$type  	-- type of membership function of A[[i]]:
###									"gaussMF", "genTrapMF", "trapMF", "triMF", ###									"singtletonMF"
###				A[[i]]$para  	-- parameters defining membership function of A[[i]]
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
					A[[i]]$type=c("gaussMF")
					A[[i]]$para=parFS[[i]]
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
					A[[i]]$type=c("genTrapMF")
					A[[i]]$para=parFS[[i]]					
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
					A[[i]]$type=c("trapMF")
					A[[i]]$para=parFS[[i]]		
				}
		}

		if(fsSty=="triangular")
		{
			parFS[[1]]=c(1.25,1.65,1.9, 0.9);
			
			for(i in 1:numObj)
				{
					A[[i]]$mfValue=trimf2(domainX,parFS[[i]]);
					A[[i]]$domain=domainX;	
					A[[i]]$type=c("triMF")
					A[[i]]$para=parFS[[i]]	
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
###				W[[i]]$mfValue  -- membership values of W[[i]]
###				W[[i]]$domain  	-- domain of W[[i]]
###				W[[i]]$type  	-- type of membership function of W[[i]]
###									"gaussMF", "genTrapMF", "trapMF", "triMF", ###									"singtletonMF"
###				W[[i]]$para  	-- parameters defining membership function of W[[i]]
###
#######################################################################

## guidelines for selecting types of weights
#
#	swOpt		 - 		1 = 'Meet'/'singletonMeet'/'fuzzfiedMinimum'
#						2 = 'Join'/'singletonJoin'/'fuzzifiedMaximum'
#						3 = 'Mean'/'singletonAvg'/'fuzzifiedAverage'
#						4 =	Not use crisp weights
#
#	For swOpt	==4:
#	 lwCat		 -		1 =	'MeetLike'
#						2 =	'JoinLike'
#						3 =	'AvgLike'
#
#		specificWty	- 1)For lwCat==1 # 'MeetLike'
#						1 = 'unEqualDomainPartition'
#						2 = 'equalDomainPartition'	# 'left2right' 
#						3 = 'type-2 quantifier "most"'
#
#		specificWty	- 2)For lwCat==2 # 'JoinLike'
#						1 = 'unEqualDomainPartition'
#						2 = 'equalDomainPartition' # 'right2left' 
#
#		specificWty	- 3)For lwCat==3 #'AvgLike'
#						1 = 'unEqualDomainPartition'
#
#############################################################################

	myW=genWeightChoice()
	#$swOpt   			-- 	singleton weight option
	#$lwOpt
	#$lwOpt$lwCat 		-- 	linguistic weight category for $swOpt==4
	#$lwOpt$specificWt	--	specific weight chosen for a given linguistic weight category (lwCat) 

	W=fuzzySetWeightsGen(numObj,myW$swOpt,myW$lwOpt)
	
	# W[[k]] - a membership function	
	#		W[[k]]$domain
	#		W[[k]]$mfValue
	#		W[[k]]$type
	# 		W[[k]]$para :	parameters for triangular fuzzy set weights
	#
 
####################################################################
###
###	Step 3:	To perform T1OWA aggregtation
###
######################################################################
print("The starting time and ending time :")
timeSt=Sys.time();
print(timeSt)

aggResult=t1owaAC(W,A)

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