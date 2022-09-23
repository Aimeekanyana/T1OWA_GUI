
# source("main_aggrStationaryFS_GeneralOWA_withAlphaCuts3b_Diabetes_TimeCosts_readline.r")
# @Shang-Ming Zhou, 27 Nov 2019


rm(list=ls())
graphics.off()

### set up work directory
	rootDir="C:\Users\aimee\OneDrive\RShinyApp\srcR\main_T1OWA_AlphaCuts_CaseStudyDiabetes.r"
	
	srcDir=paste0(rootDir,"srcR") 
	dataDir=paste0(rootDir,"Data") 
	resultDir=paste0(rootDir,"Results") 
	funcDir=paste0(rootDir,"Func") 

setwd(srcDir)

## Include functions
#source("fuzzySetWeights.R")
source("fuzzySetWeights_likeOpDomainLen.r")
source("fuzzySetWeightsByQuantifier.R")

source("alphaCutsGeneralMF.R")
source("alphaCutsTrimf.R")
source("itvOWA_withNullSets.R")
source("memDegByAlphaCuts_withNullCheck.R")
source("defuzzCOG.R")

source("gaussmf.R")
source("trimf2.R")
source("trapezoidalMFs.R")
source("genTrapezoidalMFs.R")

source("maxMembershipPrinciple2.R")

############ Problem of case study 

defaultXL=101 # number of discrete points in aggregated fuzzy set (see  (resultDir,"\\mamdaniOutputMFs10Runs_4thTime_BasedOnGA.txt"))
defaultWL =30; # number of sampling points in domain of weight fuzzzy set for meet-like and join-like operators; default =30
numObj=10;  # number of fuzzy sets to be aggregated

setting <- readline(prompt="Do you want to use default setting of the domains - 'yes','no':")

if(setting=="yes"){
	numX=defaultXL;
	domainW_Lnth=defaultWL 
	numObj=10;
}

if(setting=="no"){
	numX <- readline(prompt="new number of sampling points on domain of fuzzy set (default=101):")
	numX <- as.integer(numX)
	
	domainW_Lnth <- readline(prompt="new number of sampling points on domain of linguistic weight (default=30):")
	domainW_Lnth <- as.integer(domainW_Lnth)	
	numObj<- as.integer((readline(prompt="new number of aggregated fuzzy sets (default=10):")))	
}

# Set up the cases

cases=768; #1310;

#domainX=seq(0,1,length=101); default
domainX=seq(0,1,length=numX);  #5

diaFile=paste0(dataDir,"\\pima-indians-diabetes-imputed-with-names.csv")
diabetesImputed=read.csv(file=diaFile, header=TRUE, sep=",")
dim(diabetesImputed)
#[1] 768   9
diabetesImputed[1:3,]
#  npreg plaGlu bloodP tricepSkin insulin  BMI diaPed age outcome
#1     6    148     72       35.0     148 33.6  0.627  50       1
#2     1     85     66       29.0      85 26.6  0.351  31       0
#3     8    183     64       23.3     183 23.3  0.672  32       1

diaLabels=diabetesImputed[,"outcome"]

#chemoLabels=scan("C:\\Zhou1\\Codes\\CodesAtDMU\\myCodes\\T1OWA_NonStationaryFS\\data\\chemoLabels1310InCol.dat",n=1310)
#chemoLabels=scan("E:\\Zhou1\\Codes\\CodesAtDMU\\myCodes\\T1OWA_NonStationaryFS\\data\\chemoLabels1310InCol.dat",n=1310)

######################################################################
### Step 1: Set up the weight fuzzy sets
#####################################################################

	W=vector("list",numObj);
	WmfParams=vector("list",numObj);

	fsWeights=vector("list",2);

	#quantifierType="noQuantifier" #"quantifier"; #
	
	quantifierType <- readline(prompt="Do you want to use quantifier - 1='yes'; 0='no':")
	
	#if (quantifierType=="quantifier"){
	if (quantifierType==1){
		#partitionType="t2most" # type-2 quantifier "most"
		#specificWty=partitionType;
		#fsWeights=fuzzySetWeightsByQuantifier(numObj,partitionType);
	  
		fsWeights=fuzzySetWeightsByQuantifier2(numObj);
	}

	#if (quantifierType=="noQuantifier"){
	if (quantifierType==0){

			#partitionType="trimf_unEqualPartition" #"trimf_equalPartition" #"crisp" #
			partitionX="equal"
			
			partitionType <- readline(prompt="Choose partition type of linguistic weights - 1= 'unEqualPartition';2='equalPartition'; 3='crisp':")

			#if(partitionType=="trimf_unEqualPartition")# manual design
			if(partitionType=="1")# manual design
			   {
			     #specificWty="MeetLike"#"JointLike";#"AvgLike";#
				  specificWty <- readline(prompt="Choose type of T1OWA operator - 1='MeetLike'; 2='JointLike'; 3='AvgLike':")
			  }

			#if(partitionType=="trimf_equalPartition")	{
			if(partitionType=="2")	{			
			  	#specificWty="left2right"#"right2left" #  ###################################???????????????
				  specificWty <- readline(prompt="Choose type of T1OWA operator - 1= 'left2right'; 2= 'right2left:")				
			  }

			#if(partitionType=="crisp")# sigleton fuzzy sets
			if(partitionType=="3")# sigleton fuzzy sets			
			   {
				  #specificWty="singletonMeet" #"singletonJoin" #"singletonAvg";#
				  specificWty <- readline(prompt="Choose type of T1OWA operator - 1= 'singletonMeet'; 2='singletonJoin'; 3='singletonAvg':")				  
			  }

			source("trimf.R")
			#########################plot linguistic weights######################
			#fsWeights=fuzzySetWeights(numObj,partitionType,specificWty,partitionX);
			fsWeights=fuzzySetWeights_likeOpDomainLen(numObj,partitionType,specificWty,partitionX,domainW_Lnth);
	 }

	W=fsWeights$W;
	WmfParams= fsWeights$WmfParams;	
	
cat("\nNumber of sampling points in the domain of fuzzy sets to be aggregated by T1OWA:",numX,"\n" )
cat("\nNumber of sampling points in the domain of fuzzy weights for T1OWA:", domainW_Lnth, "\n")
cat("\nnumber of fuzzy sets to be aggregated:",numObj, "\n")
cat("type of weight :",specificWty,"\n")

#################################################################################
# Step2:  Set up the classes  expressed by fuzzy sets
#################################################################################

diabFSs=vector("list",2);

diabFSs[[1]]$para=c(0,0,1.0);
diabFSs[[1]]$domain=domainX;
diabFSs[[1]]$label=0;                          # negative class
diabFSs[[1]]$type="trimf";

diabFSs[[2]]$para=c(0,1,1.0);
diabFSs[[2]]$domain=domainX;
diabFSs[[2]]$label=1;
diabFSs[[2]]$type="trimf";                  # positive class

##################################
### Structure of aggregated fuzzy sets
############################################
#			A[[i]]		:	Fuzzy set	-- 	A=vector("list",1);
#							A$para		---	parameters of A;
#							A$domain	--- domain X;
#							A$label		--- class label that A is representing
#							A$type		---	type of membership functions: "gauss", "trimf", "genTrapesoid"

# read the data

parFS=vector("list",numObj);
fs=vector("list",numObj);
A=vector("list",numObj);

diabCaseByCase=vector("list",cases);
aggResultFuzzSetsInCase=vector("list",cases);
aggResultDefuzzInCase=vector("numeric",cases);

alpha=seq(0,1,length=100);
numAlp=length(alpha);

alphaCutA=vector("list",numObj);
alphaCutW=vector("list",numObj);
alphaCutAgg=vector("list",numAlp);

plotFS=0;#0;#

#colorID=colors();
#rnd=sample(1:length(colorID));
#col=colorID[rnd[t]];

colorID=c("red","green","blue","cyan","purple","violetred1","black","pink","orange","magenta");

timeStart=Sys.time()
print(timeStart)

############################

for(s in 1:cases){  ### number of patients 

	print(s)
	if(plotFS==1){
		x11();
		plot(domainX, seq(0,1,length=length(domainX)), xlab="X",ylab="Grade of membership degree",font.lab=1,type ="n")
	}

	for (t in 1:numObj){
	 	skipValue=(s-1)*numObj+(t-1);

##		A[[t]]$mfValue=scan("C:\\Zhou1\\Codes\\CodesAtDMU\\myCodes\\T1OWA_NonStationaryFS\\data\\breastCancerNonSFs_1stTime.dat",n=101,skip=skipValue)

		#A[[t]]$mfValue=scan("E:\\Zhou1\\Codes\\CodesAtDMU\\myCodes\\T1OWA_NonStationaryFS\\data\\breastCancerNonSFs_1stTime.dat",n=101,skip=skipValue)
		#A[[t]]$mfValue=scan(paste0(resultDir,"\\mamdaniOutputMFs10Runs_1stTime.txt"),n=101,skip=skipValue)
		#A[[t]]$mfValue=scan(paste0(resultDir,"\\mamdaniOutputMFs10Runs_2ndTime.txt"),n=101,skip=skipValue)
		#A[[t]]$mfValue=scan(paste0(resultDir,"\\mamdaniOutputMFs10Runs_3rdTime.txt"),n=101,skip=skipValue)
		A[[t]]$mfValue=scan(paste0(resultDir,"\\mamdaniOutputMFs10Runs_4thTime_BasedOnGA.txt"),n=101,skip=skipValue)	
		
	 	A[[t]]$domain=domainX;

	 	if((plotFS==1)&(s<=15)){
	 		lines(domainX,A[[t]]$mfValue,lwd=2,col=colorID[t])
		}
	 }

	 diabCaseByCase[[s]]$FS=A;
	
	 
###############################################################################
### Step 3: To perform T1OWA aggregation
############################################################################### 

		for(i in 1:numAlp){
			# To get the alpha-cuts of fuzzy sets
				for(k in 1:numObj)
					{
						if(alpha[i]==0){
								alphaCutA[[k]]$itvL=min(domainX);
								alphaCutA[[k]]$itvR=max(domainX);
								alphaCutW[[k]]$itvL=min(W[[k]]$domain);
								alphaCutW[[k]]$itvR=max(W[[k]]$domain);
							} else {
								alphaCutA[[k]]=alphaCutsGeneralMF(A[[k]],alpha[i]);
								if(partitionType==3){
									alphaCutW[[k]]=alphaCutsGeneralMF(W[[k]],alpha[i]);
								} else {
											alphaCutW[[k]]=alphaCutsTrimf(WmfParams[[k]],alpha[i]);
										}
							}
					 }

				############################	Perform alpha-cuts OWA  	################################
				#alphaCutAgg[[i]]=itvOWA(alphaCutW,alphaCutA);
				alphaCutAgg[[i]]=itvOWA_withNullSets(alphaCutW,alphaCutA);

			alphaCutAgg[[i]]$alpha=alpha[i];
	}   # end-aggregation

	# To calculate the grades of membership degrees by alpha-cuts
	#aggResult=memDegByAlphaCuts(domainX,alphaCutAgg);

	aggResultFuzzSetsInCase[[s]]=memDegByAlphaCuts_withNullCheck(domainX,alphaCutAgg);

	aggResultDefuzzInCase[s]=defuzzCOG(aggResultFuzzSetsInCase[[s]]);

} # end : for(s in 1:cases)
#######################################################################

###################################################################
# Step 4: To perform the classification according to maximal membership principle
#############################################################################

#diabLabelsClassified=maxMembershipPrinciple2(aggResultDefuzzInCase, diabFSs);

for (k in 1:length(aggResultDefuzzInCase)){
	ind0=which(aggResultDefuzzInCase<0.5)
	ind1=which(aggResultDefuzzInCase>=0.5)
	
	diabLabelsClassified=aggResultDefuzzInCase
	diabLabelsClassified[ind0]=0
	diabLabelsClassified[ind1]=1
}

timeEnd=Sys.time()

cat("Time Start :")
print(timeStart)

cat("\nTime End :")
print(timeEnd)

timeUsed=timeEnd-timeStart
cat("\nTime used (total 768 samples) :")
print(timeUsed)

cat("\nNumber of sampling points in the domain of fuzzy sets to be aggregated by T1OWA:" )
cat(numX)

cat("\nNumber of sampling points in the domain of fuzzy weights for T1OWA:")
cat(domainW_Lnth)

cat("\nnumber of fuzzy sets to be aggregated:")
cat(numObj,"\n")

cat("type of T1OWA :")
cat(specificWty,"\n")	
	
stop("Finished by Direct Approach !")
##############################################################################


# To calcualte the performance metrics

#source("confusion2X2TableStatsFunc.r")
#source("confusion2X2TableStatsFunc2.r")
source("confusion2X2TableStatsFunc3.r")

#diabPerfList=confusion2X2TableStatsFunc(diabLabelsClassified,diaLabels,positiveLabel=1,negativeLabel=0)
#diabPerfList=confusion2X2TableStatsFunc2(diabLabelsClassified,diaLabels,positiveLabel=1,negativeLabel=0)
diabPerfList=confusion2X2TableStatsFunc3(diabLabelsClassified,diaLabels)

print(diabPerfList)
#### 4th time
#$confusionTable
#               trueLabels
#predictedLabels   0   1
#              0 413 100
#              1  87 168#
#
#$metrics
#      acc      sens      spec       ppv       npv        f1       bcr 
#0.7565104 0.6268657 0.8260000 0.6588235 0.8050682 0.6424474 0.7264328 
#
### 3rd time:
#$confusionTable
#               trueLabels
#predictedLabels   0   1
#              0 390  85
#              1 110 183
#$metrics
#      acc      sens      spec       ppv       npv        f1       bcr 
#0.7460938 0.6828358 0.7800000 0.6245734 0.8210526 0.6524064 0.7314179 

### 2nd time:
#$confusionTable
#               trueLabels
#predictedLabels   0   1
#             0 402  92
#              1  98 176
#
#$metrics
#      acc      sens      spec       ppv       npv        f1       bcr 
#0.7526042 0.6567164 0.8040000 0.6423358 0.8137652 0.6494465 0.7303582

### 1st time:
#$confusionTable
#               trueLabels
#predictedLabels   0   1
#              0 401  93
#              1  99 175
#
#$metrics
#      acc      sens      spec       ppv       npv        f1       bcr 
#0.7500000 0.6529851 0.8020000 0.6386861 0.8117409 0.6457565 0.7274925 


#classificationRateByNFS=1-sum(abs(diabLabelsClassified-diaLabels))\\(length(diaLabels));

#library(heuristica)
#confusion_matrix_3x3_ttb <- confusionMatrixFor_Neg1_0_1(diaLabels, diabLabelsClassified)
#confusion_matrix_3x3_ttb
#       predictions
#correct  -1   0   1
#     -1   0   0   0
#     0    0 422  78
#     1    0 105 163

#confusion_matrix_ttb <- collapseConfusionMatrix3x3To2x2(confusion_matrix_3x3_ttb)
#confusion_matrix_ttb
#       predictions
#correct    -1     1
#     -1 105.5 144.5
#     1  158.0 360.0


# To plot aggResultFuzzSetsInCase

savePic=0
plotAggFS=0;

mfValuesAll=read.csv(paste0(resultDir,"\\mamdaniOutputMFs10Runs_1stTime.csv"))

st=360
en=369

exampleMFs=mfValuesAll[st:en,];

if(plotAggFS==1){
	
	s=37  # 361:370 #  611:620 91:100 Examples of variations of 10 MFs in mamdaniOutputMFs10Runs_1stTime.csv
	
	for (t in 1:10){ 
		skipValue=(s-1)*numObj+(t-1);
		mfValue=scan(paste0(resultDir,"\\mamdaniOutputMFs10Runs_1stTime.txt"),n=101,skip=skipValue)
		
		x11()
		plot(domainX, seq(0,1,length=length(domainX)), xlab="",ylab="Grade of membership degree",font.lab=1,type ="n")
		lines(domainX,mfValue,lwd=2)
		
		
		#if(savePic==1){
		#	tiff(file = paste0(resultDir,"\\mamdaniNonStationaryOutputMFs_1stTime_",toString(s),"thSample_",toString(t),"thMF.tiff"),
		#	width = 10, height = 10, units = 'in',res=300,compression="lzw")
		# 		 
		#	plot(domainX, seq(0,1,length=length(domainX)), xlab="",ylab="Grade of membership degree",font.lab=1,type ="n")
		#	lines(domainX,mfValue,lwd=2)
		#	dev.off()
		#}
	}
	
	#t=s/10; # 
	x11();
	plot(domainX, seq(0,1,length=length(domainX)), xlab="",ylab="Grade of membership degree",font.lab=1,type ="n")
	lines(aggResultFuzzSetsInCase[[s]]$domain,aggResultFuzzSetsInCase[[s]]$mfValue,lwd=2)
	
		if(savePic==1){
			tiff(file = paste0(resultDir,"\\mamdaniNonStationaryOutputMFs_1stTime_",toString(s),"thSample_", "aggregatedByJointLikeT1OWA.tiff"),
			 width = 10, height = 10, units = 'in',res=300,compression="lzw")
			 plot(domainX, seq(0,1,length=length(domainX)), xlab="",ylab="Grade of membership degree",font.lab=1,type ="n")
			lines(aggResultFuzzSetsInCase[[s]]$domain,aggResultFuzzSetsInCase[[s]]$mfValue,lwd=2)
			dev.off()
		}
		
	## plot in one figure

	mamdaniOutputMFs10Runs <- read.delim(paste0(resultDir,"\\mamdaniOutputMFs10Runs_1stTime.txt"))
	dim(mamdaniOutputMFs10Runs)
	#[1] 7679  101
	
	st=(s-1)*numObj
	en=(s-1)*numObj+9
	
	examplesOutMFs10=mamdaniOutputMFs10Runs[st:en,]
	
	x11()
	par(mfrow=c(2,5))
	for (t in 1:10){ 
		plot(domainX, seq(0,1,length=length(domainX)), xlab="",ylab="Grade of membership degree",font.lab=1,type ="n")
		lines(domainX,examplesOutMFs10[t,],lwd=2)
	}
	
	if(savePic==1){
		tiff(file = paste0(resultDir,"\\mamdaniNonStationaryOutputMFs10All_1stTime.tiff"),
			 width = 10, height = 10, units = 'in',res=300,compression="lzw")

		par(mfrow=c(2,5))
		for (t in 1:10){ 
			plot(domainX, seq(0,1,length=length(domainX)), xlab="",ylab="Grade of membership degree",font.lab=1,type ="n")
			lines(domainX,examplesOutMFs10[t,],lwd=2)
		}
		dev.off()
	}	
}

#save(x, y, file = "xy.Rdata")
#unlink("xy.Rdata")

#save.image()
#save.image(file = paste0(resultDir,"\\diabetesResultByT1OWA_1stTime_jointLike.RData"))
#save.image(file = paste0(resultDir,"\\diabetesResultByT1OWA_2ndTime_jointLike.RData"))
#save.image(file = paste0(resultDir,"\\diabetesResultByT1OWA_3rdTime_jointLike.RData"))

#save.image(file = paste0(resultDir,"\\diabetesResultByT1OWA_4thTime_GA_jointLike.RData"))

#load("E:\\Codes\\!myCodes\\T1OWA_NonStationaryFS\\results\\chemoResultByT1OWA_JLike.RData")

