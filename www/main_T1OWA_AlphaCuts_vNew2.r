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
	

#### including functions

source("./www/gaussmf.R")
source("./www/trimf.R")
source("./www/t1owaAC.r")
source("./www/fuzzySetWeightsByTrimf.R")
source("./www/genWeightChoice.r")
source("./www/fuzzySetWeightsGen.r")

source("./www/trimf2.R")
source("./www/postRefiningMF.R")
#source("./www/joinnessOfOWA.R")
#source("./www/alphaCutsTrapezoidMF.R")
source("./www/trapezoidalMFs.R")
source("./www/genTrapezoidalMFs.R")
#source("./www/alphaCutsGenTrapezoidMF.R")



aggregateGen <- function(numObj, swOpt, lwOpt, specificWt, tri, tra, gau){

    LW <- vector("list")
    domainX=seq(0, 5, length=100);
    
    pl <- reactiveValues(i = 1)
    
    if(length(tri$i) > 0){
      print('in1')
      print(length(tri$i))
      for(i in 1:length(tri$i)){
        LW[[pl$i]] <- tri$i[[i]][[1]]
        pl$i <- pl$i + 1
      }
    }
    if(length(tra$i) > 0){
      print('in2')
      for(i in 1:length(tra$i)){
        LW[[pl$i]] <- tra$i[[i]][[1]]
        pl$i <- pl$i + 1
      }
    }
    if(length(gau$i) > 0){
      print("in3")
      for(i in 1:length(gau$i)){
        LW[[pl$i]] <- gau$i[[i]][[1]]
        pl$i <- pl$i + 1
      }
    }
    
    print(LW)
    print("end")
    

    W = fuzzySetWeightsGen(numObj, swOpt, lwOpt, specificWt)
        
    print("The starting time and ending time :")
    timeSt=Sys.time();
    print(timeSt)
    
    aggResult = t1owaAC(numObj, W, LW)
    #domainX = seq(0, 100, length=100);
    
    timeEn=Sys.time();
    print(timeEn)
    
    print("Time used :")
    timeUsed=timeEn-timeSt;
    print(timeUsed)

    
    
    plot(domainX, seq(0, 1, length=length(domainX)), xlab="U", ylab="Grade of membership degree", font.lab=1, type ="n")
    for(i in 1:length(LW)){
          #points(LW[[i]][[j]][[k]]$domain, LW[[i]][[j]][[k]]$mfValue, pch=20)						
          lines(LW[[i]]$domain, LW[[i]]$mfValue, lwd=2)
    }
    
    lines(aggResult$domain, aggResult$mfValue, col="red", lty="dashed", lwd=2)
     
    ### Aggregatation result in a single graph 
    #plot(aggResult$domain, seq(0,1,length=length(aggResult$domain)), xlab="X",ylab="Grade of membership degree", font.lab=1, type="n") 
    #lines(aggResult$domain, aggResult$mfValue, lwd=2)
    
    #joinnessRun=0;#1
    #if(joinnessRun==1)
    #{
    #	### To discretise the linguistic weights in more fine points
    #	# to see "refineJointnessOfOWA.R"
    #	
    #	## To compute the joinnes##########
    #	OWA_jointness=joinnessOfOWA(fsW,"min")
    #
    #	precision=0.02
    #	finalOWA_jointness=postRefiningMF(OWA_jointness$domain, OWA_jointness$mfValue, precision)
    #	
    #	#precision=0.1
    #	#finalOWA_jointness=postRefiningMF2(OWA_jointness$domain,OWA_jointness$mfValue,precision)
    #	
    #	plot(seq(0,1,0.1), seq(0,1,0.1), xlab="v",ylab="Grade of membership degree",font.lab=1,type ="n") 
    #	lines(finalOWA_jointness$domain, finalOWA_jointness$mfValue, lwd=2)
    #	lines(c(0,min(finalOWA_jointness$domain)), c(0,0), lwd=2)
    #	lines(c(max(finalOWA_jointness$domain),1), c(0,0), lwd=2)
    #}
}

