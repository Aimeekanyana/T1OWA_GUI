
#This function is to calculate the interval values of aggregating the
# interval objects with interval weights 
#
#	Inputs:
#			W[[i]]	: list structure of the ith interval weight
#						W[[i]]$itvL--left value of the interval
#						W[[i]]$itvR--Right value of the interval
#
#						note: creating W by using W=vector("list",n);
#
#			A[[i]]	:	list structure of the ith interval object
#						A[[i]]$itvL--left value of the interval
#						A[[i]]$itvR--Right value of the interval
#	Outputs:
#			$itvL	:	the left end value of the interval result
#			$itvR	:  	the right end value of the interval result
#
#	Shang-Ming Zhou, De Montfort University, 20/11/2006
#
#  To check if the A[[i]] are null sets: 7/11/2008
#

itvOWA_withNullSets <- function(W, A){
  
  
  n=length(W);
  nullFlg=0;
  
  # For null sets A, return the null aggregation
  
  for(i in 1:n)
  {				
    if(is.na(A[[i]]$itvL))
    {
      nullFlg=1;
    }
  }
  
  #print(nullFlg)
  
  if (nullFlg==0)	
  {	
    WL=vector("logical",n);
    WR=vector("logical",n);
    AL=vector("logical",n);
    AR=vector("logical",n);
    
    for(i in 1:n)
    {
      WL[i]<-W[[i]]$itvL;
      WR[i]<-W[[i]]$itvR;				
      AL[i]<-A[[i]]$itvL;
      AR[i]<-A[[i]]$itvR;
    }
    
    # to sort the AL and AR in decreasing order
    t1=order(AL,decreasing=T);
    t2=order(AR,decreasing=T);
    AL=AL[t1];
    AR=AR[t2];
    
    # To calculate the right value of the interval 
    
    #create the auxiliary function
    
    tmpH=WR[1]+sum(WL[2:n]); 
    rhoR=(WR[1]*AR[1]+sum(WL[2:n]*AR[2:n]))/tmpH;
    
    k=2;
    
    while(rhoR<AR[k])
    {
      k <- k+1;
      
      tmpH <- sum(WR[1:(k-1)])+sum(WL[k:n]);
      rhoR <- (sum(WR[1:(k-1)]*AR[1:(k-1)])+sum(WL[k:n]*AR[k:n]))/tmpH;
    }
    
    # To calculate the left value of the interval 
    #create the auxiliary function
    tmpH=WL[1]+sum(WR[2:n]);
    rhoL=(WL[1]*AL[1]+sum(WR[2:n]*AL[2:n]))/tmpH;
    
    k=2;
    
    while(rhoL<AL[k])
    {
      k <- k+1;		
      tmpH <- sum(WL[1:(k-1)])+sum(WR[k:n]);
      rhoL <- (sum(WL[1:(k-1)]*AL[1:(k-1)])+sum(WR[k:n]*AL[k:n]))/tmpH;
    }
    
    # return(list(itvL=rhoL,itvR=rhoR));
  }
  
  # return result
  
  if (nullFlg==1)
  {
    return(list(itvL=NA,itvR=NA));
  }
  if (nullFlg==0)
  {
    return(list(itvL=rhoL,itvR=rhoR));
  }
}