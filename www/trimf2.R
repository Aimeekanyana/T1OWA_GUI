# 
#Function to compute the triangular MFs
#
#Inputs: 
#			par[1]			:	left ending point in the domain 
#			par[2]			:	the maximal point in the domain with maximal MD
#           par[3]		    : 	right ending point in the domain # par[1] < par[2] < par[3]
#			par[4]		    : 	maximal MF degree (default=1) 
#           domainX       	: 	the N input sample values on one dimension space
#
# Outputs:   
#          MF values
#           
#
#Author:    Shang-Ming Zhou, De Montfort University, 07/11/2008
#
trimf2 <- function(x, mfParams) {
  a <- mfParams[1]
  b <- mfParams[2]
  c <- mfParams[3]
  
  mxD= mfParams[4]
  
 # y= pmax(pmin( (x-a)/(b-a), (c-x)/(c-b) ), 0)
  FS1=pmin(pmax((x-a)/(b-a),0),mxD)
  FS2=pmin(pmax((c-x)/(c-b),0),mxD)
  
  y=pmin(FS1,FS2);
  
  #y[is.na(y)]= 1; 
  y
}