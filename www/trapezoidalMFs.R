#
#Function to compute the trapezoidal MFs
#
#Inputs: 
#			par$lp			:	left ending point in the domain 
#			par$mx1			:	1st maximal point in the domain
#			par$mx2			:	2nd maximal point in the domain
#           par$rp		    : 	right ending point in the domain # lp < mx1 < mx2 < rp
#			par$mxD		    : 	maximal MF degree (default=1) 
#           domainX       	: 	the N input sample values on one dimension space
#
# Outputs:   
#           itvL        	: 	the lower MF values
#           itvR        	: 	the upper MF values
#
#Author:    Shang-Ming Zhou, De Montfort University, 19/10/2007
#
#

trapezoidalMFs <- function (domainX, par)
{
	
	lp=par[1];		#par$lp;
	mx1=par[2];		#par$mx1;
	mx2=par[3];		#par$mx2;
	rp=par[4];		#par$rp;
	mxD=par[5];		#par$mxD;
	
	epu=0.00000001;
	if(mx1==lp){
   		tmpFS1=pmin(pmax(mxD*(domainX-lp)/epu,0),mxD);	
	} else {
	   tmpFS1=pmin(pmax(mxD*(domainX-lp)/(mx1-lp),0),mxD);
	}

	if(mx2==rp){
   		tmpFS2=pmin(pmax(mxD*(domainX-rp)/epu,0),mxD);		
	} else {
   		tmpFS2=pmin(pmax(mxD*(domainX-rp)/(mx2-rp),0),mxD);
   }
   
   FS=pmin(tmpFS1,tmpFS2);    
   FS
       
   #return(list(itvL=lowerMD,itvR=upperMDnew,domain=domainX));
}