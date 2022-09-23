  #
#Function to compute the general trapezoidal MFs: two steps on each side of the MF
#
#Inputs: 
#			par$lp1			:	1st left point in the domain 
#			par$lp2			:	2nd left point in the domain: 
#			par$lp3			:	3rd left point in the domain
#			par$lp4			:	4th left point in the domain
 
#			par$rp1			:	1st right point in the domain 
#			par$rp2			:	2nd right point in the domain
#			par$rp3			:	3rd right point in the domain
#			par$rp4			:	4th right point in the domain

#           par$mxD1		: 	sub-maximal MF degree of left part of x: lp1<x<lp3
#			par$mxD2		: 	sub-maximal MF degree of right part of x: rp2<x<rp4
#			par$mxD3		: 	maximal MF degree of right part of x: lp4<x<rp1
#
#           domainX       	: 	the N input sample values on one dimension space
#
# Outputs:   
#			FS				: 	memebership degrees
#
#Author:    Shang-Ming Zhou, De Montfort University, 10/11/2008
#
#	Usage: 
#			parFS[[1]]=c(lp1,lp2,lp3,lp4,rp1,rp2,rp3,rp4, mxD1,mxD2,mxD3);
#			fs[[1]]=genTrapezoidalMFs(domainX,parFS[[1]]);
#
#			Normal trapesoidal MF:
#				parFS[[1]]=c(lp1,lp1,lp1,lp4,rp1,rp1,rp1,rp4, NA,NA,mxD3);
#
#			Special trapesoidal MFs:
#				parFS[[1]]=c(lp1,lp1,lp3,lp4,rp1,rp1,rp3,rp4, mxD1,mxD2,mxD3);
#				parFS[[1]]=c(lp1,lp1,lp1,lp4,rp1,rp1,rp3,rp4, NA,mxD2,mxD3);
#				parFS[[1]]=c(lp1,lp1,lp3,lp3,rp1,rp1,rp1,rp3, mxD1,NA,mxD3);
#				parFS[[1]]=c(lp1,lp1,lp3,lp3,rp1,rp1,rp3,rp3, mxD1,mxD2,mxD3);
#
#
genTrapezoidalMFs <- function (domainX, par)
{
	
	lp1=par[1];		#par$lp;
	lp2=par[2];		#par$lp;
	lp3=par[3];		#par$lp;
	lp4=par[4];		#par$lp;
	
	rp1=par[5];		#par$lp;
	rp2=par[6];		#par$lp;
	rp3=par[7];		#par$lp;
	rp4=par[8];		#par$lp;
		
	mxD1=par[9];		#par$mxD1;
	mxD2=par[10];	#par$mxD2;
	mxD3=par[11];	#par$mxD3;	

	epu=0.000000001;
	
	# to compute the left part		
	
	if(lp2==lp3)
	{
		lpFS=pmin(pmax(mxD3*(domainX-lp1)/(lp4-lp1),0),mxD3);		
	}
	
	if(lp2!=lp3)	
	{
		if(lp3!=lp4)
			{
				tmp1=(mxD3-mxD1)*(lp1-lp3)/(lp4-lp3)+mxD1;
			}
		if(lp3==lp4)
			{
				tmp1=-10000;#(mxD3-mxD1)*(lp1-lp3)/epu+mxD1;
			}
   
  		 if(tmp1<=0)
  		 {
  		 	if(lp1!=lp2)
  		 	{
	   			lpFS1=pmin(pmax(mxD1*(domainX-lp1)/(lp2-lp1),0),mxD1);  
	   			
		   		if(lp3!=lp4)	   		   
	   			{
	   				lpFS2=pmin(pmax((mxD3-mxD1)*(domainX-lp3)/(lp4-lp3)+mxD1,0),mxD3);   
	   			}
		   		if(lp3==lp4)
		   		{
		   			lpFS2=pmin(pmax((mxD3-mxD1)*(domainX-lp3)/epu+mxD1,0),mxD3); 	
		   		}	   				   			
	   			 
	   		}
	   		if(lp1==lp2)
	   		{
	   			lpFS1=pmin(pmax(mxD1*(domainX-lp1)/epu,0),mxD1);  
		   		if(lp3!=lp4)	   		   
	   			{
	   				lpFS2=pmin(pmax((mxD3-mxD1)*(domainX-lp3)/(lp4-lp3)+mxD1,0),mxD3);   
	   			}
		   		if(lp3==lp4)
		   		{
		   			lpFS2=pmin(pmax((mxD3-mxD1)*(domainX-lp3)/epu+mxD1,0),mxD3); 	
		   		}   			
	   		}	   		

		   lpFS=pmax(lpFS1,lpFS2)
   		 }
    
	   if(tmp1>0)    
	    {
	   		if(lp1!=lp2)
	    	{
	    		lpFS1=pmin(pmax(mxD1*(domainX-lp1)/(lp2-lp1),0),mxD1); 
		   		if(lp3!=lp4)	   		   
	   			{
	   				lpFS2=pmin(pmax((mxD3-mxD1)*(domainX-lp3)/(lp4-lp3)+mxD1,0),mxD3);   
	   			}
		   		if(lp3==lp4)
		   		{
		   			lpFS2=pmin(pmax((mxD3-mxD1)*(domainX-lp3)/epu+mxD1,0),mxD3); 	
		   		}		    		
	   		}
	   		if(lp1==lp2)
	   		 {
	   		 	lpFS1=pmin(pmax(mxD1*(domainX-lp1)/epu,0),mxD1); 
		   		if(lp3!=lp4)	   		   
	   			{
	   				lpFS2=pmin(pmax((mxD3-mxD1)*(domainX-lp3)/(lp4-lp3)+mxD1,0),mxD3);   
	   			}
		   		if(lp3==lp4)
		   		{
		   			lpFS2=pmin(pmax((mxD3-mxD1)*(domainX-lp3)/epu+mxD1,0),mxD3); 	
		   		}	   		 	
	   		 }
	   		
	   		tmpLpFS1=pmax(lpFS1,lpFS2)
	   		
	   		if(lp1!=lp2)
	   			{
	   				tmpLpFS2=pmax(mxD1*(domainX-lp1)/(lp2-lp1),0);
	   			}
	   		if(lp1==lp2)
	   			{
	   				tmpLpFS2=pmax(mxD1*(domainX-lp1)/epu,0);
	   			}	   		
	   		
	   		lpFS=pmin(tmpLpFS1,tmpLpFS2)  
	    }  
	}

	# to compute the right part
	
	if(rp2==rp3)
	{
		rpFS=pmin(pmax(mxD3*(domainX-rp4)/(rp1-rp4),0),mxD3);
	
	}
	
	if(rp2!=rp3)
	{
		if(rp1!=rp2)
	   	{
	   		tmp2=(mxD2-mxD3)*(rp4-rp1)/(rp2-rp1)+mxD3;
   		}
   		
		if(rp1==rp2)
	   	{
	   		tmp2=-1000; #(mxD2-mxD3)*(rp4-rp1)/epu+mxD3;	   		
   		}   		
   		#print(tmp2)
   		
	   if(tmp2<=0)
	   {    
	   		if(rp1!=rp2)
	   		{
	   			rpFS1=pmin(pmax((mxD2-mxD3)*(domainX-rp1)/(rp2-rp1)+mxD3,0),mxD3); 
	   			   
		   		if(rp3!=rp4)
		   		{
		   			rpFS2=pmin(pmax(mxD2*(domainX-rp4)/(rp3-rp4),0),mxD2); 
		   		}	   			
		   		if(rp3==rp4)
		   		{
		   			rpFS2=pmin(pmax(mxD2*(rp4-domainX)/epu,0),mxD2); 
		   		}		   			
	   		}
	   		
	   		if(rp1==rp2)
	   		{	
	   			rpFS1=pmin(pmax((mxD2-mxD3)*(domainX-rp1)/epu+mxD3,0),mxD3); 
	   			  
		   		if(rp3!=rp4)
		   		{
		   			rpFS2=pmin(pmax(mxD2*(domainX-rp4)/(rp3-rp4),0),mxD2); 
		   		}	   			
		   		if(rp3==rp4)
		   		{
		   			rpFS2=pmin(pmax(mxD2*(rp4-domainX)/epu,0),mxD2); 
		   		} 		
	   		}
  		
	   		rpFS=pmax(rpFS1,rpFS2)
	   }   
	   
	   if(tmp2>0)
	   {
	   		if(rp1!=rp2)
	      	{	
	      		rpFS1=pmin(pmax((mxD2-mxD3)*(domainX-rp1)/(rp2-rp1)+mxD3,0),mxD3);  
		   		if(rp3!=rp4)	      	 
		   		{
		   			rpFS2=pmin(pmax(mxD2*(domainX-rp4)/(rp3-rp4),0),mxD2); 
		   		}	
		   		if(rp3==rp4)	      	 
		   		{
		   			rpFS2=pmin(pmax(mxD2*(rp4-domainX)/epu,0),mxD2); 
		   		}			   		      		 
	      	} 
	   		if(rp1==rp2)
	   		{	
	   			rpFS1=pmin(pmax((mxD2-mxD3)*(domainX-rp1)/epu+mxD3,0),mxD3); 
		   		if(rp3!=rp4)	      	 
		   		{
		   			rpFS2=pmin(pmax(mxD2*(domainX-rp4)/(rp3-rp4),0),mxD2); 
		   		}	
		   		if(rp3==rp4)	      	 
		   		{
		   			rpFS2=pmin(pmax(mxD2*(rp4-domainX)/epu,0),mxD2); 
		   		}	   			
	   		}
	   		
	   		tmpRpFS1=pmax(rpFS1,rpFS2)
	   		
	   		if(rp3!=rp4)	
	   		{
	   			tmpRpFS2=pmax(mxD2*(domainX-rp4)/(rp3-rp4),0);
	   		}
	   		if(rp3==rp4)	
	   		{
	   			tmpRpFS2=pmax(mxD2*(rp4-domainX)/epu,0);
	   		}	   		

	   		rpFS=pmin(tmpRpFS1,tmpRpFS2)	  		
	   }	
	}

   FS=pmin(lpFS,rpFS);      
   FS
       
   #return(list(itvL=lowerMD,itvR=upperMDnew,domain=domainX));
}