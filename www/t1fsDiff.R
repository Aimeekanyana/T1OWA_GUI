
#This function is to perform the difference operation on two type-1 fuzzy sets 
#
#	Inputs:
#
#			fs1			:	the 1st type-1 fuzzy set
#
#							fs1$mfValue	--	MF values of the 1st FS
#							fs1$domain	--	the domain of the 1st FS
#
#			fs2			:	the 2nd type-1 fuzzy set
#
#							fs2$mfValue	--	MF values of the 2nd FS
#							fs2$domain	--	the domain of the 2nd FS
#		
#			dom			:	the whole domain
#
#	Outputs:
#			fs			:	difference of fs1 and fs2
#							fs$mfValue
#							fs$domain-  	the domain of the resulting FS
#
#	Shang-Ming Zhou, De Montfort University, 22/11/2006
#	version	: 2
#
# 	revisions: 1) reduced some variables on requirement about allocating memory, 
#			   2) Simutaneously deal with the points with multiple membership degrees
#			22/11/2006
#
#

t1fsDiff <- function(fs1,fs2,dom,tnorm){
noDom=length(dom);
noX1=length(fs1$domain);
noX2=length(fs2$domain);

mfVal=vector("numeric",noDom);

tol=0.00000001;

for(i in 1:noDom)
	{
		x=dom[i];
		#print(x)
		
		for (t1 in 1:noX1)
		{
		  u=fs1$domain[t1];
		  for (t2 in 1:noX2)
		  {
		    
		    s=fs2$domain[t2];
		    
		    dif=u-s;
		    
		   # if(i==70) # check 0.69
		    #{
		    #print(u)
		    #print(s)
		    #print(u-s)
		    #print(x)		  
		    #print(as.logical((u-s)==x))  
		    #}	        
		    
		    #if((u-s)==x)
		    #if(as.logical((u-s)==x))
		    
		    if(abs(dif-x)<tol)
		    {
				if(tnorm=="min")
				  {tmpMf1=min(fs1$mfValue[t1],fs2$mfValue[t2]);}				    
				if(tnorm=="product")
				  {tmpMf1=prod(fs1$mfValue[t1],fs2$mfValue[t2]);}
				  
				if(x==0.69)
				{print(u)
				 print(s) }
				 
				mfVal[i]=max(mfVal[i],tmpMf1);	 	    
		    }		  
		  }		
		}
		
	}

return(list(mfValue=mfVal,domain=dom));

}