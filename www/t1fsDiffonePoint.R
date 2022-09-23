
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

t1fsDiffonePoint <- function(fs1,fs2,x,tnorm){
#noDom=length(dom);
noX1=length(fs1$domain);
noX2=length(fs2$domain);

#sprintf("No. of total data points FS1 :\n",noX1);

#mfVal=vector("numeric",noDom);
mfVal=vector("numeric",1);
k1=vector("numeric",1);
k2=vector("numeric",1);
tmpMf1=vector("numeric",1);

#for(i in 1:noDom)
#	{
#		x=dom[i];
		for (k1 in 1:noX1)
		{
		  u=fs1$domain[k1];
		  print(u)
		  
		  for (k2 in 1:noX2)
		  {
		    
		    s=fs2$domain[k2];
		    #print(s)
		    
		    dif=u-s;
		    #print(dif)		    
		    #formatC(dif, format="f")
		    
		    print(as.logical(dif==x))
		    
		    #if(dif==x)
		    if(as.logical(dif==x))
		    {
				if(tnorm=="min")
				  {tmpMf1=min(fs1$mfValue[k1],fs2$mfValue[k2]);
				   print(tmpMf1)
				  
				  }				    
				if(tnorm=="product")
				  {tmpMf1=prod(fs1$mfValue[k1],fs2$mfValue[k2]);}
				  						
				#mfVal[i]=max(mfVal[i],tmpMf1);	
				mfVal=max(mfVal,tmpMf1); 	    
		    }		  
		  }		
		}
		
#	}

return(list(mfValue=mfVal,domain=x));

}