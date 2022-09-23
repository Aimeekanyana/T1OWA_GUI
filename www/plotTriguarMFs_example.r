	source("trimf.r")
	numObj=3;#3;#5;

		parFS=vector("list",numObj);
		A=vector("list",numObj);

		domainX=seq(0,5,length=100);

			parFS[[1]]=c(1.25,1.65,1.9);
			parFS[[2]]=c(2.2,2.6,2.9);
			parFS[[2]]=c(3.1,3.2,3.9);
			parFS[[3]]=c(4.25,4.65,5);

			for(i in 1:numObj)
				{
					A[[i]]$mfValue=trimf(domainX,parFS[[i]]);
					A[[i]]$domain=domainX;	
					A[[i]]$type=c("triMF")
					A[[i]]$para=parFS[[i]]	
				}
				
		
#plot aggregated objects
	X11();
	plot(domainX, seq(0,1,length=length(domainX)), xlab="X",ylab="Grade of membership degree",font.lab=1,type ="n") 

	for(i in 1:numObj)
		{
			 lines(domainX,A[[i]]$mfValue,lwd=2)
		}