## guidelines for selecting types of weights
#
#	crispWeightChoice - 1 = 'Meet'/'singletonMeet'/'fuzzfiedMinimum'
#						2 = 'Join'/'singletonJoin'/'fuzzifiedMaximum'
#						3 = 'Mean'/'singletonAvg'/'fuzzifiedAverage'
#						4 =	Not use crisp weights
#
#	For crispWeightChoice==0:
#	 lweightType	 -	1 =	'MeetLike'
#						2 =	'JoinLike'
#						3 =	'AvgLike'
#
#		specificWty	- 1)For weightType==1 # 'MeetLike'
#						1 = 'unEqualDomainPartition' 
#						2 = 'equalDomainPartition'	# 'left2right' 
#						3 = 'type-2 quantifier "most"'
#
#		specificWty	- 1)For weightType==2 # 'JoinLike'
#						1 = 'unEqualDomainPartition' 
#						2 = 'equalDomainPartition' # 'right2left' 
#						3 = 'type-2 quantifier "most"'
#
#		specificWty	- 1)For weightType==3 #'AvgLike'
#						1 = 'unEqualDomainPartition'
#
#	@Shang-Ming Zhou, 15 June 2022
#
genWeightChoice<-function(){

	source("showMenu.r")

	crispWeightChoice= c("Meet","Join", "Mean","Not use singlton weights")

	lweightTypes=c("MeetLike","JoinLike","AvgLike")
	lweightType1Choices=c("unEqualDomainPartition","equalDomainPartition", "type-2 quantifier 'most'")
	lweightType2Choices=c("unEqualDomainPartition","equalDomainPartition")
	lweightType3Choices=c("unEqualDomainPartition")
	
	 singleWopt=showMenu(crispWeightChoice)
	 
	 lwOpt=list("lwCat","specificWt")
	 
	 #default
	 lwOpt$lwCat=0
	 lwOpt$specificWt=0
	 
	 if(singleWopt==4)
		{
			lwCat =showMenu(lweightTypes)
		
			if(lwCat==1) #'meetLike'
			{
				specificWty=showMenu(lweightType1Choices)				
			}else if (lwCat==2) #'joinLike'
		    {
				specificWty=showMenu(lweightType2Choices)	
			}else if (lwCat==3) #'avgLike'
			{
				#specificWty=showMenu(lweightType3Choices)	 #'unEqualDomainPartition'
				specificWty=1
			}	
		
		  lwOpt$lwCat=lwCat
		  lwOpt$specificWt=specificWty
		}
	 	 
	 return(list(swOpt=singleWopt,lwOpt=lwOpt))
}