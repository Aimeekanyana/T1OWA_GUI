# Displays prompt and ask user to enter a number
# 
# Usage: num=enterNum(promot)
#
# Author: @Shang-Ming Zhou,15 June 2022
#
enterNum<-function(prompt){

	while(TRUE){
		num=suppressWarnings(as.numeric(readline(prompt)))
		if(!is.na(num)){
			break
		}
	 }
	return(num)
}