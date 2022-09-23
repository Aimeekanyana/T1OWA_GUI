# Display and choose type of weights to define a type1 OWA operator
# 
# Usage: opt=weightSelectionMenu()
#
# Input : options - menu options default 
# Output  opt  - parameters of weight types
#
#	Shang-Ming Zhou, 15 June 2022
#
showMenu<-function(options){

source("enterNum.r")

for (i in 1:length(options)){
	cat(sprintf("%d. %s\n",i, options[i]))
}

# Choose a valid option
opt=0
while(!any(opt==1:length(options))){
	opt=enterNum("Please choose the number for an option:")
}

return(opt)

}