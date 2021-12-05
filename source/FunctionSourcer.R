#Set working directory, import packages, source functions, 
setwd(paste(directory,"/source/", sep = ''))    # set temp working directory 

#import packages
#library()

#source functions

source(paste(getwd(), "/FarmersInit.R", sep = ''))
source(paste(getwd(), "/Generate_LS_Factor.R", sep = ''))
source(paste(getwd(), "/Generate_K_Factor.R", sep = ''))
source(paste(getwd(), "/Generate_C_Factor.R", sep = ''))
source(paste(getwd(), "/Generate_Runoff_Factor.R", sep = ''))
#source(paste(getwd(), "/NewPop.R", sep = ''))
#source(paste(getwd(), "/MoveIndv.R", sep = ''))