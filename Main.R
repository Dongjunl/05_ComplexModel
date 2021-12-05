## Programed by Dongjun Lee for predicting amount of soil loss 

## Step#1: Set up functions and work space
setwd("D:/00_AuburnUniversity/03_Fall,2021/03_Developing_Agent-Based_Models(WILD-7400)/05_ComplexModel/")
directory <- getwd()
outdir <- paste(directory,"/Output/", sep="")
source(paste(directory, "/Source/FunctionSourcer.R", sep="")) 

## Step#2: Define parameters 
nfarmers <- 10 # number of farmers

## Input data
dem_file = "D:/00_AuburnUniversity/03_Fall,2021/03_Developing_Agent-Based_Models(WILD-7400)/05_ComplexModel/Input/dem.tif"
soil_k_map = "D:/00_AuburnUniversity/03_Fall,2021/03_Developing_Agent-Based_Models(WILD-7400)/05_ComplexModel/Input/soil_K.tif"
landuse_map = "D:/00_AuburnUniversity/03_Fall,2021/03_Developing_Agent-Based_Models(WILD-7400)/05_ComplexModel/Input/landuse.tif"
hsg_map = "D:/00_AuburnUniversity/03_Fall,2021/03_Developing_Agent-Based_Models(WILD-7400)/05_ComplexModel/Input/hsg.tif"
rainfall_file = 'D:/00_AuburnUniversity/03_Fall,2021/03_Developing_Agent-Based_Models(WILD-7400)/05_ComplexModel/Input/rainfall_monthly.csv'

## Generate Runoff map
Creat_runoff_map(landuse_map,hsg_map,rainfall_file)

## Generate LS map
Creat_LS_map(dem_file)

## Generate K map (Soil erosibility)
Creat_K_map(soil_k_map)

## Generate C map 
Creat_C_map(landuse_map)

## Generate P map (based on farmer characteristics)
FarmersInit_P_map(nfarmers, landuse_map)

## runoff factor map



