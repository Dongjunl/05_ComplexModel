Creat_K_map=function(soil_k_map){
  
  ## Import packages
  library(raster)
  library(sp)
  library(rgdal)
  
  ## Load DEM data
  #soil_k_map = "D:/00_AuburnUniversity/03_Fall,2021/03_Developing_Agent-Based_Models(WILD-7400)/05_ComplexModel/Input/soil_K.tif"
  
  ## Read DEM file
  k_map = raster(soil_k_map)
  k_map = aggregate(k_map, fact = 10) ## aggregate data (optional)
  df_k = as.data.frame(k_map, xy=T)
  plot(k_map)
  
  return(k_map)
}