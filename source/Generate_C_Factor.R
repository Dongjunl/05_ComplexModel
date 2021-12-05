Creat_C_map=function(landuse_map){
  
  ## Import packages
  library(raster)
  library(sp)
  library(rgdal)
  
  ## Load DEM data
  #landuse_map = "D:/00_AuburnUniversity/03_Fall,2021/03_Developing_Agent-Based_Models(WILD-7400)/05_ComplexModel/Input/landuse.tif"
  
  ## Read DEM file
  landuse = raster(landuse_map)
  landuse = aggregate(landuse, fact = 10, fun=max) ## aggregate data (optional)
  df_lulc = as.data.frame(landuse, xy=T)
  #plot(landuse)
  
  ## managing Nodata 
  df_lulc$landuse[is.nan(df_lulc$landuse)] = -9999 #change NaN to -9999
  df_lulc$landuse[is.na(df_lulc$landuse)] = -9999 #change Na to -9999
  
  ## estimate c value 
  df_lulc$cfactor = 0
  
  for(i in 1:nrow(df_lulc)){
    if (df_lulc[i,3] == -9999){
      df_lulc[i,4] = 0
    } else {
      if (df_lulc[i,3] == 11){df_lulc[i,4] = 0}  # water
      if (df_lulc[i,3] == 21){df_lulc[i,4] = 0.2}   # urban 
      if (df_lulc[i,3] == 22){df_lulc[i,4] = 0.2}   # urban
      if (df_lulc[i,3] == 23){df_lulc[i,4] = 0.2}   # urban
      if (df_lulc[i,3] == 24){df_lulc[i,4] = 0.2}   # urban
      if (df_lulc[i,3] == 31){df_lulc[i,4] = 1.0}   # barren land
      if (df_lulc[i,3] == 41){df_lulc[i,4] = 0.01}   # forest
      if (df_lulc[i,3] == 42){df_lulc[i,4] = 0.01}   # forest
      if (df_lulc[i,3] == 43){df_lulc[i,4] = 0.01}   # forest
      if (df_lulc[i,3] == 52){df_lulc[i,4] = 0.1}   # shrub/scurb
      if (df_lulc[i,3] == 71){df_lulc[i,4] = 0.16}   # Herbaceuous
      if (df_lulc[i,3] == 81){df_lulc[i,4] = 0.1}   # Pasture
      if (df_lulc[i,3] == 82){df_lulc[i,4] = 0.5}   # crop
      if (df_lulc[i,3] == 90){df_lulc[i,4] = 0.01}   # wetland
      if (df_lulc[i,3] == 95){df_lulc[i,4] = 0.01}   # wetland
      
    }
  }
  
  #Create C raster data
  df_lulc2 = df_lulc
  df_lulc2$z = df_lulc2$cfactor
  df_lulc2 = subset(df_lulc2, select = -c(landuse,cfactor))
  c_map = rasterFromXYZ(df_lulc2)
  plot(c_map)
  
  return(df_lulc2)
  return(c_map)
  
}