## Required the packagee list to estimate slope and slopelength
#install.packages("raster")
#install.packages("rgdal")
#install.packages("dplyr")

Creat_LS_map=function(dem_file){
  
  ## Import packages
  library(raster)
  library(sp)
  library(rgdal)
  
  #dem_file = "D:/00_AuburnUniversity/03_Fall,2021/03_Developing_Agent-Based_Models(WILD-7400)/05_ComplexModel/Input/dem.tif"
  
  ## calculate  
  dem = raster(dem_file)
  dem = aggregate(dem, fact = 10, fun=max) ## aggregate data (optional)
  df_dem = as.data.frame(dem, xy=T)
  
  ## calculate slope 
  slp = terrain(dem, opt = "slope", unit = "tangent")
  df_slp = as.data.frame(slp, xy=T)
  
  ## calculate flow direction
  flwdir = terrain(dem, opt = "flowdir")
  #lot(flwdir)
  df_flwdir = as.data.frame(flwdir, xy=T)
  
  ## Database combined
  df = cbind(df_dem, df_slp["slope"]*100) #covert tangent to percent (slope)
  df = cbind(df, df_flwdir["flowdir"])
  
  ## managing Nodata 
  df$slope[is.nan(df$slope)] = -9999 #change NaN to -9999
  df$slope[is.na(df$slope)] = -9999 #change Na to -9999
  df$flowdir[is.nan(df$flowdir)] = -9999 #change NaN to -9999
  df$flowdir[is.na(df$flowdir)] = -9999 #change Na to -9999
  
  
  #cell size = 30m
  cellsize = 30
  # N = 64; NE = 128; E = 1; SE = 2; S = 4; SW = 8; W = 16; NW = 32
  df$length = 0
  for( i in 1:nrow(df)){
    if (df[i,5] == -9999){
      df[i,6] = 0
    } else {
      if (df[i,5] == 64){df[i,6] = cellsize}  # North
      if (df[i,5] == 1){df[i,6] = cellsize}   # East 
      if (df[i,5] == 4){df[i,6] = cellsize}   # South
      if (df[i,5] == 16){df[i,6] = cellsize}  # West
      if (df[i,5] == 128){df[i,6] = cellsize*1.414}  #NE
      if (df[i,5] == 2){df[i,6] = cellsize*1.414}  #NE
      if (df[i,5] == 8){df[i,6] = cellsize*1.414}  #NE
      if (df[i,5] == 32){df[i,6] = cellsize*1.414}  #NE
    }
  }
  
  #LS factor
  df$ls = 0
  for (i in 1:nrow(df)){
    if (df[i,5] == -9999){
      df[i,7] = 0
    } else {
      df[i,7]=(0.065+0.0456*df[i,4]+0.006541*(df[i,4]**2)*(df[i,6]/22.1)**0.5)
    }
  }
  
  #Create LS raster data
  df2 = df
  df2$z = df2$ls
  df_ls = subset(df2, select = -c(dem,slope,flowdir,length,ls))
  ls_map = rasterFromXYZ(df2)
  plot(ls_map)
  
  return(df_ls)
  return(ls_map)
}




