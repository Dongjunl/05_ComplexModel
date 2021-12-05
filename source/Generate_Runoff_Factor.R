Creat_runoff_map=function(landuse_map,hsg_map,rainfall_file){
  
  ## Import packages
  library(raster)
  library(sp)
  library(rgdal)
  
  ## Load input data
  #landuse_map = "D:/00_AuburnUniversity/03_Fall,2021/03_Developing_Agent-Based_Models(WILD-7400)/05_ComplexModel/Input/landuse.tif"
  #hsg_map = "D:/00_AuburnUniversity/03_Fall,2021/03_Developing_Agent-Based_Models(WILD-7400)/05_ComplexModel/Input/hsg.tif"
  
  ## Read input file
  landuse = raster(landuse_map)
  landuse = aggregate(landuse, fact = 10, fun=max) ## aggregate data (optional)
  df_lulc = as.data.frame(landuse, xy=T)
  
  hsg = raster(hsg_map)
  hsg = aggregate(hsg, fact = 10, fun=max) ## aggregate data (optional)
  df_hsg = as.data.frame(hsg, xy=T)
  
  ## managing Nodata 
  df_lulc$landuse[is.nan(df_lulc$landuse)] = -9999 #change NaN to -9999
  df_lulc$landuse[is.na(df_lulc$landuse)] = -9999 #change Na to -9999
  df_hsg$landuse[is.na(df_lulc$landuse)] = -9999 #change Na to -9999
  df_hsg$landuse[is.na(df_lulc$landuse)] = -9999 #change Na to -9999
  
  for (i in 1:nrow(df_hsg)){
    if(df_hsg[i,3] == 128){
      df_hsg[i,3] = -9999
    } else {
      df_hsg[i,3] = df_hsg[i,3]
    }
  }
  
  # Making database
  df = cbind(df_lulc,df_hsg["hsg"])
  
  # calculate CN numer
  df$cn = 0
  for(i in 1:nrow(df)){
    if (df[i,3] == -9999){
      df[i,5] = 0
    } else {
      if (df[i,3] == 11){df[i,5] = 0}  # water
      if (df[i,3] == 21){df[i,5] = 98} # urban 
      if (df[i,3] == 22){df[i,5] = 98} # urban 
      if (df[i,3] == 23){df[i,5] = 98} # urban 
      if (df[i,3] == 24){df[i,5] = 98} # urban
      if (df[i,3] == 31){df[i,5] = 80.5} # barren Land
      if (df[i,3] == 41){df[i,5] = 50}   # forest
      if (df[i,3] == 42){df[i,5] = 50}   # forest
      if (df[i,3] == 43){df[i,5] = 50}   # forest
      if (df[i,3] == 52){df[i,5] = 70.25}   # shrub/scurb
      if (df[i,3] == 71){df[i,5] = 70.25}   # Herbaceuous
      if (df[i,3] == 81){df[i,5] = 60.25}   # Pasture
      if (df[i,3] == 82){df[i,5] = 74}   # crop
      if (df[i,3] == 90){df[i,5] = 0}   # wetland
      if (df[i,3] == 95){df[i,5] = 0}   # wetland
      
    }
  }
  
  
  ## Read rainfall file 
  #rainfall_file = 'D:/00_AuburnUniversity/03_Fall,2021/03_Developing_Agent-Based_Models(WILD-7400)/05_ComplexModel/Input/rainfall_monthly.csv'
  df_rainfall = read.csv(rainfall_file, fileEncoding = "UCS-2LE")
  
  df$S = (25400/df[,5])-254
  
  
  
  ## Calculate monthly runoff volume
  Q_matrix = matrix (nrow = nrow(df), ncol=nrow(df_rainfall))
  qp_matrix = matrix (nrow = nrow(df), ncol=nrow(df_rainfall))
  
  for (i in 1:nrow(df_rainfall)){
    P = df_rainfall[i,2]
    qu = 0.2*1166/P
    Area = 0.000347
    Fp = 1
    for (j in 1:nrow(df)){
      temp_Q = (P-0.2*df[j,6])^2/(P+0.8*df[j,6])
      temp_Q2 = temp_Q*0.0393
      Q_matrix[j,i] = temp_Q
      qp_matrix[j,i] = qu*Area*temp_Q2*Fp
    }
  }
  
  ## Generate runoff factor 
  df_runoff = Q_matrix[,]*qp_matrix[,]
  
  df = cbind(df,df_runoff)
  
  ##Generate runoff factor map 
  df_Jan = df
  df_Jan = subset(df, select = -c(3,4,5,6,8,9,10,11,12,13,14,15,16,17,18))
  Jan_map = rasterFromXYZ(df_Jan)
  plot(Jan_map)
  
  df_Feb = df
  df_Feb = subset(df, select = -c(3,4,5,6,7,9,10,11,12,13,14,15,16,17,18))
  Feb_map = rasterFromXYZ(df_Feb)
  plot(Feb_map)
  
  df_Mar = df
  df_Mar = subset(df, select = -c(3,4,5,6,7,8,10,11,12,13,14,15,16,17,18))
  Mar_map = rasterFromXYZ(df_Mar)
  plot(Mar_map)
  
  df_Apr = df
  df_Apr = subset(df, select = -c(3,4,5,6,7,8,9,11,12,13,14,15,16,17,18))
  Apr_map = rasterFromXYZ(df_Apr)
  plot(Apr_map)
  
  df_May = df
  df_May = subset(df, select = -c(3,4,5,6,7,8,9,10,12,13,14,15,16,17,18))
  May_map = rasterFromXYZ(df_May)
  plot(May_map)
  
  df_Jun = df
  df_Jun = subset(df, select = -c(3,4,5,6,7,8,9,10,11,13,14,15,16,17,18))
  Jun_map = rasterFromXYZ(df_Jun)
  plot(Jun_map)
  
  df_Jul = df
  df_Jul = subset(df, select = -c(3,4,5,6,7,8,9,10,11,12,14,15,16,17,18))
  Jul_map = rasterFromXYZ(df_Jul)
  plot(Jul_map)
  
  df_Aug = df
  df_Aug = subset(df, select = -c(3,4,5,6,7,8,9,10,11,12,13,15,16,17,18))
  Aug_map = rasterFromXYZ(df_Aug)
  plot(Aug_map)
  
  df_Sep = df
  df_Sep = subset(df, select = -c(3,4,5,6,7,8,9,10,11,12,13,14,16,17,18))
  Sep_map = rasterFromXYZ(df_Sep)
  plot(Sep_map)
  
  df_Oct = df
  df_Oct = subset(df, select = -c(3,4,5,6,7,8,9,10,11,12,13,14,15,17,18))
  Oct_map = rasterFromXYZ(df_Oct)
  plot(Oct_map)
  
  df_Nov = df
  df_Nov = subset(df, select = -c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,18))
  Nov_map = rasterFromXYZ(df_Nov)
  plot(Nov_map)
  
  df_Dec = df
  df_Dec = subset(df, select = -c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))
  Dec_map = rasterFromXYZ(df_Dec)
  plot(Dec_map)

  return(df_Jan)
  return(Jan_map)
  return(df_Feb)
  return(Feb_map)
  return(df_Mar)
  return(Mar_map)
  return(df_Apr)
  return(Apr_map)
  return(df_May)
  return(May_map)
  return(df_Jun)
  return(Jun_map)
  return(df_Jul)
  return(Jul_map)
  return(df_Aug)
  return(Aug_map)
  return(df_Sep)
  return(Sep_map)
  return(df_Oct)
  return(Oct_map)
  return(df_Dec)
  return(Dec_map)
}  