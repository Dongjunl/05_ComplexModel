FarmersInit_P_map = function(nfarmers,landuse_map){
  ############################################
  ## Parameters explanation
  # FS: Farm size based on the distribution (low/large)
  # Income : Farmer's income level. (high/low)
  # Edu_lv: Faremr agents' education level. (middle/high)
  # AT: Agents' attitude values
  # SN: Agents' subjective norm values
  # PBC: Perceived behavioral control 
  # I: Intentsion level
  # B: Behavior decision 
  ###########################################
  
  ## Import packages
  library(raster)
  library(sp)
  library(rgdal)
  
  ## Load input
  nfarmers = 10
  landuse_map = "D:/00_AuburnUniversity/03_Fall,2021/03_Developing_Agent-Based_Models(WILD-7400)/05_ComplexModel/Input/landuse.tif"
  
  ## Read DEM file
  landuse = raster(landuse_map)
  landuse = aggregate(landuse, fact = 10, fun=max) ## aggregate data (optional)
  df_lulc = as.data.frame(landuse, xy=T)
  #plot(landuse)
  
  ## managing Nodata 
  df_lulc$landuse[is.nan(df_lulc$landuse)] = -9999 #change NaN to -9999
  df_lulc$landuse[is.na(df_lulc$landuse)] = -9999 #change Na to -9999
  
  ## distribute farmers into crop land type
  df_lulc$fowner = 0
  
  for(i in 1:nrow(df_lulc)){
    if (df_lulc[i,3] == -9999){
      df_lulc[i,4] = 0
    } else if(df_lulc[i,3]==82){
      df_lulc[i,4] = sample(1:nfarmers,1)
    } else{df_lulc[i,4]=0
    }
  }
  
  # estimate farmer size
  FS = table(df_lulc$fowner)
  FS = data.frame(FS)
  FS = FS[-c(1),]  # remove 0 which is other landuse type
  
  # Generate farmer properties data and data frame
  index_Income_lv = as.integer(runif(nfarmers, min=0, max=10))
  index_Edu_lv = as.integer(runif(nfarmers, min=0, max=10))
  IC = data.frame(index_Income_lv)
  EL = data.frame(index_Edu_lv)
  
  df = cbind(FS,IC)
  df = cbind(df,EL)
  
  ## Estimate intention of behavior 
  df$intention_FS = 0
  df$intention_IC = 0
  df$intention_EL = 0
  
  # Farm size behavior 
  for (i in 1:nrow(df)){
    if(df[i,2]>mean(df$Freq)){
      df[i,5] = rnorm(1, mean=0.53, sd=0.1)
    } else{
      df[i,5] = rnorm(1, mean=0.73, sd=0.1)
    }
  }
  
  # Income behavior
  for (i in 1:nrow(df)){
    if(df[i,3]> 5){
      df[i,6] = rnorm(1, mean=0.59, sd=0.1)
    } else{
      df[i,6] = rnorm(1, mean=0.71, sd=0.1)
    }
  }
  
  # Education level behavior 
  for (i in 1:nrow(df)){
    if(df[i,4]>5){
      df[i,7] = rnorm(1, mean=0.63, sd=0.1)
    } else{
      df[i,7] = rnorm(1, mean=0.76, sd=0.1)
    }
  }
  
  # Final behavior index
  df$BI = 0
  for (i in 1:nrow(df)){
    df[i,8] = df[i,5]*df[i,6]*df[i,7]
  } 
  
  #calculate p factor of each farmer
  df$pfactor = 0
  for (i in 1:nrow(df)){
    if(df[i,8] >= mean(df$BI)){
      df[i,9] = rnorm(1, mean=0.75, sd=0.15)
    } else{
      df[i,9] = rnorm(1, mean=0.25, sd=0.15)
    }
  }
  
  ## Making P map 
  df_lulc$pfactor = 0
  
  for(i in 1:nrow(df_lulc)){
    if (df_lulc[i,3] == -9999){
      df_lulc[i,5] = 0
    } else if(df_lulc[i,3] == 82){
      for (j in 1:nrow(df)){
        if (df_lulc[i,4]==df[j,1]){ df_lulc[i,5] = df[j,9]}
      }
    }
    else {df_lulc[i,5] = 1}
  }
  
  #Create p raster data
  df_lulc2 = df_lulc
  df_lulc2$z = df_lulc2$pfactor
  df_lulc2 = subset(df_lulc2, select = -c(landuse,fowner,pfactor))
  df_p = df_lulc2
  p_map = rasterFromXYZ(df_lulc2)
  plot(p_map)
  
  return(df_p)
  return(p_map)
}