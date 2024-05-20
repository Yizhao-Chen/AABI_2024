########################################################################################################
#Spatial analysis: detrend the rasters and perform correlation analysis
########################################################################################################
library(raster)
library(pracma)

#Spatial data detrend
{
  #root
  #TRENDY NPP as an example
  root = paste0(".\\Spatial_analysis\\TRENDYv7_S2_ensmeble_NPP_1984_2010\\")
  root1 = paste0(".\\Spatial_analysis\\TRENDYv7_S2_ensmeble_NPP_1984_2010_detrend\\")
  out_name <- paste0("trendyv7_S2_npp_mem_",year,"_ESM_detrend.tif")
  
  #list files (in this case raster TIFFs)
  grids <- list.files(root, pattern = "*.tif$")
  
  #check the number of files in the raster list (grids)
  length1 <- length(grids)
  s <- stack(root, grids)
  time <- 1:nlayers(s)
  
  # Get residuals to detrend the raw data
  get_residuals <- function(x) {
    if (is.na(x[1])){ 
      rep(NA, length(x)) } 
    else {
      m <- lm(x~time)
      q <- residuals(m)
      return(q)
    }
  }
  
  detrended_fns <- calc(s, get_residuals) # Create the residual (detrended) time series stack
  
  for (i in 1:length1){
    year = i + 1983
    rf <- writeRaster(detrended_fns[[i]], filename=file.path(root1,out_name), format="GTiff", overwrite=TRUE)
  }
  rm(s)    
  rm(detrended_fns)
}

########################################################################################################
#The TRENDY detrended outputs were extracted to global forest areas using GLC2000 mask. The detrended outputs were further extracted to North America forest areas
#Extractions were completed in Arc GIS 10.5
########################################################################################################

#Correlation analysis between the detrended outputs
{
  #TRENDY GPP and NPP as an example
  root1 = ".\\Spatial_analysis\\detrend\\TRENDYv7_S2_ensemble_GPP_1984_2010_detrend\\"
  root2 = ".\\Spatial_analysis\\detrend\\TRENDYv7_S2_ensemble_NPP_1984_2010_detrend\\"
  out_file = ".\\Spatial_analysis\\corr\\corr_trendy_gpp_npp_s2_ensemble_1984_2010_north_america_GLC.tif"
  
  grids1 <- list.files(paste0(root1), pattern = "*.tif$")
  s1 <- stack(paste0(root1, grids1))
  
  grids2 <- list.files(paste0(root2), pattern = "*.tif$")
  s2 <- stack(paste0(root2, grids2))
  
  rstack <- stack(s1,s2)
  
  corvec <- function(vec = NULL) {
    cor(
      # 'top' half of stack
      x      = vec[1:(length(vec)/2)],
      # 'bottom' half of stack
      y      = vec[((length(vec)/2) + 1):length(vec)],
      #use    = 'complete.obs',
      method = 'pearson'
    )
  }
  
  corlyrs <- calc(
    rstack,
    fun = function(x) {
      # handle areas where all cell vals are NA, e.g. ocean
      if (all(is.na(x))) {
        NA_real_
      } else {
        corvec(vec = x)
      }
    }
  )
  
  rf <- writeRaster(corlyrs, filename=file.path(out_file, format="GTiff", overwrite=TRUE))
}























