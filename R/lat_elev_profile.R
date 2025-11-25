## create the latitude-elevation
lat_elev_profile <- function(file) {
  
  # require(rgdal) 
  
  options(digits=10)
  
  dtm_terra = terra::rast(file)
  
  dtm = raster::raster(dtm_terra)
  
  dtm_p = raster::rasterToPoints(dtm)
  dtm_lat_elev = as.data.frame(dtm_p[,2:3])
  colnames(dtm_lat_elev) = c('y', 'z')
  
  lat_elev_profile = stats::aggregate(dtm_lat_elev['z'], by = dtm_lat_elev['y'], FUN = max)
  return(lat_elev_profile)
}
