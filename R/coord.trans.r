twcoor.trans <- function(coords, src, dst) {
  # projection definition (from Proj.4, coordinate systems see http://spatialreference.org)
  # Taiwan Datum 1997 Transverse Mercator EPSG: 3826 
  TWD97TM2 <- "+proj=tmerc +ellps=GRS80 +lon_0=121 +x_0=250000 +k=0.9999 +units=m +no_defs"
  # Taiwan Datum 1967 Transverse Mercator
  TWD67TM2 <- "+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=aust_SA +units=m +towgs84=-752,-358,-179,-0.0000011698,0.0000018398,0.0000009822,0.00002329 +no_defs"
  # WGS84 Longitude-latitude 
  WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  # Parameters for converting twd67 to twd97 and vise versa
  A <- 0.00001549
  B <- 0.000006521
  EW <- 807.8
  SN <- 248.6
  
  sf_transform_coords <- function(coords, src.proj, dst.proj) {

    pts <- sf::st_as_sf(as.data.frame(coords), 
                        coords = c(1, 2), 
                        crs = src.proj)
    trans_pts <- sf::st_transform(pts, dst.proj)
    sf::st_coordinates(trans_pts)
  }
  
  if ( src == 84 ){
    src.proj <- WGS84

    if ( dst == 97 ){
      dst.proj <- TWD97TM2 
      sf_transform_coords(coords, src.proj, dst.proj)
    } else if ( dst == 67 ) {
      dst.proj <- TWD67TM2
      sf_transform_coords(coords, src.proj, dst.proj)
    } else if ( dst == 84 ) {
      coords
    } else print("Unsupported coordinate system!")
    
  } else if ( src == 97 ) {
    src.proj <- TWD97TM2
    if ( dst == 84 ) {
      dst.proj <- WGS84

      sf_transform_coords(coords, src.proj, dst.proj)
    } else if ( dst == 67 ){
      dst.proj <- TWD67TM2
      Y67 <- coords[, 2] + SN - A * coords[, 2] - B * coords[, 1]
      X67 <- coords[, 1] - EW - A * coords[, 1] - B * coords[, 2]
      cbind(X67, Y67)
    } else if ( dst == 97 ) {
      coords
    } else print("Unsupported coordinate system!")
    
  } else if ( src == 67 ) {
    src.proj <- TWD67TM2
    if ( dst == 84 ) {
      dst.proj <- WGS84
      sf_transform_coords(coords, src.proj, dst.proj)
    } else if ( dst == 97 ){
      dst.proj <- TWD97TM2

      X97 <- coords[, 1] + EW + A * coords[, 1] + B * coords[, 2] 
      Y97 <- coords[, 2] - SN + A * coords[, 2] + B * coords[, 1]
      cbind(X97, Y97)
    } else if ( dst == 67 ) {
      coords
    } else print("Unsupported coordinate system!")
  }
  else print("Exception caught! Please report a bug! Thank you")
}
