#' Taiwan Coordinate Transformation (TWD67, TWD97, WGS84)
#'
#' This function performs coordinate system transformations between WGS84 (84),
#' TWD97 (97, using TM2 projection), and TWD67 (67, using TM2 projection).
#'
#' It utilizes custom empirical formulas for TWD67 <-> TWD97 conversion, and the
#' \code{sf} package for transformations involving WGS84 (geographic coordinates).
#'
#' @param coords A two-column matrix or data frame of coordinates, where the first column is X (Easting/Longitude) and the second column is Y (Northing/Latitude).
#' @param src An integer (84, 97, or 67) indicating the source coordinate system.
#' \itemize{
#'   \item \strong{84:} WGS84 (Longitude-Latitude).
#'   \item \strong{97:} TWD97 / EPSG:3826 (TM2, meters).
#'   \item \strong{67:} TWD67 (TM2, meters).
#' }
#' @param dst An integer (84, 97, or 67) indicating the destination coordinate system.
#'
#' @return A matrix of transformed coordinates (X, Y).
#' @export
#' @importFrom sf st_as_sf st_set_crs st_transform st_coordinates
#' @examples
#' \dontrun{
#' # Load example coordinate data (WGS84 Lon/Lat)
#' data(twsp)
#' head(twsp)
#'
#' # Example 1: WGS84 (84) to TWD97 TM2 (97)
#' twd97_coords <- twcoor.trans(twsp, src = 84, dst = 97)
#' head(twd97_coords)
#'
#' # Example 2: TWD97 TM2 (97) back to WGS84 (84)
#' wgs84_coords <- twcoor.trans(twd97_coords, src = 97, dst = 84)
#' head(wgs84_coords)
#'
#' # Example 3: TWD97 TM2 (97) to TWD67 TM2 (67) using empirical formula
#' # Note: Must use a TWD97 TM2 matrix for 'coords' input for this conversion.
#' twd67_coords <- twcoor.trans(twd97_coords, src = 97, dst = 67)
#' head(twd67_coords)
#' }
#' 
twcoor.trans <- function(coords, src, dst) {
  # projection definition (from Proj.4, coordinate systems see http://spatialreference.org)
  # Taiwan Datum 1997 Transverse Mercator EPSG: 3826 
  TWD97TM2_CRS <- "+proj=tmerc +ellps=GRS80 +lon_0=121 +x_0=250000 +k=0.9999 +units=m +no_defs"
  # Taiwan Datum 1967 Transverse Mercator
  TWD67TM2_CRS <- "+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=aust_SA +units=m +towgs84=-752,-358,-179,-0.0000011698,0.0000018398,0.0000009822,0.00002329 +no_defs"
  # WGS84 Longitude-latitude 
  WGS84_CRS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  # Parameters for converting twd67 to twd97 and vise versa
  A <- 0.00001549
  B <- 0.000006521
  EW <- 807.8
  SN <- 248.6
  
  if (src == dst) {
    return(coords)
  }
  
  src.proj <- switch(as.character(src),
                     "84" = WGS84_CRS,
                     "97" = TWD97TM2_CRS,
                     "67" = TWD67TM2_CRS,
                     stop("Unsupported source coordinate system!"))
  
  dst.proj <- switch(as.character(dst),
                     "84" = WGS84_CRS,
                     "97" = TWD97TM2_CRS,
                     "67" = TWD67TM2_CRS,
                     stop("Unsupported destination coordinate system!"))
  
  #pts <- sf::st_as_sf(as.data.frame(coords),
  #                    coords = c(1, 2),
  #                    crs = src.proj)
  
  pts <- sf::st_as_sf(as.data.frame(coords), coords = c(1, 2))
  pts <- sf::st_set_crs(pts, src.proj)
  
  # --- TWD67 <-> TWD97 ---
  
  if (src == 97 && dst == 67) {
    # TWD97 to TWD67
    Y67 <- coords[, 2] + SN - A * coords[, 2] - B * coords[, 1]
    X67 <- coords[, 1] - EW - A * coords[, 1] - B * coords[, 2]
    return(cbind(X67, Y67))
    
  } else if (src == 67 && dst == 97) {
    # TWD67 to TWD97
    X97 <- coords[, 1] + EW + A * coords[, 1] + B * coords[, 2]
    Y97 <- coords[, 2] - SN + A * coords[, 2] + B * coords[, 1]
    return(cbind(X97, Y97))
    
  } else {
    # --- general sf projection transformation (WGS84 <-> TWD ) ---
    trans_pts <- sf::st_transform(pts, dst.proj)
    return(sf::st_coordinates(trans_pts))
  }
}
