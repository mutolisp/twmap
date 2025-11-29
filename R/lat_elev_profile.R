#' Create the Latitude-Elevation Profile
#'
#' This function reads a Digital Terrain Model (DTM) file (usually a GeoTIFF)
#' and calculates the **maximum elevation** for each latitude (Y-coordinate) line,
#' creating a latitude-elevation profile dataset. This profile represents the
#' highest ridge line of the island when viewed from the side.
#'
#' @param file Character string specifying the file path to the DTM (e.g., GeoTIFF).
#'
#' @return A data frame with two columns:
#'   \item{y}{The latitude (Y-coordinate) in the DTM's coordinate system (e.g., TWD97 TM2 Y or WGS84 Latitude).}
#'   \item{z}{The maximum elevation (Z) found at that latitude, in meters.}
#' @export
#'
#' @importFrom terra rast as.data.frame
#' @importFrom stats aggregate
#'
#' @examples
#' \dontrun{
#' # Assuming TWN30MDTM.tif is located in inst/extdata/
#' # You need to run this on your local machine with the file present.
#' dtm_path <- system.file("extdata", "LanyuDTM.tif", package = "twmap")
#' if (file.exists(dtm_path)) {
#'   profile <- lat_elev_profile(dtm_path)
#'   head(profile)
#'   # Plotting the result
#'   graphics::plot(profile, type = "l", xlab = "Y-Coordinate", ylab = "Max Elevation (m)")
#' } else {
#'   message("DTM file not found. Skipping example.")
#' }
#' }
lat_elev_profile <- function(file) {
  
  options(digits=10)
  
  dtm_terra <- terra::rast(file)
  
  dtm_data <- terra::as.data.frame(dtm_terra, xy = TRUE, na.rm = TRUE)

  colnames(dtm_data) <- c('x', 'y', 'z')
  
  lat_elev_profile <- stats::aggregate(dtm_data['z'], by = dtm_data['y'], FUN = max)
  
  return(lat_elev_profile)
}
