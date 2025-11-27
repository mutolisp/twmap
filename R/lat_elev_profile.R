#' Create the Latitude-Elevation Profile
#'
#' This function reads a Digital Terrain Model (DTM) file (usually a GeoTIFF)
#' and calculates the maximum elevation for each latitude (Y-coordinate) line,
#' creating a latitude-elevation profile dataset.
#'
#' @param file Character string specifying the file path to the DTM (e.g., GeoTIFF).
#'
#' @return A data frame with two columns:
#'   \item{y}{The latitude (Y-coordinate).}
#'   \item{z}{The maximum elevation (Z) found at that latitude.}
#' @export
#'
#' @importFrom terra rast as.data.frame
#' @importFrom stats aggregate
#'
#' @examples
#' \dontrun{
#' # Assuming TWN30MDTM.tif is located in inst/extdata/
#' dtm_path <- system.file("extdata", "TWN30MDTM.tif", package = "twmap")
#' profile <- lat_elev_profile(dtm_path)
#' head(profile)
#' graphics::plot(profile, type = "l")
#' }
lat_elev_profile <- function(file) {
  
  options(digits=10)
  
  dtm_terra <- terra::rast(file)
  
  dtm_data <- terra::as.data.frame(dtm_terra, xy = TRUE, na.rm = TRUE)

  colnames(dtm_data) <- c('x', 'y', 'z')
  
  lat_elev_profile <- stats::aggregate(dtm_data['z'], by = dtm_data['y'], FUN = max)
  
  return(lat_elev_profile)
}
