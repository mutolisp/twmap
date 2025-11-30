#' Draw Taiwan Map with Elevation and Optional River Data
#'
#' This function plots the Taiwan map using pre-processed elevation classification
#' data (c0-c8) and boundary data (twbound), which are stored as sf objects.
#' It uses Base R graphics for plotting.
#'
#' @param theme An integer (1, 2, 3, or 4) selecting the color theme for the map:
#'   \itemize{
#'     \item \strong{1:} Terrain Colors (Default).
#'     \item \strong{2:} Greens/Oranges with transparency.
#'     \item \strong{3:} Grayscale.
#'     \item \strong{4:} Black/White Border only (No elevation fill).
#'   }
#' @param lwd Numeric. The default line width for boundaries and grids.
#' @param draw_river Logical. If \code{TRUE} (default), the river lines from the \code{river} dataset are drawn (only for theme 1-3).
#'@param draw_grid_lines Logical. If \code{TRUE} (default), longitudinal and latitudinal grid lines are drawn.
#' @return The function is used for its side effect (plotting the map) and returns \code{NULL} invisibly.
#' @export
#'
#' @import methods
#' @importFrom utils data
#' @importFrom graphics abline axis box lines plot text rect par
#' @importFrom sf st_as_sf st_coordinates st_set_crs st_transform
#'
#' @name draw.twmap
#' @examples
#' \dontrun{
#' # Example 1: Default Terrain Colors with all elements
#' draw.twmap(theme = 1, lwd = 1)
#'
#' # Example 2: DTM map, no rivers, show grid lines
#' draw.twmap(theme = 3, draw_river = FALSE, draw_grid_lines = TRUE)
#'
#' # Example 3: Black/White Border Only (Minimal map)
#' draw.twmap(theme = 4, lwd = 1, draw_river = FALSE, draw_grid_lines = FALSE)
#' }
draw.twmap <- function(theme, lwd, draw_river = TRUE, draw_grid_lines = TRUE) {
  
  # --- A. Color Definition ---
  if ( theme == 1 ) {
    my.colors <- c("#B0C48CFF","#B0C47CFF", "#F2EEA2FF", "#F2E096FF",
                   "#F2CE85FF", "#D9A982FF", "#C28C7CFF", "#E3B8C1FF", "#FFF2FFFF","#63B8FFAA")
  } else if ( theme == 2 ) {
    my.colors <-c ("#37AA0050","#5FBE0050","#8CD20050","#BEE60050",
                   "#FFDE0050","#FFBE0050","#FF820050","#FF460050","#963C0050","#FFFFFFFF")
  } else if ( theme == 3 ) {
    my.colors <- c("#000000FF", "#191919FF", "#323232FF", "#4B4B4BFF",
                   "#646464FF", "#7D7D7DFF", "#969696FF", "#AFAFAFFF", "#C8C8C8FF", "#FFFFFFFF")
  } else if ( theme == 4 ) {
    # theme 4 (Black solid border map)
    my.colors <- c("black", rep(NA, 9)) # Only black is needed
  } else {
    base::print("Please choose template number: 1 (color), 2 (gray), 3 (dark), or 4 (border)")
    return(invisible(NULL))
  }
  
  # --- B. Data Loading and Coordinate Transformation (TM2 -> WGS84) ---
  gridlwd <- lwd
  
  # Load data (Assuming they are TWD97 TM2 / EPSG: 3826)
  utils::data(list = c("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "twbound", "river"), package = "twmap")
  
  ORIGINAL_CRS <- 3826
  
  c0 <- sf::st_set_crs(c0, ORIGINAL_CRS)
  c1 <- sf::st_set_crs(c1, ORIGINAL_CRS)
  c2 <- sf::st_set_crs(c2, ORIGINAL_CRS)
  c3 <- sf::st_set_crs(c3, ORIGINAL_CRS)
  c4 <- sf::st_set_crs(c4, ORIGINAL_CRS)
  c5 <- sf::st_set_crs(c5, ORIGINAL_CRS)
  c6 <- sf::st_set_crs(c6, ORIGINAL_CRS)
  c7 <- sf::st_set_crs(c7, ORIGINAL_CRS)
  c8 <- sf::st_set_crs(c8, ORIGINAL_CRS)
  twbound <- sf::st_set_crs(twbound, ORIGINAL_CRS)
  river <- sf::st_set_crs(river, ORIGINAL_CRS)
  
  TARGET_CRS <- 4326
  
  # Transform twbound and river
  twbound_wgs84 <- sf::st_transform(twbound, crs = TARGET_CRS)
  if (draw_river == TRUE) {
    river_wgs84 <- sf::st_transform(river, crs = TARGET_CRS)
  }
  
  # Transform DEM layers (only needed for theme 1-3)
  if (theme %in% 1:3) {
    elevation_layers <- list(c0, c1, c2, c3, c4, c5, c6, c7, c8)
    elevation_layers_wgs84 <- lapply(elevation_layers, function(x) sf::st_transform(x, crs = TARGET_CRS))
  }
  
  # --- C. Set Graphics Parameters (WGS84 Range) ---
  graphics::par(oma = c(2, 2, 2, 3.5))
  graphics::par(xpd = FALSE)
  base::print("Generating map(s), how about have a cup of coffee and take a rest?")
  
  # WGS84 plotting range (Longitude and Latitude)
  o.xlim <- c(119.3, 122.1)
  o.ylim <- c(21.5, 25.5)
  
  # --- D. Graphics Initialization and Grid Drawing ---
  
  # 1. Initialize canvas (using WGS84 boundaries)
  graphics::plot(twbound_wgs84$geometry,
                 xlim = o.xlim,
                 ylim = o.ylim,
                 main = "",
                 reset = FALSE,
                 col = NA, # Do not fill color
                 lwd = 0.1)
  
  # WGS84 grid line coordinates
  grid_long <- c(120, 121, 122)
  grid_lat <- c(22, 23, 24, 25)
  tcancer_lat <- 23.5 # Tropic of Cancer (WGS84)
  
  # 2. Grid and Tropic of Cancer (Drawn at the bottom layer)
  graphics::abline(h = tcancer_lat, b = 0, lwd = gridlwd, lty = 2, col = "gray")
  
  if (draw_grid_lines) {
    graphics::abline(v = grid_long, lwd = gridlwd, col = "gray")
    graphics::abline(h = grid_lat, b = 0, lwd = gridlwd, col = "gray")
  }
  
  # --- E. Draw Core Map Content ---
  
  if (theme %in% 1:3) {
    # Draw elevation layers
    for (i in 1:9) {
      graphics::plot(elevation_layers_wgs84[[i]]$geometry,
                     add = TRUE,
                     col = my.colors[i],
                     border = FALSE)
    }
    
    # Draw rivers
    if (draw_river == TRUE) {
      graphics::plot(river_wgs84$geometry, col = my.colors[10], add = TRUE, lwd = 2)
    }
    
  } else if (theme == 4) {
    # theme 4: Black/White border only (filling with white, no border on fill layer)
    graphics::plot(twbound_wgs84$geometry, add = TRUE, col = "white", border = NA)
  }
  
  # Draw final boundary line (Overlays other layers)
  graphics::plot(twbound_wgs84$geometry, add = TRUE, lwd = 1, border = "grey30", col = NA)
  
  # --- F. Axes (WGS84 Labels) ---
  
  latdeg <- base::parse(text = base::paste(grid_lat, "*degree~", "N", sep=""))
  longdeg <- base::parse(text = base::paste(grid_long, "*degree~", "E", sep=""))
  
  graphics::axis(side = 1, grid_long, longdeg)
  #graphics::axis(side = 3, grid_long, longdeg)
  graphics::axis(side = 2, grid_lat, latdeg, las = 3)
  #graphics::axis(side = 4, grid_lat, latdeg, las = 3)
  
  graphics::box(lwd = lwd)
  
  # --- G. Legend (WGS84 Coordinates) ---
  
  if (theme %in% 1:3) {
    # Draw legend in WGS84 coordinates (Based on estimated position in the margin)
    # TM2(60000, 2370000) is roughly WGS84(119.95, 21.8) (This is an estimate)
    legend_x_start <- 119.3
    legend_y_start <- 21.5
    legend_width <- 0.1
    legend_height_unit <- 0.1 # Adjusted unit height to fit WGS84 range
    
    # Draw color blocks
    for (i in 1:9) {
      graphics::rect(legend_x_start,
                     legend_y_start + legend_height_unit * (i - 1),
                     legend_x_start + legend_width,
                     legend_y_start + legend_height_unit * i,
                     col = my.colors[i], lwd = 0)
    }
    
    # Draw legend frame
    graphics::rect(legend_x_start,
                   legend_y_start,
                   legend_x_start + legend_width,
                   legend_y_start + legend_height_unit * 9.5,
                   lwd = 3)
    
    # Draw labels
    graphics::par(xpd = TRUE)
    elev_labels <- c(0, 250, 500, 1000, 1500, 2000, 2500, 3000, 3500, 3952)
    
    graphics::text(legend_x_start + legend_width + 0.05,
                   legend_y_start + legend_height_unit * 0,
                   label = elev_labels[1], adj = 0)
    
    for (i in 1:9) {
      graphics::text(legend_x_start + legend_width + 0.05,
                     legend_y_start + legend_height_unit * i,
                     label = elev_labels[i + 1], adj = 0)
    }
    
    graphics::text(legend_x_start - 0.08,
                   legend_y_start + legend_height_unit * 10,
                   label = "Elevation (m)", pos = 4, srt = 0)
    
    graphics::par(xpd = FALSE)
  }
  # theme 4 does not need a legend, so section G is skipped
  
  return(invisible(NULL))
}
