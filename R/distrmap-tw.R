#' @title Draw Species Distribution Map and Vertical Profile for Taiwan (Simplified)
#'
#' @description This is a simplified wrapper function that draws the species
#' distribution points (\code{pts}) on the Taiwan map and displays the
#' corresponding vertical elevation profile, automatically using the package's
#' built-in \code{twbound} and \code{elevprof} datasets.
#'
#' @param pts A data frame containing species distribution points. Must include
#'   at least three columns: \code{x}, \code{y} (WGS84 or TWD97 horizontal coordinates),
#'   and \code{z} (elevation in meters).
#' @param theme Integer. The map style to draw. \code{1} for colored terrain
#'   (using \code{draw.twmap}), \code{3} for black/white border only, etc.
#' @param cex Numeric. Magnification factor for plotting symbols. Default is 0.4.
#' @param pch Numeric. Plotting character used for points. Default is 19 (solid circle).
#' @param col Character or vector of characters. Color(s) used for plotting the species points. Default is "black".
#' @param ... Additional graphical parameters passed to \code{graphics::points()} for the map points.
#'
#' @export
#'
#' @importFrom graphics layout par plot points box
#' @importFrom utils data
#'
#' @examples
#' \dontrun{
#' # Assuming functions like draw.twmap and draw.vertmap exist in the package.
#' 
#' # 1. Load the example species distribution data for Taiwan
#' utils::data("sample_spts.tw") 
#' 
#' # Example 1: Colored terrain map (theme=1)
#' # Note: The vertical profile uses default longitudinal orientation (latitude vs. elevation)
#' distrmap.tw(sample_spts.tw, theme = 1, pch = 16, col = "red")
#' 
#' # Example 2: Black/white border map (theme=4)
#' distrmap.tw(sample_spts.tw, theme = 4, pch = 1, col = "blue")
#' }
distrmap.tw <- function(pts, theme = 1, cex = 0.4, pch = 19, col = "black", ...) {
  
  # --- I. Parameter Check and Data Preparation ---
  if (!all(c("x", "y", "z") %in% names(pts))) {
    stop("Input 'pts' must be a data frame containing 'x', 'y', and 'z' columns.")
  }
  
  # --- II. Set Graphics Layout (1 Row, 2 Columns) ---
  old.par <- graphics::par(no.readonly = TRUE)
  # Ensure graphical parameters are reset upon exit
  on.exit(graphics::par(old.par))
  
  # Layout the plots: Map (3 units wide), Profile (1 unit wide)
  graphics::layout(matrix(c(1, 2), 1, 2, byrow = TRUE), widths = c(3, 1))
  # Map (Left plot) margins: bottom, left, top, right (right margin is 0)
  graphics::par(mar = c(5, 4, 4, 0)) 
  
  # --- III. Draw Map ---
  
  if (theme %in% 1:4) {
    # 1. Draw the map (using draw.twmap function, assumed external)
    draw.twmap(theme = theme, lwd = 1, draw_river = FALSE, draw_grid_lines = FALSE)
    
  } else {
    stop("Invalid 'theme' parameter. Use 1 (color), 2 (lightcor), 3 (DTM), or 4 (border).")
  }
  
  # Plot species points on the map (Horizontal Distribution)
  graphics::points(pts$x, 
                   pts$y,
                   pch = pch,
                   cex = cex,
                   col = col,
                   ...) # Pass additional graphical parameters
  
  # --- IV. Draw Vertical Profile Plot ---
  
  # Vertical Profile (Right plot) margins: bottom, left, top, right (left margin is 0)
  graphics::par(mar = c(5, 0, 4, 2)) 
  
  # 1. Draw the vertical profile background (using draw.vertmap function, assumed external)
  # Load elevation profile data first (assumed necessary for draw.vertmap)
  utils::data("elevprof", package = "twmap")
  
  # Draw vertical profile background
  # We assume pts$y are WGS84 latitude coordinates.
  draw.vertmap(
    coorsys = 84, # Assumes pts$y is WGS84 latitude
    orient = 3, # Orientation/profile type (fixed by existing code)
    mountain = FALSE, # Do not draw mountain profile background
    lwd = 1.5 
  )
  
  # 2. Plot species points on the vertical profile (Vertical Distribution)
  # Note: draw.vertmap's plot space uses elevation (z) as the X-axis 
  # and latitude/distance (y) as the Y-axis.
  graphics::points(pts$z, # X-axis: Elevation (m)
                   pts$y, # Y-axis: Latitude/Distance
                   pch = pch,
                   cex = cex,
                   col = col)
  
}
