#' Plot Species Distribution Map with Vertical Profile
#'
#' This function generates a map displaying species distribution points (x, y) 
#' and their corresponding elevation profiles (y, z). It uses Base R graphics.
#'
#' @param pts A data frame or list with three columns: x (longitude), y (latitude), and z (elevation).
#' @param boundary Character. File path or name of the boundary shapefile (to be loaded as an sf object).
#' @param col Character or vector of characters. Color(s) used for plotting the species points.
#' @param vert_prof A data frame or list containing the main island's elevation profile data 
#'   (typically from the `elevprof` dataset loaded via \code{data()}).
#' @param cex Numeric. Magnification factor for plotting symbols. Default is 0.3.
#' @param pch Numeric. Plotting character used for points. Default is 19 (solid circle).
#'
#' @return The function is used for its side effect (plotting) and returns \code{NULL} invisibly.
#' @export
#'
#' @importFrom graphics abline axis box lines par segments text plot.xy
#' @importFrom grDevices xy.coords
#' @importFrom sf st_read
#' @importFrom methods as
#'
distrmap <- function (pts,
                      boundary,
                      col,
                      vert_prof,
                      cex = 0.3,
                      pch = 19) {
  
  basemap <- function (boundary, vert_prof) {
    vert_bound <- 122.25
    ly <- 21.8
    uy <- 25.4
    vmar <- 0.1
    
    tw_sf <- sf::st_read(boundary, quiet = TRUE)
    tw_boundary <- methods::as(tw_sf, "Spatial")
    
    graphics::plot(
      tw_boundary,
      col = "white",
      border = "#aaaaaa",
      xlim = c(119.8, 123),
      ylim = c(21.5, 25.5),
      axes = FALSE
    )
    
    # draw tropical of cancer
    graphics::abline(h = 23.5,
                     lty = 2,
                     col = 'grey')
    
    axis_latlab <- base::parse(text = base::paste(22:25, "*degree~", "N", sep = ""))
    
    graphics::axis(
      side = 2,
      lwd = 0.8,
      labels = axis_latlab ,
      lwd.ticks = 0.5,
      at = 22:25,
      cex = 0.8
    )
    
    axis_longlab <- base::parse(text = base::paste(120:122, "*degree~", "E", sep = ""))
    
    graphics::axis(
      side = 1,
      labels = axis_longlab,
      at = 120:122,
      line = NA,
      lwd = 0.8,
      cex = 0.8
    )
    
    # graphics::box()
    graphics::par(new = TRUE)
    
    graphics::segments(vert_bound - vmar, ly, vert_bound - vmar, uy)
    graphics::segments(vert_bound - vmar, ly, vert_bound + 1, ly)
    
    for (i in 0:4) {
      # draw for elevation profile
      graphics::segments(vert_bound + i / 4, ly - 0.05, vert_bound + i / 4, ly)
      ticlab <- i * 1000
      graphics::text(vert_bound + i / 4, ly - 0.1, ticlab, cex = 0.7)
    }
    
    graphics::text(vert_bound + 2.3 / 4, ly - 0.2, 'Elevation (m)', cex = 0.8)
    
    for (i in 1:4) {
      graphics::segments(vert_bound + i / 4,
                         ly,
                         vert_bound + i / 4,
                         uy,
                         col = 'grey',
                         lty = 1)
    }
    
    # draw latitude-elevation profile
    vert_prof$z <- vert_bound + (vert_prof$z / 4000)
    vf <- base::cbind(vert_prof$z, vert_prof$y)
    
    graphics::plot.xy(
      grDevices::xy.coords(vf),
      type = 'p',
      col = 'white',
      pch = 19,
      cex = 0.5
    )
    
    graphics::lines(vf, col = 'darkgrey')
    graphics::box()
  }
  
  # --- draw_sp_pts ---
  draw_sp_pts <- function(pts,
                          col,
                          cex = 0.3,
                          pch = 19) {
    
    vert_bound <- 122.25
    
    if (base::dim(pts)[2] != 3) {
      base::print('The input points should have x (longitude), y (latitude) and z (elevation)')
      
    } else {
      horizon_pts <- base::cbind(pts$x, pts$y)
      base::colnames(horizon_pts) <- c('x', 'y')
      
      pts$z <- vert_bound + (pts$z / 4000)
      
      vert_pts <- base::cbind(pts$z, pts$y)
      base::colnames(vert_pts) <- c('z', 'y')
      
      graphics::plot.xy(
        grDevices::xy.coords(horizon_pts),
        type = 'p',
        col = col,
        pch = pch,
        cex = cex
      )
      
      graphics::plot.xy(
        grDevices::xy.coords(vert_pts),
        type = 'p',
        col = col,
        pch = pch,
        cex = cex
      )
    }
  }
  
  basemap(boundary, vert_prof)
  draw_sp_pts(pts, col)
}
