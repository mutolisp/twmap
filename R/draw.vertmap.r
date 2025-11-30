#' Draw Vertical Elevation Profile Map of Taiwan
#'
#' Draws the latitude-elevation profile of Taiwan's main island based on the `elevprof` data.
#' This profile shows the relationship between elevation and latitude/TWD97 Y coordinate.
#'
#' @param coorsys An integer specifying the coordinate system for the primary axis: 84 for WGS84 Latitude (degrees), or 97 for TWD97 TM2 Y (meters).
#' @param orient An integer (1, 2, or 3) specifying the plot orientation:
#'   \itemize{
#'     \item \strong{1:} Landscape mode (Latitude/TWD97 Y on X-axis, Elevation on Y-axis).
#'     \item \strong{2:} Portrait mode (Elevation on X-axis, Latitude/TWD97 Y on Y-axis, Y-axis labels on the left).
#'     \item \strong{3:} Portrait mode (Elevation on X-axis, Latitude/TWD97 Y on Y-axis, Y-axis labels on the right).
#'   }
#' @param mountain Logical. If \code{TRUE} (default), labels for major mountain peaks are displayed.
#' @param lwd Numeric. Line width for the profile line and grid lines.
#'
#' @return The function is primarily used for its side effect (plotting the map) and returns \code{NULL} invisibly.
#' @export
#'
#' @importFrom utils data
#' @importFrom grDevices xy.coords
#' @importFrom graphics abline axis box lines plot text par
#'
#' @name draw.vertmap
#' @examples
#' \dontrun{
#' # Example 1: Landscape mode using TWD97 coordinates, with mountain labels
#' draw.vertmap(coorsys = 97, orient = 1, mountain = TRUE)
#'
#' # Example 2: Landscape mode using WGS84 coordinates, with mountain labels
#' draw.vertmap(coorsys = 84, orient = 1, mountain = TRUE)
#'
#' # Example 3: Portrait mode (for combined plots)
#' draw.vertmap(coorsys = 84, orient = 3, mountain = FALSE, lwd = 0.5)
#' }
#' 
draw.vertmap <- function(coorsys,
                         orient = 1,
                         mountain = T,
                         lwd = 1) {
  
  # NOTE: data(elevprof) will now be imported correctly via utils::data
  # The global variable 'elevprof' must be declared using globalVariables() elsewhere.
  utils::data(elevprof) 
  
  # transformation of wgs84 and twd97
  proflong <- rep(20000, dim(elevprof)[1])
  elevprof84 <- cbind(twcoor.trans(as.matrix(cbind(
    proflong, elevprof[, 1]
  )), 97, 84)[, 2], elevprof[, 2])
  # orientation selection
  if (orient == 1) {
    o.side1 <- 1
    o.side2 <- 2
    o.ylab <- "Elevation (m)"
    
    # coordinate system selection
    if (coorsys == 84) {
      elev <- cbind(elevprof84[, 1], elevprof84[, 2])
      o.xlim <- c(25.5, 21.5)
      o.ylim <- c(0, 4000)
      o.xlab <- "Latitude"
      latdeg <- parse(text = paste(25:22, "*degree~", "N", sep = ""))
      gridv <- 25:22
      gridv.sub <- 215:256 / 10
      datun.x <- 25.2
      datun.y <- 1200
      hsueh.x <- 24.35
      hsueh.y <- 4000
      yushan.x <- 23.45
      yushan.y <- 4000
      beidawu.x <- 22.6
      beidawu.y <- 3200
    } else if (coorsys == 97) {
      elev <- cbind(elevprof[, 1], elevprof[, 2])
      o.xlim <- c(2832000, 2378000)
      o.ylim <- c(0, 4200)
      o.xlab <- "TWD97 TM2 Y (meter)"
      latdeg <- c(28:24 * 100000)
      gridv <- 28:24 * 100000
      gridv.sub <- 237:283 * 10000
      datun.x <- 2790000
      datun.y <- 1200
      hsueh.x <- 2700000
      hsueh.y <- 4000
      yushan.x <- 2600000
      yushan.y <- 4000
      beidawu.x <- 2500000
      beidawu.y <- 3200
    } else
      base::print("Unsupported coordinate system!")

    graphics::plot(
      elev,
      pch = ".",
      col = "#FFFFFF00",
      ylim = o.ylim,
      xlim = o.xlim,
      axes = F,
      ylab = o.ylab,
      xlab = o.xlab
    )
    graphics::abline(
      h = c(0:4) * (1000),
      b = 0,
      lwd = 1.2 * lwd,
      col = "grey"
    )
    graphics::abline(
      h = c(0:42) * (100),
      b = 0,
      lwd = 0.5 * lwd,
      col = "grey"
    )
    graphics::abline(v = gridv,
                     lwd = 1.2 * lwd,
                     col = "grey")
    graphics::abline(v = gridv.sub,
                     lwd = 0.5 * lwd,
                     col = "grey")
    graphics::lines(elev, lwd = lwd)
    
    # draw axis
    graphics::axis(side = o.side1, gridv, latdeg, las = 1)
    graphics::axis(side = o.side2,
                   1000 * (0:4),
                   cbind("0", "1000", "2000", "3000", "4000"))
    
    # draw frame
    graphics::box(lwd = lwd)
    if (mountain == T) {
      graphics::text(datun.x, datun.y, labels = 'Datun Shan')
      graphics::text(hsueh.x, hsueh.y, labels = 'Hsueh Shan')
      graphics::text(yushan.x, yushan.y, labels = 'Yu Shan')
      graphics::text(beidawu.x, beidawu.y, labels = 'BeiDawu Shan')
    }
    
  } else if (orient == 2) {
    o.side1 <- 2
    o.side2 <- 1
    o.xlab <- "Elevation (m)"
    
    # coordinate system selection
    if (coorsys == 84) {
      elev <- cbind(elevprof84[, 2], elevprof84[, 1])
      o.xlim <- c(4200, 0)
      o.ylim <- c(21.5, 25.5)
      o.ylab <- "Latitude"
      latdeg <- parse(text = paste(22:25, "*degree~", "N", sep = ""))
      gridv <- 22:25
      gridv.sub <- 215:256 / 10
      datun.y <- 25.2
      datun.x <- 1200
      hsueh.y <- 24.35
      hsueh.x <- 4000
      yushan.y <- 23.45
      yushan.x <- 4000
      beidawu.y <- 22.6
      beidawu.x <- 3200
    } else if (coorsys == 97) {
      elev <- cbind(elevprof[, 2], elevprof[, 1])
      o.xlim <- c(4200, 0)
      o.ylim <- c(2378000, 2832000)
      o.ylab <- "TWD97 TM2 Y (meter)"
      latdeg <- c(24:28 * 100000)
      gridv <- 24:28 * 100000
      gridv.sub <- 238:283 * 10000
      datun.y <- 2790000
      datun.x <- 1200
      hsueh.y <- 2700000
      hsueh.x <- 4000
      yushan.y <- 2600000
      yushan.x <- 4000
      beidawu.y <- 2500000
      beidawu.x <- 3200
    } else
      base::print("Unsupported coordinate system!")
    
    graphics::plot(
      elev,
      pch = ".",
      col = "#FFFFFF00",
      ylim = o.ylim,
      xlim = o.xlim,
      axes = F,
      ylab = o.ylab,
      xlab = o.xlab
    )
    graphics::abline(v = c(0:4) * (1000),
                     lwd = 1.2 * lwd,
                     col = "grey")
    graphics::abline(v = c(0:42) * (100),
                     lwd = 0.5 * lwd,
                     col = "grey")
    graphics::abline(
      h = gridv,
      b = 0,
      lwd = 1.2 * lwd,
      col = "grey"
    )
    graphics::abline(
      h = gridv.sub,
      b = 0,
      lwd = 0.5 * lwd,
      col = "grey"
    )
    graphics::lines(elev, lwd = lwd)
    
    # draw axis
    graphics::axis(side = o.side1, gridv, latdeg, las = 3)
    graphics::axis(side = o.side2,
                   1000 * (0:4),
                   cbind("0", "1000", "2000", "3000", "4000"))
    
    # draw frame
    graphics::box(lwd = lwd)
    if (mountain == T) {
      graphics::text(datun.x, datun.y, labels = 'Datun Shan', srt = 90)
      graphics::text(hsueh.x, hsueh.y, labels = 'Hsueh Shan', srt = 90)
      graphics::text(yushan.x, yushan.y, labels = 'Yu Shan', srt = 90)
      graphics::text(beidawu.x, beidawu.y, labels = 'BeiDawu Shan', srt = 90)
    }
    
  } else if (orient == 3) {
    o.side1 <- 4
    o.side2 <- 1
    o.xlab <- "Elevation (m)"
    
    # coordinate system selection
    if (coorsys == 84) {
      elev <- cbind(elevprof84[, 2], elevprof84[, 1])
      o.xlim <- c(0, 4200)
      o.ylim <- c(21.5, 25.5)
      o.ylab <- "Latitude"
      latdeg <- parse(text = paste(22:25, "*degree~", "N", sep = ""))
      gridv <- 22:25
      gridv.sub <- 215:256 / 10
      datun.y <- 25.2
      datun.x <- 1200
      hsueh.y <- 24.35
      hsueh.x <- 4000
      yushan.y <- 23.45
      yushan.x <- 4000
      beidawu.y <- 22.6
      beidawu.x <- 3200
    } else if (coorsys == 97) {
      elev <- cbind(elevprof[, 2], elevprof[, 1])
      o.xlim <- c(0, 4200)
      o.ylim <- c(2382000, 2837000)
      o.ylab <- "TWD97 TM2 Y (meter)"
      latdeg <- c(24:28 * 100000)
      gridv <- 24:28 * 100000
      gridv.sub <- 238:283 * 10000
      datun.y <- 2790000
      datun.x <- 1200
      hsueh.y <- 2700000
      hsueh.x <- 4000
      yushan.y <- 2600000
      yushan.x <- 4000
      beidawu.y <- 2500000
      beidawu.x <- 3200
    } else
      base::print("Unsupported coordinate system!")
    
    #par(oma=c(0,0,0,4))
    par(mgp = c(1.2, 0.25, 0))
    graphics::plot(
      elev,
      pch = ".",
      col = "#FFFFFF00",
      ylim = o.ylim,
      xlim = o.xlim,
      axes = F,
      ylab = "",
      xlab = o.xlab
    )
    #mtext(o.ylab, side=4, las=3, outer=T)
    graphics::abline(v = c(0:4) * (1000),
                     lwd = 0.8 * lwd,
                     col = "gray")
    #graphics::abline(v = c(0:42) * (100),
    #                 lwd = 0.5 * lwd,
    #                 col = "grey")
    #graphics::abline(
    #  h = gridv,
    #  b = 0,
    #  lwd = 1.2 * lwd,
    #  col = "grey"
    #)
    #graphics::abline(
    #  h = gridv.sub,
    #  b = 0,
    #  lwd = 0.5 * lwd,
    #  col = "grey"
    #)
    graphics::lines(elev, lwd = lwd, col = "#aaaaaa")
    
    # draw axis
    # graphics::axis(side = o.side1, gridv, latdeg, las = 3)
    graphics::axis(side = o.side2,
                   1000 * (0:4),
                   cbind("0", "1000", "2000", "3000", "4000"),
                   cex.axis = 0.65,
                   las = 1,
                   font = 2)
    #graphics::axis(side = o.side2 + 2, # side 1 + 2 = side 3 (top)
    #               1000 * (0:4),
    #               cbind("0", "1000", "2000", "3000", "4000"))
    
    # draw frame
    graphics::box(lwd = lwd)
    if (mountain == T) {
      graphics::text(datun.x, datun.y, labels = 'Datun Shan', srt = 90)
      graphics::text(hsueh.x, hsueh.y, labels = 'Hsueh Shan', srt = 90)
      graphics::text(yushan.x, yushan.y, labels = 'Yu Shan', srt = 90)
      graphics::text(beidawu.x, beidawu.y, labels = 'BeiDawu Shan', srt = 90)
    }
    
  } else
    base::print("orientation = 1(Landscape) / 2(Portrait left) / 3(Portrait right)")
}

