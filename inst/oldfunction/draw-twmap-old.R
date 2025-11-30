#' @title Draw Taiwan Map with DTM, River, and Species Distribution Data
#'
#' @description Generates a map of Taiwan displaying Digital Terrain Model (DTM) 
#' (using preloaded data c0-c8), river networks, and the island boundary.
#'
#' @param theme Integer. The color theme to use: 1 (color), 2 (gray scale), 3 (dark).
#' @param lwd Numeric. Line width for boundaries and grids.
#' @param river Logical. If \code{TRUE} (default), the river network is plotted.
#'
#' @return The function is used for its side effect (plotting) and returns \code{NULL} invisibly.
#' @export
#'
#' @importFrom graphics abline axis box lines par rect text plot
#' @importFrom utils data
#' 
draw.twmap <- function(theme, lwd, river=TRUE){
  # define colors (terrain.colors series)
  if ( theme == 1 ) {
    my.colors <- c("#B0C48CFF","#B0C47CFF", "#F2EEA2FF", "#F2E096FF",
                   "#F2CE85FF", "#D9A982FF", "#C28C7CFF", "#E3B8C1FF", "#FFF2FFFF","#63B8FFAA")
  } else if ( theme == 2 ) {
    my.colors <-c ("#37AA0050","#5FBE0050","#8CD20050","#BEE60050",
                   "#FFDE0050","#FFBE0050","#FF820050","#FF460050","#963C0050","#FFFFFFFF")
  } else if ( theme == 3 ) {
    my.colors <- c("#000000FF", "#191919FF", "#323232FF", "#4B4B4BFF",
                   "#646464FF", "#7D7D7DFF", "#969696FF", "#AFAFAFFF", "#C8C8C8FF", "#FFFFFFFF")
  } else {
    base::print("Please choose template number: 1 (color) or 2 (gray scale)")
    return(invisible(NULL))
  }
  
  gridlwd <- lwd
  
  utils::data(c0,c1,c2,c3,c4,c5,c6,c7,c8,twbound,river)
  
  # set parameters for page margin
  graphics::par(oma=c(2,2,2,3.5))
  # set paste opacity (default is F)
  graphics::par(xpd=FALSE)
  base::print("Generating map(s), how about have a cup of coffee and take a rest?")
  
  # plot shp files
  o.ylim <- c(2378200,2832000)
  
  if ( river == TRUE ) {
    river.lwd <- 2

    graphics::plot(twbound, ylim=o.ylim)
    graphics::plot(c0, add=TRUE, col=my.colors[1], border=FALSE)
    graphics::plot(c1, add=TRUE, col=my.colors[2], border=FALSE)
    graphics::plot(c2, add=TRUE, col=my.colors[3], border=FALSE)
    graphics::plot(c3, add=TRUE, col=my.colors[4], border=FALSE)
    graphics::plot(c4, add=TRUE, col=my.colors[5], border=FALSE)
    graphics::plot(c5, add=TRUE, col=my.colors[6], border=FALSE)
    graphics::plot(c6, add=TRUE, col=my.colors[7], border=FALSE)
    graphics::plot(c7, add=TRUE, col=my.colors[8], border=FALSE)
    graphics::plot(c8, add=TRUE, col=my.colors[9], border=FALSE)
    # graphics::plot(river, col=my.colors[10], add=TRUE, lwd=river.lwd)
    graphics::plot(twbound, add=TRUE, lwd=2)
  }
  else {
    graphics::plot(twbound, ylim=o.ylim)
    graphics::plot(c0, add=TRUE, col=my.colors[1], border=FALSE)
    graphics::plot(c1, add=TRUE, col=my.colors[2], border=FALSE)
    graphics::plot(c2, add=TRUE, col=my.colors[3], border=FALSE)
    graphics::plot(c3, add=TRUE, col=my.colors[4], border=FALSE)
    graphics::plot(c4, add=TRUE, col=my.colors[5], border=FALSE)
    graphics::plot(c5, add=TRUE, col=my.colors[6], border=FALSE)
    graphics::plot(c6, add=TRUE, col=my.colors[7], border=FALSE)
    graphics::plot(c7, add=TRUE, col=my.colors[8], border=FALSE)
    graphics::plot(c8, add=TRUE, col=my.colors[9], border=FALSE)
    graphics::plot(twbound, add=TRUE, lwd=2)
  }
  
  # plot grid
  grid_lat <- c(2434907.81937665,2544632.70497163,
                2655023.12495712,2766149.87190775)
  grid_long <- c(147484.07577807, 250000.00000000, 350943.31271824)
  latdeg <- base::parse(text=base::paste(22:25,"*degree~", "N", sep=""))
  longdeg <- base::parse(text=base::paste(120:122,"*degree~", "E", sep=""))
  
  # plot tropical of cancer
  tcancer_lat <- 2601073.49484586
  graphics::abline(h=tcancer_lat, b=0, lwd=gridlwd, lty=2, col="gray")
  # plot longitudinal and latitudinal grids
  graphics::abline(v=grid_long, lwd=gridlwd, col="gray")
  graphics::abline(h=grid_lat, b=0, lwd=gridlwd, col="gray")
  
  # draw grid legend for bottom, left, top and right
  graphics::axis(side=1, grid_long, longdeg)
  graphics::axis(side=3, grid_long, longdeg)
  graphics::axis(side=2, grid_lat, latdeg, las=3)
  graphics::axis(side=4, grid_lat, latdeg, las=3)
  
  # draw a box
  graphics::box(lwd=lwd)
  
  # draw elevation legend
  for ( i in 1:9 ){
    graphics::rect(60000,2355000+15000*i,65000,2370000+15000*i,col=my.colors[i], lwd=0)
  }
  graphics::rect(60000-10,2355000+15000-10,65000+10,2370000+15000*9+10, lwd=3)
  
  graphics::par(xpd=TRUE)
  for ( i in 0:9 ) {
    if ( i <= 2 ) {
      legend_elev <- base::paste(i*250,"", sep="")
    } else if ( i > 2 & i < 9 ) {
      legend_elev <- base::paste((i-1)*500,"", sep="")
    } else if ( i == 9 ) {
      legend_elev <- 3952
    } else base::print("Exception caught!")
    graphics::text(68000,2355000+15000*(i+1), label=legend_elev, adj=0)
  }
  
  graphics::par(xpd=TRUE)
  graphics::text(55000,2520000,label="Elevation (m)", pos=4)
  graphics::par(xpd=FALSE)
  
  # plot species data
  # plot.xy(xy.coords(species.data), lwd=3, pch=sp.pch, type="p", col=sp.col)
  
  return(invisible(NULL))
}