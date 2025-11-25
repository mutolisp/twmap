# draw Taiwan map with DTM, river and species distribution data

#drawtwmap <- function(lwd, png=T, file.name, col) {
#    if ( png == T ) {
#        png(file=file.name, width=2480, height=3500, pointsize=48)
#            drawDTMSP(lwd, col)
#        dev.off()
#    }
#    else
#        drawDTMSP(lwd, col)
#}

draw.twmap <- function(theme, lwd, river = T) {
  # define colors (terrain.colors series)
  if (theme == 1) {
    my.colors <- c(
      "#B0C48CFF",
      "#B0C47CFF",
      "#F2EEA2FF",
      "#F2E096FF",
      "#F2CE85FF",
      "#D9A982FF",
      "#C28C7CFF",
      "#E3B8C1FF",
      "#FFF2FFFF",
      "#63B8FFAA"
    )
  } else if (theme == 2) {
    my.colors <- c (
      "#37AA0050",
      "#5FBE0050",
      "#8CD20050",
      "#BEE60050",
      "#FFDE0050",
      "#FFBE0050",
      "#FF820050",
      "#FF460050",
      "#963C0050",
      "#FFFFFFFF"
    )
  } else if (theme == 3) {
    my.colors <- c(
      "#000000FF",
      "#191919FF",
      "#323232FF",
      "#4B4B4BFF",
      "#646464FF",
      "#7D7D7DFF",
      "#969696FF",
      "#AFAFAFFF",
      "#C8C8C8FF",
      "#FFFFFFFF"
    )
  } else
    print("Please choose template number: 1 (color) or 2 (gray scale)")
  
  # library(maptools)
  
  gridlwd <- lwd
  # load r binary data (processed with readShapePoly(), readShapeLines()
  # in maptools package
  data(c0, c1, c2, c3, c4, c5, c6, c7, c8, twbound, river)
  
  # set parameters for page margin
  par(oma = c(2, 2, 2, 3.5))
  # set paste opacity (default is F)
  par(xpd = F)
  print("Generating map(s), how about have a cup of coffee and take a rest?")
  # plot shp files
  o.ylim <- c(2378200, 2832000)
  #o.ylim <- c(2350000,2800000)
  # for ocean
  #rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=my.colors[10])
  if (river == T) {
    river.lwd <- 2
    plot(twbound, ylim = o.ylim)
    plot(c0,
         add = T,
         col = my.colors[1],
         border = F)
    plot(c1,
         add = T,
         col = my.colors[2],
         border = F)
    plot(c2,
         add = T,
         col = my.colors[3],
         border = F)
    plot(c3,
         add = T,
         col = my.colors[4],
         border = F)
    plot(c4,
         add = T,
         col = my.colors[5],
         border = F)
    plot(c5,
         add = T,
         col = my.colors[6],
         border = F)
    plot(c6,
         add = T,
         col = my.colors[7],
         border = F)
    plot(c7,
         add = T,
         col = my.colors[8],
         border = F)
    plot(c8,
         add = T,
         col = my.colors[9],
         border = F)
    #plot(river, col=my.colors[10], add=T, lwd=river.lwd)
    plot(twbound, add = T, lwd = 2)
  }
  else {
    plot(twbound, ylim = o.ylim)
    plot(c0,
         add = T,
         col = my.colors[1],
         border = F)
    plot(c1,
         add = T,
         col = my.colors[2],
         border = F)
    plot(c2,
         add = T,
         col = my.colors[3],
         border = F)
    plot(c3,
         add = T,
         col = my.colors[4],
         border = F)
    plot(c4,
         add = T,
         col = my.colors[5],
         border = F)
    plot(c5,
         add = T,
         col = my.colors[6],
         border = F)
    plot(c6,
         add = T,
         col = my.colors[7],
         border = F)
    plot(c7,
         add = T,
         col = my.colors[8],
         border = F)
    plot(c8,
         add = T,
         col = my.colors[9],
         border = F)
    plot(twbound, add = T, lwd = 2)
  }
  
  # plot grid
  grid_lat <- c(2434907.81937665,
                2544632.70497163,
                2655023.12495712,
                2766149.87190775)
  grid_long <- c(147484.07577807, 250000.00000000, 350943.31271824)
  latdeg <- parse(text = paste(22:25, "*degree~", "N", sep = ""))
  longdeg <- parse(text = paste(120:122, "*degree~", "E", sep = ""))
  # plot tropical of cancer
  tcancer_lat <- 2601073.49484586
  abline(
    h = tcancer_lat,
    b = 0,
    lwd = gridlwd,
    lty = 2,
    col = "gray"
  )
  # plot longitudinal and latitudinal grids
  abline(v = grid_long,
         lwd = gridlwd,
         col = "gray")
  abline(
    h = grid_lat,
    b = 0,
    lwd = gridlwd,
    col = "gray"
  )
  # draw grid legend for bottom, left, top and right
  axis(side = 1, grid_long, longdeg)
  axis(side = 3, grid_long, longdeg)
  axis(side = 2, grid_lat, latdeg, las = 3)
  axis(side = 4, grid_lat, latdeg, las = 3)
  # draw a box
  box(lwd = lwd)
  
  # draw elevation legend
  for (i in 1:9) {
    rect(60000,
         2355000 + 15000 * i,
         65000,
         2370000 + 15000 * i,
         col = my.colors[i],
         lwd = 0)
  }
  rect(60000 - 10,
       2355000 + 15000 - 10,
       65000 + 10,
       2370000 + 15000 * 9 + 10,
       lwd = 3)
  par(xpd = T)
  for (i in 0:9) {
    if (i <= 2) {
      legend_elev <- paste(i * 250, "", sep = "")
    } else if (i > 2 & i < 9) {
      legend_elev <- paste((i - 1) * 500, "", sep = "")
    } else if (i == 9) {
      legend_elev <- 3952
    } else
      print("Exception caught!")
    text(68000,
         2355000 + 15000 * (i + 1),
         label = legend_elev,
         adj = 0)
  }
  par(xpd = T)
  text(55000, 2520000, label = "Elevation (m)", pos = 4)
  par(xpd = F)
  
  # plot species data
  #plot.xy(xy.coords(species.data), lwd=3, pch=sp.pch, type="p", col=sp.col)
}
