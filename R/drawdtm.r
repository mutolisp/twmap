# draw Taiwan map with DTM, river and species distribution data

drawtwmap <- function(species.data, sp.pch, sp.col, lwd, png=T, file.name) {
    if ( png == T ) {
        png(file=file.name, width=2480, height=3509, pointsize=48)
            drawDTMSP(species.data, sp.pch, sp.col, lwd)
        dev.off()
    }
    else 
        drawDTMSP(species.data, sp.pch, sp.col, lwd)
}

drawDTMSP <- function(species.data, sp.pch, sp.col, lwd){
    # define colors (terrain.colors series)
    my.colors <- c("#B0C47CFF", "#F2EEA2FF", "#F2E096FF", "#F2CE85FF", "#D9A982FF", "#C28C7CFF", "#E3B8C1FF", "#FFF2FFFF")

    library(maptools)
    #library(rgdal)
    library(lattice)

    gridlwd <- lwd
    river.lwd <- 2
    # load r binary data (processed with readShapePoly(), readShapeLines()
    # in maptools package
    data(c1,c2,c3,c4,c5,c6,c7,c8,twbound,river)
   
    # set parameters for page margin
    par(oma=c(2,2,2,3.5))
    # set paste opacity (default is F)
    par(xpd=F)
    print("Generating map(s), how about have a cup of coffee and take a rest?")
    # plot shp files
    plot(twbound)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="#63B8FFAA")
    plot(c1, add=T, col=my.colors[1], border=F)
    plot(c2, add=T, col=my.colors[2], border=F)
    plot(c3, add=T, col=my.colors[3], border=F)
    plot(c4, add=T, col=my.colors[4], border=F)
    plot(c5, add=T, col=my.colors[5], border=F)
    plot(c6, add=T, col=my.colors[6], border=F)
    plot(c7, add=T, col=my.colors[7], border=F)
    plot(c8, add=T, col=my.colors[8], border=F)
    plot(twbound, add=T, lwd=2)
    plot(river, col="#63B8FFAA", add=T, lwd=river.lwd)

    # plot grid
    grid_lat <- c(2434907.81937665,2544632.70497163,
        2655023.12495712,2766149.87190775)
    grid_long <- c(147484.07577807, 250000.00000000, 350943.31271824)
    latdeg <-parse(text=paste(22:25,"*degree~", "N", sep=""))
    longdeg <-parse(text=paste(120:122,"*degree~", "E", sep=""))
    # plot tropical of cancer
    tcancer_lat <- 2601073.49484586
    abline(h=tcancer_lat, b=0, lwd=gridlwd, lty=2, col="gray")
    # plot longitudinal and latitudinal grids
    abline(v=grid_long, lwd=gridlwd, col="gray")
    abline(h=grid_lat, b=0, lwd=gridlwd, col="gray")
    # draw grid legend for bottom, left, top and right
    axis(side=1, grid_long, longdeg)
    axis(side=3, grid_long, longdeg)
    axis(side=2, grid_lat, latdeg, las=3)
    axis(side=4, grid_lat, latdeg, las=3)
    # draw a box
    box(lwd=gridlwd)
    
    # draw elevation legend
    for ( i in 1:8 ){
        rect(75000,2400000+15000*i,80000,2415000+15000*i,col=my.colors[i], lwd=0)
    }
    rect(75000-10,2400000+15000-10,80000+10,2415000+15000*8+10, lwd=3)
    par(xpd=T)
    for ( i in 0:7 ) {
        legend_elev <- paste(i*500,"", sep="")
        text(83000,2400000+15000*(i+1), label=legend_elev, adj=0)
    }
    par(xpd=T)
    text(70000,2540000,label="Elevation (m)", pos=4)
    par(xpd=NA)
   
    # plot species data
    plot.xy(xy.coords(species.data), lwd=3, pch=sp.pch, type="p", col=sp.col)
}

twcoor.trans <- function(coords, src, dst){
    # projection definition (from Proj.4, coordinate systems see http://spatialreference.org)
    # Taiwan Datum 1997 Transverse Mercator EPSG: 3826 
    TWD97TM2 <- "+proj=tmerc +ellps=GRS80 +lon_0=121 +x_0=250000 +k=0.9999 +units=m +no_defs"
    # Taiwan Datum 1967 Transverse Mercator
    TWD67TM2 <- "+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=aust_SA +units=m +towgs84=-752,-358,-179,-0.0000011698,0.0000018398,0.0000009822,0.00002329 +no_defs"
    # WGS84 Longitude-latitude 
    WGS84 <- "+proj=latlong +ellps=WGS84 +datum=WGS84 +no_defs"

    library(proj4)

     #ptransform(coords, src.proj=src, dst.proj=dst)/pi*180

     if ( src == 84 ){
         #src.proj <- "+proj=latlong +ellps=WGS84 +datum=WGS84 +no_defs"
         src.proj <- WGS84
         dcoords <- coords/180*pi
         if ( dst == 97 ){
            dst.proj <- TWD97TM2 
         } else if ( dst == 67 ) {
            dst.proj <- TWD67TM2
         } else if ( dst == 84 ) {
            coords
         } else print("Unsupported coordinate system!")
         ptransform(dcoords, src.proj, dst.proj)[, 1:2]

     } else if ( src == 97 ) {
         src.proj <- TWD97TM2
         if ( dst == 84 ) {
             dst.proj <- WGS84
             ptransform(coords, src.proj, dst.proj)/pi*180[, 1:2]
         } else if ( dst == 67 ){
             dst.proj <- TWD67TM2
             print("Under construction!")
         } else if ( dst == 97 ) {
             coords
         } else print("Unsupported coordinate system!")

     } else if ( src == 67 ) {
         src.proj <- TWD67TM2
         if ( dst == 84 ) {
             dst.proj <- WGS84
             ptransform(coords, src.proj, dst.proj)/pi*180[, 1:2]
         } else if ( dst == 97 ){
             dst.proj <- TWD97TM2
             print("Under construction!")
         } else if ( dst == 67 ) {
             coords
         } else print("Unsupported coordinate system!")
     }
     else print("Exception caught! Please report a bug! Thank you")
}
