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
    # Parameters for converting twd67 to twd97 and vise versa
    A <- 0.00001549
    B <- 0.000006521
    EW <- 807.8
    SN <- 248.6

    library(proj4)

    # TWD67 <-> TWD97 convertion formula
    # http://gis.thl.ncku.edu.tw/coordtrans/coordtrans.aspx
    #
    # A= 0.00001549
    # B= 0.000006521
    # E67 = E97 - 807.8 - A * E97 - B * N97
    # N67 = N97 + 248.6 - A * N97 - B * E97
    # E97 = E67 + 807.8 + A * E67 + B * N67
    # N97 = N67 - 248.6 + A * N67 + B * E67
    # (error range: within 2 meters
    # 
    # TO DO: check the precision of this and compare to defined projection
    # in EPSG

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
             ptransform(coords, src.proj, dst.proj)[, 1:2]/pi*180
         } else if ( dst == 67 ){
             dst.proj <- TWD67TM2
             Y67 <- coords[, 2] + SN - A * coords[, 2] - B * coords[, 1]
             X67 <- coords[, 1] - EW - A * coords[, 1] - B * coords[, 2]
             cbind(X67, Y67)
         } else if ( dst == 97 ) {
             coords
         } else print("Unsupported coordinate system!")

     } else if ( src == 67 ) {
         src.proj <- TWD67TM2
         if ( dst == 84 ) {
             dst.proj <- WGS84
             ptransform(coords, src.proj, dst.proj)[, 1:2]/pi*180
         } else if ( dst == 97 ){
             dst.proj <- TWD97TM2
             X97 <- coords[, 1] + EW + A * coords[, 1] + B * coords[, 2] 
             Y97 <- coords[, 2] - SN + A * coords[, 2] + B * coords[, 1]
             cbind(X97, Y97)
         } else if ( dst == 67 ) {
             coords
         } else print("Unsupported coordinate system!")
     }
     else print("Exception caught! Please report a bug! Thank you")
}

draw.vertmap <- function(coorsys, orient=1, mountain=T) {
    data(elevprof)
    # transformation of wgs84 and twd97
    proflong <- rep(20000, dim(elevprof)[1])
    elevprof84 <- cbind(twcoor.trans(as.matrix(cbind(proflong, elevprof[,1])), 97, 84)[, 2], elevprof[, 2])
    # orientation selection
    if ( orient == 1 ) {
        o.side1 <- 1
        o.side2 <- 2
        o.ylab <- "Elevation (m)"
        # coordinate system selection
        if ( coorsys == 84 ) {
            elev <- cbind(elevprof84[, 1], elevprof84[, 2])
            o.xlim <- c(25.7,21.5)
            o.ylim <- c(0, 4000)
            o.xlab <- "Latitude"
            latdeg <- parse(text=paste(25:22,"*degree~", "N", sep=""))
            gridv <- 25:22
            gridv.sub <- 215:257/10 
            datun.x <- 25.2
            datun.y <- 1200
            hsueh.x <- 24.35
            hsueh.y <- 4000
            yushan.x <- 23.45
            yushan.y <- 4000
            beidawu.x <- 22.6
            beidawu.y <- 3200
        } else if ( coorsys == 97 ) {
            elev <- cbind(elevprof[, 1], elevprof[, 2])
            o.xlim <- c(2850000,2380000)
            o.ylim <- c(0, 4200)
            o.xlab <- "TWD97 TM2 Y (meter)"
            latdeg <- c(28:24*100000)
            gridv <- 28:24*100000
            gridv.sub <- 235:287*10000 
            datun.x <- 2790000
            datun.y <- 1200
            hsueh.x <- 2700000
            hsueh.y <- 4000
            yushan.x <- 2600000
            yushan.y <- 4000
            beidawu.x <- 2500000 
            beidawu.y <- 3200
        } else print("Unsupported coordinate system!")

    plot(elev, pch=".", ylim=o.ylim, xlim=o.xlim, axes=F,
                        ylab=o.ylab, xlab=o.xlab)
    lines(elev)
    abline(h=c(0:4)*(1000), b=0, lwd=1.2, col="grey")
    abline(h=c(0:42)*(100), b=0, lwd=0.5, col="grey")
    abline(v=gridv,lwd=1.2,col="grey")
    abline(v=gridv.sub,lwd=0.5,col="grey")
    # draw axis
    axis(side=o.side1, gridv, latdeg, las=1)
    axis(side=o.side2, 1000*(0:4), cbind("0","1000","2000","3000","4000"))
    # draw frame
    box()
    if ( mountain == T ) {
        text(datun.x,datun.y, labels='Datun Shan')
        text(hsueh.x,hsueh.y, labels='Hsueh Shan')
        text(yushan.x,yushan.y, labels='Yu Shan')
        text(beidawu.x,beidawu.y, labels='BeiDawu Shan')
    }

    } else if ( orient == 2 ){
        o.side1 <- 2
        o.side2 <- 1
        o.xlab <- "Elevation (m)"
        # coordinate system selection
        if ( coorsys == 84 ) {
            elev <- cbind(elevprof84[, 2], elevprof84[, 1])
            o.xlim <- c(4200,0)
            o.ylim <- c(21.5, 25.7)
            o.ylab <- "Latitude"
            latdeg <- parse(text=paste(22:25,"*degree~", "N", sep=""))
            gridv <- 22:25
            gridv.sub <- 215:257/10 
            datun.y <- 25.2
            datun.x <- 1200
            hsueh.y <- 24.35
            hsueh.x <- 4000
            yushan.y <- 23.45
            yushan.x <- 4000
            beidawu.y <- 22.6
            beidawu.x <- 3200
        } else if ( coorsys == 97 ) {
            elev <- cbind(elevprof[, 2], elevprof[, 1])
            o.xlim <- c(4200,0)
            o.ylim <- c(2380000,2850000)
            o.ylab <- "TWD97 TM2 Y (meter)"
            latdeg <- c(24:28*100000)
            gridv <- 24:28*100000
            gridv.sub <- 235:287*10000
            datun.y <- 2790000
            datun.x <- 1200
            hsueh.y <- 2700000
            hsueh.x <- 4000
            yushan.y <- 2600000
            yushan.x <- 4000
            beidawu.y <- 2500000 
            beidawu.x <- 3200
        } else print("Unsupported coordinate system!")

    plot(elev, pch=".", ylim=o.ylim, xlim=o.xlim, axes=F,
                        ylab=o.ylab, xlab=o.xlab)
    lines(elev)
    abline(v=c(0:4)*(1000), lwd=1.2, col="grey")
    abline(v=c(0:42)*(100), lwd=0.5, col="grey")
    abline(h=gridv,b=0,lwd=1.2,col="grey")
    abline(h=gridv.sub,b=0,lwd=0.5,col="grey")
    # draw axis
    axis(side=o.side1, gridv, latdeg, las=3)
    axis(side=o.side2, 1000*(0:4), cbind("0","1000","2000","3000","4000"))
    # draw frame
    box()
    if ( mountain == T ) {
        text(datun.x,datun.y, labels='Datun Shan', srt=90)
        text(hsueh.x,hsueh.y, labels='Hsueh Shan', srt=90)
        text(yushan.x,yushan.y, labels='Yu Shan', srt=90)
        text(beidawu.x,beidawu.y, labels='BeiDawu Shan', srt=90)
    }
    } else if ( orient == 3 ){
        o.side1 <- 4 
        o.side2 <- 1
        o.xlab <- "Elevation (m)"
        # coordinate system selection
        if ( coorsys == 84 ) {
            elev <- cbind(elevprof84[, 2], elevprof84[, 1])
            o.xlim <- c(0,4200)
            o.ylim <- c(21.5, 25.7)
            o.ylab <- "Latitude"
            latdeg <- parse(text=paste(22:25,"*degree~", "N", sep=""))
            gridv <- 22:25
            gridv.sub <- 215:257/10 
            datun.y <- 25.2
            datun.x <- 1200
            hsueh.y <- 24.35
            hsueh.x <- 4000
            yushan.y <- 23.45
            yushan.x <- 4000
            beidawu.y <- 22.6
            beidawu.x <- 3200
        } else if ( coorsys == 97 ) {
            elev <- cbind(elevprof[, 2], elevprof[, 1])
            o.xlim <- c(0,4200)
            o.ylim <- c(2380000,2850000)
            o.ylab <- "TWD97 TM2 Y (meter)"
            latdeg <- c(24:28*100000)
            gridv <- 24:28*100000
            gridv.sub <- 235:287*10000
            datun.y <- 2790000
            datun.x <- 1200
            hsueh.y <- 2700000
            hsueh.x <- 4000
            yushan.y <- 2600000
            yushan.x <- 4000
            beidawu.y <- 2500000 
            beidawu.x <- 3200
        } else print("Unsupported coordinate system!")

    par(oma=c(0,0,0,4))
    plot(elev, pch=".", ylim=o.ylim, xlim=o.xlim, axes=F,
                        ylab="", xlab=o.xlab)
    mtext(o.ylab, side=4, las=3, outer=T)
    lines(elev)
    abline(v=c(0:4)*(1000), lwd=1.2, col="grey")
    abline(v=c(0:42)*(100), lwd=0.5, col="grey")
    abline(h=gridv,b=0,lwd=1.2,col="grey")
    abline(h=gridv.sub,b=0,lwd=0.5,col="grey")
    # draw axis
    axis(side=o.side1, gridv, latdeg, las=3)
    axis(side=o.side2, 1000*(0:4), cbind("0","1000","2000","3000","4000"))
    # draw frame
    box()
    if ( mountain == T ) {
        text(datun.x,datun.y, labels='Datun Shan', srt=90)
        text(hsueh.x,hsueh.y, labels='Hsueh Shan', srt=90)
        text(yushan.x,yushan.y, labels='Yu Shan', srt=90)
        text(beidawu.x,beidawu.y, labels='BeiDawu Shan', srt=90)
    }

    } else print("orientation=1(Landscape)/2(Portrait left)/3(Portrait right)")
}
