distrmap = function (pts, boundary, col, vert_prof, cex=0.3, pch=19) {
  vert_bound = 122.2; ly = 21.8; uy = 25.4; vmar = 0.1
  
  basemap = function (boundary, vert_prof) {
    
    tw_sf <- sf::st_read(boundary)
    
    tw_boundary <- as(tw_sf, "Spatial")
    
    plot(tw_boundary, col="white",border="#aaaaaa", 
         xlim=c(119.8, 123), ylim=c(21.5, 25.5), axes=FALSE)
    # draw tropical of cancer
    abline(h=23.5, lty=2, col='grey')
    axis_latlab = parse(text=paste(22:25, "*degree~", "N", sep=""))
    axis(side=2, lwd=0.8, labels = axis_latlab , lwd.ticks = 0.5, 
         at=22:25, cex=0.8)
    #axis(side=4, lwd=0.8, labels = lat , lwd.ticks = 0.5,
    #     at=22:25, cex=0.8)
    axis_longlab = parse(text=paste(120:122, "*degree~", "E", sep=""))
    axis(side=1, labels = axis_longlab, at=120:122, line = NA, lwd = 0.8, cex=0.8)
    #box()
    
    par(new=TRUE)
    
    #abline(v=vert_bound, col='grey')
    
    segments(vert_bound - vmar, ly, vert_bound - vmar, uy)
    segments(vert_bound - vmar, ly, vert_bound + 1, ly) 
    for ( i in 0:4 ) {
      # draw for elevation profile
      segments(vert_bound + i/4, ly-0.05, vert_bound + i/4, ly)
      ticlab = i*1000
      text(vert_bound + i/4, ly-0.1, ticlab, cex=0.7)  
    }
    text(vert_bound + 2.3/4, ly-0.2, 'Elevation (m)', cex=0.8)
    
    
    for ( i in 1:4){
      segments(vert_bound + i/4, ly, vert_bound + i/4, uy, col='grey', lty = 1) 
    }
    # draw latitude-elevation profile
    vert_prof$z = vert_bound + (vert_prof$z/4000)
    vf = cbind(vert_prof$z, vert_prof$y)
    plot.xy(xy.coords(vf), type='p', col='white', pch=19, cex=0.5)
    lines(vf, col='darkgrey')    
    box()
  }
  
  # draw_sp_pts 
  draw_sp_pts = function(pts, col, cex=0.3, pch=19) {
    if ( dim(pts)[2] != 3 ) {
      print('The input points should have x (longitude), y (latitude) and z (elevation)')
    } else {
      horizon_pts = cbind(pts$x, pts$y); colnames(horizon_pts) = c('x', 'y')
      pts$z = vert_bound + (pts$z/4000)
      vert_pts = cbind(pts$z, pts$y); colnames(vert_pts) = c('z', 'y')
      plot.xy(xy.coords(horizon_pts), type='p', col=col, pch=pch, cex=cex)
      plot.xy(xy.coords(vert_pts), type='p', col=col, pch=pch, cex=cex)
    }
  }
  
  basemap(boundary, vert_prof)
  draw_sp_pts(pts, col)
}
