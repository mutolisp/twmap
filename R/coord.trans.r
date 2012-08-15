twcoor.trans <- function(coords, src, dst) {
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
