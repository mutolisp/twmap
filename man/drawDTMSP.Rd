\name{drawDTMSP}
\alias{drawDTMSP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Draw a longitude-latitude Taiwan map with species plots.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
drawDTMSP(species.data, sp.pch, sp.col, lwd, river = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{species.data}{
%%     ~~Describe \code{species.data} here~~
        species data, it should contain x, y in TWD1997 TM2.
}
  \item{sp.pch}{
%%     ~~Describe \code{sp.pch} here~~
        species plot symbol type
}
  \item{sp.col}{
%%     ~~Describe \code{sp.col} here~~
        species plot color
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
        line width
}
  \item{river}{
%%     ~~Describe \code{river} here~~
        River layer of Taiwan, default is TRUE
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
    Lin, Cheng-Tao 
    School of Forestry and Resource Conservation, 
    National Taiwan University
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (species.data, sp.pch, sp.col, lwd, river = T) 
{
    my.colors <- c("#B0C47CFF", "#F2EEA2FF", "#F2E096FF", "#F2CE85FF", 
        "#D9A982FF", "#C28C7CFF", "#E3B8C1FF", "#FFF2FFFF")
    library(maptools)
    library(rgdal)
    library(lattice)
    gridlwd <- lwd
    river.lwd <- 2
    c1 <- readShapePoly("gislayers/tw_elev_500.shp")
    c2 <- readShapePoly("gislayers/tw_elev_g500.shp")
    c3 <- readShapePoly("gislayers/tw_elev_g1000.shp")
    c4 <- readShapePoly("gislayers/tw_elev_g1500.shp")
    c5 <- readShapePoly("gislayers/tw_elev_g2000.shp")
    c6 <- readShapePoly("gislayers/tw_elev_g2500.shp")
    c7 <- readShapePoly("gislayers/tw_elev_g3000.shp")
    c8 <- readShapePoly("gislayers/tw_elev_g3500.shp")
    twbound <- readShapePoly("gislayers/Taiwan_penhu_TWD97TM2.shp")
    par(oma = c(2, 2, 2, 3.5))
    par(xpd = F)
    print("Generating map(s), how about have a cup of coffee and take a rest?")
    plot(c1, col = my.colors[1], border = F)
    plot(c2, add = T, col = my.colors[2], border = F)
    plot(c3, add = T, col = my.colors[3], border = F)
    plot(c4, add = T, col = my.colors[4], border = F)
    plot(c5, add = T, col = my.colors[5], border = F)
    plot(c6, add = T, col = my.colors[6], border = F)
    plot(c7, add = T, col = my.colors[7], border = F)
    plot(c8, add = T, col = my.colors[8], border = F)
    plot(twbound, add = T, lwd = 2)
    grid_lat <- c(2434907.81937665, 2544632.70497163, 2655023.12495712, 
        2766149.87190775)
    grid_long <- c(147484.07577807, 250000, 350943.31271824)
    latdeg <- parse(text = paste(22:25, "*degree~", "N", sep = ""))
    longdeg <- parse(text = paste(120:122, "*degree~", "E", sep = ""))
    tcancer_lat <- 2601073.49484586
    abline(h = tcancer_lat, b = 0, lwd = gridlwd, lty = 2, col = "gray")
    abline(v = grid_long, lwd = gridlwd, col = "gray")
    abline(h = grid_lat, b = 0, lwd = gridlwd, col = "gray")
    axis(side = 1, grid_long, longdeg)
    axis(side = 3, grid_long, longdeg)
    axis(side = 2, grid_lat, latdeg, las = 3)
    axis(side = 4, grid_lat, latdeg, las = 3)
    box(lwd = gridlwd)
    for (i in 1:8) {
        rect(75000, 2400000 + 15000 * i, 80000, 2415000 + 15000 * 
            i, col = my.colors[i], lwd = 0)
    }
    rect(75000 - 10, 2400000 + 15000 - 10, 80000 + 10, 2415000 + 
        15000 * 8 + 10, lwd = 3)
    par(xpd = T)
    for (i in 0:7) {
        legend_elev <- paste(i * 500, "", sep = "")
        text(83000, 2400000 + 15000 * (i + 1), label = legend_elev, 
            adj = 0)
    }
    par(xpd = T)
    text(70000, 2540000, label = "Elevation (m)", pos = 4)
    par(xpd = NA)
    if (river == T) {
        library(maptools)
        river <- readShapeLines("gislayers/River_twd97.shp")
        plot(river, col = "#63B8FFAA", add = T, lwd = river.lwd)
    }
    plot.xy(xy.coords(species.data), lwd = 3, pch = sp.pch, type = "p", 
        col = sp.col)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
