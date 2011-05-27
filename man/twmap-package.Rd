\name{twmap-package}
\alias{twmap-package}
\alias{twmap}
\docType{package}
\title{Draw species plots and elevation in Taiwan}
\description{
It's lazy-man-drawing Taiwan map and species data program with classified elevation and rivers.
}
\details{
\tabular{ll}{
Package: \tab twmap\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2011-05-25\cr
License: \tab FreeBSD License\cr
LazyLoad: \tab yes\cr
}
drawtwmap()
draw.vertmap()
twcoor.trans()

This extension is intended to draw high quality maps displaying
species distribution with related geographical information.

twmap also can use twcoor.trans() to transform coordinate systems
amongst WGS84, Taiwan Datum 1967 and Taiwan Datum 1997
}
\author{
Lin, Cheng-Tao
Maintainer: Lin, Cheng-Tao <mutolisp@gmail.com>
}
\references{
~~ Literature or other references for background information ~~
}
\keyword{ graphics, map, Taiwan }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
# Draw a standard species plot with elevation and water,
# then export to 'sp_name.png'
library(twmap)
data(twsp)
drawtwmap(sp, lwd=4, sp.pch=16, sp.col="red", png=T, file.name='sp_name.png')

coor <- matrix(c(121,122,120.5,22,22.5,23.5),3,2)
# transform WGS84 to TWD97 TM2
coor97 <- twcoor.trans(coor, 84, 97)
coor97
# transform TWD97 TM2 to WGS84
twcoor.trans(coor97, 97, 84)
}
