\name{twcoor.trans}
\alias{twcoor.trans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform coordinate systems in Taiwan.
}
\description{
Transform coordinate system amongst WGS84, Taiwan Datum 1967 and Taiwan Datum 1997.
}
\usage{
twcoor.trans(coords, src, dst)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coords}{
Coordinates (x,y) or (longtitude, latitude) in 2-column matrix
}
  \item{src}{
Source coordinate system. 84 (WGS84), 97 (TWD97 TM2) or 67 (TWD67 TM2)
}
  \item{dst}{
Destination coordinate system. 84 (WGS84), 97 (TWD97 TM2) or 67 (TWD67 TM2)
}
}
\details{
twcoor.trans uses Proj4 to transform coordinate systems. But the transformation
between TWD97 and TWD67 uses 4-parameter transformation. The error range is within
2 meters.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
http://gis.thl.ncku.edu.tw/coordtrans/coordtrans.aspx
}
\author{
Lin, Cheng-Tao
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
http://wiki.osgeo.org/index.php?title=Taiwan_datums&uselang=zh-tw
}
\examples{
coor <- matrix(c(121,122,120.5,22,22.5,23.5),3,2)
# transform WGS84 to TWD97 TM2
coor97 <- twcoor.trans(coor, 84, 97)
coor97
# transform TWD97 TM2 to WGS84
twcoor.trans(coor97, 97, 84)
}
\keyword{ gis }
\keyword{ spatial }
\keyword{ transformation }
