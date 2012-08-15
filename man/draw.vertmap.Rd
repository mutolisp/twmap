\name{draw.vertmap}
\alias{draw.vertmap}
\title{
Draw vertical maps  
}
\description{
Draw vertical maps by latitude and elevation
}
\usage{
draw.vertmap(coorsys, orient=1, mountain=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coorsys}{
Coordinate systems. 84 for WGS84 longitude and latitude, 97 for Taiwan Datum 1997.
}
  \item{orient}{
Map orientation. 1: landscape view (x-axis: latitude; y-axis: elevation); 
2: portrait view (x-axis: elevation; y-axis: latitude, mountain peak on the left side); and
3: portrait view (y-axis on the right side). Default is landscape view
}
  \item{mountain}{
Mark the main mountains (Datun Shan; Hsueh Shan; Yu Shan and BeiDawu Shan) along 
the latitudinal gradient. Default is true.
}
  \item{lwd} {
export line weights
}
}
\details{
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
}
\author{
Lin, Cheng-Tao
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
twcoor.trans
}
\examples{
library(twmap)
data(vertplots)
# draw vertical map in landscape mode
draw.vertmap(97, 1, T, lwd=1)
# plot sample releves
plot.xy(xy.coords(vertplots), type="p", col="red")

# draw vertical map in portrait mode, y-axis on the right side
draw.vertmap(84, 3, F, lwd=1)
plot.xy(xy.coords(vertplots), type="p", col="red")
}
\keyword{ gis }
\keyword{ spatial }
\keyword{ map }
