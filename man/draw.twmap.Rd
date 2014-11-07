\name{draw.twmap}
\alias{draw.twmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw Taiwan map with latitudinal gradients
}
\description{
It's lazy-man-drawing Taiwan map and species data program with classified elevation and rivers.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
draw.twmap(theme, lwd)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theme}{
      Color themes, 1 is color map, 2 is grey gradient map.
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
    Line width
}
%%  \item{river}{
%%     ~~Describe \code{river} here~~
%%    River layer of Taiwan, default is TRUE.
%%}
%%  \item{png}{
%%     ~~Describe \code{png} here~~
%%    Generate output png file. Default is TRUE.
%%}
%%  \item{file.name}{
%%     ~~Describe \code{file.name} here~~
%%    If you specify png=T, file.name is the output filename.
%%}
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
maptools, sp
}
\examples{
# Draw a standard species plot with elevation and water,
library(twmap)
# export to png file
# NOT RUN
data(twsp)
png("/path/to/export/file.png", width=2000, height=2800, pointsize=46)
draw.twmap(theme=1, lwd=3, river=T)
plot.xy(xy.coords(twsp), type="p", pch=19, col="red")
dev.off()
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~map }
\keyword{ ~spatial }% __ONLY ONE__ keyword per line
