\name{drawtwmap}
\alias{drawtwmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw species plots in Taiwan
}
\description{
It's lazy-man-drawing Taiwan map and species data program with classified elevation and rivers.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
drawtwmap(species.data, sp.pch, sp.col, lwd, river = T, png = T, file.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{species.data}{
%%     ~~Describe \code{species.data} here~~
    Species data coordinates in TWD1997 TM2 
}
  \item{sp.pch}{
%%     ~~Describe \code{sp.pch} here~~
    Species plot symbol type (16 is dot)
}
  \item{sp.col}{
%%     ~~Describe \code{sp.col} here~~
    Species plot colors
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
    Line width
}
  \item{river}{
%%     ~~Describe \code{river} here~~
    River layer of Taiwan, default is TRUE.
}
  \item{png}{
%%     ~~Describe \code{png} here~~
    Generate output png file. Default is TRUE.
}
  \item{file.name}{
%%     ~~Describe \code{file.name} here~~
    If you specify png=T, file.name is the output filename.
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
maptools, sp
}
\examples{
# Draw a standard species plot with elevation and water,
# then export to 'sp_name.png'
library(twmap)
data(twsp)
drawtwmap(sp, lwd=4, sp.pch=16, sp.col="red", png=T, file.name='sp_name.png')
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
