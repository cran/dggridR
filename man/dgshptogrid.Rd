% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dggridR.R
\name{dgshptogrid}
\alias{dgshptogrid}
\title{Return boundary coordinates for cells intersecting a
                 shapefile}
\usage{
dgshptogrid(dggs, shpfname, cellsize = 0.1, ...)
}
\arguments{
\item{dggs}{A dggs object from dgconstruct()}

\item{shpfname}{Either a sf data frame or the file name of the shapefile. Filename should end with '.shp'.}

\item{cellsize}{Distance, in degrees, between the sample points used to
generate the grid. Small values yield long generation times
while large values may omit cells.}

\item{\dots}{Further arguments passed to \code{\link{dgcellstogrid}}.}
}
\value{
Returns an sf object.
        If \code{!is.na(savegrid)}, returns a filename.
}
\description{
Returns the coordinates constituting the boundary of a
                 set of cells which intersect or are contained by a polygon
                 (or polygons) specified in a shapefile. Note that grid cells
                 are also generated for holes in the shapefile's polygon(s).

                 Note that coordinates in the shapefile must be rounded to
                 check polygon intersections. Currently this round preserves
                 eight decimal digits of precision.

                 The eighth decimal place is worth up to 1.1 mm of precision:
                 this is good for charting the motions of tectonic plates and
                 the movements of volcanoes. Permanent, corrected,
                 constantly-running GPS base stations might be able to
                 achieve this level of accuracy.

                 In other words: you should be just fine with this level of
                 precision.
}
\examples{
library(dggridR)

dggs <- dgconstruct(spacing=25, metric=FALSE, resround='nearest')
south_africa_grid <- dgshptogrid(dggs,dg_shpfname_south_africa())
}
