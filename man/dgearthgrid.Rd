% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dggridR.R
\name{dgearthgrid}
\alias{dgearthgrid}
\title{Return the coordinates constituting the boundary of cells for the
         entire Earth}
\usage{
dgearthgrid(dggs, savegrid = NA, return_sf = TRUE)
}
\arguments{
\item{dggs}{A dggs object from dgconstruct().}

\item{savegrid}{If savegrid is set to a file path, then a shapefile
containing the grid is written to that path and the filename
is returned. No other manipulations are done.
Default: NA (do not save grid, return it)}

\item{return_sf}{logical. If \code{FALSE}, a long-format data frame giving the coordinates of the vertices of each cell is returned. This is is considerably faster and more memory efficient than creating an sf data frame.}
}
\value{
Returns an sf object.
        If \code{!is.na(savegrid)}, returns a filename.
}
\description{
Note: If you have a high-resolution grid this may take a
                 very long time to execute.
}
\examples{
\donttest{
library(dggridR)
dggs         <- dgconstruct(res=20)
res          <- dg_closest_res_to_spacing(dggs,spacing=1000,round='down',metric=FALSE)
dggs         <- dgsetres(dggs,res)
gridfilename <- dgearthgrid(dggs,savegrid=tempfile(fileext=".shp")) #Save directly to a file
}
}
