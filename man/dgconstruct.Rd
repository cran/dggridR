% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dggridR.R
\name{dgconstruct}
\alias{dgconstruct}
\title{Construct a discrete global grid system (dggs) object}
\usage{
dgconstruct(type = "ISEA3H", res = NA, precision = 7, area = NA,
  spacing = NA, cls = NA, resround = "nearest", metric = TRUE,
  show_info = TRUE)
}
\arguments{
\item{type}{Type of grid to use. Options are: ISEA3H, ISEA4H, ISEA43H, 
ISEA4T, ISEA4D, FULLER3H, FULLER4H, FULLER43H, FULLER4T,
and FULLER4D. Default: ISEA3H}

\item{res}{Resolution. Must be in the range [0,35]. Larger values represent
finer resolutions. Appropriate resolutions can be found with
dg_closest_res_to_area(), dg_closest_res_to_spacing(), and
dg_closest_res_to_cls(). Default is 9, which corresponds to a
cell area of ~2600 sq km and a cell spacing of ~50 km.
Only one of res, area, length, or cls should be used.}

\item{precision}{Round output to this number of decimal places. Must be in
the range [0,30]. Default: 7.}

\item{area}{The desired area of the grid's cells.
Only one of res, area, length, or cls should be used.}

\item{spacing}{The desired spacing between the center of adjacent cells.
Only one of res, area, length, or cls should be used.}

\item{cls}{The desired CLS of the cells.
Only one of res, area, length, or cls should be used.}

\item{resround}{What direction to search in. Must be nearest, up, or down.}

\item{metric}{Whether input and output should be in metric (TRUE) or
imperial (FALSE)}

\item{show_info}{Print the area, spacing, and CLS of the chosen resolution.}
}
\value{
Returns a dggs object which can be passed to other dggridR
                 functions
}
\description{
Construct a discrete global grid system (dggs) object
}
\examples{
library(dggridR)
dggs <- dgconstruct(res=20)

dggs <- dgconstruct(area=5,metric=FALSE)

}
