% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{vector_to_pg}
\alias{vector_to_pg}
\title{vector_to_pg}
\usage{
vector_to_pg(sfdf, variable, fun, need_aggregation = TRUE, missval = -1)
}
\arguments{
\item{sfdf}{a sf (simple features) data frame}

\item{variable}{the variable to convert to PRIO-GRID}

\item{fun}{aggregation function. only used if need_aggregation is TRUE.}

\item{need_aggregation}{if FALSE, will use velox, which only extracts the last value for each feature. fast when applicable.}

\item{missval}{only used if need_aggregation is FALSE. velox sets missing data to this value.}
}
\value{
A raster with same extent and crs as PRIO-GRID.
}
\description{
Converts a sf data frame to PRIO-GRID as a crossection.
}
