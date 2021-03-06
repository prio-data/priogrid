% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{map_pg_crossection}
\alias{map_pg_crossection}
\title{map_pg_crossection}
\usage{
map_pg_crossection(pgdf, variable, myyear = NULL, mymonth = NULL, ...)
}
\arguments{
\item{pgdf}{a prio-grid dataframe with x, y, year, month, variable}

\item{variable}{the variable to convert to plot}

\item{myyear}{the year of the crossection to plot}

\item{mymonth}{the month of the crossection to plot}

\item{...}{function accepts other parameters passed to raster::plot()}
}
\value{
A plot based on raster::plot()
}
\description{
Plots a PRIO-GRID crossection on a map.
}
