% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pgfunctions.R, R/raster_functions.R
\name{raster_points}
\alias{raster_points}
\title{Raster points}
\usage{
raster_points(raster, na.rm = TRUE)

raster_points(raster, na.rm = TRUE)
}
\value{
An sf with column val = values(raster)
}
\description{
Converts a raster to a set of points, optionally
removing missing values.

Simple function that returns a raster as a sf set of points
}
