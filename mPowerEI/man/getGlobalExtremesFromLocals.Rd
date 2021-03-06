% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildFeatures.R
\name{getGlobalExtremesFromLocals}
\alias{getGlobalExtremesFromLocals}
\title{Global Extremes from Local Extremes}
\usage{
getGlobalExtremesFromLocals(tpobj, dir = "min", cuts = 3)
}
\arguments{
\item{tpobj}{turningpoints obj}

\item{dir}{"min" or "max"}

\item{cuts}{number of points to consider in a local scan (for hip motion 3 seems to work ok)}
}
\value{
list of indexes related to global extreme
}
\description{
Global Extremes from Local Extremes
}
