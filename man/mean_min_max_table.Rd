% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dfmip.R
\name{mean_min_max_table}
\alias{mean_min_max_table}
\title{Make a table with min, max, and mean values by location}
\usage{
mean_min_max_table(forecast.distributions, outfile, n.draws)
}
\arguments{
\item{forecast.distributions}{The forecast.distributions output from the dfmip function. The first 6 columns are informational,
followed by n.draw prediction columns, followed by any columns added subsequently by the user.}

\item{outfile}{The file to be written (including the path where it should be written to, if not in the current workspace)}

\item{n.draws}{The number of columns corresponding to forecast predictions}
}
\description{
Goal is to plot out the extremes and the mean for each county in the US. May take a short while to run (~1min).
}
