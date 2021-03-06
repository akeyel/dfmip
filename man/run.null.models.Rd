% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dfmip.R
\name{run.null.models}
\alias{run.null.models}
\title{Generate null models based on mean values}
\usage{
run.null.models(forecast.targets, forecasts.df, forecast.distributions,
  human.data, week.id, weekinquestion, n.draws, point.estimate,
  analysis.locations, mosq.data = NA, population.df = NA,
  model.name = "NULL.MODELS")
}
\arguments{
\item{forecast.targets}{See \code{\link{dfmip.forecast}}}

\item{forecasts.df}{See \code{\link{dfmip.forecast}}}

\item{forecast.distributions}{See \code{\link{dfmip.forecast}}}

\item{human.data}{See \code{\link{dfmip.forecast}}}

\item{week.id}{See \code{\link{dfmip.forecast}}}

\item{weekinquestion}{See \code{\link{dfmip.forecast}}}

\item{n.draws}{The number of draws for the forecast distributions. Should be 1 if a point estimate is used, otherwise should be a large enough number to adequately represent the variation in the underlying data}

\item{point.estimate}{Whether a single point estimate should be returned for forecast distributions representing the mean value. Otherwise past years are sampled at random.}

\item{analysis.locations}{locations to include in the analysis. This may include locations with no human cases that would otherwise be dropped from the modeling.}

\item{mosq.data}{Only required if a mosquito output is among the forecast targets. See \code{\link{dfmip.forecast}}}

\item{population.df}{A data frame with locations (counties) and their populations. A column labeled SPATIAL should contain the county/location information, while population should be in a TOTAL_POPULATION column. Only required if incidence calculations are desired.}

\item{model.name}{The name of the model to use in forecasts.df and forecast.distributions}
}
\value{
A list consisting of a data frame with forecast results and a list of forecast distributions
}
\description{
Create simple null models for comparison
}
