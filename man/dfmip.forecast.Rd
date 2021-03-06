% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dfmip.R
\name{dfmip.forecast}
\alias{dfmip.forecast}
\title{Disease Forecast Model Intercomparison Project (dfmip) Forecast}
\usage{
dfmip.forecast(forecast.targets, models.to.run, human.data, mosq.data,
  weather.data, weekinquestion, week.id, results.path,
  model.inputs = list(), population.df = "none", n.draws = 1000,
  point.estimate = 0, analysis.locations = "default",
  is.test = FALSE)
}
\arguments{
\item{forecast.targets}{The quantities for which hindcasts are to be made. Options are: \tabular{ll}{
annual.human.cases \tab Number of human cases\cr
seasonal.mosquito.MLE \tab Mosquito infection rate maximum likelihood estimate averaged over the entire season\cr }}

\item{models.to.run}{A string vector of the models to run. Options are: \tabular{ll}{
NULL.MODELS \tab Forecasts based on statewide incidence\cr
ArboMAP \tab Development version ArboMAP forecasts, see details section.\cr
ArboMAP.MOD \tab Modified version of ArboMAP model.\cr
RF1_C \tab Random Forest model, climate inputs only (i.e. equivalent inputs to ArboMAP).\cr
RF1_A \tab Random Forest model, all available inputs.\cr  }
Note these entries are case-sensitive and are run by keyword, so run in a fixed order (NULL.MODELS, ArboMAP, ArboMAP.MOD, RF1_C, RF1_A),
regardless of the order specified in the models.to.run vector.}

\item{human.data}{Data on human cases of the disease. Must be formatted with two columns: location and date (in format M/D/Y, with forward slashes as delimiters). An optional 'year' column may also be present. If absent, it will be generated from the date column. The location column contains the spatial unit (typically county), while the date corresponds to the date of the onset of symptoms for the human case.
#**# WHAT IF THESE DATA ARE MISSING? I.E. just making a mosquito forecast with RF1?}

\item{mosq.data}{Data on mosquito pools tested for the disease. Must be formatted with 4 columns: location (the spatial unit, e.g. county), col_date: the date the mosquitoes were tested, wnv_result: whether or not the pool was positive, pool_size: the number of mosquitoes tested in the pool. A fifth column species is optional but is not used by the code}

\item{weather.data}{Data on weather variables to be included in the analysis. See the read.weather.data function for details about data format.
The read.weather.data function from ArboMAP is a useful way to process one or more data files downloaded via Google Earth Engine.}

\item{weekinquestion}{The focal week for the forecast. For the Random Forest model, this will be the last day used for making the forecast}

\item{week.id}{A two-part ID for the analysis run to distinguish it from other weeks or runs. The first part is a character string, while the second part is a date in the format YYYY-MM-DD #**# analysis.id would be a better name, but would require changes to the code in multiple places}

\item{results.path}{The base path in which to place the modeling results. Some models will create sub-folders for model specific results}

\item{model.inputs}{A keyed list of model-specific inputs. Keyed entry options are: \tabular{ll}{
arbo.inputs \tab Inputs specific to the ArboMAP model. #**# DOCUMENTATION NEEDED\cr
rf1.inputs \tab Inputs specific to the RF1 model, see \code{\link{rf1.inputs}}.\cr}}

\item{population.df}{Census information for calculating incidence. Can be set to 'none' or omitted from the function call #**# NEEDS FORMAT INSTRUCTIONS}

\item{n.draws}{The number of draws for the forecast distributions. Should generally be 1 if a point estimate is used, otherwise should be a large enough number to adequately represent the variation in the underlying data}

\item{point.estimate}{Whether a single point estimate should be returned for forecast distributions representing the mean value. Otherwise past years are sampled at random.}

\item{analysis.locations}{locations to include in the analysis. This may include locations with no human cases that would otherwise be dropped from the modeling.}

\item{is.test}{Default is FALSE (runs all models). If set to TRUE, saved results will be used for the Random Forest model. For testing purposes only.}
}
\value{
dfmip.outputs: List of three objects: \tabular{ll}{
forecasts.df \tab A data frame with 7 columns: The model name, the forecast id, the forecast target, the geographic scope of the forecast
the forecast date, the forecast year, and the estimated value for the forecast target.\cr
forecast.distributions \tab A data frame with 6 fixed columns, and n.draws additional columns. The six fixed columns are:
The model name, the forecast id, the forecast target, the geographic scope of the forecast
the forecast date, and the forecast year.\cr
other.results \tab A keyed list of model-specific results that are not extracted in forecasts.df or forecast.distributions.
They can be accessed by keyword, for example, for the RF1_C model, other.results$rf1c will
return a list with the random forest outputs see rf1.outputs in RF1 package for details.\cr}
}
\description{
Generate forecasts for multiple mosquito-borne disease models using a single interface.
It is currently configured to give estimates of human cases from the ArboMAP model and a Random Forest model
as implemented by Keyel et al. 2019 (see details for citations)
}
\details{
ArboMAP Details: See \code{\link[ArboMAP]{ArboMAP}} for more details
   ArboMAP uses a distributed lags statistical approach to forecast vector-borne disease risk
   based on historical and current mosquito surveillance, historical and current weather data,
   and historical numbers of human cases.

   The original code for ArboMAP is available: www.github.com/ecograph/arbomap
   The fork compatible with dfmip is available: www.github.com/akeyel/arbomap/ArboMAP_package
   Citations:
   Davis et al. 2017 PLoS Currents Outbreaks 9 10.1371/currents.outbreaks.90e80717c4e67e1a830f17feeaaf85de.
   Davis et al. 2018 Acta Tropica 185: 242-250

RF1 Details: See rf1 package: ?rf1

Null model uses historical probabilities.
}
