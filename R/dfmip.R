#' dfmip: Disease Forecast Model Inter-comparison Project package
#'
#' dfmip runs multiple disease forecasting models for the purpose of
#' comparing and contrasting methods. Currently designed for mosquito-borne
#' diseases, the aim is for future versions to be more generally applicable
#'
#' @docType package
#' @name dfmip
NULL

# Identify dependencies used in dfmip (see also DESCRIPTION file)
# Set up package imports for NAMESPACE
#' @import dplyr
#' @import caret
#' @import psych
#' @import randomForest
#' @importFrom grDevices dev.off tiff
#' @importFrom graphics barplot curve hist par plot segments text
#' @importFrom stats aggregate as.formula cor dnorm median na.exclude optimize predict qchisq runif sd uniroot
#' @importFrom utils read.csv write.table
NULL
# @import ArboMAP # Not listed - see !require statement that loads it later. Listed under Suggests in the dependency

#' Disease Forecast Model Intercomparison Project (dfmip) Forecast
#'
#' Generate forecasts for multiple mosquito-borne disease models using a single interface.
#' It is currently configured to give estimates of human cases from the ArboMAP model and a Random Forest model
#' as implemented by Keyel et al. 2019 (see details for citations)
#'
#'@details
#' ArboMAP Details: See \code{\link[ArboMAP]{ArboMAP}} for more details
#'    ArboMAP uses a distributed lags statistical approach to forecast vector-borne disease risk
#'    based on historical and current mosquito surveillance, historical and current weather data,
#'    and historical numbers of human cases.
#'
#'    The original code for ArboMAP is available: www.github.com/ecograph/arbomap
#'    The fork compatible with dfmip is available: www.github.com/akeyel/arbomap/ArboMAP_package
#'    Citations:
#'    Davis et al. 2017 PLoS Currents Outbreaks 9 10.1371/currents.outbreaks.90e80717c4e67e1a830f17feeaaf85de.
#'    Davis et al. 2018 Acta Tropica 185: 242-250
#'
#' RF1 Details: See \code{\link{rf1}}
#'
#' Null models are at the scale of the state, but future updates aim to allow variation by district
#'
#' @param forecast.targets The quantities for which hindcasts are to be made. Options are: \tabular{ll}{
#' annual.human.cases \tab Number of human cases\cr
#' seasonal.mosquito.MLE \tab Mosquito infection rate maximum likelihood estimate averaged over the entire season\cr }
#' @param models.to.run A string vector of the models to run. Options are: \tabular{ll}{
#' NULL.MODELS \tab Forecasts based on statewide incidence\cr
#' ArboMAP \tab Development version ArboMAP forecasts, see details section.\cr
#' ArboMAP.MOD \tab Modified version of ArboMAP model.\cr
#' RF1_C \tab Random Forest model, climate inputs only (i.e. equivalent inputs to ArboMAP).\cr
#' RF1_A \tab Random Forest model, all available inputs.\cr  }
#' Note these entries are case-sensitive and are run by keyword, so run in a fixed order (NULL.MODELS, ArboMAP, ArbMAP.MOD, RF1_C, RF1_A),
#' regardless of the order specified in the models.to.run vector.
#' @param human.data Data on human cases of the disease. Must be formatted with two columns: district and date. The district column contains the spatial unit (typically county), while the date corresponds to the date of the onset of symptoms for the human case.
#' #**# WHAT IF THESE DATA ARE MISSING? I.E. just making a mosquito forecast with RF1?
#' #**# Does the date need to be in a particular format? The sample data is "/" delimited.
#' @param mosq.data Data on mosquito pools tested for the disease. Must be formatted with 4 columns: district (the spatial unit, e.g. county), col_date: the date the mosquitoes were tested, wnv_result: whether or not the pool was positive, pool_size: the number of mosquitoes tested in the pool. A fifth column species is optional but is not used by the code
#' @param weather.data Data on weather variables to be included in the analysis. See the read.weather.data function for details about data format.
#' The read.weather.data function from ArboMAP is a useful way to process one or more data files downloaded via Google Earth Engine.
#' @param districtshapefile The shapefile with polygons representing the districts. #**# Are there required fields?
#' @param weekinquestion The focal week for the forecast. For the Random Forest model, this will be the last day used for making the forecast
#' @param week.id An arbitrary ID for the analysis run to distinguish it from other weeks or runs for different locations. #**# analysis.id would be a better name, but would require changes to the code in multiple places
#' @param results.path The base path in which to place the modeling results. Some models will create sub-folders for model specific results
#' @param arbo.inputs Inputs specific to the ArboMAP model. If the ArboMAP model or ArboMAP.MOD model are not being run, this should be set to 'none' or omitted from the function call
#' @param observed.inputs Observed values to be used for comparison purposes. Not relevant for forecasts, but of interest for evaluating hindcasts. If unused, set the variable to NA or omit from the function call
#' @param population.df Census information for calculating incidence. Can be set to 'none' or omitted from the function call #**# NEEDS FORMAT INSTRUCTIONS
#' @param rf1.inputs Inputs specific to the RF1 model, see \code{\link{rf1.inputs}}. If this model is not included, this should be set to 'none' or omitted from the function call #**# LINK TO AN OBJECT WITH MORE DETAILS
#'
#' @return dfmip.outputs: List of two objects: forecasts.df (see above) and other.results
#' other.results contains model-specific results that do not fit in the forecasts.df format
#' and can be accessed by keywords. For example, for the RF1 model, other.results$rf1 will
#' return a list. The first list entry contains the mosquito results, the second contains the human results.
#' See Random Forest 1 Model outputs (below) for details
#'
#' @export dfmip.forecast
dfmip.forecast = function(forecast.targets, models.to.run, human.data, mosq.data,
                     weather.data, districtshapefile, weekinquestion, week.id,
                     results.path, arbo.inputs = 'none', observed.inputs = 'none',
                     population.df = 'none', rf1.inputs = 'none'){

  # Check that models to run are all valid model names
  models.in.dfmip = c("NULL.MODELS", "ArboMAP", "ArboMAP.MOD", "RF1_C", "RF1_A")
  check.models(models.to.run, models.in.dfmip)

  # Initialize the forecast object
  forecasts.df = NA
  #message(paste(observed.inputs, collapse = ", "))

  # Initialize the forecast distributions object
  #**# This still needs to be added
  forecast.distributions = NA

  # Create an object to hold other model outputs
  other.outputs = list()

  # Run the Null models
  if ("NULL.MODELS" %in% models.to.run){

    message("Running NULL models")

    # Statewide incidence
    # Calculate total number of human cases
    tot.cases = nrow(human.data)

    # Calculate total number of years
    n.years = length(seq(min(human.data$year), max(human.data$year)))

    # Estimate mean cases per year
    mean.annual.cases = tot.cases / n.years

    # Estimate mean annual MLE
    mean.seasonal.mosquito.MLE = NA
    if ("seasonal.mosquito.MLE" %in% forecast.targets){
      md.data = calculate.MLE.v2(mosq.data, "annual")
      MLE.vec = c()
      for (year in unique(md.data$YEAR)){
        year.MLE = md.data[md.data$YEAR == year, ]
        seasonal.mosquito.MLE = mean(year.MLE$IR, na.rm = TRUE) #**# Average is problematic here, but I am not sure of a better approach at this point
        MLE.vec = c(MLE.vec, seasonal.mosquito.MLE)
      }
      mean.seasonal.mosquito.MLE = mean(MLE.vec, na.rm = TRUE)
    }

    # Estimate incidence
    if ("human.incidence" %in% forecast.targets){
      mean.incidence = mean.incidence.model(human.data, population.df, mean.annual.cases)
      incidence.per.year = mean.incidence[[1]]
      spatial.cases.per.year = mean.incidence[[2]]
    }

    # Get the first 4 characters of weekinquestion and convert to a numeric year
    focal.year = as.numeric(substring(as.character(weekinquestion), 1,4))
    focal.month = as.numeric(substring(as.character(weekinquestion),6,7))
    focal.day = as.numeric(substring(as.character(weekinquestion), 9,10))
    seasonal.cases = seasonal.incidence(human.data, focal.year, mean.annual.cases)

    # results needs to include these things:
    #weeks.cases = Districts.With.Cases.Focal.Week, annual.positive.district.weeks = positivesthisyear, multiplier = multiplier, annual.human.cases = human.cases)
    #new.row = c(model.name, forecast.id, annual.positive.district.weeks, annual.human.cases, multiplier, weeks.cases)

    # Somehow get the focal week out of seasonal cases.
    week.doy = get.DOY(focal.year, focal.month, focal.day)
    weeks.cases = seasonal.cases$ESTIMATED.CASES[seasonal.cases$WEEK.START == week.doy]

    if (length(weeks.cases) == 0){
      m1 = sprintf("No cases returned, week.doy was %s, seasonal cases includes weeks %s",
                   week.doy, paste(seasonal.cases$WEEK.START, collapse = ", "))
      stop(m1)
    }

    if (length(weeks.cases) > 1){  stop("More than one result was returned for weeks.cases")  }

    # Initialize statewide results
    statewide.results = list()
    statewide.results$model.name = "NULL.MODELS"
    statewide.results$forecast.id = week.id
    statewide.results$multiplier = NA #**# Should this be incidence?
    statewide.results$annual.human.cases = mean.annual.cases
    statewide.results$annual.positive.district.weeks = NA #**# In the future, extract this from the historical mosquito data
    statewide.results$weeks.cases = weeks.cases
    statewide.results$seasonal.mosquito.MLE = NA #**# NEED TO SCRIPT

    # Initialize statewide.distributions
    statewide.distributions = list()
    statewide.distributions$annual.human.cases = mean.annual.cases #**# ALL WE NEED IS A SD AND WE COULD GIVE A NORMAL DISTRIBUTION HERE!
    statewide.distributions$seasonal.mosquito.MLE = NA #**# NEED TO SCRIPT THIS
    forecast.distributions = update.distribution(forecast.targets, statewide.results$model.name, statewide.results$forecast.id, forecast.distributions, statewide.distributions)

    # Initialize district-specific results
    district.results = list()
    district.results$model.name = "DISTRICT.INCIDENCE"
    district.results$forecast.id = week.id
    district.results$multiplier = NA # Use incidence?
    #**# Add additional fields once this is supported

    forecasts.df = update.df(forecast.targets, forecasts.df, statewide.results)
    #**# Un-comment once district-specific results are supported by the code
    #forecasts.df = update.df(forecasts.df, district.results, observed.inputs)


  }

  # Run ArboMAP model
  if ("ArboMAP" %in% models.to.run){

    # Check that ArboMAP is installed
    if(!require(ArboMAP)){
      stop('ArboMAP package must be installed. You can do this with devtools::install_github("akeyel/ArboMAP/ArboMAP_package", ref = "package_test")')
    }

    # Check that the arbo.inputs object is defined
    if (arbo.inputs[[1]] == 'none'){ stop("arbo.inputs object is required to run the ArboMAP model. Please make sure it exists and is properly input.")    }

    message("Running ArboMAP model")

    # Unpack the arbo.inputs object
    stratafile = arbo.inputs$stratafile
    maxobservedhumandate = arbo.inputs$maxobservedhumandate
    var1name = arbo.inputs$var1name
    var2name = arbo.inputs$var2name
    compyear1 = arbo.inputs$compyear1
    compyear2 = arbo.inputs$compyear2

    # [[1]] is necessary to get just the results. [[2]] returns the inputs, and is for compatibility with the .Rmd format in which ArboMAP was initially developed
    ArboMAP.results = ArboMAP(human.data, mosq.data, districtshapefile, stratafile, weather.data,
                              weathersummaryfile, maxobservedhumandate, weekinquestion,
                              var1name, var2name, compyear1, compyear2, results.path, original.metrics = 1)[[1]]
    ArboMAP.results$model.name = "ArboMAP"
    ArboMAP.results$forecast.id = week.id
    ArboMAP.results$seasonal.mosquito.MLE = NA

    # Set up distributions - currently just a single entry because ArboMAP does not produce probabilistic outputs
    ArboMAP.distributions = list()
    ArboMAP.distributions$annual.human.cases = ArboMAP.results$annual.human.cases
    ArboMAP.distributions$seasonal.mosquito.MLE = NA

    forecasts.df = update.df(forecast.targets, forecasts.df, ArboMAP.results)
    forecast.distributions = update.distribution(forecast.targets, ArboMAP.results$model.name, ArboMAP.results$forecast.id, forecast.distributions, ArboMAP.distributions)
  }

  # Run the modified ArboMAP model
  if ("ArboMAP.MOD" %in% models.to.run){
    # Check that ArboMAP is installed
    if(!require(ArboMAP)){
      stop('ArboMAP package must be installed. You can do this with devtools::install_github("akeyel/ArboMAP/ArboMAP_package", ref = "package_test")')
    }


    # Check that the arbo.inputs object is defined
    if (arbo.inputs[[1]] == 'none'){ stop("arbo.inputs object is required to run the ArboMAP model. Please make sure it exists and is properly input.")    }

    message("Running ArboMAP.MOD model")

    # Unpack the arbo.inputs object
    stratafile = arbo.inputs$stratafile
    maxobservedhumandate = arbo.inputs$maxobservedhumandate
    var1name = arbo.inputs$var1name
    var2name = arbo.inputs$var2name
    compyear1 = arbo.inputs$compyear1
    compyear2 = arbo.inputs$compyear2

    # [[1]] is necessary to get just the results. [[2]] returns the inputs, and is for compatibility with the .Rmd format in which ArboMAP was initially developed
    ArboMAP.results = ArboMAP(human.data, mosq.data, districtshapefile, stratafile, weather.data,
                              weathersummaryfile, maxobservedhumandate, weekinquestion,
                              var1name, var2name, compyear1, compyear2, results.path, original.metrics = 0)[[1]]
    ArboMAP.results$model.name = "ArboMAP.MOD"
    ArboMAP.results$forecast.id = week.id
    ArboMAP.results$seasonal.mosquito.MLE = NA

    # Set up distributions - currently just a single entry because ArboMAP does not produce probabilistic outputs
    ArboMAP.distributions = list()
    ArboMAP.distributions$annual.human.cases = ArboMAP.results$human.cases
    ArboMAP.distributions$seasonal.mosquito.MLE = NA

    forecasts.df = update.df(forecast.targets, forecasts.df, ArboMAP.results)

    forecast.distributions = update.distribution(forecast.targets, ArboMAP.results$model.name, ArboMAP.results$forecast.id, forecast.distributions, ArboMAP.distributions)

  }

  # Run the Random Forest 1 model with only climate inputs
  if ("RF1_C" %in% models.to.run){

    check.dependencies("RF1", c("randomForest", "psych"))
    message("Running Random Forest 1 model")

    # Set up the sub-model output results
    rf1.results.path = sprintf("%s/rf1_results", results.path)
    if (!file.exists(rf1.results.path)){ dir.create((rf1.results.path))  }

    # Create a new rf1.inputs object and clear any added inputs
    rf1.inputs.no.extras = rf1.inputs
    rf1.inputs.no.extras[[1]] = NA
    rf1.inputs.no.extras[[2]] = NA

    RF1.out = rf1(human.data, mosq.data, districtshapefile, weather.data,
                      weekinquestion, rf1.inputs.no.extras, rf1.results.path)

    RF1.results = RF1.out[[1]]
    RF1.results$model.name = "RF1_C"
    RF1.results$forecast.id = week.id
    other.outputs$rf1c = RF1.results$other.results

    forecasts.df = update.df(forecast.targets, forecasts.df, RF1.results)

    RF1.forecast.distributions = RF1.out[[2]]
    forecast.distributions = update.distribution(forecast.targets, RF1.results$model.name, RF1.results$forecast.id, forecast.distributions, RF1.forecast.distributions)
  }

  # Run the Random Forest 1 model with all available inputs
  if ("RF1_A" %in% models.to.run){

    check.dependencies("RF1", c("randomForest", "psych"))
    message("Running Random Forest 1 model")

    # Set up the sub-model output results
    rf1.results.path = sprintf("%s/rf1_results", results.path)
    if (!file.exists(rf1.results.path)){ dir.create((rf1.results.path))  }

    #**# FIX OUTPUT FROM HERE TO ACCOUNT FOR DISTRIBUTIONS
    #**# LEFT OFF HERE
    RF1.out = rf1(human.data, mosq.data, districtshapefile, weather.data,
                      weekinquestion, rf1.inputs, rf1.results.path)

    RF1.results = RF1.out[[1]]
    RF1.results$model.name = "RF1_A"
    RF1.results$forecast.id = week.id
    other.outputs$rf1a = RF1.results$other.results

    forecasts.df = update.df(forecast.targets, forecasts.df, RF1.results)

    RF1.forecast.distributions = RF1.out[[2]]
    forecast.distributions = update.distribution(forecast.targets, RF1.results$model.name, RF1.results$forecast.id, forecast.distributions, RF1.forecast.distributions)
  }

  return(list(forecasts.df, forecast.distributions, other.outputs))
}

#' rf1.inputs
#'
#' The model-specific inputs required to run the RF1 model
#'
#' @param files.to.add A vector of file names of other data sources to be
#' included in the Random Forest model. If no additional data is to be added,
#' this should be an empty vector (i.e. c()) or NA.
#' @param merge.type.vec A vector identifying how the files should be joined to
#' the mosquito and human data. Options are spatial_temporal where merges will
#' be performed on county and year (e.g., climate data); state_temporal where
#' merges will be performed on state and year (e.g. BBS data at state level),
#' and spatial, where merges will be performed on county only (e.g., landcover
#' and census data). This must have the same length as files.to.add, and if no
#' additional data is to be added, this should be an empty vector (i.e. c()) or NA.
#' @param analysis.counties A list of counties included in the analysis (this
#' is for ensuring that a county and year that does not have a human case is
#' treated as a 0)
#' @param analysis.years A list of years included in the analysis (this is for
#' ensuring that a county and year that does not have a human case is treated
#' as a 0 rather than as missing) #**# This is problematic if a specific county
#' or year is missing.
#' @param user.drop.vars A list of independent variables that should be
#' excluded from the analysis. If no variables should be excluded, this should
#' be an empty vector c() or NA.
#' @param mosq.model The Random Forest model to use for forecasting mosquito
#' infection rates. If this is to be fitted from the empirical data, this
#' should be set to NA
#' @param human.model The Random Forest model to use for forecasting human
#' cases. If this is to be fitted from the empirical data, this should be set
#' to NA.
#' @param analyze.mosquitoes Whether (1) or not (0) results should be produced
#' for mosquitoes. These will only be in the other.results$rf1 object and
#' will not affect the forecast.df object
#' @param analyze.humans Whether (analyze.humans set to 1) or not (set to 0)
#' results for the human analysis should be produced. If set to 0, the
#'forecast.df object will return only NA's
#' @param no.data.exceptions #**# COMING SOON A list of county-years within the
#' range of analysis.years and analysis.counties that are missing data rather
#' than 0's
#'
#' @name rf1.inputs
NULL

#' Random Forest 1 Model Outputs
#'
#'#**# Link to documentation from main RF1 description
#'#**# SWITCH DESCRIPTION TO USE A TABLE/LIST FORMAT
#' @description other.outputs$rf1 gives a list of 2 objects. The first is the
#' mosquito results, the second is the human results. Each of these objects are
#' lists of objects. The first $MODEL is the Random Forest Model object. This
#' can be used to generate novel predictions. The second $TEMPORAL.ACCURACY is
#' a summary of the cross-validation accuracy, with $OVERALL giving the overall
#' accuracy. Other accuracy results include $RMSE, $SPEARMAN, $N, $ERROR.DF
#' The third is $ SPATIAL.ACCURACY which performs the same cross-validation,
#' except on spatial units instead of on years. This has the same structure as
#' $TEMPORAL.ACCURACY. The fourth is $TEMPORAL.NULL, which contains two null
#' model results for comparison ($NULL.MEAN and $NULL.CI) each with the same
#' structure as $TEMPORAL.ACCURACY. The fifth is $SPATIAL.NULL, which contains
#' null model results for spatial structure, following the same structure as
#' $TEMPORAL.NULL. The sixth object is $RETAINED.VARS, which includes a list of
#' independent variables that were retained in the final model. (NOTE: These
#' can also be extracted from the $MODEL object)
#'
#' @name rf1.outputs
NULL

#' DFMIP Hindcasts
#'
#' Generate hindcasts for a suite of models using common data inputs, and evaluate their accuracy using a standardized suite of validation metrics
#' @details Forecast targets not yet supported, but in development: \tabular{ll}{
#' human_incidence \tab Human cases divided by district population.\cr
#' peak_mosquito_MLE \tab Peak mosquito infection rate (maximum likelihood estimate) during the season (averaged over what time period?)\cr
#' number_positive_pools \tab The number of positive mosquito pools observed by district\cr
#' human_cases_binary \tab Whether or not human cases will occur in a district\cr
#' positive_pools_binary \tab Whether or not a district will have any positive mosquito pools\cr
#' peak_timing \tab The week (day?) of the peak mosquito infection rate\cr}
#'
#' @param forecast.targets The quantities for which hindcasts are to be made. Options are: \tabular{ll}{
#' annual.human.cases \tab Number of human cases\cr
#' seasonal.mosquito.MLE \tab Mosquito infection rate maximum likelihood estimate averaged over the entire season\cr}
#' @param models.to.run A string vector of the models to run. Options are: \tabular{ll}{
#' NULL.MODELS \tab Forecasts based on statewide incidence\cr
#' ArboMAP \tab Development version ArboMAP forecasts, see details section.\cr
#' ArboMAP.MOD \tab Modified version of ArboMAP model.\cr
#' RF1_C \tab Random Forest model, climate inputs only (i.e. equivalent inputs to ArboMAP).\cr
#' RF1_A \tab Random Forest model, all available inputs.\cr  }
#' Note these entries are case-sensitive and are run by keyword, so run in a fixed order (NULL.MODELS, ArboMAP, ArbMAP.MOD, RF1_C, RF1_A),
#' regardless of the order specified in the models.to.run vector.
#' @param focal.years The years for which hindcasts will be made. Hindcasts will use all prior years as training data.
#' @param human.data Data on human cases of the disease. Must be formatted with two columns: district and date. The district column contains the spatial unit (typically county), while the date corresponds to the date of the onset of symptoms for the human case.
#' #**# WHAT IF THESE DATA ARE MISSING? I.E. just making a mosquito forecast with RF1?
#' #**# Does the date need to be in a particular format? The sample data is "/" delimited.
#' @param mosq.data Data on mosquito pools tested for the disease. Must be formatted with 4 columns: district (the spatial unit, e.g. county), col_date: the date the mosquitoes were tested, wnv_result: whether or not the pool was positive, pool_size: the number of mosquitoes tested in the pool. A fifth column species is optional but is not used by the code
#' @param weather.data Data on weather variables to be included in the analysis. See the read.weather.data function for details about data format.
#' The read.weather.data function from ArboMAP is a useful way to process one or more data files downloaded via Google Earth Engine.
#' @param districtshapefile The shapefile with polygons representing the districts. #**# Are there required fields?
#' @param results.path The base path in which to place the modeling results. Some models will create sub-folders for model specific results
#' @param arbo.inputs Inputs specific to the ArboMAP model. If the ArboMAP model or ArboMAP.MOD model are not being run, this should be set to 'none' or omitted from the function call
#' @param population.df Census information for calculating incidence. Can be set to 'none' or omitted from the function call #**# NEEDS FORMAT INSTRUCTIONS
#' @param rf1.inputs Inputs specific to the RF1 model, see \code{\link{rf1.inputs}}. If this model is not included, this should be set to 'none' or omitted from the function call #**# LINK TO AN OBJECT WITH MORE DETAILS
#' @param threshold For continuous and discrete forecasts, a threshold of error to be used in classifying the forecast as "accurate". The default is +/- 1 human case, +/- 1 week, otherwise the default is 0.
#' @param percentage For continuous and discrete forecasts, if the prediction is wihtin the specified percentage of the observed value, the forecast is considered accurate. The default is +/- 25 percent of the observed.
#' @param id.string An ID to include in the forecast ID for this hindcast run (e.g., state)
#'
#' @export dfmip.hindcasts
dfmip.hindcasts = function(forecast.targets, models.to.run, focal.years, human.data, mosq.data,
                           weather.data, districtshapefile, results.path, arbo.inputs = 'none',
                           population.df = 'none', rf1.inputs = 'none', threshold = 'default',
                           percentage = 'default', id.string = "", season_start_month = 7, weeks_in_season = 2, sample_frequency = 1){

  # Indicator to denote the first time through the loop
  first = 1
  # List to hold outputs
  observation.list = list()
  # If seasonal.mosquito.MLE is a forecast target, calculate the observed
  if ("seasonal.mosqutio.MLE" %in% forecast.targets){
    md.data = calculate.MLE.v2(mosq.data, "annual")
    #**# Should be able to use a similar approach to estimate peak. Should make it easy to adapt the RF model to estimate a peak MLE as well.
    #**# Might be able to get timing at the same time.
  }

  # Run hindcast analysis for selected years
  for (year in focal.years){
    last.date = sprintf("%s-12-31", (year - 1))
    maxobservedhumandate <- as.Date(last.date, "%Y-%m-%d") # Last date of previous year

    temporal.vectors = get_sampling_weeks(year, season_start_month, weeks_in_season, sample_frequency)
    month.vec = temporal.vectors[[1]]
    day.vec = temporal.vectors[[2]]
    year.vec = temporal.vectors[[3]]

    # Other observed variables to consider calculating
    #mosq.data.year = mosq.data.all[mosq.data.all$year == year, ]
    #positive.districts = aggregate(mosq.data.year$wnv_result, by = list(mosq.data.year$district), max) # Max will be 1 if a district is positive, or 0 if it is negative
    #year.positive.districts = sum(positive.districts$x) #**# CHECK THAT THIS IS EQUIVALENT TO WHAT ARBOMAP IS ACTUALLY CALCULATING
    year.positive.districts = NA #**# THIS WAS NOT BEING CALCULATED CORRECTLY!

    # Format data to ensure compatibility with code below
    human.data$year = sapply(as.character(human.data$date), splitter,  "/", 3)
    mosq.data$date = mosq.data$col_date

    if ("annual.human.cases" %in% forecast.targets){
      #**# ALSO NEED THIS INFO BY DISTRICT WHEN ADDING THAT FUNCTIONALITY
      # Calculate the observed human cases for the year
      # Human data is in format of one case per year, so nrow works the same as a sum
      #**# This will fail if another date delimiter is used! Need to think about making it more general - can script a date.splitter function with grep and other tools to ensure the proper processing
      year.human.cases = nrow(human.data[human.data$year == year, ])
      year.key = sprintf("x%s", year) # Use of a numeric year causes the next step to fail. This makes it character with a non-numeric leading character.
      observation.list$annual.human.cases[[year.key]] = year.human.cases
    }

    if ("seasonal.mosquito.MLE" %in% forecast.targets){
      year.MLE = md.data[md.data$YEAR == year, ]
      seasonal.mosquito.MLE = mean(year.MLE$IR, na.rm = TRUE) #**# Average is problematic here, but I am not sure of a better approach at this point
      #**# Also need this per district when adding that functionality
      year.key = sprintf("x%s", year) # Use of a numeric year causes the next step to fail. This makes it character with a non-numeric leading character.
      observation.list$seasonal.mosquito.MLE[[year.key]] = seasonal.mosquito.MLE
    }


    # Loop through weeks to get a new forecast for each week for ArboMAP
    for (i in 1:length(month.vec)){

      month = month.vec[i]
      day = day.vec[i]
      year = year.vec[i]

      week.id = sprintf("%s:%04d-%02d-%02d", id.string, year, month, day)

      # Update the observed inputs and predict this weeks' cases
      this.weeks.cases = NA #**# NOT SCRIPTED as this is not one of the key comparison outputs

      # #**# THIS NEEDS TO BE REFRAMED IN THE CONTEXT OF FORECAST TARGETS
      # observed.inputs = list(observed.positive.districts = year.positive.districts,
      #                       observed.human.cases = year.human.cases,
      #                       observed.weeks.cases = this.weeks.cases)

      # Subset the human data object
      human.data.subset = human.data[human.data$year < year, ]
      #**# Current year needs to be dropped for ArboMAP, but this may not be appropriate for other approaches. Need to re-address when this is an issue.
      #**# NOTE: the current year is dropped in this step. For the Random Forest analysis, it comes back with just 0's for cases, which is not correct, but allows the code to run and does not cause any errors (as those observations are not used for predictions or extracted as true observations)
      #**# This step would corrupt the data from the standpoint of Nick DeFelice's approach.

      # Subset the mosquito data object
      mosq.data.subset = date.subset(mosq.data, year, month, day, 2)

      # Subset the weather data object
      weather.data.subset = date.subset(weather.data, year, month, day, 1)

      # Run analysis for selected weeks
      in.date = sprintf("%s-%02d-%02d", year, month, day)
      weekinquestion <- as.Date(in.date, "%Y-%m-%d")


      actual.models.to.run = models.to.run
      # If it is not the first iteration, DO NOT run the Random Forest model and NULL.MODELS (They can be run once as they do not update weekly)
      if (i != 1){
        actual.models.to.run = models.to.run[!models.to.run %in% c("RF1_C", "RF1_A", "NULL.MODELS")]
      }

      # If there are no models to run, give a warning (e.g., if multiple weeks are specified, but no models that update weekly)
      if (length(actual.models.to.run) == 0){
        warning(sprintf("No models for %s. Null and Random Forest models do not currently update weekly.", week.id))
      }

      # Otherwise, run the models
      if (length(actual.models.to.run) > 0){
        # Update the max observed human date (but this should not be critical, as we have removed data from the current year)
        #**# but a future version might make that change specific to ArboMAP
        arbo.inputs[[2]] = maxobservedhumandate

        out = dfmip.forecast(forecast.targets, actual.models.to.run, human.data.subset, mosq.data.subset, weather.data.subset,
                             districtshapefile, weekinquestion, week.id, results.path,
                             arbo.inputs, observed.inputs, population.df, rf1.inputs)
        out.forecasts = out[[1]]
        out.distributions = out[[2]]

        if (first == 1){
          forecasts.df = out.forecasts
          forecast.distributions = out.distributions
          first = 0
        }else{
          #test.type(out.forecasts, 'Line536')
          forecasts.df = rbind(forecasts.df, out.forecasts)

          if ("annual.human.cases" %in% forecast.targets){
            human.cases.list = forecast.distributions[['annual.human.cases']]
            human.cases.list = append(human.cases.list, out.distributions$annual.human.cases)
            forecast.distributions[['annual.human.cases']] = human.cases.list
          }
          if ("seasonal.mosquito.MLE" %in% forecast.targets){
            seasonal.mosquito.MLE.list = forecast.distributions[['seasonal.mosquito.MLE']]
            seasonal.mosquito.MLE.list = append(seasonal.mosquito.MLE.list, out.distributions$seasonal.mosquito.MLE)
            forecast.distributions[['seasonal.mosquito.MLE']] = seasonal.mosquito.MLE.list
          }
        }
      }
    }
  }

  # Create accuracy summary object
  accuracy.summary = data.frame(model = rep(models.to.run, length(forecast.targets)))

  # Loop through forecast targets
  for (i in 1:length(forecast.targets)){
    forecast.target = forecast.targets[i]
    #message("Checking distributions that have been output")
    #message(str(forecast.distributions))

    target.distributions = forecast.distributions[[forecast.target]]
    #message(names(target.distributions))

    keys = names(target.distributions)
    models = sapply(keys, splitter, ':', 1, as.string = 1)

    # Calculate accuracy over all forecasts for each model
    for (j in 1:length(models.to.run)){
      model = models.to.run[j]

      model.regex = sprintf("\\b%s\\b", model) # Add breaks to search for ONLY the model, and not any that have the model as part of the name.
      # Get an index of which forecasts belong to this model
      model.index = grep(model.regex, models)

      # Check that model results were extracted. If not, throw an error, and present possible causes
      if (length(model.index) == 0){
        stop(sprintf("No results for %s model. Models included in the results are %s. Please check for a typo in the model names", model, paste(models, collapse = ', ')))
      }

      observations.vec = c()

      # Create forecast distributions matrix and observation vector
      for (k in 1:length(model.index)){
        this.index.value = model.index[k]
        this.key = keys[this.index.value]
        #message(this.key)
        this.distribution = target.distributions[[this.key]]

        this.key = as.character(this.key) # Ensure the key is in character format
        this.date = splitter(this.key, ":", 3, 1)
        this.date = as.character(this.date)
        this.year = splitter(this.date, '-', 1)
        this.year.key = sprintf("x%s", this.year)

        #message(model)
        #message(this.index.value)
        #message(this.key)
        #message(this.date)
        #message(forecast.target)
        #message(this.year.key)
        #message(str(observation.list))
        #message(paste(model.index, collapse = ',')) #**# IS model.index NA for soemthing?
        this.observation = observation.list[[forecast.target]][[this.year.key]]

        # Create/update a distributions matrix to hold values
        if (k == 1){
          model.forecast.distributions = matrix(this.distribution, nrow = 1)
          rownames(model.forecast.distributions) = this.key
        }else{
          #**# Rbind assumes the matrices are the same length. Otherwise need to add code to pad with NA's. Solve this defect when it is actually a problem.
          #test.type(this.distribution, 'Line536')
          model.forecast.distributions = rbind(model.forecast.distributions, this.distribution)
          rownames(model.forecast.distributions)[k] = this.key
        }

        # Create/update an observation vector to hold observations
        observations.vec = c(observations.vec, this.observation)
      }

      # Assess hindcast accuracy for each target
      target.accuracy.metrics = assess.accuracy(model.forecast.distributions, observations.vec, forecast.target)

      # Update accuracy.summary
      for (item in names(target.accuracy.metrics)){
        position.index = j + (j* (i-1))
        accuracy.summary$forecast.target = forecast.target
        accuracy.summary[[item]][position.index] = target.accuracy.metrics[[item]]
      }
    }
  }

  return(list(accuracy.summary, forecasts.df, forecast.distributions))
}


#' Update data frame
#'
#' @description forecasts.df should be initialized as NA if it does not already exist and contain data.
#' Otherwise, it is updated with the results. This function provides a standardized format for inputting
#' data from different models in to a common data frame
#'
#' @param forecast.targets The targets to include in the output data frame. See \code{\link{dfmip.forecast}} for options.
#' @param forecasts.df The data frame object to contain the results from dfmip
#' @param results.object The results object to be integrated into forecasts.df
#'
update.df = function(forecast.targets, forecasts.df, results.object){

  #' annual.human.cases \tab Number of human cases\cr
  #' seasonal.mosquito.MLE \tab Mosquito infection rate maximum likelihood estimate averaged over the entire season\cr }

  # Unpack the results object
  model.name = results.object$model.name
  forecast.id = results.object$forecast.id

  # Plot observed vs. forecast human cases by state, year, and date of forecast
  UNIT = splitter(as.character(forecast.id), ":", 1, 1)
  date = splitter(as.character(forecast.id), ":", 2, 1)
  year = splitter(date, "-", 1, 0)

  # Update human cases, if applicable
  if ("annual.human.cases" %in% forecast.targets){
    annual.human.cases = results.object$annual.human.cases

  }

  # Update seasonal mosquito MLE, if applicable
  if ("seasonal.mosquito.MLE" %in% forecast.targets){
    seasonal.mosquito.MLE = results.object$seasonal.mosquito.MLE
  }

  # Other former targets
  #annual.positive.district.weeks = results.object$annual.positive.district.weeks
  #multiplier = results.object$multiplier
  #weeks.cases = results.object$weeks.cases


  # Create the results object if it does not already exist
  if (is.na(forecasts.df)){
    forecasts.df = data.frame(MODEL.NAME = model.name, FORECAST.ID = as.character(forecast.id), UNIT = as.character(UNIT), DATE = as.character(date), YEAR = year)

    if ("annual.human.cases" %in% forecast.targets){  forecasts.df$annual.human.cases = annual.human.cases }
    if ("seasonal.mosquito.MLE" %in% forecast.targets){ forecasts.df$seasonal.mosquito.MLE = seasonal.mosquito.MLE  }
    #ANNUAL.POSITIVE.DISTRICT.WEEKS = annual.positive.district.weeks,
    #FORECAST.WEEK.CASES = weeks.cases

    # Ensure that fields come in as character, so that factor levels are not locked and fields are fully updateable
    forecasts.df$MODEL.NAME = as.character(forecasts.df$MODEL.NAME)
    forecasts.df$FORECAST.ID = as.character(forecasts.df$FORECAST.ID)
    forecasts.df$UNIT = as.character(forecasts.df$UNIT)
    forecasts.df$DATE = as.character(forecasts.df$DATE)

    # Otherwise, update it using rbind
  }else{
    #**# Watch that there are not issues with this order-based approach. Could have troubles if orders are changed.
    targets = c()
    if ("annual.human.cases" %in% forecast.targets){  targets = c(targets, annual.human.cases) }
    if ("seasonal.mosquito.MLE" %in% forecast.targets){ targets = c(targets, seasonal.mosquito.MLE)  }
    #annual.positive.district.weeks
    #weeks.cases

    new.row = c(model.name, forecast.id, UNIT, date, year, targets)
    #test.type(new.row, 'L682')
    forecasts.df = rbind(forecasts.df, new.row)
  }

  return(forecasts.df)
}


#' Update list of forecast distributions
#'
#' @description forecasts.df should be initialized as NA if it does not already exist and contain data.
#' Otherwise, it is updated with the results. This function provides a standardized format for inputting
#' data from different models in to a common data frame
#'
#' @param forecast.targets The targets to include in the output data frame. See \code{\link{dfmip.forecast}} for options.
#' @param model.name The name of the model run
#' @param forecast.id The ID for this forecast. Together with Model Name this should be a unique key
#' @param forecast.distributions The list object to contain the forecast distribution information
#' @param results.object The results object to be integrated into forecast.distributions
#'
#' @return forecast.distributions A list object containing forecast probability distributions organized by forecast.keys consisting of model names and forecast IDs.
#'
update.distribution = function(forecast.targets, model.name, forecast.id, forecast.distributions, results.object){

  # #' annual.human.cases \tab Number of human cases\cr
  # #' seasonal.mosquito.MLE \tab Mosquito infection rate maximum likelihood estimate averaged over the entire season\cr }

  # Create a forecast key
  forecast.key = sprintf("%s:%s", model.name, forecast.id)

  # Update human cases, if applicable
  if ("annual.human.cases" %in% forecast.targets){ annual.human.cases = results.object$annual.human.cases  }

  # Update seasonal mosquito MLE, if applicable
  if ("seasonal.mosquito.MLE" %in% forecast.targets){  seasonal.mosquito.MLE = results.object$seasonal.mosquito.MLE  }

  # Other former targets
  #annual.positive.district.weeks = results.object$annual.positive.district.weeks
  #multiplier = results.object$multiplier
  #weeks.cases = results.object$weeks.cases

  # Create the results object if it does not already exist
  if (is.na(forecast.distributions)){
    forecast.distributions = list()

    if ("annual.human.cases" %in% forecast.targets){
      human.case.list = list()
      human.case.list[[forecast.key]] = annual.human.cases
      forecast.distributions[['annual.human.cases']] = human.case.list
    }
    if ("seasonal.mosquito.MLE" %in% forecast.targets){
      seasonal.mosquito.MLE.list = list()
      seasonal.mosquito.MLE.list[[forecast.key]] = seasonal.mosquito.MLE
      forecast.distributions[['seasonal.mosquito.MLE']] = seasonal.mosquito.MLE.list
    }
    #ANNUAL.POSITIVE.DISTRICT.WEEKS = annual.positive.district.weeks,
    #FORECAST.WEEK.CASES = weeks.cases

    # Otherwise, update the existing lists
  }else{
    if ("annual.human.cases" %in% forecast.targets){
      if ('annual.human.cases' %in% names(forecast.distributions)){
        human.case.list = forecast.distributions[['annual.human.cases']]
        human.case.list[[forecast.key]] = annual.human.cases
        forecast.distributions[['annual.human.cases']] = human.case.list
      }else{
        human.case.list = list()
        human.case.list[[forecast.key]] = annual.human.cases
        forecast.distributions[['annual.human.cases']] = human.case.list
      }
    }

    if ("seasonal.mosquito.MLE" %in% forecast.targets){
      if ('seasonal.mosquito.MLE' %in% names(forecast.distributions)){
        seasonal.mosquito.MLE.list = forecast.distributions[['seasonal.mosquito.MLE']]
        seasonal.mosquito.MLE.list[[forecast.key]] = seasonal.mosquito.MLE
        forecast.distributions[['seasonal.mosquito.MLE']] = seasonal.mosquito.MLE.list
      }else{
        seasonal.mosquito.MLE.list = list()
        seasonal.mosquito.MLE.list[[forecast.key]] = seasonal.mosquito.MLE
        forecast.distributions[['seasonal.mosquito.MLE']] = seasonal.mosquito.MLE.list
      }
    }
    #annual.positive.district.weeks
    #weeks.cases
  }

  return(forecast.distributions)
}


#' Update data frame (old version no longer compatible)
#'
#' @description forecasts.df should be initialized as NA if it does not already exist and contain data.
#' Otherwise, it is updated with the results. This function provides a standardized format for inputting
#' data from different models in to a common data frame
#'
#' @param forecasts.df The data frame object to contain the results from dfmip
#' @param results.object The results object to be integrated into forecasts.df
#' @param observed.inputs If the model is being used to hindcast, include the
#' observed values for comparison to the model predictions
#'
update.df.old = function(forecasts.df, results.object, observed.inputs){

  # Unpack the results object
  model.name = results.object$model.name
  forecast.id = results.object$forecast.id

  annual.positive.district.weeks = results.object$annual.positive.district.weeks
  annual.human.cases = results.object$annual.human.cases
  multiplier = results.object$multiplier
  weeks.cases = results.object$weeks.cases

  # Plot observed vs. forecast human cases by state, year, and date of forecast
  state = splitter(as.character(forecast.id), "_", 1, 1)
  date = splitter(as.character(forecast.id), "_", 2, 1)
  year = splitter(date, "-", 1, 0)

  if (!is.na(observed.inputs)){
    #message(sprintf("Observed human cases was %s", observed.human.cases))
    observed.positive.districts = observed.inputs$observed.positive.districts
    observed.human.cases = observed.inputs$observed.human.cases
    observed.weeks.cases = observed.inputs$observed.weeks.cases
  }else{
    observed.positive.districts = observed.human.cases = observed.weeks.cases = NA
    #message("observed.inputs was NA")
  }

  # Create the results object if it does not already exist
  if (is.na(forecasts.df)){
    forecasts.df = data.frame(MODEL.NAME = model.name, FORECAST.ID = as.character(forecast.id), STATE = as.character(state), DATE = as.character(date), YEAR = year,
                              ANNUAL.POSITIVE.DISTRICT.WEEKS = annual.positive.district.weeks,
                              OBSERVED.POSITIVE.DISTRICTS = observed.positive.districts, annual.human.cases = annual.human.cases,
                              OBSERVED.HUMAN.CASES = observed.human.cases, MULTIPLIER = multiplier,
                              FORECAST.WEEK.CASES = weeks.cases, OBSERVED.WEEK.CASES = observed.weeks.cases)

    # Ensure that fields come in as character, so that factor levels are not locked and fields are fully updateable
    forecasts.df$MODEL.NAME = as.character(forecasts.df$MODEL.NAME)
    forecasts.df$FORECAST.ID = as.character(forecasts.df$FORECAST.ID)
    forecasts.df$STATE = as.character(forecasts.df$STATE)
    forecasts.df$DATE = as.character(forecasts.df$DATE)
  # Otherwise, update it using rbind
  }else{
    new.row = c(model.name, forecast.id, state, date, year, annual.positive.district.weeks, observed.positive.districts,
                annual.human.cases, observed.human.cases, multiplier, weeks.cases, observed.weeks.cases)
    #test.type(new.row, 'L815')
    forecasts.df = rbind(forecasts.df, new.row)
  }

  return(forecasts.df)
}

#' Split strings using strsplit in a way that can easily be used within sapply
#'
#' @param string The string to be split
#' @param delimiter What to use to split the string
#' @param position Which part of the string to retain. Only one piece can be retained
#' To retain multiple pieces, do a separate function call for each piece
#' @param as.string 0 Result will be numeric, 1 Result will be string
#'
#'@export splitter
splitter = function(string, delimiter, position, as.string = 0){
  parts = strsplit(string, delimiter)[[1]]
  out = parts[position]

  if (as.string == 0){
    out = as.numeric(as.character(out)) # Make sure it is in number format and drop any leading 0's
  }
  if (as.string == 1){  out = as.character(out)  }

  return(out)
}

#' Make a function that can be applied with sapply to split text
#' #**# REPLACE INSTANCES OF THIS WITH splitter function
split.text = function(x, part){
  parts = strsplit(x, "_")[[1]]
  out = parts[part]
  return(out)
}

#' Split strings using substr in a way that can easily be used within sapply
#'
#' @param string The string to be split
#' @param start The starting position for the split
#' @param end The ending position for the split
#'
splitter2 = function(string, start, end){
  out = substr(string,start, end)
  out = as.numeric(as.character(out))
  return(out)
}

#' Restrict data to a single date
#'
#' df must have a column "date" in format YYYY-MM-DD (date.format = 1) or MM/DD/YYYY (date.format = 2)
#' Note that the date format was slow for string operations, so a datestr field will be added
#' df must have a column "year" in format YYYY
#'
#'@export date.subset
date.subset = function(df, end.year, end.month, end.day, date.format){

  # Format for splitting out the weather data
  if (date.format == 1){
    # add df$month and df$day columns
    df$datestr = as.character(df$date) # Speeds up the splitting process
    df$month = sapply(df$datestr, splitter, '-', 2)
    df$day = sapply(df$datestr, splitter, "-", 3)
  }

  # Format for human and mosquito data
  if (date.format == 2){
    df$datestr = as.character(df$date)
    df$year = sapply(df$datestr, splitter, "/", 3)
    df$month = sapply(df$datestr, splitter, "/", 1)
    df$day = sapply(df$datestr, splitter, '/', 2)
  }

  #df$month = sapply(df$date, splitter2, 6,7) #**# Rate limiter was the date format, not the function used to split the date field

  # First remove all records beyond the year in question
  df.1 = df[df$year <= end.year, ]

  # Then remove all records beyond the last month in the last of the years
  index = df.1$year == end.year & df.1$month > end.month
  df.2 = df.1[!index, ]
  #df.2$datestr[(nrow(df.2)-10):nrow(df.2)]

  index.3 = df.2$year == end.year & df.2$month == end.month & df.2$day > end.day
  df.3 = df.2[!index.3, ]
  #df.3$datestr[(nrow(df.3)-10):nrow(df.3)] #Check sorting; data is sorted by year, and looking at the end shows the dropping of the non-matching dates

  return(df.3)
}


#' Mean Incidence Model
#'
#' Predict the number of human cases of WNV based on historical number of human cases, adjusted for population
#' This model does not adjust risk based on county - clearly an incorrect model assumption, but one that is of interest
#'
#'@param human.data Standardized human data input
#'@param population.df A vector with districts (counties) and their populations. A column labeled SPATIAL should contain the county/district information, while population should be in a TOTAL_POPULATION column.
#'
mean.incidence.model = function(human.data, population.df, cases.per.year){

  # Subset the population data to just spatial locations included in human.data
  population.df = population.df[as.character(population.df$SPATIAL) %in% as.character(human.data$district), ]

  incidence.per.year = cases.per.year / sum(population.df$TOTAL_POPULATION)
  spatial.cases.per.year = data.frame(SPATIAL = population.df$SPATIAL, TOTAL_POPULATION = population.df$TOTAL_POPULATION, CASES.STATEWIDE.INCIDENCE = (population.df$TOTAL_POPULATION * incidence.per.year), STATEWIDE.INCIDENCE = incidence.per.year)

  # Calculate indicence on a district-specific basis
  spatial.cases.per.year$CASES.DISTRICT.INCIDENCE = NA
  spatial.cases.per.year$DISTRICT.INCIDENCE = NA

  # Loop over districts
  for (i in 1:length(spatial.cases.per.year$SPATIAL)){
    district = as.character(spatial.cases.per.year$SPATIAL[i])
    district.population = spatial.cases.per.year$TOTAL_POPULATION[i]
    human.data.subset = human.data[as.character(human.data$district) == district, ]
    total.district.cases = nrow(human.data.subset)
    district.cases.per.year = total.district.cases / n.years
    district.incidence = district.cases.per.year / district.population

    # update spatial.cases.per.year
    spatial.cases.per.year$CASES.DISTRICT.INCIDENCE[i] = district.cases.per.year
    spatial.cases.per.year$DISTRICT.INCIDENCE[i] = district.incidence
  }

  #**# Think about an area-based incidence model - except this would be more of a thought exercise for NYS, as some of the smallest counties have the highest numbers of cases

  return(list(incidence.per.year, spatial.cases.per.year))
}

#' Seasonal Incidence Model
#'
#'#**# STILL UNDER DEVELOPMENT
#'#**# NEED TO THINK ABOUT WEEK STARTING POINT
#'
#'@param week.start The day of year on which the week starts in the focal year
#'#**# should this just be year? I bet there is a pattern based on year.
#'
#' Can apply this at the district level by multiplying the estimates by the mean annual cases for each district.
#' But likely too few in NYS for that to be particularly useful?
#'
seasonal.incidence = function(human.data, focal.year, mean.annual.cases){

  # Convert date to day of year
  human.data$month = sapply(as.character(human.data$date), splitter, "/", 1)
  human.data$day = sapply(as.character(human.data$date), splitter, "/", 2)
  human.data$doy = mapply(get.DOY, human.data$year, human.data$month, human.data$day)

  hist(human.data$doy, breaks = seq(1,366))

  # Use truncation dates from ArboMAP
  Early.Cutpoint = get.DOY(2001,5,1) # Assume a non-leap year
  Late.Cutpoint = get.DOY(2001, 11,30) # Assume a non-leap year
  human.data.truncated = human.data[human.data$doy >= Early.Cutpoint & human.data$doy <= Late.Cutpoint, ]
  out = hist(human.data.truncated$doy, breaks = seq(Early.Cutpoint,Late.Cutpoint), prob = TRUE)
  midpoints = out$breaks + 0.5 #**# 0.5 is hardcoded and corresponds to a bin size of 1
  midpoints = midpoints[1:(length(midpoints) - 1)] # Drop last midpoint - it is out of range

  # Estimate mean and sd for fitting normal distribution
  hd.mean = mean(human.data.truncated$doy)
  hd.sd = sd(human.data.truncated$doy)
  curve(dnorm(x, hd.mean, hd.sd), col = 2, add = TRUE) # I have no idea where the 'x' comes from, but it is really unhappy if it is not there!

  # Look at log-normal distribution (not appreciably different for the NY data)
  #human.data.truncated$ln.doy = log(human.data.truncated$doy)
  #hd.ln.mean = mean(human.data.truncated$ln.doy)
  #hd.ln.sd = sd(human.data.truncated$ln.doy)
  #curve(dlnorm(x, hd.ln.mean, hd.ln.sd), col = 3, add = TRUE)

  # Split the incidence up over weeks in the focal period
  jan.start.week = get.start.week(focal.year)

  start.week = jan.start.week
  # Add 7 days until we enter the data range
  while(start.week < Early.Cutpoint){
    start.week = start.week + 7
  }

  # Identify end week when the doy is no longer in the range
  # (subtracting 14 from Late.Cutpoint keeps the end of the end week from exceeding the late cutpoint range)
  end.week = start.week
  while(end.week <= (Late.Cutpoint - 14)){
    end.week = end.week + 7
  }

  #**# I THINK I NEED TO OFFSET BY 3.5? AND MAYBE PAD WITH A WEEK ON EITHER END?
  # Use the normal distribution to make an incidence prediction for every week from start week to end week
  #estimates = dnorm(seq(start.week, end.week, 7), hd.mean, hd.sd) # sum(estimates) = 0.14 - this is not correct!, but multiply by 7 and you get 0.999992, which is correct.
  estimates = dnorm(seq(start.week, end.week, 1), hd.mean, hd.sd) # sum(estimates) = 0.9999831 - that works, but need to aggregate by 7's #**# Would that be the same as multiplying the above by 7?

  # Offset by 3.5 to set the estimate for the mid-point of the weeks #**# I THINK THIS IS RIGHT? #**# CHECK WITH A SIMULATED NORMAL DISTRIBUTION
  estimates = dnorm(seq(start.week + 3.5, end.week + 3.5, 7), hd.mean, hd.sd) #**# Do we need buffers on the edges to remove edge effects? I.e. do the end bins contain all the probability below that point?
  #estimates = estimates[2:length(estimates)]
  # sum(estimates) * 7 = 0.9999827
  estimates = estimates * 7 # Multiply by 7 days in intervals
  week.doy = seq(start.week, end.week, 7)

  # Multiply by mean number of cases in a year to get the fraction that should happen in each week
  estimated.cases = round(estimates * mean.annual.cases, 1)

  seasonal.cases = data.frame(WEEK.START = week.doy, PROBABILITY.DENSITY = estimates, ESTIMATED.CASES = estimated.cases)

  return(seasonal.cases)

}

#' Goal is to be able to have predictions comparable to ArboMAP
get.start.week = function(year){

  if (year <= 1900 | year >= 2100){
    stop("get.start.week will not work for years before 1901 or after 2099")
  }

  #Goal is to efficiently code this pattern
  # In 2021, First sunday is Jan 3
  # In 2020, First sunday is Jan 5
  # In 2019, First sunday is Jan 6
  # In 2018, First sunday is Jan 7
  # in 2017, First sunday is Jan 1
  # in 2016, First sunday is Jan 3

  # Select a reference year
  ref.year = 2020
  # use the observed date from that year
  ref.start = 5

  # Calculate the difference to the year in question
  delta = ref.year - year

  # Account for the missing leap years
  # Note: 2000 is a leap year, so don't have to worry about break in pattern
  leaps = floor(delta / 4)

  start = ref.start + delta + leaps

  # Ensure that start never exceeds 7
  while(start > 7){
    start = start - 7
  }

  # Ensure that start never drops below 1
  while(start < 1){
    start = start + 7
  }

  return(start)
}


#' Get Day of Year, given year, month, and day
#' COPIED FROM wnv_hlpr.R
get.DOY = function(year, month, day){

  # Determine if it is a leap year
  days = get.days(year)

  # Sum up days from months
  Jan = 31
  Feb = 28
  Mar = 31
  Apr = 30
  May = 31
  Jun = 30
  Jul = 31
  Aug = 31
  Sep = 30
  Oct = 31
  Nov = 30
  Dec = 31

  if (days == 366){  Feb = 29  }
  months = c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)

  month.days = 0
  if (month != 1){
    month.days = sum(months[1:(month - 1)])
  }

  # Add days from days
  doy = month.days + day

  return(doy)
}

#' Function to get number of days in a year (copied from wnv_hlpr.R)
#'
get.days = function(year){

  # Add check - not designed for 1900 leap year, and won't work past 2400
  if (year <= 1900 | year >= 2100){ stop("Not designed years <= 1900 or >= 2100")}

  # Days are 365 unless it is a leap year
  days = 365
  if (year %% 4 == 0){
    days = 366
  }
  return(days)
}


#' Run the rf1 model
#'
#' The RF1 model first fits a random forest, then excludes all variables with
#' importance scores below the mean importance (NOTE: This will be problematic
#' if all input variables are relevant) It then further pares down the variable
#' list using variance partitioning. It retains only variables that contribute
#' uniquely to explaining variation in the unmeasured year (via
#' leave-one-year-out cross-validation). The RF1 model uses the randomForest
#' package by Liaw & Wiener 2002, that implements the Random Forest method
#' developed by Breiman 2001. MLE calcualtions from MLE_IR.R were written by
#' Williams and Moffit 2005 (reformatted by A. Keyel). Note that an arbitrary
#' starting seed is set, to ensure that results are repeatable.
#'
#' Citations
#'    Breiman, L. 2001. Random forests. Machine Learning 45: 5- 32
#'    Keyel, A.C. et al. 2019 PLOS ONE 14(6): e0217854. https://doi.org/10.1371/journal.pone.0217854
#'    Liaw & Wiener 2002. Classification and Regression by randomForest. R News 2: 18-22
#'    Williams, C and C. Moffitt 2005. Estimation of pathogen prevalence in pooled samples
#'    using maximum likelihood methods and open source software. Journal of Aquatic Animal Health 17: 386-391
#'
#' @param human.data Data on human cases of the disease. Must be formatted with
#'   two columns: district and date. The district column contains the spatial
#'   unit (typically county), while the date corresponds to the date of the
#'   onset of symptoms for the human case.
#' @param mosq.data Data on mosquito pools tested for the disease. Must be
#'   formatted with 4 columns: district (the spatial unit, e.g. county),
#'   col_date: the date the mosquitoes were tested, wnv_result: whether or not
#'   the pool was positive, pool_size: the number of mosquitoes tested in the
#'   pool. A fifth column species is optional but is not used by the code
#' @param districtshapefile The shapefile with polygons representing the
#'   districts. #**# Is this used by RF1?
#' @param weather.data Data on weather variables to be included in the analysis.
#'   See the read.weather.data function for details about data format. The
#'   read.weather.data function from ArboMAP is a useful way to process one or
#'   more data files downloaded via Google Earth Engine.
#' @param weekinquestion The focal week for the forecast. For the Random Forest
#'   model, this will be the last day used for making the forecast
#' @param rf1.inputs Inputs specific to the RF1 model, see
#'   \code{\link{rf1.inputs}}. If this model is not included, this should be set
#'   to 'none' or omitted from the function call #**# LINK TO AN OBJECT WITH
#'   MORE DETAILS
#' @param results.path The base path in which to place the modeling results.
#'   Some models will create sub-folders for model specific results
#' @param break.type The temporal frequency to use for the data. The default is
#'   'seasonal' which breaks the environmental data into January, February,
#'   March; April, May, June; July, August, September; October, November,
#'   December. Other options may be supported in the future.
#' @param response.type Whether data should be treated as continuous (mosquito
#'   rates, number of cases) or binary (0 or 1).
#'
#' @export
rf1 = function(human.data, mosq.data, districtshapefile, weather.data,
               weekinquestion, rf1.inputs, results.path, break.type = "seasonal", response.type = "continuous"){

  out = FormatDataForRF1(human.data, mosq.data, weekinquestion, weather.data, rf1.inputs, results.path, break.type)
  my.data = out[[1]]
  independent.vars = out[[2]]

  # Split the data set into the forecast year and the historical data
  forecast.year = as.numeric(substr(as.character(weekinquestion), 1, 4))

  forecast.data = my.data[my.data$year == forecast.year, ]
  historical.data = my.data[my.data$year < forecast.year, ] #**# This will prevent later years from informing hindcasts of earlier years

  #**# How do I go from a MIR to positive district weeks? Isn't the MIR more useful?
  # First question: Could estimate number of trap nights, number of mosquitoes sampled, and then use the infection rate to get an estimate of the number of positive district-weeks.
  # For second question - does seem like a lot of work to get at a number that tells us what? In ArboMAP it is used to estimate human cases, but we do that directly.
  #**# So for now, maybe the mosquito results aren't relevant? Or phrased better - are not relevant to the specific comparison question being asked.
  annual.positive.district.weeks = NA

  #**# WHAT AM I DOING WITH INFECTION RATE? PREVIOUSLY I ESTIMATED IT OVER THE ENTIRE YEAR, BUT HERE, THE DATA MIGHT BE MISSING OR FOR PART OF THE YEAR.
  #**# I could use the forecast mosquito infection rate? Would that be more useful than just leaving it out?
  # NOTE: the mosquito-only analysis is not directly connected to the prediction of human cases #**# Is there a way to do that, if human cases are not directly estimated?
  analyze.mosquitoes = rf1.inputs[[8]]
  mosquito.results = NA # If not analyzing mosquitoes
  seasonal.mosquito.MLE = NA
  if (analyze.mosquitoes == 1){
    # First forecast mosquito infection rates
    mosq.model = rf1.inputs[[6]]

    # If the mosquito model needs to be estimated, estimate it
    if (is.na(mosq.model)){
      dep.var = "IR"
      # drop IR variable from independent.vars
      m.independent.vars = independent.vars[independent.vars != "IR"]
      mosquito.results.path = sprintf("%s/mosquitoes", results.path)
      m.label = "mosquitoes" #**# THINK ABOUT THIS AND WHETHER THIS SHOULD BE AN ENTRY in rf1.inputs
      mosquito.results = do.rf(historical.data, dep.var, m.independent.vars, mosquito.results.path,
                               response.type = response.type, label = m.label) #do.spatial = 0, create.test.set = 0, create.ci.null = 0
      mosq.model = mosquito.results[[1]]
    }

    # Use the fitted model to make a prediction for the forecast year
    forecast.data = cbind(forecast.data, predict(mosq.model, newdata = forecast.data))

    # Rename the added column to distinguish it from the human predictions
    colnames(forecast.data)[ncol(forecast.data)] = "MOSQ.PREDICTION"
    seasonal.mosquito.MLE = mean(forecast.data$MOSQ.PREDICTION, na.rm = TRUE) #**# NOTE: perhaps districts should be weighted in this estimate
  }

  # If not running the human model
  human.results = NA
  annual.human.cases = NA

  # Run the human analysis (or not)
  analyze.humans = rf1.inputs[[9]]
  if (analyze.humans == 1){
    # Second, forecast human cases
    human.model = rf1.inputs[[7]]

    # If the human model needs to be estimated, estimate it
    if (is.na(human.model)){
      dep.var = "Cases"
      human.results.path = sprintf("%s/humans", results.path)
      h.label = "humans"
      human.results = do.rf(historical.data, dep.var, independent.vars, human.results.path, label = h.label, response.type = response.type)
      human.model = human.results[[1]]
    }

    forecast.data = cbind(forecast.data, predict(human.model, newdata = forecast.data))
    colnames(forecast.data)[ncol(forecast.data)] = "HUMAN.PREDICTION"

    annual.human.cases = sum(forecast.data$HUMAN.PREDICTION)

  }

  # Output required data for dfmip
  RF1.results = list()
  RF1.results$annual.positive.district.weeks = annual.positive.district.weeks
  RF1.results$annual.human.cases = annual.human.cases
  RF1.results$seasonal.mosquito.MLE = seasonal.mosquito.MLE
  RF1.results$multiplier = NA
  RF1.results$weeks.cases = NA
  RF1.results$other.results = list(mosquito.results, human.results)

  # Currently outputting a non-probabilistic distribution, so just return the single point estimates
  RF1.distributions = list()
  RF1.distributions$annual.human.cases = annual.human.cases
  RF1.distributions$seasonal.mosquito.MLE = seasonal.mosquito.MLE

  return(list(RF1.results, RF1.distributions))
}

#' A random forest based disease forecasting approach
#'
#'@description The do.rf function runs the random forest portion of the code
#'
#'@details Majority of code was writen in wnv_hlpr.R; January - June 2018.
#'Transferred to this file & modified beginning in March 2019. Code adapted to
#'use common inputs with the ArboMAP code of Davis & Wimberly
#'
#' #**# Add parameter descriptions.
#'
do.rf = function(trap.data, dep.var, independent.vars, results.path, do.spatial = 1, create.test.set = 1,
                  create.ci.null = 1, label = "", response.type = "continuous", exploratory = TRUE,
                  input.seed = 20180830, temporal.field = "year", spatial.field = "district"){

  require(psych)

  response.types = c("continuous", "binary")
  if (!response.type %in% response.types){ stop(sprintf("Response type must be %s, and is case sensitive", paste(response.types, collapse = ", "))) }

  # Reset the seed to ensure that the random elements in the code are repeataable
  set.seed(input.seed)

  # Subset data to just non-NA values
  trap.data = trap.data[!is.na(trap.data[[dep.var]]), ]

  # Add MERGE_ID #**# FIX THIS TO BE MORE GENERAL / MORE ROBUST
  trap.data$MERGE_ID = trap.data$county_year

  #**# Also formally output these somewhere  Report after test data set is removed
  # Report sample size
  N = nrow(trap.data)
  NO.IR.N = nrow(trap.data[is.na(trap.data$IR), ]) # Get number of NA values for Infection Rate
  IR.not.zero = nrow(trap.data[trap.data$IR > 0, ])
  WNV.N = IR.not.zero + NO.IR.N
  message(sprintf("Total N: %s\nWNV+ N: %s\nNo IR N: %s", N, WNV.N, NO.IR.N))
  message("These numbers are not valid for the human case analyses")


  # Set up the dependent variable
  if (response.type == "continuous"){ trap.data[[dep.var]] = as.numeric(as.character(trap.data[[dep.var]])) }
  if (response.type == "binary"){ trap.data[[dep.var]] = as.factor(trap.data[[dep.var]]) }

  ### EXAMINE PREDICTORS FOR COLLINEARITY
  # Run for Everything #**# Put someplace that makes more sense
  correlation.path = sprintf("%s/ModelResults/correlations", results.path)
  dir.create(correlation.path, showWarnings = FALSE, recursive = TRUE)
  tiff(filename = sprintf("%s/pairspanel_%s.tif", correlation.path, label), height = 3000, width = 3000)
  psych::pairs.panels(trap.data[ , independent.vars])
  dev.off()

  if (exploratory == TRUE){
    ### RUN MODEL FOR ALL PREDICTOR VARIABLES
    f = as.formula(paste(dep.var, ' ~ ', paste(independent.vars, collapse = "+")))

    best.m = get.best.m(f, independent.vars, trap.data, response.type)
    #best.m = 3 #**#

    # Regenerate best model using more trees
    message("Creating the model with the best m using 5000 trees")
    rf.model = randomForest(f, data = trap.data, na.action = na.exclude, stringsAsFactors = TRUE, importance = TRUE, mtry = best.m, ntree = 5000)

    important.stuff = sort(rf.model$importance[,1], decreasing = TRUE)

    # Show plot of mean importance, with a line indicating the mean importance cut-off
    importance.path = sprintf("%s/ModelResults/importance", results.path)
    dir.create(importance.path, showWarnings = FALSE, recursive = TRUE)
    tiff(filename = sprintf("%s/importance_%s.tif", importance.path, label))
    plot(seq(1,length(important.stuff)), important.stuff)
    segments(0,mean(important.stuff), length(independent.vars), mean(important.stuff))
    dev.off()

    # Keep any variables above the mean importance
    best.vars = names(important.stuff)[important.stuff > mean(important.stuff)]

    message("Creating a refined model that only includes variables of high importance")
    f2 = as.formula(paste(dep.var, ' ~ ', paste(best.vars, collapse = "+")))
    best.m = get.best.m(f2, best.vars, trap.data, response.type)# Recreate best.m variable for the subset. This should remove one of the warnings R was giving me
    #best.m = 3 #**#
    rf.model2 = randomForest(f2, data = trap.data, na.action = na.exclude, stringsAsFactors = TRUE, importance = TRUE, mtry = best.m, ntree = 5000)

    ## Refine variable set based on variance partitioning results
    # Calculate a preliminary temporal accuracy to provide a baseline
    temporal.accuracy.prelim = systematic.validation(trap.data, dep.var, f2, best.m, "year", response.type)
    temporal.accuracy.R2 = temporal.accuracy.prelim$OVERALL[["R2.sasha"]]
    correlation.threshold = "" # 0.5 #**# not using this aspect initially
    drop.thresholds = c(0, 0.001, 0.002, 0.005, 0.01) #**# Try a vector of thresholds. Use the threshold that corresponds to the fewest variables with no more than a 5% decrease in prediction accuracy

    # Run variance partitioning
    kept.vars = do.variance.partitioning(trap.data, dep.var, best.vars, best.m, temporal.accuracy.R2, drop.thresholds,
                                         correlation.threshold, response.type, do.spatial, results.path,
                                         spatial.field, temporal.field, label, temporal.field)

    # Re-run the model with just those that are kept after the variance partitioning process
    f3 = as.formula(paste(dep.var, ' ~ ', paste(kept.vars, collapse = "+")))
    best.m = get.best.m(f3, kept.vars, trap.data, response.type)# Recreate best.m variable for the subset. This should remove one of the warnings R was giving me
    #best.m = 3 #**#
    rf.model3 = randomForest(f3, data = trap.data, na.action = na.exclude, stringsAsFactors = TRUE, importance = TRUE, mtry = best.m, ntree = 5000)

  }

  # Just run the model for the input independent variables
  if (exploratory == FALSE){
    f3 = as.formula(paste(dep.var, ' ~ ', paste(independent.vars, collapse = "+")))
    best.m = get.best.m(f3, independent.vars, trap.data, response.type)# Recreate best.m variable for the subset. This should remove one of the warnings R was giving me
    rf.model3 = randomForest(f3, data = trap.data, na.action = na.exclude, stringsAsFactors = TRUE, importance = TRUE, mtry = best.m, ntree = 5000)
    kept.vars = independent.vars # For results output
  }

  ## Temporal validation step
  message("Calculating temporal accuracy statistics")
  temporal.accuracy = systematic.validation(trap.data, dep.var, f3, best.m, temporal.field, response.type) # formerly "TEMPORAL" instead of being temporal.field

  if (do.spatial == 1){
    ## Spatial validation step
    message("Calculating spatial accuracy statistics")
    spatial.accuracy = systematic.validation(trap.data, dep.var, f3, best.m, spatial.field, response.type) #**# formerly SPATIAL instead of district
  }else{
    message("Skipping spatial accuracy assessment (may take > 17 hours for some analyses)")
    spatial.accuracy = NA
  }

  message("Creating null models")
  temporal.null = null.validation(trap.data, dep.var, temporal.field, create.ci.null)
  spatial.null = null.validation(trap.data, dep.var, spatial.field, create.ci.null)

  # Write overall model observations and predictions to file #**# But these will be predictions from the same data used to generate the model. Yes. But the accuracy was assessed separately, and those will be the accuracy metrics reported.
  message("Writing predictions")
  write.predictions(trap.data, dep.var, rf.model3, results.path, spatial.field, temporal.field, label)

  #**# MAKE OUTPUTS #**# CLEAN THIS UP
  #make.maps(model.results, spatial.resolution, temporal.resolution, results.path)
  #plot.residuals(model.results, spatial.resolution, temporal.resolution)
  if (do.spatial == 1){
    message("Creating barplots")
    spatial.temporal.barplots(temporal.accuracy, spatial.accuracy, spatial.field, temporal.field, results.path, label)
  }

  return(list(MODEL = rf.model3, TEMPORAL.ACCURACY = temporal.accuracy, SPATIAL.ACCURACY = spatial.accuracy, TEMPORAL.NULL = temporal.null, SPATIAL.NULL = spatial.null, RETAINED.VARS = kept.vars))
}


#' Convert data in the format for ArboMAP to a format that can be used by the
#' RF1 model
#'
#' @param human.data human data from the main input, but read into memory
#' @param mosq.data mosquito data from main input, but read into memory
#' @param weekinquestion #**# ADD
#' @param weather.data non-temporal data from main input
#' @param rf1.inputs see \code{\link{rf1.inputs}}
#' @param results.path #**# ADD
#' @param break.type #**# ADD
#'
FormatDataForRF1 = function(human.data, mosq.data, weekinquestion, weather.data, rf1.inputs, results.path, break.type){

  # Need to convert mosquito data into mosquito infection rates (uses MLE code, which is not mine. Need to wait for response from Moffit. Perhaps a phone call? If no email response.)
  temporal.resolution = "annual"
  md.data = calculate.MLE.v2(mosq.data, temporal.resolution)

  # Need to convert human data to cases per county per year, and add 0's for county years without cases
  all.counties = rf1.inputs[[3]] # Get list of counties
  all.years = rf1.inputs[[4]] # Get list of all years of analysis
  # Ensure human data is in data frame format, and not just a file name
  if (typeof(human.data) == "character"){  human.data = read.csv(human.data)  }

  hd.data = convert.human.data(human.data, all.counties, all.years)

  breaks = assign.breaks(weekinquestion, break.type)

  # Need to aggregate the climate data by season & merge with static.data
  env.data = convert.env.data(weather.data, all.counties, breaks) #**# Could save this for faster re-use. How should I do that?
  #**# Watch for problem where county_year contains lower-case entries, while district has been converted to upper case for merging purposes.

  env.data = add.rf1.inputs(env.data, rf1.inputs, breaks)

  # Merge on county.year. Keep only records that have mosquito data
  my.data = merge(md.data, hd.data, by = "county_year") #, all = TRUE

  # Merge to environmental data.
  my.data = merge(my.data, env.data, by = "county_year") # , all = TRUE
  my.data = cleanup.garbage(my.data)

  # Pull independent variable names from the climate data. Add those from the mosquito data
  independent.vars = colnames(my.data)

  #**# maybe independent.vars should be an input? That might be easier, and give the user more control. But how will the user know what fields there are?

  # Remove non-analysis variables #**# This needs an upgrade to allow a user input and user control!
  user.drop.vars = rf1.inputs[[5]]
  drop.vars = c(user.drop.vars, "county_year", "GROUP", "CI.lower", "CI.upper", "COUNTY", "Cases", "YEAR", "year", "county", "district", "breaks") #**# I have a names problem! Should really clean up the name usage!

  for (drop.var in drop.vars){
    if (drop.var %in% independent.vars){
      independent.vars = independent.vars[independent.vars != drop.var]
    }
  }

  out = list(my.data, independent.vars)
  return(out)
}

#' Convert human data
#'
#' Simple little function to aggregate human case data by year and county
#'
convert.human.data = function(hd, all.counties, all.years){
  #**# The steps creating the year were already done in forecast_NYCT.R and could be required as part of the input.
  hd$year = mapply(substr, hd$date, nchar(as.character(hd$date)) - 3, nchar(as.character(hd$date)))
  hd$year = as.numeric(hd$year)
  hd$county_year = sprintf("%s_%s", hd$district, hd$year)
  hd$count = 1 # One case per entry
  hd.data = aggregate(hd$count, by = list(hd$county_year), "sum")
  colnames(hd.data) = c("county_year", "Cases")
  hd.data$county = sapply(hd.data$county_year, split.text, part = 1)
  hd.data$year =  sapply(hd.data$county_year, split.text, part = 2)

  # Make sure there is a record for every year and county included in the data set
  for (county in all.counties){
    for (year in all.years){
      county_year = sprintf("%s_%s", county, year)
      if (!county_year %in% hd.data$county_year){
        # Add 0 cases for missing county_years
        #test.type(c(county_year, 0, county, year), 'L1480')
        hd.data = rbind(hd.data, c(county_year, 0, county, year))
      }
    }
  }

  # For some reason, this field is being converted to text following the rbind. Odd
  hd.data$Cases = as.numeric(hd.data$Cases)

  return(hd.data)
}

#' Calculate MLE for a specified subset of the data using R
#'
#' Modified from wnv_hlpr.R
#'
calculate.MLE.v2 = function(md, temporal.resolution = "annual"){

  # Call dprev function from MLE_IR.R
  if (!exists("dprev")){ stop(" Load the dprev function from Williams & MOffit 2005 into computer memory. It is required for the calculation of MLE's") }
  # Make sure mosq.data is a data frame and not a file string
  if (typeof(md) == "character"){  md = read.csv(md)  }

  if (length(unique(md$species)) > 1){ warning(sprintf("Pooling across %s species", length(unique(md$species))))  }
  if (temporal.resolution != "annual"){ stop("Need to code other temporal resolution options")  }

  #**# THIS IS SPECIFIC TO THE FORMAT MIKE & JUSTIN USED FOR ARBOMAP - REVISIT TO SEE IF A MORE GENERAL APPROACH IS DESIRABLE
  # Get year from date
  md$year = mapply(substr, md$col_date, nchar(as.character(md$col_date)) - 3, nchar(as.character(md$col_date)))
  md$year = as.numeric(md$year)

  # Add VIRUS and ABUND fields #**# CONDSIDER WHETHER THESE ARE THE FIELD NAMES YOU REALLY WANT TO USE

  if (length(md$wnv_result) == 0){
    stop("A field with WNV result must be present, and must be labeled 'wnv_result'")
  }

  md$VIRUS = md$wnv_result
  md$wnv_result = NULL # Prevent an overabundance of redundant fields

  if (length(md$pool_size) == 0){
    stop("A field with mosquito pool sizes must be present, and must be labeled 'pool_size'")
  }

  md$ABUND = md$pool_size
  md$pool_size = NULL # Prevent redundnat fields

  # Drop any missing abundance field #**# Perhaps want to do this PRIOR to this function to ensure quality data inputs?
  md = md[!is.na(md$ABUND), ]

  # Aggregate by spatial field and year
  md$district_year = sprintf("%s_%s", md$district, md$year)

  # Get group ID's
  group.ids = unique(md$district_year)

  # Create a new data frame with group ID information
  md.data = data.frame(GROUP = group.ids, CI.lower = NA, CI.upper = NA,
                       IR = NA, COUNTY = NA)

  # Loop through each group and calculate MLE
  for (i in 1:length(group.ids)){
    group.id = group.ids[i]
    # Subset to just this group.id
    group.data = md[md$district_year == group.id, ]
    county = as.character(group.data$district[1]) # Should all be the same, just use first value
    this.year = group.data$year[1] # Should all be same, just use first value #**# Modify if temporal resolution is not annual

    # Get number of positive and negative pools for this group
    positives.index = group.data$VIRUS == 1
    negatives.index = group.data$VIRUS == 0
    #Explicitly finding negatives to exclude any NA values (i.e. from NY)

    # Get positive and negative pools including pool sizes
    positives = group.data$ABUND[positives.index]
    negatives = group.data$ABUND[negatives.index]
    abundance = sum(positives) + sum(negatives) # Get total number of mosquitoes tested. In CT, this is true abundance, in NY, pool size is capped, so this will underestimate abundance
    density = abundance / (length(positives) + length(negatives)) # Get number of mosquitoes divided by number of pools tested

    # If no pools, assign a value of -999 to distinguish from undefined IR's
    if (length(positives) == 0 & length(negatives) == 0){
      message(sprintf("No pools for group %s. Not sure how that happened. Values of -999 assigned", group.id))
      IR = -999
      CI.upper = -999
      CI.lower = -999
      abundance = 0
      density = 0
    }else{
      # Proceed with checking for missing positives or negatives alone
      # If no positive pools, assign an IR of 0 and CI's of 0 to 0
      if (length(positives) == 0){
        IR = 0
        CI.lower = 0
        CI.upper = 0
      }

      # If no negative pools, assign an IR of NA, and CI's of NA
      if (length(negatives) == 0){
        IR = NA
        CI.lower = NA
        CI.upper = NA
      }

      if (length(positives) != 0 & length(negatives) != 0){
        # Calculate the infection rate (IR)
        output = dprev(positives, negatives, disp = 'n')
        CI.lower = output[1]
        CI.upper = output[3]
        IR = output[2]
      }
    } # END OF ELSE STATEMENT

    # update the data frame
    md.data$CI.lower[i] = CI.lower
    md.data$CI.upper[i] = CI.upper
    md.data$IR[i] = IR
    md.data$abundance[i] = abundance
    md.data$density[i] = density

    # Update the COUNTY entry
    md.data$COUNTY[i] = county

    # Ensure there is a clearly delineated year field. Adjust as appropriate for other temporal resolutions
    if (temporal.resolution == "annual") { md.data$YEAR[i] = this.year }
  } # END OF LOOP OVER GROUPS

  md.data$county_year = sprintf("%s_%s", md.data$COUNTY, md.data$YEAR)

  return(md.data)
}

#' Assign Breaks
#'
assign.breaks = function(weekinquestion, break.type){
  # Get the date of the forecast. Only include breaks prior to forecast.doy
  forecast.year=  as.numeric(substr(as.character(weekinquestion), 1, 4))
  forecast.month = as.numeric(substr(as.character(weekinquestion), 6,7))
  forecast.day = as.numeric(substr(as.character(weekinquestion), 9,10))
  forecast.doy = get.DOY(forecast.year, forecast.month, forecast.day)

  # Assign all breaks
  if (break.type == "seasonal"){
    # Breaks on 3/31, 6/30, 9/30, 12/31
    breaks = c(get.DOY(forecast.year,3,31), get.DOY(forecast.year, 6, 30), get.DOY(forecast.year,9,30), get.DOY(forecast.year, 12, 31))
  }

  if (break.type != "seasonal"){ stop(sprintf("%s needs to be scripted out", break.type)) }

  # Drop any breaks that go past the forecast.doy - these cannot be used to make a prediction as they have not been observed yet!
  breaks = breaks[breaks < forecast.doy] # Assume we have data up to the day before the forecast.doy #**# May not be a good assumption

  return(breaks)
}

#' Convert environmental data to the seasonal format used by the RF1 model
#'
#'
convert.env.data = function(weather.data, all.counties, season.breaks){

  #Save processing time by restricting to just districts of interest
  weather.data = weather.data[weather.data$district %in% all.counties, ]

  # Drop weather data beyond the last break
  last.break = season.breaks[length(season.breaks)]
  weather.data = weather.data[weather.data$doy <= last.break, ]

  # Add grouping factor based on day of year
  weather.data$wbreaks = sapply(weather.data$doy, assign.groups, season.breaks)
  weather.data$county_year = sprintf("%s_%s", weather.data$district, weather.data$year)

  # This looks like a job for dplyr; https://datacarpentry.org/dc_zurich/R-ecology/04-dplyr
  #**# This is hard-coded to specific variables. Can dplyr take a more general input?
  env.data.pre = weather.data %>%
    group_by(county_year, wbreaks) %>%
    dplyr::summarize(TMINC = mean(tminc), TMEANC = mean(tmeanc), TMAXC = mean(tmaxc), PR = mean(pr),
                     RMEAN = mean(rmean), VPD = mean(vpd))


  #**# May be a better way to do this - this is not optimized for speed
  # Reformat env.data to have a separate column per break
  env.data = data.frame(county_year = unique(env.data.pre$county_year))

  # Add variables to the env.data data frame
  vars = c("TMINC", "TMEANC", "TMAXC", "PR", "RMEAN", "VPD") #**# HARD CODED, BECAUSE dplyr step is hard coded
  for (var in vars){
    for (b in unique(env.data.pre$wbreaks)){
      this.var = sprintf("%s_%s", var, b)
      env.data[[this.var]] = rep(NA, nrow(env.data))

      # Subset data frame to just this break
      env.data.subset = env.data.pre[env.data.pre$wbreaks == b, ]
      # Loop through subset and extract break values for this variable
      for (i in 1:nrow(env.data.subset)){
        county_year = env.data.subset$county_year[i]
        this.value = env.data.subset[[var]][env.data.subset$county_year == county_year]
        env.data[[this.var]][env.data$county_year == county_year] = this.value
      }
    }
  }

  env.data$county_year = as.character(env.data$county_year) # Somehow this was turning into a factor (!)
  env.data$district = sapply(env.data$county_year, splitter, "_", 1, 1)
  env.data$year = sapply(env.data$county_year, splitter, "_", 2, 0)

  env.data$district = toupper(env.data$district)

  return(env.data)
}


#' Assign break grouping for aggregating weather data
#'
#'
assign.groups = function(doy, breaks){

  out.group = 1

  # Increment the out.group until the appropriate break period is found
  # Starts at 1
  for (b in breaks){
    if (doy > b){
      out.group = out.group + 1
    }
  }

  if (out.group > length(breaks)){
    stop("An input day of year (doy) exceeded the last break")
  }

  # Simple manual version
  # Use of get.DOY function adjusts based on whether the year is a leap year
  #if (doy <= breaks[1]){  out.group = 1  }
  #if (doy > breaks[1] & doy <= breaks[2]){ out.group = 2  }
  #if (doy > breaks[2] & doy <= breaks[3]){ out.group = 3  }
  #if (doy > breaks[3]){ out.group = 4 }

  return(out.group)
}

#' Add data layers specific for the Random Forest model
#'
#' Modified from the add.other function in wnv_hlpr.R
#'
#' @param env.data The environmental data object
#' @param files.to.add A vector containing the full paths for .csv files to merge into the env.data object
#' @param merge.type.vec A vector containing the merge type for each of the files to be added
#'
add.rf1.inputs = function(env.data, rf1.inputs, breaks){

  # unpack rf1.inputs
  files.to.add = rf1.inputs[[1]]
  merge.type.vec = rf1.inputs[[2]]

  # env.data will be coming in with a district and year field. For now, use consistent terminology

  # If other inputs are present, merge them in
  if (length(files.to.add) > 0){

    if(!is.na(files.to.add)){
      # Check that files.to.add has same length as merge.type
      if (length(files.to.add) != length(merge.type.vec)){  stop("files.to.add must have the same number of elements as merge.type.vec")  }

      for (i in 1:length(files.to.add)){
        # Add covariate information
        this.file = files.to.add[i]
        this.merge = merge.type.vec[i]

        env.data = add.data(env.data, this.file, this.merge, breaks)
        env.data = cleanup.garbage(env.data) #**# This step may need modification in the context of the changes made to dfmip
      }
    }
  }

  # If rf1.inputs are NULL, do nothing, and just return the env.data object

  return(env.data)
}

#' Cleanup Garbage
#'
#' Taken from wnv_hlpr.R, rs.data is env.data object, but with the name used in wnv_hlpr.R
#'
#' Quick function to restore original field names after the merge
cleanup.garbage = function(rs.data){
  # Clean up merge garbage to restore original variable names
  if (length(rs.data$year) == 0){
    rs.data$year = rs.data$year.x
    rs.data$year.x = NULL
    rs.data$year.y = NULL
  }

  #**# Using year as the temporal field for now
  #if (length(rs.data$TEMPORAL) == 0){
  #  rs.data$TEMPORAL = rs.data$TEMPORAL.x
  #  rs.data$TEMPORAL.x = NULL
  #  rs.data$TEMPORAL.y = NULL
  #}

  if (length(rs.data$district) == 0){
    rs.data$district = rs.data$district.x
    rs.data$district.x = NULL
    rs.data$district.y = NULL
  }

  return(rs.data)
}

#' Function to add data to the env.data object
#'
#' @param rs.data env.data, the object to have data appended to
#' @param this.file the file containing the data to append
#' @param this.merge the structure of the data for the merge - i.e. "spatial_temporal" for spatial and temporal,
#' "spatial" for spatial only or "state_temporal" for statewide data that varies by year (i.e. the BBS data)
#' temporal only would be theoretically possible, but has not been scripted.
#' For now, it assumes the spatial is county, and temporal is year. Need to generalize this to spatial and temporal.
#'
#' #**# do I need the field renames, or does the cleanup.garbage step adequately address these issues?
#' #**# If it doesn't, can I move these corrections there, and streamline this function?
#'
#' Modified from wnv_hlpr.R
#'
add.data = function(rs.data, this.file, this.merge, breaks){
  these.data = read.csv(this.file)

  if (this.merge == "spatial_temporal"){


    # Format for my climate and anomaly data was variable_break. As this is being done for a specific forecast day, we need to drop any that exceed the forecast day
    # Drop any fields that have a numeric ending beyond the last break
    these.names = colnames(these.data)
    keep.vec = c()
    #**# This is a tricky split, as the variable names might have variable numbers of "_", but if we ever move past seasons to months, this will require a double-digit extraction

    if (max(breaks) < 10){
      # For now, do the simple approach and assume a single-digit extraction
      for (name in these.names){
        keep = FALSE
        # Just get last digit
        season = as.numeric(as.character(substr(name,length(name) - 1, length(name))))

        # Only apply exclusion if this variable corresponds to a season (i.e. ends in a number)
        if (season %in% seq(1,9)){
          if (season > max(breaks)){
            keep = TRUE
          }
        }
        keep.vec = c(keep.vec, exclude)
      }
    }

    if (max(breaks) > 9){ stop("Need to upgrade the add.data function to handle more than 9 breaks")  }

    keep.vars = these.names[keep.vec] #NOTE: 0 and 1 do not seem to work here.

    these.data$county_year = these.data$COUNTY_YEAR
    rs.data = merge(rs.data, these.data, by = "county_year")
  }

  if (this.merge == "state_temporal"){
    stop("Need to add state as an input to the rf1 function")
    # Merge by State and year
    if (length(rs.data$year) > 0){ rs.data$STATE_YEAR = sprintf("%s_%s", rs.data$STATE, rs.data$year) }
    rs.data = merge(rs.data, these.data, by = "STATE_YEAR") #, all.x = TRUE)
  }

  if (this.merge == "spatial"){
    # Merge by spatial unit - no temporal resolution
    these.data$district = toupper(these.data$SPATIAL) # Ensure data is upper-case for proper join
    these.data$SPATIAL = NULL # Remove this field, otherwise it will later get converted to a SPATIAL.x if more than one gets merged. Can always re-assign from district if it is needed later

    rs.data = merge(rs.data, these.data, by = "district")
  }

  return(rs.data)
}


#' Function to return break points by break type
#'
get.breaks = function(days, break.type){

  Jan = 31
  Feb = 28
  Mar = 31
  Apr = 30
  May = 31
  Jun = 30
  Jul = 31
  Aug = 31
  Sep = 30
  Oct = 31
  Nov = 30
  Dec = 31

  if (days == 366){  Feb = 29  }

  month.vec = c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)

  cumulative = 0
  breaks = c()
  count = 0
  for(month in month.vec){
    count = count + 1
    cumulative = cumulative + month

    # Assign monthly breaks
    if (break.type == "monthly"){
      breaks = c(breaks, cumulative)
    }

    if (break.type == "seasonalDJF"){
      stop("Sorry, we have not programmed this yet, because we need to figure out how to include the previous year's December")
    }

    # Assign easy seasonal breaks #Might be more realistic from a WNV perspective and easier to program
    if (break.type == "seasonal"){
      if (count %% 3 == 0){
        breaks = c(breaks, cumulative)
      }
    }
  } # End of for loop

  return(breaks)
}


#### HELPER FUNCTIONS FOR do.rf FUNCTION CALL ####
#### MODIFIED (OR NOT) FROM wnv_hlpr.R ####


#' Function to identify the m value that produces the best results
#'
get.best.m = function(f, independent.vars, trap.data, response.type){
  best.r2 = 0
  best.m = 0
  # Try different values of "m" # With 37 iterations of 5000 trees, this is going slow.
  # Demo with 500 trees - probably too few.
  # Getting m of 3-4. Seems about right
  for (m in 1:length(independent.vars)){
    message(sprintf("Trying m = %s", m))
    rf.model = randomForest(f, data = trap.data, na.action = na.exclude, stringsAsFactors = TRUE, importance = TRUE, mtry = m, ntree = 1000)

    if (response.type == "continuous"){
      if(median(rf.model$rsq) > best.r2){
        best.m = m
        best.r2 = median(rf.model$rsq)
      }
    }
    if (response.type == "binary"){
      message("R2 used for choice of m is actually the out-of-box error rate, not the R2")
      if(median(rf.model$err.rate[ ,1]) < best.r2){
        best.m = m
        best.r2 = median(rf.model$err.rate[ ,1])
      }

    }

  }
  return(best.m)
}

systematic.validation = function(trap.data, dep.var, f2, best.m, drop.field, response.type = "continuous"){
  require(caret)

  # Get list of unique values from the field to have one unit sequentially dropped
  drop.vals = unique(trap.data[[drop.field]])
  #message(paste(names(trap.data), collapse = ", "))
  #message(drop.field)
  #message(drop.vals)

  # Create a data frame to hold the results
  errors = data.frame(observed = NA, predicted = NA, unit = NA, MERGE_ID = NA)
  rmse.errors = c()
  spearmans = c()
  samples = c()
  to.exclude = c()

  for (drop.val in drop.vals){
    # Subset the data to exclude the field in question
    sub.data = trap.data[trap.data[[drop.field]] != drop.val, ]
    this.sample = nrow(sub.data)
    samples = c(samples, this.sample)

    # Check to make sure there is variation to explain with a random forest
    if (length(unique(sub.data[[dep.var]])) > 0){
      # Fit the random forest
      this.result = randomForest(f2, data = sub.data, na.action = na.exclude, stringsAsFactors = TRUE, importance = FALSE, mtry = best.m, ntree = 5000)

      # Get validation subset
      validation.data = trap.data[trap.data[[drop.field]] == drop.val, ]
      outcomes = predict(this.result, newdata = validation.data)

      # Get RMSE for this unit
      this.rmse = caret::RMSE(validation.data[[dep.var]], outcomes, na.rm = TRUE)
      rmse.errors = c(rmse.errors, this.rmse)

      # Get scaled RMSE for this unit #**# Don't need this for every unit - just do once overall
      #scaled.rmse = this.rmse / (mean(validation.data[[dep.var]]))

      if (response.type == "continuous"){
        # Get Spearman correlations for this unit
        this.spearman = cor(validation.data[[dep.var]], outcomes, use = "complete.obs")
        spearmans = c(spearmans, this.spearman)
      }
      if (response.type == "binary"){
        spearmans = NA
      }

      # Add results to data frame
      stuff = cbind(validation.data[[dep.var]], outcomes, rep(as.character(drop.val), length(outcomes)), validation.data$MERGE_ID)
      colnames(stuff) = c("observed", "predicted", "unit", "MERGE_ID")
      #test.type(stuff, 'L1991')
      errors = rbind(errors, stuff)
    }else{
      message(sprintf("%s had no variation in the dependent variable %s", drop.val, dep.var))
      to.exclude = c(to.exclude, drop.val)
    }

  }

  # Exclude any variables where the Random Forest could not be tested due to lack of variation
  for (ex in to.exclude){
    if (ex %in% drop.vals){
      drop.vals = drop.vals[drop.vals != ex]
    }
  }

  # Drop NAs in first row added during data frame initialization
  errors = errors[2:nrow(errors), ]
  errors$observed = as.numeric(as.character(errors$observed))
  errors$predicted = as.numeric(as.character(errors$predicted))
  overall.rmse = caret::RMSE(errors$observed, errors$predicted, na.rm = TRUE)
  #0.0039 So, our average error is 3.9 cases per 1000.
  # Our average infection rate is 3.0 cases per 1000. So... on average we are just very wrong.
  min.error = min(abs(errors$observed - errors$predicted))
  max.error = max(abs(errors$observed - errors$predicted))
  #0.0231 (!) That is a HUGE mistake!

  custom.r2 = estimate.custom.r2(errors$observed, errors$predicted)
  #0.26 (!) # Better than I expected!

  # get median rmse of those calculated for each unit
  median.rmse = median(rmse.errors)
  # The median is 0.0029. So better than the mean overall RMSE.
  # Are the years with the most WNV the ones with the worst predictions?

  # Divide the root mean squared error by the mean value in the observed dataset. How big is the typical error relative to the mean?
  # One citation said a good rule of thumb is aim for an RMSE that is 10% of the target value or less
  scaled.rmse = overall.rmse / mean(errors$observed)

  #errors.by.year = cbind(as.numeric(as.character(drop.vals)), rmse.errors)
  #errors.by.year = errors.by.year[order(errors.by.year[ ,2], decreasing = TRUE), ]

  if (response.type == "continuous"){
    # Look at rank correlation ? See how well it does at ranking the magnitude of WNV?
    spearman = cor(errors[,c(1,2)], method = "spearman")[1,2]
    # 0.58 spearman correlation. So... there is a correlation between what the model predicts is a bad year and how bad the year is.
    # But it's not a great correlation.
    # And this is at the county level. Ouch.
    pearson = cor(errors[,c(1,2)], method = "pearson")[1,2]
  }

  if (response.type == "binary"){
    spearman = NA
    pearson = NA
  }

  overall.results = c(overall.rmse, median.rmse, scaled.rmse, custom.r2, spearman, pearson, min.error, max.error)
  #message(overall.rmse)
  #message(median.rmse)
  #message(scaled.rmse)
  #message(custom.r2)
  #message("break")
  #message(spearman)
  #message(pearson)
  #message(min.error)
  #message(max.error)
  names(overall.results) = c("Overall.RMSE", "Median.RMSE", "Scaled.RMSE", "R2.sasha", "Spearman", "Pearson", "Min.Error", "Max.Error")
  #**# 2012 was a bad year - totally underpredicted the WNV infection rates
  #**# So... this is limited as an early warning system (!)

  # Add unit informatoin to the rmse.errors & spearman correlations
  names(rmse.errors) = as.character(drop.vals)
  if (response.type == "continuous"){ names(spearmans) = as.character(drop.vals) }
  names(samples) = as.character(drop.vals)

  #hist(temporal.accuracy$SPEARMAN, breaks = seq(-1,1,0.1))

  return(list(OVERALL = overall.results, RMSE = rmse.errors, SPEARMAN = spearmans, N = samples, ERROR.DF = errors))
}


#' Similar to systematic, but for the production of the null comparisons
null.validation = function(trap.data, dep.var, drop.field, create.ci.null){
  require(caret)

  # Add a message to warn users that some functionality has been turned off - to ensure it is intentional
  if (create.ci.null != 1){ message("Not creating a Null model based on the confidence intervals of the original data")}

  # Get list of unique values from the field to have one unit sequentially dropped
  drop.vals = unique(trap.data[[drop.field]])

  # Create a data frame to hold the results
  errors = data.frame(observed = NA, mean.predicted = NA, ci.predicted = NA, unit = NA, MERGE_ID = NA)
  mean.rmse.errors = c()
  mean.spearmans = c()

  if (create.ci.null == 1){
    ci.rmse.errors = c()
    ci.spearmans = c()
  }
  samples = c()

  for (drop.val in drop.vals){
    # Subset the data to exclude the field in question
    sub.data = trap.data[trap.data[[drop.field]] != drop.val, ]
    this.sample = nrow(sub.data)
    samples = c(samples, this.sample)

    # Fit the random forest
    mean.result = mean(sub.data[[dep.var]])

    # Get validation subset
    validation.data = trap.data[trap.data[[drop.field]] == drop.val, ]
    mean.outcomes = rep(mean.result, nrow(validation.data))

    # Get RMSE for this unit
    this.mean.rmse = caret::RMSE(validation.data[[dep.var]], mean.outcomes)
    mean.rmse.errors = c(mean.rmse.errors, this.mean.rmse)

    if (create.ci.null == 1){
      # Repeat for CI Null
      ci.outcomes = mapply(runif, 1, validation.data$CI.lower, validation.data$CI.upper)
      this.ci.rmse = caret::RMSE(validation.data[[dep.var]], ci.outcomes)
      ci.rmse.errors = c(ci.rmse.errors, this.ci.rmse)

      # Get Spearman correlations for this unit
      # correlations for the mean values are meaningless! they're all the same predictions!
      #this.mean.spearman = cor(validation.data[[dep.var]], mean.outcomes)
      #mean.spearmans = c(mean.spearmans, this.mean.spearman)

      this.ci.spearman = cor(validation.data[[dep.var]], ci.outcomes)
      ci.spearmans = c(ci.spearmans, this.ci.spearman)
    }else{ ci.outcomes = rep(NA, length(mean.outcomes)) }

    # Add results to data frame
    stuff = cbind(validation.data[[dep.var]], mean.outcomes, ci.outcomes, rep(as.character(drop.val), length(mean.outcomes)), validation.data$MERGE_ID)
    colnames(stuff) = c("observed", "mean.predicted", "ci.predicted", "unit", "MERGE_ID")
    #test.type(stuff, 'L2128')
    errors = rbind(errors, stuff)
  }

  # Drop NAs in first row added during data frame initialization
  errors = errors[2:nrow(errors), ]
  errors$observed = as.numeric(as.character(errors$observed))
  errors$mean.predicted = as.numeric(as.character(errors$mean.predicted))

  mean.overall.rmse = RMSE(errors$observed, errors$mean.predicted)

  # Scale by mean to put into the context of the data
  mean.scaled.rmse = mean.overall.rmse / mean(errors$observed)

  mean.min.error = min(abs(errors$observed - errors$mean.predicted))
  mean.max.error = max(abs(errors$observed - errors$mean.predicted))
  mean.custom.r2 = estimate.custom.r2(errors$observed, errors$mean.predicted)

  # Look at median of the individual RMSE's
  mean.median.rmse = median(mean.rmse.errors)

  mean.overall.results = c(mean.overall.rmse, mean.median.rmse, mean.scaled.rmse, mean.custom.r2, NA, NA, mean.min.error, mean.max.error)
  names(mean.overall.results) = c("Overall.RMSE", "Median.RMSE", "Scaled.RMSE", "R2.sasha", "Spearman", "Pearson", "Min.Error", "Max.Error")

  # Add unit informatoin to the rmse.errors & spearman correlations
  names(mean.rmse.errors) = as.character(drop.vals)
  #names(mean.spearmans) = as.character(drop.vals)
  names(samples) = as.character(drop.vals)

  mean.results = list(OVERALL = mean.overall.results, RMSE = mean.rmse.errors, SPEARMAN = NA, N = samples, ERROR.DF = errors)
  #hist(temporal.accuracy$SPEARMAN, breaks = seq(-1,1,0.1))

  # Repeat process for CI results, if applicable
  ci.results = NA
  if (create.ci.null == 1){
    errors$ci.predicted = as.numeric(as.character(errors$ci.predicted))
    ci.overall.rmse = caret::RMSE(errors$observed, errors$ci.predicted)
    ci.scaled.rmse = ci.overall.rmse / mean(errors$observed)

    ci.min.error = min(abs(errors$observed - errors$ci.predicted))
    ci.max.error = max(abs(errors$observed - errors$ci.predicted))
    ci.custom.r2 = estimate.custom.r2(errors$observed, errors$ci.predicted)

    ci.median.rmse = median(ci.rmse.errors)
    # Look at rank correlation ? See how well it does at ranking the magnitude of WNV?
    #mean.spearman = cor(errors[,c(1,2)], method = "spearman")[1,2]
    #mean.pearson = cor(errors[,c(1,2)], method = "pearson")[1,2]
    ci.spearman = cor(errors[,c(1,3)], method = 'spearman')[1,2]
    ci.pearson = cor(errors[,c(1,3)], method = 'pearson')[1,2]

    ci.overall.results = c(ci.overall.rmse, ci.median.rmse, ci.scaled.rmse, ci.custom.r2, ci.spearman, ci.pearson, ci.min.error, ci.max.error)
    names(ci.overall.results) = c("Overall.RMSE", "Median.RMSE", "Scaled.RMSE", "R2.sasha", "Spearman", "Pearson", "Min.Error", "Max.Error")

    names(ci.rmse.errors) = as.character(drop.vals)
    names(ci.spearmans) = as.character(drop.vals)

    ci.results = list(OVERALL = ci.overall.results, RMSE = ci.rmse.errors, SPEARMAN = ci.spearmans, N = samples, ERROR.DF = errors)

  }

  return(list(NULL.MEAN = mean.results, NULL.CI = ci.results))
}

#' Calculate R2 estimate of variation explained
#'
#' This R2 is not bounded as zero, as the model is not derived from the original data
#' The model is derived from a novel data set, consquently, it could fail to explain even
#' as much variation as the data mean. In these cases, negative R2's will be obtained
#'
#' Consequently, a negative R2 might be better than no knowledge at all, but it's not apt to be very good (depends on how variable the data are from the mean!)
#'  Basic assumption is that there is some variability around the mean, and that typically, a model should be able to explain that variation better than a static mean.
#'
estimate.custom.r2 = function(observed, predicted){

  deviation = observed - predicted

  squared.deviation = deviation ^ 2

  # Calculate prediction R2 (this is not PRESS, which is based on leaving one out. This is an entirely new validation data set, and so the R2 could be negative)
  sum.squared.deviation = sum(squared.deviation)

  data.mean = mean(observed, na.rm = TRUE)
  deviation.from.mean = data.mean - observed
  deviation.from.mean.squared = deviation.from.mean ^ 2
  sum.dfm.squared = sum(deviation.from.mean.squared)

  # Scale the error remaining after the model by the error obtained from just using the mean
  # Subtract this from one.
  R2.pred = 1 - (sum.squared.deviation / sum.dfm.squared)

  return(R2.pred)
}

#' Determine the contribution of each variable to the total variation explained
do.variance.partitioning = function(trap.data, dep.var, best.vars, best.m, temporal.accuracy.R2,
                                    drop.thresholds, correlation.threshold, response.type,
                                    do.spatial, results.path,
                                    spatial.resolution, temporal.resolution, this.label,
                                    temporal.field){

  message("Running variance partitioning")
  message(sprintf("Baseline R2 is %.3f", temporal.accuracy.R2))

  # Check if a spatial partitioning is desired - if so, throw a warning
  if (do.spatial == 1){ warning("Variance partitioning was performed for temporal crossvalidation, NOT spatial crossvalidation. The code could easily be adapted to support the spatial partitioning, but currently it does not.") }

  if (correlation.threshold != ""){
    # Identify correlations among variables
    cor.matrix = cor(trap.data[ ,best.vars])
    corr.variation = c()
    n.corr = c()
  }

  unique.variation = c()

  # Run variance partitioning leaving each variable out in sequence
  for (this.var in best.vars){
    message(sprintf("Processing %s", this.var))
    remaining.vars = best.vars[best.vars != this.var] # Drop the variable in question
    out.R2 = run.partitioning(trap.data, dep.var, remaining.vars, best.m, temporal.accuracy.R2, response.type, temporal.field)
    unique.variation = c(unique.variation, out.R2)

    if (correlation.threshold != ""){
      # Run variance partitioning leaving out correlated suites of variables (greater than a threshold)
      cor.row = cor.matrix[this.var, ]
      cor.index = sapply(cor.row, simple.threshold, correlation.threshold)
      #cor.index = matrix(cor.index, ncol = 1)
      remaining.vars2 = best.vars[!cor.index]
      out.cor.R2 = run.partitioning(trap.data, dep.var, remaining.vars2, best.m, temporal.accuracy.R2, response.type, temporal.field)
      corr.variation = c(corr.variation, out.cor.R2)
      n.corr = c(n.corr, sum(cor.index)) # This works because TRUE evaluates to 1 and FALSE evaluates to 0
    }
  }

  out.df = data.frame(VARIABLES = best.vars, UNIQUE.R2 = unique.variation)

  if (correlation.threshold != ""){
    out.df$CORR.R2 = corr.variation
    out.df$N.CORRELATED = n.corr
  }

  best.threshold = NA
  best.R2.diff = 0
  kept.vars = best.vars

  for (threshold in drop.thresholds){
    # Try dropping all variables that contribute less than 0.1% uniquely to the overall result #**# 0.1% is also arbitrary
    remaining.vars3 = out.df[out.df$UNIQUE.R2 > threshold, 1] # 1 indicates the first column

    if (length(remaining.vars3) == 0){
      message(sprintf("No variables remaining at threshold %s", threshold))
    }else{
      this.R2.diff = run.partitioning(trap.data, dep.var, remaining.vars3, best.m, temporal.accuracy.R2, response.type, temporal.field)
      message(sprintf("R2 difference for threshold %s is %.3f", threshold, this.R2.diff))

      # Right now, this JUST accepts models that constitute an improvement.
      # We likely want to add a penalty term that favors simpler models with similar explanatory power.
      if (this.R2.diff < best.R2.diff){
        best.R2.diff = this.R2.diff
        best.threshold = threshold
        kept.vars = remaining.vars3
      }
    }
  }

  # Add a threshold column to the data set for reference as to what was used to identify the final model variables
  out.df$best.threshold = best.threshold

  # Write variance partitioning results to an output file
  out.path = sprintf("%s/ModelResults/VariancePartitions", results.path)
  dir.create(out.path, showWarnings = FALSE, recursive = TRUE) # showWarnings = FALSE suppresses a warning if the path already exists
  out.file = sprintf("%s/varpar_%s_%s_%s.csv", out.path, spatial.resolution, temporal.resolution, this.label)
  write.table(out.df, out.file, sep = ',', row.names = FALSE, col.names = TRUE)

  return(kept.vars)
}

#' Calculate the variance partitioning run
run.partitioning = function(trap.data, dep.var, remaining.vars, best.m, temporal.accuracy.R2, response.type, temporal.field = "TEMPORAL"){

  my.f = as.formula(paste(dep.var, ' ~ ', paste(remaining.vars, collapse = "+")))

  # Redefine best.m if it is no longer applicable when the variable set is reduced
  if (best.m > length(remaining.vars)){ best.m = length(remaining.vars) }

  temporal.accuracy = systematic.validation(trap.data, dep.var, my.f, best.m, temporal.field, response.type)

  # Calculate difference between the main run with all variables and this run
  R2.difference = temporal.accuracy.R2 - temporal.accuracy$OVERALL[["R2.sasha"]]

  return(R2.difference)
}

simple.threshold = function(x, threshold){
  y = NA
  if (x >= threshold){ y = TRUE }
  if (x < threshold){ y = FALSE }
  return(y)
}

#' Write model predictions
#'
write.predictions = function(trap.data, dep.var, rf.model, results.path, spatial.resolution, temporal.resolution, label){
  # Code used to patch the human analyses prior to addition of dep.var
  # DiseasePath = "C:/Users/ak697777/University at Albany - SUNY/Elison Timm, Oliver - CCEID/RESULTS/WNV_statistical/ModelResults"
  # spatial.resolution = "county"; temporal.resolution = "annual"; analysis.label = label = "human" #"human_subset"
  # trap.data = read.csv(sprintf("%s/%s_%s_%s.csv", DiseasePath, spatial.resolution, temporal.resolution, analysis.label))
  # load(sprintf("C:/Users/ak697777/University at Albany - SUNY/Elison Timm, Oliver - CCEID/RESULTS/WNV_statistical/ModelRuns/Results_%s_%s_%s.RData", spatial.resolution, temporal.resolution, analysis.label))
  # rf.model = model.results$MODEL
  trap.data$OBSERVED = trap.data[[dep.var]]
  trap.data$PREDICTED = predict(rf.model) #**# More commands needed?
  #output = trap.data[ , c("OBSERVED", "PREDICTED", "SPATIAL", "TEMPORAL")] #**# Do we need more than this? Yes for the visualization

  out.file = sprintf("%s/ModelResults/%s_%s_%s.csv", results.path, spatial.resolution, temporal.resolution, label)

  write.table(trap.data, file = out.file, sep = ',', row.names = FALSE)

}

spatial.temporal.barplots = function(temporal.accuracy, spatial.accuracy, spatial.resolution, temporal.resolution, results.path, label){

  outfile = sprintf("%s/RMSE_Spearman_%s_%s%s.tif", results.path, spatial.resolution, temporal.resolution, label)
  tiff(file = outfile, compression = c("lzw"), height = 2400, width = 2400, res = 300)

  par(mfrow = c(2,2))
  par(mar = c(8,6,2,0))
  rmse.y.lim = c(0, 10)
  sp.y.lim = c(-1,1)

  x.axis = "s"
  x.offset = 0.8
  x.adj1 = x.adj2 = 0
  if (spatial.resolution == "point"){
    x.axis = "n"
    x.offset = 0.8
    x.adj1 = 20
    x.adj2 = 4.2
  }


  barplot(sort(temporal.accuracy$RMSE * 1000), ylab = "RMSE\n(# infected per 1000 mosquitoes)" , las = 2, ylim = rmse.y.lim, xpd = NA) #main = "RMSE"
  text(par("usr")[1] + x.offset, 9.8, "a.", cex = 1, xpd = NA)
  barplot(sort(spatial.accuracy$RMSE * 1000), ylab = 'RMSE\n(# infected per 1000 mosquitoes)', las = 2, ylim = rmse.y.lim, xaxt = x.axis, xpd = NA) #main = "RMSE"
  text(par("usr")[1] + x.offset + x.adj1, 9.8, "b.", cex = 1, xpd = NA)
  barplot(sort(temporal.accuracy$SPEARMAN), ylab = "Spearman coefficient", las = 2, ylim = sp.y.lim)
  text(par("usr")[1] + x.offset, 0.95, "c.", cex = 1, xpd = NA)
  barplot(sort(spatial.accuracy$SPEARMAN), ylab = "Spearman coefficient", las = 2, ylim = sp.y.lim, xaxt = x.axis)
  text(par("usr")[1] + x.offset + x.adj2, 0.95, "d.", cex = 1, xpd = NA)

  dev.off()
}

#' Check that required packages are installed prior to running the code
check.dependencies = function(model.name, packages){
  m.base = "Please install package '%s' to run the %s model (install.packages('%s'))"
  m = ""
  is.error = 0
  for (package.name in packages){
    if (!require(package.name, character.only = TRUE)){
      this.m = sprintf(m.base, package.name, model.name, package.name)
      m = sprintf("%s\n%s", m, this.m)
      is.error = 1
    }
  }

  # If there is an error, print the error message
  if (is.error == 1){ stop(m)  }
}

#' Create a vector of days and months, starting with the first Sunday
#'
#' See also \code{\link{get.start.week}}
#' NOTE: Could generalize, by making the start day an input.
#'
#' @param year The input year to evaluate (must be >1900 and <2100)
#'
#'@return A list containing a vector of numeric months and a vector with numeric days of the months
#'
#'@examples
#' month.day.vecs = create.month.day.vecs(2000)
#' month.day.vecs = create.month.day.vecs(2012)
#' month.day.vecs = create.month.day.vecs(2013)
#' month.day.vecs = create.month.day.vecs(2014)
#' month.day.vecs = create.month.day.vecs(2015)
#' month.day.vecs = create.month.day.vecs(2016)
#' month.day.vecs = create.month.day.vecs(2020)
#'
#'@export create.month.day.vecs
create.month.day.vecs = function(year){

  if (year <= 1900 | year >= 2100){
    stop("get.start.week will not work for years before 1901 or after 2099")
  }

  base.year = 2014 # completely arbitrary, happens to be first year I ran the model for using this framework
  delta.year = year - base.year

  # Calculate number of extra days that need to be adjusted for based on leap years
  if (delta.year >= 0){
    leap.adjustments = floor((abs(delta.year) + 1) / 4)
  }
  if (delta.year < 0){
    leap.adjustments = floor((abs(delta.year) + 2) / 4)
  }

  # Give same sign as delta.year
  if (delta.year < 0){ leap.adjustments = -leap.adjustments }

  # Compute change in number of days #**# Fix for going in the other direction
  day.adj = delta.year + leap.adjustments


  # Identify first Sunday of the year (5th for 2014)
  first.sunday.base = 5
  first.sunday = first.sunday.base - day.adj

  # Correct for going over
  while (first.sunday > 7){
    first.sunday = first.sunday - 7
  }

  # Correct for going under
  while (first.sunday < 0){
    first.sunday = first.sunday + 7
  }

  # Create all day/month pairs for the Sundays of the year
  month.vec = c(1)
  day.vec = c(first.sunday)
  day.count = first.sunday

  days.in.year = get.days(year)
  days.in.months = c(31,28,31,30,31,30,31,31,30,31,30,31)
  if (days.in.year == 366){ days.in.months[2] = 29  }
  prev.day = first.sunday
  this.month = 1

  # Add days as long as there are days in the year
  while (day.count <= (days.in.year - 7)){
    this.day = prev.day + 7
    days.in.month = days.in.months[this.month]
    if (this.day > days.in.month){
      this.day = this.day - days.in.month # Get the first Sunday of the next month
      this.month = this.month + 1 #Increment months
    }
    day.vec = c(day.vec, this.day)
    month.vec = c(month.vec, this.month)
    # update for next iteration
    prev.day = this.day
    day.count = day.count + 7
  }

  return(list(month.vec, day.vec))
}

#' Get Weeks to sample
#'
#' Create vectors of month, day, and year to use for running forecasts.
#' Samples begin on Sunday of the week, starting with the first Sunday of the month
#'
#' @param year The year in question
#' @param start_season_month The first month of the mosquito season, as a number. E.g., July would be 7.
#' @param weeks_in_season The number of weeks to sample
#' @param sample_frequency How frequently sample (default, 1 = weekly) #**# Other options are not currently supported
#'
#' @return A list of three vectors: month.vec has months, day.vec has days, and year.vec contains years. These are all parallel to each other and need to be the same length
#'
get_sampling_weeks = function(year, season_start_month, weeks_in_season, sample_frequency = 1){
  month.day.vecs = create.month.day.vecs(year)
  month.vec = month.day.vecs[[1]]
  day.vec = month.day.vecs[[2]]

  if (sample_frequency != 1){stop("Currently only weekly sampling is scripted. Sorry!")}

  # Subset to just the start season month and the specified number of weeks in season (need to subtract 1 to account for inclusion of the first week)
  first.to.use = grep(season_start_month, month.vec)[1] # Take just the first instance
  month.vec = month.vec[first.to.use:(first.to.use + (weeks_in_season - 1))] # Parentheses are required, otherwise it does the : and then adds to that result!
  day.vec = day.vec[first.to.use:(first.to.use + (weeks_in_season - 1))]
  #**# Can use index = rep(c(1,0), length(month.vec) / 2) as a starting point to get biweekly forecasts - then just select from month.vec where the index is 1

  year.vec = rep(year, length(month.vec))

  return(list(month.vec, day.vec, year.vec))
}


#' test.type
#'
#' Simple testing function to identify a bug with rbind. Bug appears to be external to dfmip.R and output_accuracy.R
test.type = function(vector, test.name){
  message(test.name)
  for (item in vector){
    message(class(item))
    if (class(item) == 'factor'){
      stop("Here is one of your factor problems")
    }
  }
}


#' Check models
#'
#' Check that the selected model names are supported by the dfmip code. Main aim is to catch minor typos, such as entering 'NULL' instead of 'NULL.MODELS'.
#'
#' @param models.to.run The list of models to be run
#' @param models.in.dfmip The list of models scripted in dfmip, as a vector
#'
#' @return NONE: this function throws an error if something is wrong.
check.models = function(models.to.run, models.in.dfmip){
  model.error = 0
  bad.names = c()
  for (model in models.to.run){
    if (!model %in% models.in.dfmip){
      model.error = 1
      bad.names = c(bad.names, model)
    }
  }
  if (model.error == 1){
    m1 = sprintf("The following entered model names are not supported by dfmip:\n%s\n",paste(bad.names, collape = ', '))
    m2 = sprintf("The following names ARE supported:\n%s\n", paste(models.in.dfmip, collapse = ', '))
    m3 = "Please check documentation and/or check for typos"
    stop(sprintf("%s%s%s", m1,m2,m3))
  }

}

