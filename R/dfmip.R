#' dfmip: Disease Forecast Model Inter-comparison Project package
#'
#' dfmip runs multiple disease forecasting models for the purpose of
#' comparing and contrasting methods. Currently designed for mosquito-borne
#' diseases, the aim is for future versions to be more generally applicable
#'
#' @docType package
#' @name dfmip
NULL

# Code for building package (Not Run)
#devtools::document()
#devtools::build_vignettes()
#devtools::check()
#devtools::check(build_args = '--no-build-vignettes') # The vignettes were taking too long to build, wanted to skip that step
#codemetar::write_codemeta('dfmip') # Writes .json metadata (whatever that means). Also gives opinions about code improvement
#spelling::spell_check_packages()
#goodpractice::gp()
#covr::package_coverage() # 29.49% #**# Interesting, because I thought each function was included. Ah. We're skipping most of the tests. Bet it would be different if they were not skipped.

# Identify dependencies used in dfmip (see also DESCRIPTION file)
# Set up package imports for NAMESPACE
#' @import dplyr
#' @importFrom grDevices dev.off tiff
#' @importFrom graphics barplot curve hist par plot segments text
#' @importFrom stats aggregate as.formula cor dnorm median na.exclude optimize predict qchisq rnorm runif sd uniroot
#' @importFrom utils read.csv write.table
NULL
# @import ArboMAP # Not listed - see !require statement that loads it later. Listed under Suggests in the dependency
# @import rf1 # Not listed - see !require statement that loads it later. Listed under Suggests in the dependency
# #' @importFrom devtools install_github # It is saying Namespace dependency not required!

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
#' RF1 Details: See rf1 package: ?rf1
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
#' Note these entries are case-sensitive and are run by keyword, so run in a fixed order (NULL.MODELS, ArboMAP, ArboMAP.MOD, RF1_C, RF1_A),
#' regardless of the order specified in the models.to.run vector.
#' @param human.data Data on human cases of the disease. Must be formatted with two columns: district and date (in format M/D/Y, with forward slashes as delimiters). An optional 'year' column may also be present. If absent, it will be generated from the date column. The district column contains the spatial unit (typically county), while the date corresponds to the date of the onset of symptoms for the human case.
#' #**# WHAT IF THESE DATA ARE MISSING? I.E. just making a mosquito forecast with RF1?
#' @param mosq.data Data on mosquito pools tested for the disease. Must be formatted with 4 columns: district (the spatial unit, e.g. county), col_date: the date the mosquitoes were tested, wnv_result: whether or not the pool was positive, pool_size: the number of mosquitoes tested in the pool. A fifth column species is optional but is not used by the code
#' @param weather.data Data on weather variables to be included in the analysis. See the read.weather.data function for details about data format.
#' The read.weather.data function from ArboMAP is a useful way to process one or more data files downloaded via Google Earth Engine.
#' @param weekinquestion The focal week for the forecast. For the Random Forest model, this will be the last day used for making the forecast
#' @param week.id A two-part ID for the analysis run to distinguish it from other weeks or runs. The first part is a character string, while the second part is a date in the format YYYY-MM-DD #**# analysis.id would be a better name, but would require changes to the code in multiple places
#' @param results.path The base path in which to place the modeling results. Some models will create sub-folders for model specific results
#' @param model.inputs A keyed list of model-specific inputs. Keyed entry options are: \tabular{ll}{
#' arbo.inputs \tab Inputs specific to the ArboMAP model. #**# DOCUMENTATION NEEDED\cr
#' rf1.inputs \tab Inputs specific to the RF1 model, see \code{\link{rf1.inputs}}.\cr}
#' @param population.df Census information for calculating incidence. Can be set to 'none' or omitted from the function call #**# NEEDS FORMAT INSTRUCTIONS
#' @param n.draws The number of draws for the forecast distributions. Should generally be 1 if a point estimate is used, otherwise should be a large enough number to adequately represent the variation in the underlying data
#' @param point.estimate Whether a single point estimate should be returned for forecast distributions representing the mean value. Otherwise past years are sampled at random.
#' @param analysis.districts Districts to include in the analysis. This may include districts with no human cases that would otherwise be dropped from the modeling.
#' @param is.test Default is FALSE (runs all models). If set to TRUE, saved results will be used for the Random Forest model. For testing purposes only.
#'
#' @return dfmip.outputs: List of three objects: \tabular{ll}{
#' forecasts.df \tab A data frame with 7 columns: The model name, the forecast id, the forecast target, the geographic scope of the forecast
#' the forecast date, the forecast year, and the estimated value for the forecast target.\cr
#' forecast.distributions \tab A data frame with 6 fixed columns, and n.draws additional columns. The six fixed columns are:
#' The model name, the forecast id, the forecast target, the geographic scope of the forecast
#' the forecast date, and the forecast year.\cr
#' other.results \tab A keyed list of model-specific results that are not extracted in forecasts.df or forecast.distributions.
#' They can be accessed by keyword, for example, for the RF1_C model, other.results$rf1c will
#' return a list with the random forest outputs see rf1.outputs in RF1 package for details.\cr}
#'
#' @export dfmip.forecast
dfmip.forecast = function(forecast.targets, models.to.run, human.data, mosq.data,
                     weather.data, weekinquestion, week.id,
                     results.path, model.inputs = list(),
                     population.df = 'none', n.draws = 1000, point.estimate = 0,
                     analysis.districts = 'default', is.test = FALSE){

  # Check that models to run are all valid model names
  models.in.dfmip = c("NULL.MODELS", "ArboMAP", "ArboMAP.MOD", "RF1_C", "RF1_A", "FLM")
  check.models(models.to.run, models.in.dfmip)

  # Check which models support the selected forecast targets
  check.models.and.targets(models.to.run, forecast.targets)

  # Check that the appropriate input objects have been provided for the desired models
  check.model.inputs(models.to.run, model.inputs)

  # Configure default for analysis.districts
  analysis.districts = configure.analysis.districts(analysis.districts, forecast.targets, human.data, mosq.data)

  # Restrict data analysis to analysis.districts
  human.data =   human.data[as.character(human.data$district) %in% analysis.districts, ]
  if (!length(mosq.data) == 1){
    mosq.data = mosq.data[as.character(mosq.data$district) %in% analysis.districts, ]
  }
  if (!length(weather.data) == 1){
    weather.data = weather.data[as.character(weather.data$district) %in% analysis.districts, ]
  }

  # Initialize the forecast object
  forecasts.df = NA
  #message(paste(observed.inputs, collapse = ", "))

  # Initialize the forecast distributions object
  forecast.distributions = NA

  # Create an object to hold other model outputs
  other.outputs = list()

  # unpack week.id
  UNIT = splitter(as.character(week.id), ":", 1, 1)
  date = splitter(as.character(week.id), ":", 2, 1)
  year = splitter(date, "-", 1, 0)

  # Run the Null models
  if ("NULL.MODELS" %in% models.to.run){

    message("Running NULL models")

    null.out = run.null.models(forecast.targets, forecasts.df, forecast.distributions, human.data,
                            week.id, weekinquestion, n.draws, point.estimate, analysis.districts, mosq.data,
                            population.df, model.name = "NULL.MODELS")

    forecasts.df = null.out[[1]]
    forecast.distributions = null.out[[2]]
  }

  # Run ArboMAP model
  if ("ArboMAP" %in% models.to.run){

    stop("ArboMAP model requires updates")

    # Check that ArboMAP is installed
    if(!requireNamespace('ArboMAP')){
      stop('ArboMAP package must be installed. You can do this with devtools::install_github("akeyel/ArboMAP/ArboMAP_package", ref = "package_test")')
    }

    # Check that the arbo.inputs object is defined
    if (arbo.inputs[[1]] == 'none'){ stop("arbo.inputs object is required to run the ArboMAP model. Please make sure it exists and is properly input.")    }

    message("Running ArboMAP model")

    # Unpack the arbo.inputs object
    arbo.inputs = model.inputs[['arbo.inputs']]
    stratafile = arbo.inputs$stratafile
    maxobservedhumandate = arbo.inputs$maxobservedhumandate
    var1name = arbo.inputs$var1name
    var2name = arbo.inputs$var2name
    compyear1 = arbo.inputs$compyear1
    compyear2 = arbo.inputs$compyear2
    districtshapefile = arbo.inputs$districtshapefile
    weathersummaryfile = NA

    #stop("GOT HERE")

    # [[1]] is necessary to get just the results. [[2]] returns the inputs, and is for compatibility with the .Rmd format in which ArboMAP was initially developed
    ArboMAP.results = ArboMAP::ArboMAP(human.data, mosq.data, districtshapefile, stratafile, weather.data,
                              weathersummaryfile, maxobservedhumandate, weekinquestion,
                              var1name, var2name, compyear1, compyear2, results.path, original.metrics = 1)[[1]]
    ArboMAP.results$model.name = "ArboMAP"
    ArboMAP.results$forecast.id = week.id
    ArboMAP.results$seasonal.mosquito.MLE = NA

    # Set up distributions - currently just a single entry because ArboMAP does not produce probabilistic outputs
    ArboMAP.distributions = list()
    ArboMAP.distributions$annual.human.cases = ArboMAP.results$annual.human.cases
    ArboMAP.distributions$seasonal.mosquito.MLE = NA

    #**# OUT-DATED FIX WITH NEW CALL
    #forecasts.df = update.df(forecast.targets, forecasts.df, ArboMAP.results)
    #forecast.distributions = update.distribution(forecast.targets, ArboMAP.results$model.name, ArboMAP.results$forecast.id, forecast.distributions, ArboMAP.distributions)

  }

  # Run the modified ArboMAP model
  if ("ArboMAP.MOD" %in% models.to.run){

    stop("ArboMAP model requires updates")

    # Check that ArboMAP is installed
    if(!requireNamespace("ArboMAP")){
      stop('ArboMAP package must be installed. You can do this with devtools::install_github("akeyel/ArboMAP/ArboMAP_package", ref = "package_test")')
    }


    # Check that the arbo.inputs object is defined
    if (arbo.inputs[[1]] == 'none'){ stop("arbo.inputs object is required to run the ArboMAP model. Please make sure it exists and is properly input.")    }

    message("Running ArboMAP.MOD model")

    # Unpack the arbo.inputs object
    arbo.inputs = model.inputs[['arbo.inputs']]
    stratafile = arbo.inputs$stratafile
    maxobservedhumandate = arbo.inputs$maxobservedhumandate
    var1name = arbo.inputs$var1name
    var2name = arbo.inputs$var2name
    compyear1 = arbo.inputs$compyear1
    compyear2 = arbo.inputs$compyear2
    districtshapefile = arbo.inputs$districtshapefile
    weathersummaryfile = NA

    # [[1]] is necessary to get just the results. [[2]] returns the inputs, and is for compatibility with the .Rmd format in which ArboMAP was initially developed
    ArboMAP.results = ArboMAP::ArboMAP(human.data, mosq.data, districtshapefile, stratafile, weather.data,
                              weathersummaryfile, maxobservedhumandate, weekinquestion,
                              var1name, var2name, compyear1, compyear2, results.path, original.metrics = 0)[[1]]
    ArboMAP.results$model.name = "ArboMAP.MOD"
    ArboMAP.results$forecast.id = week.id
    ArboMAP.results$seasonal.mosquito.MLE = NA

    # Set up distributions - currently just a single entry because ArboMAP does not produce probabilistic outputs
    ArboMAP.distributions = list()
    ArboMAP.distributions$annual.human.cases = ArboMAP.results$human.cases
    ArboMAP.distributions$seasonal.mosquito.MLE = NA

    #**# UPDATE THIS
    #forecasts.df = update.df(forecast.targets, forecasts.df, ArboMAP.results)

    #forecast.distributions = update.distribution(forecast.targets, ArboMAP.results$model.name, ArboMAP.results$forecast.id, forecast.distributions, ArboMAP.distributions)

  }

  # Run the Random Forest 1 model with only climate inputs
  if ("RF1_C" %in% models.to.run){

    # Check that rf1 is installed
    if(!requireNamespace('rf1')){
      stop('rf1 package must be installed. You can do this with devtools::install_github("akeyel/rf1")')
    }

    check.dependencies("RF1", c("randomForest", "psych"))
    message("Running Random Forest 1 model")

    # Set up the sub-model output results
    rf1.results.path = sprintf("%s/rf1_results", results.path)
    if (!file.exists(rf1.results.path)){ dir.create(rf1.results.path, recursive = TRUE)  }

    # Create a new rf1.inputs object and clear any added inputs
    rf1.inputs.no.extras = model.inputs[['rf1.inputs']]
    rf1.inputs.no.extras[[1]] = NA
    rf1.inputs.no.extras[[2]] = NA

    input.districts = paste(sort(rf1.inputs.no.extras[[3]]), collapse = " ")
    analysis.districts.str = paste(sort(analysis.districts), collapse = " ")

    if (input.districts != analysis.districts.str){
      rf1.inputs.no.extras[[3]] = analysis.districts
      warning("Analysis districts in rf1.inputs over-ridden by the analysis districts supplied to dfmip as an input")
    }

    RF1.out = rf1::rf1(forecast.targets, human.data, mosq.data, weather.data,
                      weekinquestion, rf1.inputs.no.extras, rf1.results.path, UNIT,
                      use.testing.objects = is.test, n.draws = n.draws)

    RF1.results = RF1.out[[1]]
    RF1.forecast.distributions = RF1.out[[2]]
    RF1.bins = RF1.out[[3]]
    other.outputs$rf1c = RF1.out[[4]]

    # Update Results and Distribution objects with output from the Random Forest model (both forecast targets come out merged)
    RF1.model.name = "RF1_C"
    RF1.forecast.id = sprintf("%s:%s", week.id, RF1.results$district)
    RF1.records = data.frame(model.name = RF1.model.name, forecast.id = RF1.forecast.id,
                             forecast.target = RF1.results$forecast.target,
                             UNIT = UNIT, date = date, year = year, value = RF1.results$value)

    forecasts.df = update.df2(forecasts.df, RF1.records)

    RF1.distribution.records = data.frame(model.name = RF1.model.name, forecast.id = RF1.forecast.id,
                                          forecast.target = RF1.forecast.distributions$forecast.target,
                                          UNIT = UNIT, date = date, year = year)
    # Add data columns to the new data frame
    RF1.distribution.records = cbind(RF1.distribution.records, RF1.forecast.distributions[ ,3:(n.draws + 2)])
    forecast.distributions = update.distribution2(forecast.distributions, RF1.distribution.records)
  }

  # Run the Random Forest 1 model with all available inputs
  if ("RF1_A" %in% models.to.run){

    check.dependencies("RF1", c("randomForest", "psych"))
    message("Running Random Forest 1 model")

    # Check that rf1 is installed
    if(!requireNamespace('rf1')){
      stop('rf1 package must be installed. You can do this with devtools::install_github("akeyel/rf1")')
    }
    rf1.inputs = model.inputs[["rf1.inputs"]]

    input.districts = paste(sort(rf1.inputs[[3]]), collapse = " ")
    analysis.districts.str = paste(sort(analysis.districts), collapse = " ")

    if (input.districts != analysis.districts.str){
      rf1.inputs[[3]] = analysis.districts
      warning("rf1.inputs analysis districts was replaced by the input supplied to dfmip")
    }

    # Set up the sub-model output results
    rf1.results.path = sprintf("%s/rf1_results", results.path)
    if (!file.exists(rf1.results.path)){ dir.create(rf1.results.path, recursive = TRUE)  }

    RF1.out = rf1::rf1(forecast.targets, human.data, mosq.data, weather.data,
                      weekinquestion, rf1.inputs, rf1.results.path, UNIT,
                      use.testing.objects = is.test, n.draws = n.draws)

    RF1.results = RF1.out[[1]]
    RF1.forecast.distributions = RF1.out[[2]]
    RF1.bins = RF1.out[[3]]
    other.outputs$rf1c = RF1.out[[4]]

    # Update Results and Distribution objects with output from the Random Forest model (both forecast targets come out merged)
    RF1.model.name = "RF1_A"
    RF1.forecast.id = sprintf("%s:%s", week.id, RF1.results$district)
    RF1.records = data.frame(model.name = RF1.model.name, forecast.id = RF1.forecast.id,
                             forecast.target = RF1.results$forecast.target,
                             UNIT = UNIT, date = date, year = year, value = RF1.results$value)

    forecasts.df = update.df2(forecasts.df, RF1.records)

    RF1.distribution.records = data.frame(model.name = RF1.model.name, forecast.id = RF1.forecast.id,
                                          forecast.target = RF1.forecast.distributions$forecast.target,
                                          UNIT = UNIT, date = date, year = year)
    # Add data columns to the new data frame
    RF1.distribution.records = cbind(RF1.distribution.records, RF1.forecast.distributions[ ,3:(n.draws + 2)])
    forecast.distributions = update.distribution2(forecast.distributions, RF1.distribution.records)
  }

  # Run the Functional Linear Modeling approach
  if ("FLM" %in% models.to.run){

    # Check that package is installed
    my.package = "flm" #"flm_NE_WNV"
    package.path = 'khelmsmith/flm_NE_WNV'
    if(!requireNamespace('flm')){
      stop(sprintf('%s package must be installed. You can do this with devtools::install_github("%s")', my.package, package.path))
    }

    message("Running Functional Linear Modeling model")

    # Set up the sub-model output results
    model.results.path = sprintf("%s/%s_results", results.path, my.package)
    if (!file.exists(model.results.path)){ dir.create(model.results.path, recursive = TRUE)  }

    flm.inputs = model.inputs$flm.inputs

    flm.out = flm::run.flm(human.data, weather.data, flm.inputs, weekinquestion,
                      results.path, in.seed = 4872957)

    flm.results = flm.out[[1]]
    flm.distributions = flm.out[[1]] #**# Change to 2 when that object is created
    other.outputs$flm = flm.out[[3]]

    # Update Results and Distribution objects with output from the Random Forest model (both forecast targets come out merged)
    this.model.name = my.package
    model.forecast.id = sprintf("%s:%s", week.id, flm.results$County)
    #**# annual.human.cases is currently the only forecast target supported. if another is added, the below will need to be changed
    flm.records = data.frame(model.name = this.model.name, forecast.id = model.forecast.id,
                             forecast.target = 'annual.human.cases',
                             UNIT = UNIT, date = date, year = year, value = flm.results$predcases)

    forecasts.df = update.df2(forecasts.df, flm.records)

    # Add zeros for counties that never had a human case, using analysis.districts
    warning("Still need to add zeros to flm for counties that never had a human case of WNV")

    flm.distribution.records = data.frame(model.name = this.model.name, forecast.id = model.forecast.id,
                                          forecast.target = 'annual.human.cases',
                                          UNIT = UNIT, date = date, year = year)
    # Add data columns to the new data frame. At present, this is just the point estimate repeated n.draws times
    #**# change when going from a point estimate to probabilisic estimates
    for (i in seq_len(n.draws)){
      flm.distribution.records = cbind(flm.distribution.records, flm.results$predcases)
    }
    draw.names = sprintf("DRAW%s", seq(1,n.draws))
    colnames(flm.distribution.records) = c("model.name", "forecast.id", "forecast.target", "UNIT", "date", "year", draw.names)
    forecast.distributions = update.distribution2(forecast.distributions, flm.distribution.records)
  }

  # Make sure forecasts.targets are not converted to factor
  forecasts.df$forecast.target = as.character(forecasts.df$forecast.target)
  forecasts.df$model.name = as.character(forecasts.df$model.name)

  return(list(forecasts.df, forecast.distributions, other.outputs))
}

#' rf1.inputs
#'
#' The model-specific inputs required to run the RF1 model: \tabular{ll}{
#'
#' files.to.add\tab A vector of file names of other data sources to be
#' included in the Random Forest model. If no additional data is to be added,
#' this should be an empty vector (i.e. c()) or NA.\cr
#'merge.type.vec\tab A vector identifying how the files should be joined to
#' the mosquito and human data. Options are spatial_temporal where merges will
#' be performed on county and year (e.g., climate data); state_temporal where
#' merges will be performed on state and year (e.g. BBS data at state level),
#' and spatial, where merges will be performed on county only (e.g., landcover
#' and census data). This must have the same length as files.to.add, and if no
#' additional data is to be added, this should be an empty vector (i.e. c()) or NA.\cr
#' analysis.counties\tab A list of counties included in the analysis (this
#' is for ensuring that a county and year that does not have a human case is
#' treated as a 0)\cr
#' analysis.years\tab A list of years included in the analysis (this is for
#' ensuring that a county and year that does not have a human case is treated
#' as a 0 rather than as missing) #**# This is problematic if a specific county
#' or year is missing.\cr
#' user.drop.vars\tab A list of independent variables that should be
#' excluded from the analysis. If no variables should be excluded, this should
#' be an empty vector c() or NA.\cr
#' mosq.model\tab The Random Forest model to use for forecasting mosquito
#' infection rates. If this is to be fitted from the empirical data, this
#' should be set to NA\cr
#' human.model\tab The Random Forest model to use for forecasting human
#' cases. If this is to be fitted from the empirical data, this should be set
#' to NA.\cr
#' no.data.exceptions\taba #**# COMING SOON A list of county-years within the
#' range of analysis.years and analysis.counties that are missing data rather
#' than 0's\cr}
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
#' Note these entries are case-sensitive and are run by keyword, so run in a fixed order (NULL.MODELS, ArboMAP, ArboMAP.MOD, RF1_C, RF1_A),
#' regardless of the order specified in the models.to.run vector.
#' @param focal.years The years for which hindcasts will be made. Hindcasts will use all prior years as training data.
#' @param human.data Data on human cases of the disease. Must be formatted with two columns: district and date. The district column contains the spatial unit (typically county), while the date corresponds to the date of the onset of symptoms for the human case. The date column must be in format M/D/Y, with forward slashes as delimiters
#' #**# WHAT IF THESE DATA ARE MISSING? I.E. just making a mosquito forecast with RF1?
#' @param mosq.data Data on mosquito pools tested for the disease. Must be formatted with 4 columns: district (the spatial unit, e.g. county), col_date: the date the mosquitoes were tested, wnv_result: whether or not the pool was positive, pool_size: the number of mosquitoes tested in the pool. A fifth column species is optional but is not used by the code
#' @param weather.data Data on weather variables to be included in the analysis. See the read.weather.data function for details about data format.
#' The read.weather.data function from ArboMAP is a useful way to process one or more data files downloaded via Google Earth Engine.
#' @param results.path The base path in which to place the modeling results. Some models will create sub-folders for model specific results
#' @param model.inputs A keyed list of model-specific inputs. Keyed entry options are: \tabular{ll}{
#' arbo.inputs \tab Inputs specific to the ArboMAP model. #**# DOCUMENTATION NEEDED\cr
#' rf1.inputs \tab Inputs specific to the RF1 model, see \code{\link{rf1.inputs}}.\cr}
#' @param population.df Census information for calculating incidence. Can be set to 'none' or omitted from the function call #**# NEEDS FORMAT INSTRUCTIONS
#' @param threshold For continuous and discrete forecasts, a threshold of error to be used in classifying the forecast as "accurate". The default is +/- 1 human case, +/- 1 week, otherwise the default is 0.
#' @param percentage For continuous and discrete forecasts, if the prediction is within the specified percentage of the observed value, the forecast is considered accurate. The default is +/- 25 percent of the observed.
#' @param id.string An ID to include in the forecast ID for this hindcast run (e.g., state)
#' @param season_start_month The first month of the mosquito season, as a number. E.g., July would be 7.
#' @param weeks_in_season The number of weeks to sample
#' @param sample_frequency How frequently sample (default, 1 = weekly) #**# Other options are not currently supported
#' @param n.draws The number of draws for the forecast distributions. Should generally be 1 if a point estimate is used, otherwise should be a large enough number to adequately represent the variation in the underlying data
#' @param point.estimate Whether a single point estimate should be returned for forecast distributions representing the mean value. Otherwise past years are sampled at random.
#' @param analysis.districts Districts to include in the analysis. This may include districts with no human cases that would otherwise be dropped from the modeling.
#' @param is.test Default is 0 (runs all models). If set to 1, saved results will be used for the Random Forest model. For testing purposes only.

#' @return Four objects #**# ADD DOCUMENTATION. Also consider a list to hold the 'other.outputs'
#'
#' @export dfmip.hindcasts
dfmip.hindcasts = function(forecast.targets, models.to.run, focal.years, human.data, mosq.data,
                           weather.data, results.path, model.inputs = list(),
                           population.df = 'none', threshold = 'default', percentage = 'default',
                           id.string = "", season_start_month = 7, weeks_in_season = 2, sample_frequency = 1,
                           n.draws = 1000, point.estimate = 0, analysis.districts = 'default', is.test = FALSE){

  # Indicator to denote the first time through the loop
  first = 1
  # List to hold outputs
  #observation.list = list()

  # Configure default for analysis.districts
  analysis.districts = configure.analysis.districts(analysis.districts, forecast.targets, human.data, mosq.data)

  # Restrict data analysis to analysis.districts
  human.data =   human.data[as.character(human.data$district) %in% analysis.districts, ]
  # Length == 1 corresponds to NA. Cannot use is.na() because if it the data set, R throws a warning that the condition length is >1
  if (!length(mosq.data) == 1){
    mosq.data = mosq.data[as.character(mosq.data$district) %in% analysis.districts, ]
  }
  if (!length(weather.data) == 1){
    weather.data = weather.data[as.character(weather.data$district) %in% analysis.districts, ]
  }

  # Data frame to hold outputs
  observations.df = data.frame(district = NA, year = NA, district_year = NA, forecast.target = NA, value = NA)

  # If seasonal.mosquito.MLE is a forecast target, calculate the observed
  if ("seasonal.mosquito.MLE" %in% forecast.targets){
    # Check that rf1 is installed
    if(!requireNamespace('rf1')){ stop('rf1 package must be installed. You can do this with devtools::install_github("akeyel/rf1")') }

    md.data = rf1::calculate.MLE.v2(mosq.data, "annual")
    #**# Should be able to use a similar approach to estimate peak. Should make it easy to adapt the RF model to estimate a peak MLE as well.
    #**# Might be able to get timing at the same time.
  }

  # Run hindcast analysis for selected years
  for (year in focal.years){
    message(sprintf("Generating results for year %s", year))
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
    #human.data$year = sapply(as.character(human.data$date), splitter,  "/", 3)
    human.data$year = vapply(as.character(human.data$date), splitter, FUN.VALUE = numeric(1),  "/", 3)
    if (!length(mosq.data) == 1){  mosq.data$date = mosq.data$col_date  }

    # Calculate the observed values for the year
    if ("annual.human.cases" %in% forecast.targets){
      observations.df = update.observations(observations.df, human.data, 'annual.human.cases', id.string, year, analysis.districts)
    }

    if ("seasonal.mosquito.MLE" %in% forecast.targets){
      observations.df = update.observations(observations.df, md.data, 'seasonal.mosquito.MLE', id.string, year, analysis.districts)
    }

    # Loop through weeks to get a new forecast for each week for ArboMAP
    for (i in seq_len(length(month.vec))){

      month = month.vec[i]
      day = day.vec[i]
      year = year.vec[i]

      week.id = sprintf("%s:%04d-%02d-%02d", id.string, year, month, day)

      # Update the observed inputs and predict this weeks' cases
      this.weeks.cases = NA #**# NOT SCRIPTED as this is not one of the key comparison outputs

      # Subset the human data object
      human.data.subset = human.data[human.data$year < year, ]
      #**# Current year needs to be dropped for ArboMAP, but this may not be appropriate for other approaches. Need to re-address when this is an issue.
      #**# NOTE: the current year is dropped in this step. For the Random Forest analysis, it comes back with just 0's for cases, which is not correct, but allows the code to run and does not cause any errors (as those observations are not used for predictions or extracted as true observations)
      #**# This step would corrupt the data from the standpoint of Nick DeFelice's approach.
      #**# Human data are not used in making a prediction for a new year, so if getting no rows for the forecast year, check weather and mosquitoes

      # Subset the mosquito data object
      if(!length(mosq.data) == 1){
        mosq.data.subset = date.subset(mosq.data, year, month, day, 2)
      }else{ mosq.data.subset = NA  }

      # Subset the weather data object
      if (!length(weather.data) == 1){
        weather.data.subset = date.subset(weather.data, year, month, day, 1)
      }else{ weather.data.subset = NA  }

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
        if ("ArboMAP" %in% actual.models.to.run){
          arbo.inputs = model.inputs$arbo.inputs
          arbo.inputs[[2]] = maxobservedhumandate
          model.inputs$arbo.inputs = arbo.inputs
        }

        out = dfmip.forecast(forecast.targets, actual.models.to.run, human.data.subset, mosq.data.subset, weather.data.subset,
                             weekinquestion, week.id, results.path,
                             model.inputs, population.df, n.draws = n.draws, point.estimate = point.estimate,
                             analysis.districts = analysis.districts, is.test = is.test)
        out.forecasts = out[[1]]
        out.distributions = out[[2]]

        if (first == 1){
          forecasts.df = out.forecasts
          forecast.distributions = out.distributions
          first = 0
        }else{
          #test.type(out.forecasts, 'Line536')
          forecasts.df = rbind(forecasts.df, out.forecasts)
          forecast.distributions = rbind(forecast.distributions, out.distributions)

        }
      }
    }
  }

  # Calculate accuracy of the probablistic forecasts
  accuracy.summary = NA
  accuracy.summary = try(evaluate.accuracy(models.to.run, forecast.distributions, forecast.targets,
                                           observations.df, n.draws, threshold, percentage))

  return(list(accuracy.summary, forecasts.df, forecast.distributions, observations.df))
}

#' Evaluate accuracy
#'
#' @param models.to.run See \code{\link{dfmip.forecast}}
#' @param forecast.distributions See return object 2 in \code{\link{dfmip.forecast}}
#' @param forecast.targets See \code{\link{dfmip.forecast}}
#' @param observations.df A data frame with five fields: district, year, district_year, forecast.target, and value.
#' Value contains the observed value for the district and year for the corresponding forecast.target
#' @param n.draws See \code{\link{dfmip.forecast}}
#' @param threshold For continuous and discrete forecasts, a threshold of error to be used in classifying the forecast as "accurate". The default is +/- 1 human case, +/- 1 week, otherwise the default is 0.
#' @param percentage For continuous and discrete forecasts, if the prediction is within the specified percentage of the observed value, the forecast is considered accurate. The default is +/- 25 percent of the observed.

#' @return accuracy.summary A data frame organized by model, district
#' (and an aggregation across all districts, currently denoted -STATEWIDE, but this does not have to be a state),
#' forecast.target, with entries for the following evaluation metrics: CRPS, RMSE, Scaled_RMSE, percentage, threshold,
#' percentage or threshold, and Area Under the Curve (AUC).
#'
#' @export evaluate.accuracy
#'
evaluate.accuracy = function(models.to.run, forecast.distributions, forecast.targets, observations.df, n.draws,
                             threshold = 'default', percentage = 'default'){

  # Create accuracy summary object
  accuracy.summary = data.frame(model = NA, district = NA, forecast.target = NA,
                                CRPS = NA, RMSE = NA, Scaled_RMSE = NA, within_percentage = NA,
                                within_threshold = NA, within_threshold_or_percentage = NA, AUC = NA)

  # Add district and district_years field to forecast.distributions for merging with observations.df and subsetting by district
  #**# NOTE: probably should add these fields permanently, but I did not want to make upstream edits or redo the unit tests
  #**# this is a change that could be made when someone has spare time, if it does not break backward compatibility too much.
  # Although splitter does make it easy to create these fields on demand from forecast.id, and that reduces the redundant information stored
  forecast.distributions$district = sapply(forecast.distributions$forecast.id, splitter, ":", 3, 1)
  forecast.distributions$district_year = sprintf("%s_%s", forecast.distributions$district, forecast.distributions$year)

  # Loop through forecast targets
  for (i in seq_len(length(forecast.targets))){
    forecast.target = forecast.targets[i]

    target.distributions = forecast.distributions[forecast.distributions$forecast.target == forecast.target, ]
    target.observations = observations.df[observations.df$forecast.target == forecast.target, ]

    # Loop over districts
    districts = unique(target.distributions$district)
    for (k in seq_len(length(districts))){
      district = districts[k]
      district.distributions = target.distributions[target.distributions$district == district, ]

      # Calculate accuracy over all forecasts for each model
      for (j in seq_len(length(models.to.run))){
        model = models.to.run[j]
        dist.subset = district.distributions[district.distributions$model.name == model, ]

        # Check that model results were extracted. If not, throw an error, and present possible causes
        if (nrow(dist.subset) == 0){
          stop(sprintf("No results for %s model. Models included in the results are %s. Please check for a typo in the model names", model, paste(models.to.run, collapse = ', ')))
        }

        #**# Would it be faster to move this upstream?
        #**# NOTE: Do not move above forecast target subsetting, or you could merge in values from the wrong forecast target
        # Merge observations with forecast distributions. Only keep matches for the dist.subset object
        dist.subset = merge(dist.subset, target.observations, by = 'district_year', all.x = TRUE, all.y = FALSE)

        # Do not include in accuracy assessment if there are no corresponding observations
        # This can happen because if there are training data, the model can make a forecast, but not all locations may have observations for all years
        dist.subset = dist.subset[!is.na(dist.subset$value), ]
        if (nrow(dist.subset) > 0){
          # Generate the distributions matrix and the observations vector
          # Drop all non-data columns. Row 1 is the join field, Rows 2-7 are informational from forecast.distributions, and anything added on to the end was calculated or from the observations.df
          #**# Is there a less positionally-dependent way to do this? Not that I can think of right now. Would be nice to draw the numbers of columns from somewhere though.
          #without as.numeric, it is coming out as a list and a later 'apply' step is failing. as.numeric seems to fix this issue
          # The next step should put it back into the correctly dimensioned matrix
          model.forecast.distributions.pre = dist.subset[ , 8:(n.draws + 7)]
          # Correct for R dropping a 1,1 matrix out of matrix format
          if (length(dim(model.forecast.distributions.pre)) == 0){
            model.forecast.distributions = matrix(model.forecast.distributions.pre, ncol = 1)
          }else{
            model.forecast.distributions = as.numeric(as.character(model.forecast.distributions.pre[1, ]))
            if (nrow(model.forecast.distributions.pre) > 1){
              for (m in 2:nrow(model.forecast.distributions.pre)){
                model.forecast.distributions = rbind(model.forecast.distributions, as.numeric(as.character(model.forecast.distributions.pre[m , ])))
              }
            }

            n.col = (n.draws + 7) - 8 + 1 # calculate number of columns. Data must be in matrix format, and R drops that formatting for a single entry, so we have to add it back in.
            model.forecast.distributions = matrix(model.forecast.distributions, ncol = n.col)
          }

          # Observations will be added with the field name 'value', so this should just correspond to observations in the same order
          observations.vec = as.numeric(as.character(dist.subset$value))

          # Assess hindcast accuracy for each target
          target.accuracy.metrics = assess.accuracy(model.forecast.distributions, observations.vec, forecast.target, threshold, percentage)

          # Convert from list format to vector format #**# Could just use vector format to begin with!
          target.accuracy.metrics.vec = c()
          for (item in target.accuracy.metrics){
            target.accuracy.metrics.vec = c(target.accuracy.metrics.vec, item)
          }
          names(target.accuracy.metrics.vec) = names(target.accuracy.metrics)

          # Record part 1 #**# Get names to be consistent
          part1 = c(model, district, forecast.target)
          names(part1) = c('model', 'district', 'forecast.target')

          # Update accuracy.summary
          new.record = c(part1, target.accuracy.metrics.vec)
          accuracy.summary = rbind(accuracy.summary, new.record)
          #for (item in names(target.accuracy.metrics)){
          #  position.index = j + (j* (i-1))
          #  accuracy.summary$forecast.target[position.index] = forecast.target
          #  accuracy.summary[[item]][position.index] = target.accuracy.metrics[[item]]
          #}
        }
      }
    }
  }

  # Drop first line NA observations
  accuracy.summary = accuracy.summary[2:nrow(accuracy.summary), ]

  # Ensure outputs are numeric
  accuracy.summary$CRPS = as.numeric(as.character(accuracy.summary$CRPS))
  accuracy.summary$RMSE = as.numeric(as.character(accuracy.summary$RMSE))
  accuracy.summary$Scaled_RMSE = as.numeric(as.character(accuracy.summary$Scaled_RMSE))
  accuracy.summary$within_percentage = as.numeric(as.character(accuracy.summary$within_percentage))
  accuracy.summary$within_threshold = as.numeric(as.character(accuracy.summary$within_threshold))
  accuracy.summary$within_threshold_or_percentage = as.numeric(as.character(accuracy.summary$within_threshold_or_percentage))
  accuracy.summary$AUC = as.numeric(as.character(accuracy.summary$AUC))

  return(accuracy.summary)
}


#' Update data frame (version 2)
#'
#' @description forecasts.df should be initialized as NA if it does not already exist and contain data.
#' Otherwise, it is updated with the results. This function provides a standardized format for inputting
#' data from different models in to a common data frame
#'
#' @param forecasts.df The data frame object to contain the results from dfmip
#' @param results.object The results object to be integrated into forecasts.df. Must be a data frame with the following columns:
#' model.name, forecast.id, forecast.target, UNIT, date, year, value. For more information on these columns, see #**# ADD LINK.
#'
#' @noRd
#'
update.df2 = function(forecasts.df, results.df){

  # Create the results object if it does not already exist
  if (length(forecasts.df) != 7){
    # Confirm that this was triggered by an NA
    # The length step is needed to avoid a warning about length > 1 when the forecasts.df object exists.
    # Length should be 7 for a filled data frame
    if (!is.na(forecasts.df)){ stop("Something went very wrong with the update.df function") }

    forecasts.df = results.df

    # Ensure that fields come in as character, so that factor levels are not locked and fields are fully updateable
    forecasts.df$model.name = as.character(forecasts.df$model.name)
    forecasts.df$forecast.id = as.character(forecasts.df$forecast.id)
    forecasts.df$UNIT = as.character(forecasts.df$UNIT)
    forecasts.df$date = as.character(forecasts.df$date)

    # Otherwise, update it using rbind
  }else{
    forecasts.df = rbind(forecasts.df, results.df)

    #**# Watch for problems caused by non-numeric fields. annual.human.cases and seasonal.mosquito.MLE and year should be numeric, but forecast week will need to be a date.
  }

  return(forecasts.df)
}



#' #' Update data frame
#' #'
#' #' @description forecasts.df should be initialized as NA if it does not already exist and contain data.
#' #' Otherwise, it is updated with the results. This function provides a standardized format for inputting
#' #' data from different models in to a common data frame
#' #'
#' #' @param forecast.targets The targets to include in the output data frame. See \code{\link{dfmip.forecast}} for options.
#' #' @param forecasts.df The data frame object to contain the results from dfmip
#' #' @param results.object The results object to be integrated into forecasts.df
#' #'
#' #' @noRd
#' #'
#' update.df = function(forecast.targets, forecasts.df, results.object){
#'
#'   # Unpack the results object
#'   model.name = results.object$model.name
#'   forecast.id = results.object$forecast.id
#'
#'   # Plot observed vs. forecast human cases by state, year, and date of forecast
#'   UNIT = splitter(as.character(forecast.id), ":", 1, 1)
#'   date = splitter(as.character(forecast.id), ":", 2, 1)
#'   year = splitter(date, "-", 1, 0)
#'
#'   # Update human cases, if applicable
#'   if ("annual.human.cases" %in% forecast.targets){
#'     annual.human.cases = results.object$annual.human.cases
#'
#'   }
#'
#'   # Update seasonal mosquito MLE, if applicable
#'   if ("seasonal.mosquito.MLE" %in% forecast.targets){
#'     seasonal.mosquito.MLE = results.object$seasonal.mosquito.MLE
#'   }
#'
#'   # Other former targets
#'   #annual.positive.district.weeks = results.object$annual.positive.district.weeks
#'   #multiplier = results.object$multiplier
#'   #weeks.cases = results.object$weeks.cases
#'
#'
#'   # Create the results object if it does not already exist
#'   if (length(forecasts.df) < 2){
#'     # Confirm that this was triggered by an NA
#'     # The length step is needed to avoid a warning about length > 1 when the forecasts.df object exists.
#'     # Minimum length should be 5 for 5 fields, so <2 should never happen with a filled forecasts.df object
#'     if (!is.na(forecasts.df)){ stop("Something went very wrong with the update.df function") }
#'
#'     forecasts.df = data.frame(MODEL.NAME = model.name, FORECAST.ID = as.character(forecast.id), UNIT = as.character(UNIT), DATE = as.character(date), YEAR = year)
#'
#'     if ("annual.human.cases" %in% forecast.targets){  forecasts.df$annual.human.cases = annual.human.cases }
#'     if ("seasonal.mosquito.MLE" %in% forecast.targets){ forecasts.df$seasonal.mosquito.MLE = seasonal.mosquito.MLE  }
#'     #ANNUAL.POSITIVE.DISTRICT.WEEKS = annual.positive.district.weeks,
#'     #FORECAST.WEEK.CASES = weeks.cases
#'
#'     # Ensure that fields come in as character, so that factor levels are not locked and fields are fully updateable
#'     forecasts.df$MODEL.NAME = as.character(forecasts.df$MODEL.NAME)
#'     forecasts.df$FORECAST.ID = as.character(forecasts.df$FORECAST.ID)
#'     forecasts.df$UNIT = as.character(forecasts.df$UNIT)
#'     forecasts.df$DATE = as.character(forecasts.df$DATE)
#'
#'     # Otherwise, update it using rbind
#'   }else{
#'     #**# Watch that there are not issues with this order-based approach. Could have troubles if orders are changed.
#'     targets = c()
#'     if ("annual.human.cases" %in% forecast.targets){  targets = c(targets, annual.human.cases) }
#'     if ("seasonal.mosquito.MLE" %in% forecast.targets){ targets = c(targets, seasonal.mosquito.MLE)  }
#'     #annual.positive.district.weeks
#'     #weeks.cases
#'
#'     new.row = c(model.name, forecast.id, UNIT, date, year, targets)
#'     #test.type(new.row, 'L682')
#'     forecasts.df = rbind(forecasts.df, new.row)
#'
#'     # Ensure numeric fields are numeric
#'     if ("annual.human.cases" %in% forecast.targets){ forecasts.df$annual.human.cases = as.numeric(forecasts.df$annual.human.cases) }
#'     if ('seasonal.mosquito.MLE' %in% forecast.targets){ forecasts.df$seasonal.mosquito.MLE = as.numeric(forecasts.df$seasonal.mosquito.MLE) }
#'     forecasts.df$YEAR = as.numeric(forecasts.df$YEAR)
#'   }
#'
#'   return(forecasts.df)
#' }
#'

#forecast.distributions, RF1.distribution.record

#' Update list of forecast distributions
#'
#' @description forecasts.df should be initialized as NA if it does not already exist and contain data.
#' Otherwise, it is updated with the results. This function provides a standardized format for inputting
#' data from different models in to a common data frame
#'
#' @param forecast.distributions A data frame containing the forecast distributions \code{\link{dfmip.forecast}} for format
#' @param results.object The record or records to add to the forecast.distributions object
#'
#' @return forecast.distributions The updated data frame containing the forecast distributions
#'
#' @noRd
#'
update.distribution2 = function(forecast.distributions, results.object){

  # Create the results object if it does not already exist
  if (length(forecast.distributions) < 2){
    # Confirm that this was triggered by an NA
    # The length step is needed to avoid a warning about length > 1 when the forecasts.distributions object exists.
    if (!is.na(forecast.distributions)){ stop("Something went very wrong with the update.distribution2 function") }

    forecast.distributions = results.object

    # Ensure that fields come in as character, so that factor levels are not locked and fields are fully updateable
    forecast.distributions$model.name = as.character(forecast.distributions$model.name)
    forecast.distributions$forecast.id = as.character(forecast.distributions$forecast.id)
    forecast.distributions$UNIT = as.character(forecast.distributions$UNIT)
    forecast.distributions$date = as.character(forecast.distributions$date)

    # Otherwise, update it using rbind
  }else{
    forecast.distributions = rbind(forecast.distributions, results.object)

    #**# Watch for problems caused by non-numeric fields. annual.human.cases and seasonal.mosquito.MLE and year should be numeric, but forecast week will need to be a date.
  }

  # Try to ensure character fields are in character format
  forecast.distributions$model.name = as.character(forecast.distributions$model.name)
  forecast.distributions$forecast.id = as.character(forecast.distributions$forecast.id)

  return(forecast.distributions)
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
#' @noRd
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

#' Split strings using strsplit in a way that can easily be used within vapply
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

#' Restrict data to a single date
#'
#' df must have a column "date" in format YYYY-MM-DD (date.format = 1) or MM/DD/YYYY (date.format = 2)
#' Note that the date format was slow for string operations, so a datestr field will be added
#' df must have a column "year" in format YYYY
#'
#' @param df The input data frame
#' @param end.year The last year to include in the output data frame
#' @param end.month The last month to include in the output data frame
#' @param end.day The last day to include in the output data frame
#' @param date.format A numeric code for the date input format. 1: YYYY-MM-DD format, weather data; 2: MM/DD/YYYY format, human and mosquito data
#'
#' @return A data frame that only includes data up to the specified end day, month, year
#'
#'@export date.subset
date.subset = function(df, end.year, end.month, end.day, date.format){

  # Format for splitting out the weather data
  if (date.format == 1){
    # add df$month and df$day columns
    df$datestr = as.character(df$date) # Speeds up the splitting process
    #df$year = sapply(df$datestr, splitter, "-", 1)
    #df$month = sapply(df$datestr, splitter, '-', 2)
    #df$day = sapply(df$datestr, splitter, "-", 3)

    df$year = vapply(df$datestr, splitter, FUN.VALUE = numeric(1), "-", 1)
    df$month = vapply(df$datestr, splitter, FUN.VALUE = numeric(1), '-', 2)
    df$day = vapply(df$datestr, splitter, FUN.VALUE = numeric(1), "-", 3)
  }

  # Format for human and mosquito data
  if (date.format == 2){
    df$datestr = as.character(df$date)
    #df$year = sapply(df$datestr, splitter, "/", 3)
    #df$month = sapply(df$datestr, splitter, "/", 1)
    #df$day = sapply(df$datestr, splitter, '/', 2)

    df$year = vapply(df$datestr, splitter, FUN.VALUE = numeric(1), "/", 3)
    df$month = vapply(df$datestr, splitter, FUN.VALUE = numeric(1), "/", 1)
    df$day = vapply(df$datestr, splitter, FUN.VALUE = numeric(1), '/', 2)

  }

  #df$month = sapply(df$date, splitter2, 6,7) #**# Rate limiter was the date format, not the function used to split the date field. #**# splitter2 function moved to junk.R in dfmip_development folder (a local folder on ACK's machine)

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


#' Statewide Cases Null Model
#'
#' Predict the number of human cases based on historical number of human cases for the entire unit
#'
#' @param n.draws The number of elements to draw from the distribution.
#' @param point.estimate Whether or not to use the mean value as a single point estimate
#'
#' @noRd
#'
statewide.cases.null.model = function(human.data, n.years, model.name, week.id, n.draws, point.estimate){

  # unpack week.id
  UNIT = splitter(as.character(week.id), ":", 1, 1)
  date = splitter(as.character(week.id), ":", 2, 1)
  year = splitter(date, "-", 1, 0)

  # Create a forecast.id
  forecast.id = sprintf("%s:%s-STATEWIDE", week.id, UNIT)

  # Calculate total number of human cases
  tot.cases = nrow(human.data)

  # Estimate mean cases per year
  mean.annual.cases = tot.cases / n.years

  # Put it into a data frame for joining with other results
  statewide.cases = data.frame(model.name = model.name, forecast.id = forecast.id,
                              forecast.target = "annual.human.cases",
                              UNIT = UNIT, date = date, year = year,
                              value = mean.annual.cases)


  # Create distributions data frame to hold results
  statewide.distributions = data.frame(model.name = model.name, forecast.id = forecast.id,
                                       forecast.target = "annual.human.cases",
                                       UNIT = UNIT, date = date, year = year)

  # Populate data frame with draws for the specified number of distributions
  if (point.estimate == 1){
    statewide.distributions = cbind(statewide.distributions, matrix(rep(mean.annual.cases, n.draws), nrow = 1))
  }else{
    # Draw from historical values
    #**# really should think about smoothing this distribution out)
    #**# An easy way would be to do the process at the district level, then sum across disticts. That would produce continuous statewide counts, and have the benefit of making the predictions align
    human.data$count = 1
    historical.counts = stats::aggregate(human.data$count, by = list(human.data$year), sum, na.rm = TRUE)
    historical.counts = historical.counts$x
    # Add in 0's to historical counts
    if (length(historical.counts) < n.years){
      zeros = n.years - length(historical.counts)
      historical.counts = c(historical.counts, rep(0, zeros))
    }

    #historical.counts = dplyr::count(human.data, vars = year)[ ,2] # Tibble wasn't playing well with sample
    distribution.samples = sample(historical.counts, n.draws, replace = TRUE) # SAMPLE WITH REPLACEMENT FROM THE ANNUAL HUMAN CASES #**# LEFT OFF HERE
    statewide.distributions = cbind(statewide.distributions, matrix(distribution.samples, nrow = 1))
  }

  return(list(statewide.cases, statewide.distributions))
}

#' District Cases Null Model
#'
#' Predict the number of human cases of WNV based on historical number of human cases by district
#'
#'@param human.data Standardized human data input
#'@param n.years Number of years in human.data
#'
#' @noRd
#'
district.cases.null.model = function(human.data, n.years, model.name, week.id,
                                     n.draws, point.estimate, analysis.districts){

  # unpack week.id
  UNIT = splitter(as.character(week.id), ":", 1, 1)
  date = splitter(as.character(week.id), ":", 2, 1)
  year = splitter(date, "-", 1, 0)
  human.data$count = 1 # Add a count column for aggregation
  #districts = unique(human.data$district)
  districts = analysis.districts

  # Data frame will be populated with values by looping through districts
  district.cases = data.frame(model.name = model.name, forecast.id = rep(NA, length(districts)),
                              forecast.target = "annual.human.cases",
                              UNIT = UNIT, date = date, year = year, value = NA)

  # Similar for distributions, except distributions will be added as a matrix with cbind later
  district.distributions = data.frame(model.name = model.name, forecast.id = rep(NA, length(districts)),
                                       forecast.target = "annual.human.cases",
                                       UNIT = UNIT, date = date, year = year)


  # Loop over districts
  for (i in seq_len(length(districts))){

    district = districts[i]
    forecast.id = sprintf("%s:%s", week.id, district)

    human.data.subset = human.data[as.character(human.data$district) == district, ]
    total.district.cases = nrow(human.data.subset)
    district.cases.per.year = total.district.cases / n.years

    # update spatial.cases.per.year
    district.cases$forecast.id[i] = forecast.id
    district.cases$value[i] = district.cases.per.year

    # Update distributions
    district.distributions$forecast.id[i] = forecast.id
    if (point.estimate == 1){
      if (i == 1){  distributions.matrix = matrix(rep(district.cases.per.year, n.draws), nrow = 1)
      }else{
        distributions.matrix = rbind(distributions.matrix, rep(district.cases.per.year, n.draws))
      }
    }else{
      # Draw from historical values (really should think about smoothing this distribution out)
      if (nrow(human.data.subset) == 0){
        historical.counts = 0
      }else{
        historical.counts = stats::aggregate(human.data.subset$count, by = list(human.data.subset$year), sum, na.rm = TRUE)
        historical.counts = historical.counts$x
      }
      # Add in 0's to historical counts
      if (length(historical.counts) < n.years){
        zeros = n.years - length(historical.counts)
        historical.counts = c(historical.counts, rep(0, zeros))
      }
      distribution.samples = sample(historical.counts, n.draws, replace = TRUE) # SAMPLE WITH REPLACEMENT FROM THE ANNUAL HUMAN CASES #**# LEFT OFF HERE

      if (i == 1){
        distributions.matrix = matrix(distribution.samples, nrow = 1)
      }else{
        distributions.matrix = rbind(distributions.matrix, distribution.samples)
      }
    }
  }

  #**# Watch for issues with the distributions.matrix
  distributions.matrix = matrix(distributions.matrix, nrow = length(districts)) # Ensure that it is a matrix, even if there is only one district and R changes the matrix to a row
  district.distributions = cbind(district.distributions, distributions.matrix)

  return(list(district.cases, district.distributions))
}


#' Mean Incidence Model
#'
#' Predict the number of human cases of WNV based on historical number of human cases, adjusted for population
#' This model does not adjust risk based on county - clearly an incorrect model assumption, but one that is of interest
#'
#'@param human.data Standardized human data input
#'@param population.df A data frame with districts (counties) and their populations. A column labeled SPATIAL should contain the county/district information, while population should be in a TOTAL_POPULATION column.
#'@param cases.per.year Average number of cases per year
#'@param n.years Number of years in human.data
#'
#' @noRd
#'
mean.incidence.model = function(human.data, population.df, cases.per.year, n.years){

  # Subset the population data to just spatial locations included in human.data
  population.df = population.df[as.character(population.df$SPATIAL) %in% as.character(human.data$district), ]

  incidence.per.year = cases.per.year / sum(population.df$TOTAL_POPULATION)
  spatial.cases.per.year = data.frame(SPATIAL = population.df$SPATIAL, TOTAL_POPULATION = population.df$TOTAL_POPULATION, CASES.STATEWIDE.INCIDENCE = (population.df$TOTAL_POPULATION * incidence.per.year), STATEWIDE.INCIDENCE = incidence.per.year)

  # Calculate indicence on a district-specific basis
  spatial.cases.per.year$CASES.DISTRICT.INCIDENCE = NA
  spatial.cases.per.year$DISTRICT.INCIDENCE = NA

  # Loop over districts
  for (i in seq_len(length(spatial.cases.per.year$SPATIAL))){
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
#'@param human.data Standardized human data input
#'@param focal.year The year in question
#'@param mean.annual.cases The mean number of cases each year
#'@param week.start The day of year on which the week starts in the focal year
#'#**# should this just be year? I bet there is a pattern based on year.
#'
#' Can apply this at the district level by multiplying the estimates by the mean annual cases for each district.
#' But likely too few in NYS for that to be particularly useful?
#'
#' @noRd
#'
seasonal.incidence = function(human.data, focal.year, mean.annual.cases, week.start){

  seasonal.cases = NA
  #**# TEMPORARILY DISABLED TO AVOID WARNING ABOUT NO global binding for 'x'. Need to figure out why it is needed, and how to get R to not care that it isn't defined (or define it explicitly)
  #
  # # Convert date to day of year
  # #human.data$month = sapply(as.character(human.data$date), splitter, "/", 1)
  # #human.data$day = sapply(as.character(human.data$date), splitter, "/", 2)
  # human.data$month = vapply(as.character(human.data$date), splitter, FUN.VALUE = numeric(1), "/", 1)
  # human.data$day = vapply(as.character(human.data$date), splitter, FUN.VALUE = numeric(1), "/", 2)
  # human.data$doy = mapply(get.DOY, human.data$year, human.data$month, human.data$day)
  #
  # hist(human.data$doy, breaks = seq(1,366))
  #
  # # Use truncation dates from ArboMAP
  # Early.Cutpoint = get.DOY(2001,5,1) # Assume a non-leap year
  # Late.Cutpoint = get.DOY(2001, 11,30) # Assume a non-leap year
  # human.data.truncated = human.data[human.data$doy >= Early.Cutpoint & human.data$doy <= Late.Cutpoint, ]
  # out = hist(human.data.truncated$doy, breaks = seq(Early.Cutpoint,Late.Cutpoint), prob = TRUE)
  # midpoints = out$breaks + 0.5 #**# 0.5 is hardcoded and corresponds to a bin size of 1
  # midpoints = midpoints[1:(length(midpoints) - 1)] # Drop last midpoint - it is out of range
  #
  # # Estimate mean and sd for fitting normal distribution
  # #hd.mean = mean(human.data.truncated$doy)
  # #hd.sd = sd(human.data.truncated$doy)
  # #curve(dnorm(x, hd.mean, hd.sd), col = 2, add = TRUE) # I have no idea where the 'x' comes from, but it is really unhappy if it is not there!
  #
  # # Look at log-normal distribution (not appreciably different for the NY data)
  # #human.data.truncated$ln.doy = log(human.data.truncated$doy)
  # #hd.ln.mean = mean(human.data.truncated$ln.doy)
  # #hd.ln.sd = sd(human.data.truncated$ln.doy)
  # #curve(dlnorm(x, hd.ln.mean, hd.ln.sd), col = 3, add = TRUE)
  #
  # # Split the incidence up over weeks in the focal period
  # jan.start.week = get.start.week(focal.year)
  #
  # start.week = jan.start.week
  # # Add 7 days until we enter the data range
  # while(start.week < Early.Cutpoint){
  #   start.week = start.week + 7
  # }
  #
  # # Identify end week when the doy is no longer in the range
  # # (subtracting 14 from Late.Cutpoint keeps the end of the end week from exceeding the late cutpoint range)
  # end.week = start.week
  # while(end.week <= (Late.Cutpoint - 14)){
  #   end.week = end.week + 7
  # }
  #
  # #**# I THINK I NEED TO OFFSET BY 3.5? AND MAYBE PAD WITH A WEEK ON EITHER END?
  # # Use the normal distribution to make an incidence prediction for every week from start week to end week
  # #estimates = dnorm(seq(start.week, end.week, 7), hd.mean, hd.sd) # sum(estimates) = 0.14 - this is not correct!, but multiply by 7 and you get 0.999992, which is correct.
  # estimates = dnorm(seq(start.week, end.week, 1), hd.mean, hd.sd) # sum(estimates) = 0.9999831 - that works, but need to aggregate by 7's #**# Would that be the same as multiplying the above by 7?
  #
  # # Offset by 3.5 to set the estimate for the mid-point of the weeks #**# I THINK THIS IS RIGHT? #**# CHECK WITH A SIMULATED NORMAL DISTRIBUTION
  # estimates = dnorm(seq(start.week + 3.5, end.week + 3.5, 7), hd.mean, hd.sd) #**# Do we need buffers on the edges to remove edge effects? I.e. do the end bins contain all the probability below that point?
  # #estimates = estimates[2:length(estimates)]
  # # sum(estimates) * 7 = 0.9999827
  # estimates = estimates * 7 # Multiply by 7 days in intervals
  # week.doy = seq(start.week, end.week, 7)
  #
  # # Multiply by mean number of cases in a year to get the fraction that should happen in each week
  # estimated.cases = round(estimates * mean.annual.cases, 1)
  #
  # seasonal.cases = data.frame(WEEK.START = week.doy, PROBABILITY.DENSITY = estimates, ESTIMATED.CASES = estimated.cases)

  return(seasonal.cases)

}

#' Goal is to be able to have predictions comparable to ArboMAP
#'
#' Return the day associated with the first Sunday for the given year
#'
#' @param year The selected year
#'
#' @return The day of the first Sunday for the selected year
#'
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
#'
#' COPIED FROM wnv_hlpr.R
#'
#' @param year Input year
#' @param month Input month
#' @param day Input day
#'
#' @return Return ordinal day of the year
#'
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
#' Does not work for years 1900 and before or after 2100 (i.e. it does not handle the Century cases)
#'
#' @param year The year to examine
#'
#' @return The number of days in the year (365 for non-leap years, 366 for a leap year)
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


#' Check that required packages are installed prior to running the code
#'
#' The function will provide a list of any missing packages that need to be installed by the user
#'
#' @param model.name The name of the model to check dependencies for
#' @param packages The packages required for the model in question
#'
#' @noRd
#'
check.dependencies = function(model.name, packages){
  m.base = "Please install package '%s' to run the %s model (install.packages('%s'))"
  m = ""
  is.error = 0
  for (package.name in packages){
    if (!requireNamespace(package.name, character.only = TRUE)){
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
#' @param season_start_month The first month of the mosquito season, as a number. E.g., July would be 7.
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
#' Simple testing function to identify a bug with rbind in the code. Bug appears to be external to dfmip.R and output_accuracy.R
#' #**# Consider moving to junk?
#' @param vector An input vector to check
#' @param test.name A name for the test location in the code
#'
#' @noRd
#'
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
#' @noRd
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

#' Check Model Inputs
#'
#' Check that the appropriate input objects have been provided for the desired models
#'
#' @param models.to.run a vector of the models to be run
#' @param model.inputs a list of model inputs, keyed by the name of the inputs
#' (cannot key to model name, as multiple model variants may use the same inputs)
#'
#' @noRd
#'
check.model.inputs = function(models.to.run, model.inputs){

  err.message = ""
  is.error = 0
  # Check for arbo.inputs
  if ("ArboMAP.MOD" %in% models.to.run | "ArboMAP" %in% models.to.run){
    if (length(model.inputs$arbo.inputs) == 0){
      err.message = sprintf("%s\narbo.inputs object is missing from the model.inputs list.", err.message)
      is.error = 1
    }
  }

  # Check for RF1 inputs
  if ("RF1_C" %in% models.to.run | "RF1_A" %in% models.to.run){
    if (length(model.inputs$rf1.inputs) == 0){
      err.message = sprintf("%s\nrf1.inputs object is missing from the model.inputs list.", err.message)
      is.error = 1
    }
  }
  if (is.error == 1){stop(err.message)}
}



#' Generate null models based on mean values
#'
#' Create simple null models for comparison
#'
#' @param forecast.targets See \code{\link{dfmip.forecast}}
#' @param forecasts.df See \code{\link{dfmip.forecast}}
#' @param forecast.distributions See \code{\link{dfmip.forecast}}
#' @param human.data See \code{\link{dfmip.forecast}}
#' @param week.id See \code{\link{dfmip.forecast}}
#' @param weekinquestion See \code{\link{dfmip.forecast}}
#' @param n.draws The number of draws for the forecast distributions. Should be 1 if a point estimate is used, otherwise should be a large enough number to adequately represent the variation in the underlying data
#' @param point.estimate Whether a single point estimate should be returned for forecast distributions representing the mean value. Otherwise past years are sampled at random.
#' @param analysis.districts Districts to include in the analysis. This may include districts with no human cases that would otherwise be dropped from the modeling.
#' @param mosq.data Only required if a mosquito output is among the forecast targets. See \code{\link{dfmip.forecast}}
#' @param population.df A data frame with districts (counties) and their populations. A column labeled SPATIAL should contain the county/district information, while population should be in a TOTAL_POPULATION column. Only required if incidence calculations are desired.
#' @param model.name The name of the model to use in forecasts.df and forecast.distributions
#'
#' @return A list consisting of a data frame with forecast results and a list of forecast distributions
#'
run.null.models = function(forecast.targets, forecasts.df, forecast.distributions, human.data,
                           week.id, weekinquestion, n.draws, point.estimate, analysis.districts,
                           mosq.data = NA, population.df = NA, model.name = "NULL.MODELS"){

  # Create the year column, if it is missing
  if (length(human.data$year) == 0){
    human.data$year = vapply(as.character(human.data$date), splitter, FUN.VALUE = numeric(1),  "/", 3)
  }

  # Calculate annual human cases
  if ('annual.human.cases' %in% forecast.targets){

    # Calculate total number of years
    n.years = length(unique(human.data$year))

    # Calculate statewide results
    scnm.out = statewide.cases.null.model(human.data, n.years, model.name, week.id, n.draws, point.estimate)
    statewide.cases = scnm.out[[1]]
    statewide.distributions = fix.labels(scnm.out[[2]])

    # Update outputs
    forecasts.df = update.df2(forecasts.df, statewide.cases)
    forecast.distributions = update.distribution2(forecast.distributions, statewide.distributions)

    # Calculate district-specific results
    dcnm.out = district.cases.null.model(human.data, n.years, model.name, week.id, n.draws, point.estimate, analysis.districts)
    district.cases = dcnm.out[[1]]
    district.distributions = fix.labels(dcnm.out[[2]])

    # Update outputs
    forecasts.df = update.df2(forecasts.df, district.cases)
    forecast.distributions = update.distribution2(forecast.distributions, district.distributions)
  }

  # Calculate annual human incidence
  # Estimate incidence
  if ("human.incidence" %in% forecast.targets){
    stop("human.incidence is not yet supported by the null.model")
    #**# mean.annual.cases is not currently returned, among other problems
    #mean.incidence = mean.incidence.model(human.data, population.df, mean.annual.cases, n.years)
    #incidence.per.year = mean.incidence[[1]]
    #spatial.cases.per.year = mean.incidence[[2]]
  }

  # Estimate mean annual MLE
  mean.seasonal.mosquito.MLE = NA
  if ("seasonal.mosquito.MLE" %in% forecast.targets){

    # Check that rf1 is installed
    if(!requireNamespace("rf1")){ stop('rf1 package must be installed. You can do this with devtools::install_github("akeyel/rf1")') }
    md.data = rf1::calculate.MLE.v2(mosq.data, "annual")

    # Calculate statewide results
    scnm.out = statewide.mle.null.model(md.data, n.years, model.name, week.id, n.draws, point.estimate)
    statewide.MLE = scnm.out[[1]]
    statewide.MLE.distributions = fix.labels(scnm.out[[2]])

    # Update outputs
    forecasts.df = update.df2(forecasts.df, statewide.MLE)
    forecast.distributions = update.distribution2(forecast.distributions, statewide.MLE.distributions)

    # Calculate district-specific results
    dcnm.out = district.mle.null.model(md.data, n.years, model.name, week.id, n.draws, point.estimate)
    district.MLE = dcnm.out[[1]]
    district.distributions = fix.labels(dcnm.out[[2]])

    # Update outputs
    forecasts.df = update.df2(forecasts.df, district.MLE)
    forecast.distributions = update.distribution2(forecast.distributions, district.distributions)

  }


  # Forecast expected week's cases based on previous number of cases in this particular week
  if ("weeks.cases" %in% forecast.targets){

    stop("weeks.cases not fully scripted for null model yet.")
    mean.annual.cases = NA #**# NEED TO CALCULATE OR BRING DOWN FROM ABOVE
    #**# Condense this into a separate function to improve flow of code
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
      stop(m1) #**# Should this be stop or warning? Are 0's filled in?
    }

    if (length(weeks.cases) > 1){  stop("More than one result was returned for weeks.cases")  }
    statewide.results$weeks.cases = weeks.cases
  }


  return(list(forecasts.df, forecast.distributions))
}

#' Fix Labels
#'
#' Ensure that forecast.distributions has a consistent set of names (apparently this can cause an error later)
#' @noRd
#'
fix.labels = function(forecast.distributions){
  value.labels = sprintf("DRAW%s", seq(1, ncol(forecast.distributions) - 6))
  colnames(forecast.distributions) = c("model.name", "forecast.id", "forecast.target", "UNIT", "date", "year", value.labels)
  return(forecast.distributions)
}


#' Check that forecast targets are supported by models
#'
#' Provide a clear and up-front documentation of which models support a given forecast target, and which do not.
#'
#' @param models.to.run A string vector of the models to run. See \code{\link{dfmip.forecasts}} for more details.
#' @param forecast.targets What the model is forecasting. See \code{\link{dfmip.forecast}} for more details.
#'
#' @return NULL
#' @noRd
#'
check.models.and.targets = function(models.to.run, forecast.targets){

  # Define targets once to reduce issues with typos #**# Should I get these from somewhere external, so they only have to be changed in one place?
  ahc = 'annual.human.cases'
  smMLE = 'seasonal.mosquito.MLE'

  supported.targets = NA

  for (model in models.to.run){
    for (target in forecast.targets){

      if (model == "RF1_A" | model == "RF1_C" | model == "NULL.MODELS"){
        supported.targets = c(ahc, smMLE)
      }

      if (model == "ArboMAP" | model == "ArboMAP.MOD" | model == "FLM"){
        supported.targets = c(ahc)
      }

      # Add message that seasonal mosquito MLE is not currently supported by our version of ArboMAP
      if (!target %in% supported.targets){
        message(sprintf("%s not supported for %s. Estimates will not be produced for this model", target, model))
      }


    }
  }

}



#' Calculate statewide MLE null model
#'
#' @noRd
#'
statewide.mle.null.model = function(md.data, n.years, model.name, week.id, n.draws, point.estimate){

  # unpack week.id
  UNIT = splitter(as.character(week.id), ":", 1, 1)
  date = splitter(as.character(week.id), ":", 2, 1)
  year = splitter(date, "-", 1, 0)

  # Create a forecast.id
  forecast.id = sprintf("%s:%s-STATEWIDE", week.id, UNIT)

  # Calculate mosquito MLE's statewide
  MLE.vec = c()
  for (year in unique(md.data$YEAR)){
    year.MLE = md.data[md.data$YEAR == year, ]
    seasonal.mosquito.MLE = mean(year.MLE$IR, na.rm = TRUE) #**# Average is problematic here, but I am not sure of a better approach at this point
    MLE.vec = c(MLE.vec, seasonal.mosquito.MLE)
  }
  mean.seasonal.mosquito.MLE = mean(MLE.vec, na.rm = TRUE)

  # Put it into a data frame for joining with other results
  statewide.MLE = data.frame(model.name = model.name, forecast.id = forecast.id,
                               forecast.target = "seasonal.mosquito.MLE",
                               UNIT = UNIT, date = date, year = year,
                               value = mean.seasonal.mosquito.MLE)

  # Create distributions data frame to hold results
  statewide.distributions = data.frame(model.name = model.name, forecast.id = forecast.id,
                                       forecast.target = "seasonal.mosquito.MLE",
                                       UNIT = UNIT, date = date, year = year)

  # Populate data frame with draws for the specified number of distributions
  if (point.estimate == 1){
    statewide.distributions = cbind(statewide.distributions, matrix(rep(mean.seasonal.mosquito.MLE, n.draws), nrow = 1))
  }else{
    # Draw from historical values
    #**# really should think about smoothing this distribution out)
    #**# An easy way would be to do the process at the district level, then average across disticts. That would produce continuous statewide counts, and have the benefit of making the predictions align
    distribution.samples = sample(MLE.vec, n.draws, replace = TRUE) # SAMPLE WITH REPLACEMENT FROM THE ANNUAL HUMAN CASES #**# LEFT OFF HERE
    statewide.distributions = cbind(statewide.distributions, matrix(distribution.samples, nrow = 1))
  }

  return(list(statewide.MLE, statewide.distributions))
}

#' Calculate null model results for each district
#'
#' @noRd
#'
district.mle.null.model = function(md.data, n.years, model.name, week.id, n.draws, point.estimate){
  # unpack week.id
  UNIT = splitter(as.character(week.id), ":", 1, 1)
  date = splitter(as.character(week.id), ":", 2, 1)
  year = splitter(date, "-", 1, 0)

  districts = unique(md.data$COUNTY) #**# Watch for problems for other spatial units

  # Data frame will be populated with values by looping through districts
  district.MLE = data.frame(model.name = model.name, forecast.id = rep(NA, length(districts)),
                              forecast.target = "seasonal.mosquito.MLE",
                              UNIT = UNIT, date = date, year = year, value = NA)

  # Similar for distributions, except distributions will be added as a matrix with cbind later
  district.distributions = data.frame(model.name = model.name, forecast.id = rep(NA, length(districts)),
                                      forecast.target = "seasonal.mosquito.MLE",
                                      UNIT = UNIT, date = date, year = year)
  # Loop over districts
  for (i in seq_len(length(districts))){

    district = districts[i]
    forecast.id = sprintf("%s:%s", week.id, district)

    md.data.subset = md.data[md.data$COUNTY == district, ]

    MLE.vec = c()
    for (year in unique(md.data.subset$YEAR)){
      year.MLE = md.data.subset[md.data.subset$YEAR == year, ]
      seasonal.mosquito.MLE = mean(year.MLE$IR, na.rm = TRUE) #**# Average is problematic here, but I am not sure of a better approach at this point
      MLE.vec = c(MLE.vec, seasonal.mosquito.MLE)
    }
    mean.seasonal.mosquito.MLE = mean(MLE.vec, na.rm = TRUE)

    # update district.MLE dataframe
    district.MLE$forecast.id[i] = forecast.id
    district.MLE$value[i] = mean.seasonal.mosquito.MLE

    # Update distributions
    district.distributions$forecast.id[i] = forecast.id
    if (point.estimate == 1){
      if (i == 1){  distributions.matrix = matrix(rep(mean.seasonal.mosquito.MLE, n.draws), nrow = 1)
      }else{
        distributions.matrix = rbind(distributions.matrix, rep(mean.seasonal.mosquito.MLE, n.draws))
      }
    }else{
      # Draw from historical values (really should think about smoothing this distribution out)
      distribution.samples = sample(MLE.vec, n.draws, replace = TRUE) # SAMPLE WITH REPLACEMENT FROM THE ANNUAL HUMAN CASES #**# LEFT OFF HERE

      if (i == 1){
        distributions.matrix = matrix(distribution.samples, nrow = 1)
      }else{
        distributions.matrix = rbind(distributions.matrix, distribution.samples)
      }
    }
  }

  #**# Watch for issues with the distributions.matrix
  distributions.matrix = matrix(distributions.matrix, nrow = length(districts)) # Ensure that it is a matrix, even if there is only one district and R changes the matrix to a row
  district.distributions = cbind(district.distributions, distributions.matrix)

  return(list(district.MLE, district.distributions))

}


#' Update Observations
#'
#' @param observations.df A data frame with five fields: district, year, district_year, forecast.target, and value.
#' Value contains the observed value for the district and year for the corresponding forecast.target
#' @param in.data The data set containing the observations. Must correspond to the forecast.target.
#' @param forecast.target The quantity being predicted. Should be one of the following:
#' 'annual.human.cases', 'seasonal.mosquito.MLE'
#'
#' @noRd
#' @return observations.df An updated observations.df object
#'
update.observations = function(observations.df, in.data, forecast.target, id.string, year, analysis.districts){

  # User should never see this message. More likely to be an issue during development
  if (!forecast.target %in% c('annual.human.cases', 'seasonal.mosquito.MLE')){
    stop(sprintf("forecast.target %s is not supported. Please check the code for typos", forecast.target))
  }

  # Update human cases
  if (forecast.target == 'annual.human.cases'){
    # Make sure district is treated as character
    in.data$district = as.character(in.data$district)

    # Human data is in format of one case per year, so nrow works the same as a sum
    year.data = in.data[in.data$year == year, ]
    year.human.cases = nrow(year.data)

    # Update value across all districts #**# NEED TO CHANGE STATEWIDE, but there to be consistent with other pieces of the code
    unit.id = sprintf('%s-STATEWIDE', id.string)
    new.record = c(unit.id, year, sprintf("%s_%s", unit.id, year), forecast.target, year.human.cases)
    observations.df = rbind(observations.df, new.record)

    # Update values for each district
    districts = unique(in.data$district)
    for (i in seq_len(length(districts))){
      district = districts[i]
      district.subset = year.data[year.data$district == district, ]
      district.cases = nrow(district.subset)
      new.record = c(district, year, sprintf("%s_%s", district, year), forecast.target, district.cases)
      observations.df = rbind(observations.df, new.record)
    }

    # Update values for each district in analysis.districts that has never had a human case in the date range
    # Those with one case will have nrow = 0, but those that have never had a case just don't show up.
    missed.districts = analysis.districts[!analysis.districts %in% districts]
    for (district in missed.districts){
      new.record = c(district, year, sprintf("%s_%s", district, year), forecast.target, 0)
      observations.df = rbind(observations.df, new.record)
    }
  }

  # Calculate observations for seasonal mosquito infection rate
  if (forecast.target == 'seasonal.mosquito.MLE'){
    year.MLE = in.data[in.data$YEAR == year, ]
    seasonal.mosquito.MLE = mean(year.MLE$IR, na.rm = TRUE) #**# Average is problematic here, but I am not sure of a better approach at this point
    unit.id = sprintf('%s-STATEWIDE', id.string)
    new.record = c(unit.id, year, sprintf("%s_%s", unit.id, year), forecast.target, seasonal.mosquito.MLE)
    observations.df = rbind(observations.df, new.record)

    # Update values for each district # Field is still COUNTY in the mosquito data due to historical reasons. This could be updated
    districts = unique(in.data$COUNTY)
    for (i in seq_len(length(districts))){
      # Given that data have been subset by year, all that should be left is county data. Can just pull from the county row
      district = districts[i]

      # Note: right now, CI information associated with the observations are being lost.
      # With a move to probablistic thinking, this is not optimal, as we could have probabilitic observations, too. #**# Except CRPS does not support that, to my knowledge.
      district.cases = year.MLE$IR[i]
      new.record = c(district, year, sprintf("%s_%s", district, year), forecast.target, district.cases)
      observations.df = rbind(observations.df, new.record)
    }
  }

  # Check if first line of observations.df is NA, if so, drop it
  if (is.na(observations.df[1,1])){ observations.df = observations.df[2:nrow(observations.df), ]}

  # Return observations.df
  return(observations.df)
}


#' Convert to CDC Forecast Challenge
#'
#' @param model.name The name of the model results to subset from the forecast.distributions object
#' @param forecast.distributions The forecast.distributions output created by dfmip for a single forecast model
#' @param out.file The file to contain the CDC WNV forecast
#' @param n.draws The number of forecast draws in the forecast.distributions dataframe
#' @param bins Identify bins used. Inclusive on the lower bound. The upper bin must exceed all observations, otherwise probabilities will not sum to 1.
#'
#' @return forecast.template The forecast in the format required by the CDC (also written to file).
#'
#' @export dfmip.to.cdc.challenge.format
#'
dfmip.to.cdc.challenge.format = function(model.name, forecast.distributions, out.file, n.draws,
                                         bins = c(0, 1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 101, 151, 201, 1000) ){

  # Make sure data is restricted to annual.human.cases
  forecast.distributions = forecast.distributions[forecast.distributions$forecast.target == 'annual.human.cases', ]

  # Make sure data is restricted to only the model of interest
  forecast.distributions = forecast.distributions[forecast.distributions$model.name == model.name, ]

  # Split out forecast ID
  districts = sapply(forecast.distributions$forecast.id, dfmip::splitter, ":", 3, 1)
  forecast.distributions$district = districts

  # Drop any statewide estimates
  statewide =  grep("STATEWIDE", districts)
  index = rep(1, length(districts))
  index[statewide] = 0
  forecast.distributions = forecast.distributions[index == 1, ]

  # Ensure the forecasts are numeric
  for (col in 1:ncol(forecast.distributions)){
    if (col > 6){
      if (col <= (n.draws + 6)){
        forecast.distributions[ ,col] = as.numeric(as.character(forecast.distributions[ , col]))
      }
    }
  }

  # Create a forecast dataframe to contain results
  cdc.target = "Total WNV neuroinvasive disease cases"
  cdc.forecast = data.frame(location = NA, target = cdc.target,
                            type = 'Bin', unit = 'cases', bin_start_inclusive = NA,
                            bin_end_notinclusive = NA, value = NA)

  # Loop through entries in forecast.distributions
  for (i in 1:nrow(forecast.distributions)){

    this.district = forecast.distributions$district[i]
    this.record = forecast.distributions[i, 7:(6+n.draws)]

    # For each county:
    # Add the point estimate first
    point.value = unname(apply(this.record, c(1), mean, na.rm = TRUE))

    point.forecast = data.frame(location = this.district, target = cdc.target, type = "Point",
                                unit = 'cases', bin_start_inclusive = "NA", bin_end_notinclusive = "NA",
                                value = point.value)
    cdc.forecast = rbind(cdc.forecast, point.forecast)

    # Calculate probabilities for each bin
    first.bin = bins[1]

    for (j in 2:length(bins)){
      next.bin = bins[j]

      # Get number of estimates in this bin
      index.p1 = this.record >= first.bin
      index.p2 = this.record < next.bin
      index = index.p1 & index.p2
      in.bin = this.record[ ,index == TRUE]
      bin.count = length(in.bin)

      # Divide by total number of draws to get a probability
      bin.value = bin.count / n.draws

      # Update dataframe
      bin.forecast = data.frame(location = this.district, target = cdc.target,
                                type = 'Bin', unit = 'cases', bin_start_inclusive = first.bin,
                                bin_end_notinclusive = next.bin, value = bin.value)
      cdc.forecast = rbind(cdc.forecast, bin.forecast)

      # Update lower bin
      first.bin = next.bin
    }
  }

  # Add Check that probabilities sum to 1 (Or check that all observations are less than the upper bin)

  # Drop NA initialization
  cdc.forecast = cdc.forecast[2:nrow(cdc.forecast), ]

  # Write to file
  write.table(cdc.forecast, file = out.file, sep = ',', row.names = FALSE, col.names = TRUE)

  return(cdc.forecast)
}

#' Configure analysis districts
#'
#' @noRd
#'
configure.analysis.districts = function(analysis.districts, forecast.targets, human.data, mosq.data){

  if (analysis.districts[1] == 'default'){
    # If mosquito data in forecast targets, restrict to mosquito data set
    if ('seasonal.mosquito.MLE' %in% forecast.targets){
      analysis.districts = unique(mosq.data$district)

    # Otherwise, restrict to human data set
    }else{
      analysis.districts = unique(human.data$district)
    }
  }

  # Ensure output is character, not factor
  analysis.districts = as.character(analysis.districts)
  return(analysis.districts)
}

#' Expand human data
#'
#' Takes human cases from a single row for each county and year, and adds individual rows for each county reporting cases
#' I.e. converts it into the format used by dfmip. This is unfortunate, as ArboMAP is the only model that requires the
#' detailed format, and this is not suitable for ArboMAP, as the dates are not accurate. However, it enables a common
#' workflow for all models, and that is highly desirable (i.e. the goal of the project)
#'
#' @param cases Human cases summarized by district and year
#' @param arbitrary.date The month and day to fill into the returned date field. This will be the same for all entries
#' @param case.field The field containing counts of human cases
#' @param year.field The field containing the year of the human cases
#' @param district.field The field containing the spatial extent of the human cases (e.g., county)
#'
#' @return human.data Data in the standardized dfmip input format, with a column for date, and a column for district.
#' District-years with no cases are omitted, consequently an assumption of no missing data within years is made.
#'
#' @export
expand.human.data = function(cases, arbitrary.date = "08-01", case.field = "count", year.field = 'year', district.field = 'district'){

  #**# May want to add an optional field mapping, to allow users to input their own column names

  # Create human.data object
  human.data = data.frame(date = NA, district = NA)

  years = unique(cases[[year.field]])
  districts = unique(cases[[district.field]])

  # Loop through years
  for (year in years){
    year.subset = cases[cases[[year.field]] == year, ]

    # Loop through districts
    for (district in districts){

      district.subset = year.subset[year.subset[[district.field]] == district, ]

      # Check that one or no rows are selected, otherwise throw an error
      if (nrow(district.subset) > 1){
        m1 = sprintf("More than one result found for a district and a year (%s rows).", nrow(district.subset))
        m2 = sprintf("Please inspect data for district %s and year %s", district, year)
        stop(sprintf("%s%s", m1, m2))
      }

      # If nrow == 0, no action needed, just skip that district-year
      if (nrow(district.subset) == 1){
        # Fill in human.data object
        n.cases = district.subset[[case.field]][1]
        for (i in seq_len(n.cases)){
          new.date = sprintf("%s-%s", year, arbitrary.date)
          new.record = c(new.date, district)
          human.data = rbind(human.data, new.record)
        }
      }
    }
  }

  # Remove first NA row
  human.data = human.data[2:nrow(human.data), ]

  return(human.data)
}



