#' Output accuracy assessment
#'
#' This R file is designed to compile the accuracy assessments for dfmip
#' in order to provide a standardized format across forecast models
#'
#'@name Output_accuracy_assessment
NULL

# List package imports here. Wherever possible, do not reinvent the wheel
# #' @import randomForest
# #' @importFrom scoringRules crps_sample #**# Apparently crps_sample is not an exported function
#' @importFrom caret RMSE
#' @importFrom scoringRules crps_sample
NULL

#**# Moved Generate Test Data function to junk.R.

# Example needs fixing for assess.accuracy function. Was this the issue on TRAVIS? at the very least, it was filling the console!
# #' @examples
# #' mat = matrix(rnorm(1000, 8, 1), ncol = 1)
# #' obs = rnorm(1000, 8, 1)
# #' assess.accuracy(mat, obs, c("TEST"), c("Test"), c(2015), "seasonal.mosquito.MLE")


#' Assess Accuracy
#'
#' High level handler function to decide which metrics to compute and then call
#' the code to compute them. At present, only continuous and discrete outcomes are supported.
#'
#'@param predictions.mat A matrix where each row corresponds to a separate
#'  forecast, and every column corresponds to a forecast realization. In the
#'  case of point forecasts, there will be a single column. In the case of
#'  probabilistic forecasts, there will likely be a thousand or more columns.
#'  Note that these are realizations, and not the probabilities associated with
#'  categories.
#' @param observations.vec A vector where each entry corresponds to the observed outcome for the corresponding row in predictions.mat.
#' @param model.vec A vector indicating which model is being evaluated
#' @param locations.vec A vector giving the location corresponding to each entry in observations.vec
#' @param year.vec A vector giving the year corresponding toeach entry in observations.vec
#' @param forecast.target The quantity being forecast. Forecasts targets are: \itemize{
#' \item annual.human.cases
#' \item human_incidence
#' \item seasonal.mosquito.MLE
#' \item peak_mosquito_MLE
#' \item number_positive_pools
#' \item human_cases_binary
#' \item positive_pools_binary
#' \item peak_timing }
#' @param threshold For continuous and discrete forecasts, a threshold of error to be used in classifying the forecast as "accurate".
#' @param percentage For continuous and discrete forecasts, if the prediction is within the specified percentage of the observed value, the forecast is considered accurate.
#'
#' @return accuracy.metrics A list containing accuracy information. The list is
#'   structured \tabular{ll}{
#'   RMSE \tab Root Mean Squared Error\cr
#'   Scaled_RMSE \tab RMSE scaled by the mean observed value\cr
#'   within_percentage \tab A binary accuracy classification, where a forecast within a specified percentage of the observation is considered accurate, otherwise it is inaccurate.\cr
#'   within_threshold \tab A binary accuracy classification, where a forecast within a specified threshold from the observation is considered accurate, otherwise it is inaccurate\cr
#'   within_threshold_or_percentage \tab A binary accuracy classification, where if it is accurate by either the threshold or percentage approaches, it is considered accurate.\cr
#'   AUC \tab Area Under the Curve from the Receiver Operating Characteristic Plot\cr
#'   }
#'
#' @export assess.accuracy
assess.accuracy = function(predictions.mat, observations.vec, model.vec, locations.vec, year.vec,
                           forecast.target, threshold = 'default', percentage = 'default'){

  # List of potential options #**# UPDATE DOCUMENTATION WITH THESE
  #forecast.targets = c("annual.human.cases", "human_incidence", "seasonal.mosquito.MLE",
  #                     "peak_mosquito_MLE", "number_positive_pools",
  #                     "human_cases_binary", "positive_pools_binary", "peak_timing")

  # Set up vectors of forecasting targets
  binary.targets = c("human_cases_binary" , "positive_pools_binary")
  continuous.targets = c( "human_incidence", "seasonal.mosquito.MLE", "peak_mosquito_MLE")
  discrete.targets = c("annual.human.cases", "number_positive_pools")
  time.targets = c("peak_timing")
  forecast.targets = c(binary.targets, continuous.targets, discrete.targets, time.targets)

  if (!forecast.target %in% forecast.targets){
    stop(sprintf("%s is not a valid forecast target. Valid options are %s.", forecast.target, paste(forecast.targets, collapse = ', ')))
  }

  # Create a data frame to hold accuracy metric results
  accuracy.metrics = data.frame(model = NA, location = NA, forecast.target, metric = NA, year = NA, value = NA)
  #accuracy.metrics = list(CRPS = NA, RMSE = NA, Scaled_RMSE = NA, within_percentage = NA,
  #                        within_threshold = NA, within_threshold_or_percentage = NA, AUC = NA)

  # set up threshold and percentage defaults
  t.and.p = setup.t.p.defaults(forecast.target, threshold, percentage,
                               binary.targets, continuous.targets,
                               discrete.targets, time.targets)
  threshold = t.and.p[[1]]
  percentage = t.and.p[[2]]

  # If forecast target is binary
  if (forecast.target %in% binary.targets){
    # Update AUC
    try(stop("BINARY FORECASTS ARE NOT YET SUPPORTED"))

  }

  # If forecast target is continuous
  if (forecast.target %in% continuous.targets){
    # Update RMSE, Scaled RMSE, percentage, threshold, threshold & percentage, CRPS
    accuracy.metrics = try(update.continuous.targets.v2(accuracy.metrics, predictions.mat,
                                                        observations.vec, model.vec,
                                                        locations.vec, year.vec,
                                                        threshold, percentage, forecast.target))
  }

  #**# Does this require special handling, due to the discrete nature?
  # If forecast target is discrete
  if (forecast.target %in% discrete.targets){
    # Update RMSE, scaled RMSE, percentage, threshold, threshold & percentage, CRPS
    accuracy.metrics = try(update.continuous.targets.v2(accuracy.metrics, predictions.mat,
                                                        observations.vec, model.vec,
                                                        locations.vec, year.vec,
                                                        threshold, percentage, forecast.target))
  }

  # If forecast target is a time interval
  if (forecast.target %in% time.targets){
    # update threshold
    accuracy.metrics = try(update.time.targets(accuracy.metrics, predictions.mat, observations.vec, threshold))

  }

  # Remove 1st NA row
  accuracy.metrics = accuracy.metrics[2:nrow(accuracy.metrics), ]

  return(accuracy.metrics)
}

#' Set up threshold and percentage defaults
#'
#' Simple function to configure the defaults based on the type of input
#'
#'@param forecast.target The quantity being forecast. Forecasts targets are: \itemize{
#' \item annual.human.cases
#' \item human_incidence
#' \item seasonal.mosquito.MLE
#' \item peak_mosquito_MLE
#' \item number_positive_pools
#' \item human_cases_binary
#' \item positive_pools_binary
#' \item peak_timing }
#' @param threshold For continuous and discrete forecasts, a threshold of error to be used in classifying the forecast as "accurate". The default is +/- 1 human case, +/- 1 week, otherwise the default is 0.
#' @param percentage For continuous and discrete forecasts, if the prediction is wihtin the specified percentage of the observed value, the forecast is considered accurate. The default is +/- 25 percent of the observed.
#' @param binary.targets Binary forecasting targets. Currently 'human_cases_binary' and 'positive_pools_binary'.
#' @param continuous.targets Continuous forecasting targets. Currently "annual.human.cases", "human_incidence", "seasonal.mosquito.MLE", "peak_mosquito_MLE"
#' @param discrete.targets Discrete value targets, currently 'number_positive_pools'
#' @param time.targets Time related targets, currently 'peak_timing'.
#'
#' @noRd
#'
setup.t.p.defaults = function(forecast.target, threshold, percentage,
                              binary.targets, continuous.targets,
                              discrete.targets, time.targets){

  if (!forecast.target %in% c(binary.targets, continuous.targets, discrete.targets, time.targets)){
    stop("Incorrect forecast.target specified")
  }

  # Set up threshold defaults
  if (threshold == "default"){
    # If forecast target is binary
    if (forecast.target %in% binary.targets){  threshold = NA  }

    # If forecast target is continuous
    if (forecast.target %in% continuous.targets){ threshold = 0  }

    if (forecast.target %in% discrete.targets){  threshold = 1  }

    # If forecast target is a time interval (in days)
    if (forecast.target %in% time.targets){ threshold = 7  }
  }

  # Set up percentage defaults
  if (percentage == "default"){
    if (forecast.target %in% binary.targets){ percentage = NA  }

    # If forecast target is continuous
    if (forecast.target %in% continuous.targets){ percentage = 0.25  }

    if (forecast.target %in% discrete.targets){ percentage = 0.25  }

    # If forecast target is a time interval
    if (forecast.target %in% time.targets){ percentage = NA  }

  }

  return(list(threshold, percentage))
}

#' Update the continuous accuracy targets v2
#'
#' Updated to produce a data frame containing all accuracy metrics, including individual values for each year instead of summary means
#'
#' @param accuracy.metrics A list containing accuracy information. The list is
#'   structured \tabular{ll}{
#'   RMSE \tab Root Mean Squared Error\cr
#'   Scaled_RMSE \tab RMSE scaled by the mean observed value\cr
#'   within_percentage \tab A binary accuracy classification, where a forecast within a specified percentage of the observation is considered accurate, otherwise it is inaccurate.\cr
#'   within_threshold \tab A binary accuracy classification, where a forecast within a specified threshold from the observation is considered accurate, otherwise it is inaccurate\cr
#'   within_threshold_or_percentage \tab A binary accuracy classification, where if it is accurate by either the threshold or percentage approaches, it is considered accurate.\cr
#'   AUC \tab Area Under the Curve from the Receiver Operating Characteristic Plot\cr
#'   }
#' @param predictions.mat A matrix where each row corresponds to a separate
#'  forecast, and every column corresponds to a forecast realization. In the
#'  case of point forecasts, there will be a single column. In the case of
#'  probabilistic forecasts, there will likely be a thousand or more columns.
#'  Note that these are realizations, and not the probabilities associated with
#'  categories.
#' @param observations.vec A vector where each entry corresponds to the observed outcome for the corresponding row in predictions.mat.
#' @param model.vec A vector indicating which model is being evaluated
#' @param locations.vec A vector giving the location corresponding to each entry in observations.vec
#' @param year.vec A vector giving the year corresponding toeach entry in observations.vec
#' @param threshold For continuous and discrete forecasts, a threshold of error to be used in classifying the forecast as "accurate". The default is +/- 1 human case, +/- 1 week, otherwise the default is 0.
#' @param percentage For continuous and discrete forecasts, if the prediction is within the specified percentage of the observed value, the forecast is considered accurate. The default is +/- 25 percent of the observed.
#'
#' @return accuracy.metrics An updated accuracy.metrics object
#'
#' @noRd
#'
update.continuous.targets.v2 = function(accuracy.metrics, predictions.mat, observations.vec,
                                        model.vec, locations.vec, year.vec, threshold, percentage,
                                        forecast.target){

  #**# No formal check that predictions.mat and observations.vec have the same number of entries
  # However, they both come from the same data frame in the evaluate.accuracy function

  # Loop through each entry in predictions.mat and observation.vec
  for (i in seq_len(length(observations.vec))){
    obs = observations.vec[i]
    dat = predictions.mat[i , ]
    loc = locations.vec[i]
    year = year.vec[i]
    model = model.vec[i]

    # Update RMSE
    #message(sprintf("Obs: %s, Data: %s", obs, paste(dat, collapse = ", ")))
    this.RMSE = caret::RMSE(dat, obs)
    accuracy.metrics = rbind(accuracy.metrics, c(model, loc,  forecast.target, "RMSE", year, this.RMSE))
    #message("Got Here")

    # Update scaled RMSE
    scaled.RMSE = this.RMSE / mean(observations.vec, na.rm = TRUE)
    accuracy.metrics = rbind(accuracy.metrics, c(model, loc, forecast.target, "Scaled_RMSE", year, scaled.RMSE))

    # Update percentage score
    percent.error = dat / obs
    correct.index = percent.error < (percentage + 1) & percent.error > (1 - percentage)
    correct.vec = rep(0, length(percent.error))
    correct.vec[correct.index] = 1
    accuracy.score = sum(correct.vec, na.rm = TRUE) / length(correct.vec)
    accuracy.metrics = rbind(accuracy.metrics, c(model, loc, forecast.target, "within_percentage", year, accuracy.score))

    # Update threshold score
    threshold.errors = abs(dat - obs)
    t.correct.index = threshold.errors <= threshold
    t.correct.vec = rep(0, length(threshold.errors))
    t.correct.vec[t.correct.index] = 1
    t.accuracy.score = sum(t.correct.vec, na.rm = TRUE) / length(t.correct.vec)
    accuracy.metrics = rbind(accuracy.metrics, c(model, loc, forecast.target, "within_threshold", year, t.accuracy.score))

    # Update threshold & percentage scores
    t.p.correct.index = t.correct.index | correct.index
    t.p.accuracy.score = sum(t.p.correct.index) / length(t.p.correct.index) #**# This should be more concise - TRUE = 1, FALSE = 0.
    accuracy.metrics = rbind(accuracy.metrics, c(model, loc, forecast.target, "within_threshold_or_percentage", year, t.p.accuracy.score))

    # Update CRPS
    crps = crps_sample(obs, dat)
    accuracy.metrics = rbind(accuracy.metrics, c(model, loc, forecast.target, "CRPS", year, crps))
    #message("Also here")
  }

  return(accuracy.metrics)
}


#' Update the continuous accuracy targets
#'
#' OLD VERSION, NO LONGER IN USE
#'
#' @param accuracy.metrics A list containing accuracy information. The list is
#'   structured \tabular{ll}{
#'   RMSE \tab Root Mean Squared Error\cr
#'   Scaled_RMSE \tab RMSE scaled by the mean observed value\cr
#'   within_percentage \tab A binary accuracy classification, where a forecast within a specified percentage of the observation is considered accurate, otherwise it is inaccurate.\cr
#'   within_threshold \tab A binary accuracy classification, where a forecast within a specified threshold from the observation is considered accurate, otherwise it is inaccurate\cr
#'   within_threshold_or_percentage \tab A binary accuracy classification, where if it is accurate by either the threshold or percentage approaches, it is considered accurate.\cr
#'   AUC \tab Area Under the Curve from the Receiver Operating Characteristic Plot\cr
#'   }
#' @param predictions.mat A matrix where each row corresponds to a separate
#'  forecast, and every column corresponds to a forecast realization. In the
#'  case of point forecasts, there will be a single column. In the case of
#'  probabilistic forecasts, there will likely be a thousand or more columns.
#'  Note that these are realizations, and not the probabilities associated with
#'  categories.
#' @param observations.vec A vector where each entry corresponds to the observed outcome for the corresponding row in predictions.mat.
#' @param threshold For continuous and discrete forecasts, a threshold of error to be used in classifying the forecast as "accurate". The default is +/- 1 human case, +/- 1 week, otherwise the default is 0.
#' @param percentage For continuous and discrete forecasts, if the prediction is within the specified percentage of the observed value, the forecast is considered accurate. The default is +/- 25 percent of the observed.
#'
#' @return accuracy.metrics An updated accuracy.metrics object
#'
#' @noRd
#'
update.continuous.targets = function(accuracy.metrics, predictions.mat, observations.vec, threshold, percentage){
  #**# SCRIPT PIECES

  predictions.vec = apply(predictions.mat, 1, mean, na.rm = TRUE)

  #results.df = data.frame(PREDICTION = predictions.vec, OBSERVATION = observations.vec)

  # Update RMSE
  #RMSE.vec = RMSE(predictions.vec, observations.vec, na.rm = TRUE)
  #results.df$RMSE = RMSE.vec
  accuracy.metrics$RMSE = caret::RMSE(predictions.vec, observations.vec, na.rm = TRUE)

  # Update Scaled RMSE (divided by mean - goal is to put the error in the context of the potential for variation)
  accuracy.metrics$Scaled_RMSE = accuracy.metrics$RMSE / mean(observations.vec, na.rm = TRUE)

  # Update percentage score
  percent.errors = predictions.vec / observations.vec
  correct.index = percent.errors < (percentage + 1) & percent.errors > (1 - percentage)
  correct.vec = rep(0, length(percent.errors))
  correct.vec[correct.index] = 1
  accuracy.score = sum(correct.vec, na.rm = TRUE) / length(correct.vec)
  accuracy.metrics$within_percentage = accuracy.score

  # Update threshold score
  threshold.errors = abs(predictions.vec - observations.vec)
  t.correct.index = threshold.errors <= threshold
  t.correct.vec = rep(0, length(threshold.errors))
  t.correct.vec[t.correct.index] = 1
  t.accuracy.score = sum(t.correct.vec, na.rm = TRUE) / length(t.correct.vec)
  accuracy.metrics$within_threshold = t.accuracy.score

  # Update threshold & percentage scores
  t.p.correct.index = t.correct.index | correct.index
  t.p.accuracy.score = sum(t.p.correct.index) / length(t.p.correct.index) #**# This should be more concise - TRUE = 1, FALSE = 0.
  accuracy.metrics$within_threshold_or_percentage = t.p.accuracy.score

  # Update CRPS
  crps.vec = c()
  for (i in seq_len(length(observations.vec))){
    obs = observations.vec[i]
    dat = predictions.mat[i , ]
    crps = crps_sample(obs, dat)
    crps.vec = c(crps.vec, crps)
  }
  accuracy.metrics$CRPS = mean(crps.vec, na.rm = TRUE)

  return(accuracy.metrics)
  #return(list(accuracy.metrics, results.df))
}

#' Update the continuous accuracy targets
#'
#' @param accuracy.metrics A list containing accuracy information. The list is
#'   structured \tabular{ll}{
#'   RMSE \tab Root Mean Squared Error\cr
#'   Scaled_RMSE \tab RMSE scaled by the mean observed value\cr
#'   within_percentage \tab A binary accuracy classification, where a forecast within a specified percentage of the observation is considered accurate, otherwise it is inaccurate.\cr
#'   within_threshold \tab A binary accuracy classification, where a forecast within a specified threshold from the observation is considered accurate, otherwise it is inaccurate\cr
#'   within_threshold_or_percentage \tab A binary accuracy classification, where if it is accurate by either the threshold or percentage approaches, it is considered accurate.\cr
#'   AUC \tab Area Under the Curve from the Receiver Operating Characteristic Plot\cr
#'   }
#' @param predictions.mat A matrix where each row corresponds to a separate
#'  forecast, and every column corresponds to a forecast realization. In the
#'  case of point forecasts, there will be a single column. In the case of
#'  probabilistic forecasts, there will likely be a thousand or more columns.
#'  Note that these are realizations, and not the probabilities associated with
#'  categories.
#' @param observations.vec A vector where each entry corresponds to the observed outcome for the corresponding row in predictions.mat.
#' @param threshold For continuous and discrete forecasts, a threshold of error to be used in classifying the forecast as "accurate". The default is +/- 1 human case, +/- 1 week, otherwise the default is 0.
#'
#' @return accuracy.metrics An updated accuracy.metrics object
#'
#' @noRd
#'
update.time.targets = function(accuracy.metrics, predictions.mat, observations.vec, threshold){
  #**# SCRIPT PIECES

  predictions.vec = apply(predictions.mat, 1, mean, na.rm = TRUE)

  # Update RMSE
  accuracy.metrics$RMSE = caret::RMSE(predictions.vec, observations.vec, na.rm = TRUE)

  # Update threshold score
  threshold.errors = abs(predictions.vec - observations.vec)
  t.correct.index = threshold.errors <= threshold
  t.correct.vec = rep(0, length(threshold.errors))
  t.correct.vec[t.correct.index] = 1
  t.accuracy.score = sum(t.correct.vec, na.rm = TRUE) / length(t.correct.vec)
  accuracy.metrics$within_threshold = t.accuracy.score

  # Update CRPS
  crps.vec = c()
  for (i in seq_len(length(observations.vec))){
    obs = observations.vec[i]
    dat = predictions.mat[i , ]
    crps = crps_sample(obs, dat)
    crps.vec = c(crps.vec, crps)
  }
  accuracy.metrics$CRPS = mean(crps.vec, na.rm = TRUE)

  return(accuracy.metrics)
}


