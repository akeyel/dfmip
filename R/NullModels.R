# Functions to implement different Null models for CDC West Nile Virus data

# A.C. Keyel
# Created on 2021-04-13
# Incorporated into dfmip on 2021-06-21

# Covered by package DESCRIPTION and NAMESPACE
#library(MASS)
#library(testthat)

# Index of Null Models (Unit Tests: Y/N)
# apply.always.absent (N)
# apply.mean.value (N)
# apply.stratified.mean.null (N)
# apply.historical.null (N)
# apply.prior.year.null (N)
# apply.random.incidence (N)
# apply.stratified.incidence
# apply.negative.binomial (N)
# apply.ar1 (N* Yes but not automated)
# apply.uniform (Y)

#### Create an overview function to run the null models in aggregate

#' Apply Null Models
#'
#' Runs the null models as a group and create an joint output data frame and a
#' timing data frame. NOTE: the always absent and mean value were NOT calculated
#' using the apply.always.absent.null function or the apply.mean.value.null
#' functions - these were created later and have not been edited into the code.
#'
#'@param in.data A data set containing 'location' field with locations,
#'annual human cases in a 'count' field, and year in a 'year' field. Note that
#'field names must match EXACTLY. For Stratified Incidence and Random Incidence,
#'human population also needs to be included in a 'POP' field.
#'@param n.draws The number of probabilistic draws to make for each location.
#'@param nulls Which null models to run. Default of "ALL" runs all models.
#'Individual models can be run with 2-letter abbreviations in a vector:\tabular{ll}{
#' HN \tab Historical Null\cr
#' NB \tab Negative Binomial\cr
#' AA \tab Always Absent\cr
#' SI \tab Stratified Incidence\cr
#' RI \tab Random Incidence\cr
#' AR \tab Autoregressive 1\cr
#' SM \tab Stratified Mean\cr
#' PY \tab Prior Year\cr
#' UN \tab Uniform Null\cr
#' MV \tab Mean Value\cr }
#'
#'@param out.path The path to write the crps.df and time.df data objects.
#' If no output is desired, set this to NA (default).
#'@param in.seed The starting seed to ensure replicability of the random
#'processes
#'
#'@return A list with four elements: crps.df; time.df; sample.size.vec, and a list
#'of model specific results. crps df is a data
#' frame with the average CRPS score for that year for each null model, while
#' time.df contains the timing for each null model. sample.size.vec contains
#' a list of sample sizes by year, while the model.lists object contains the
#' individual county CRPS scores by year.
#'
#'@export apply.null.models
#'
apply.null.models = function(in.data, n.draws, nulls = "ALL", out.path = NA, in.seed = 20210622){

  # Check for required fields
  check.fields(in.data, nulls)

  set.seed(in.seed)
  draw.vec = sprintf("DRAW_%04.0f", seq(1,n.draws))

  years = sort(as.numeric(unique(in.data$year)))

  crps.df = data.frame(model = NA, target.year = NA, CRPS = NA)
  time.df = data.frame(model = NA, target.year = NA, start.time = NA, end.time = NA, ELAPSED = NA)

  sample.size.vec = c()
  aa.lst = list()
  mn.lst = list()
  sm.lst = list()
  hn.lst = list()
  pn.lst = list()
  ri.lst = list()
  si.lst = list()
  nb.lst = list()
  ar.lst = list()
  un.lst = list()

  #for (i in 2:length(years)){ #
  for (i in 4:length(years)){
    year = years[i]
    train.data = in.data[in.data$year < year, ]
    #year.in.data = in.data$count[in.data$year == year]
    year.in.data = in.data[in.data$year == year, c("count", "location")]

    # Remove counties from train.data with fewer than 3 years of data (this now varies by county, because we are excluding years prior to the arrival of WNV)
    test = table(train.data$location)
    test2 = test[test > 2]
    train.data = train.data[train.data$location %in% names(test2), ]
    year.in.data = year.in.data[year.in.data$location %in% names(test2), c("count")]

    # Make sure train.data field is numeric
    train.data$count = as.numeric(as.character(train.data$count))
    train.data$year = as.numeric(as.character(train.data$year))

    message(sprintf("year %s, %s counties", year, nrow(test2)))
    sample.size.vec = c(sample.size.vec, nrow(test2))

    if (nulls == "ALL" | nulls == "AA"){
      # Apply Always Absent Null
      aa.start = Sys.time()
      n.records = length(year.in.data)
      aa.data = rep(0, n.draws * n.records)
      aa.end = Sys.time()
      time.df = rbind(time.df, c("AA", year, aa.start, aa.end))
      aa.data = matrix(aa.data, ncol = n.draws)
      aa.crps = scoringRules::crps_sample(as.numeric(year.in.data), as.matrix(aa.data))
      aa.lst = append(aa.lst, list(aa.crps))
      mean.aa.crps = mean(aa.crps)
      crps.df = rbind(crps.df, c("AA", year, mean.aa.crps))
    }

    if (nulls == "ALL" | nulls == "MV"){
      # Apply Mean Value Null
      mn.start = Sys.time()
      mean.value = mean(train.data$count)
      n.records = length(year.in.data)
      mn.data = rep(mean.value, n.draws * n.records)
      mn.end = Sys.time()
      time.df = rbind(time.df, c("MV", year, mn.start, mn.end))
      mn.data = matrix(mn.data, ncol = n.draws)

      mn.crps = scoringRules::crps_sample(as.numeric(year.in.data), as.matrix(mn.data))
      mn.lst = append(mn.lst, list(mn.crps))
      mean.mn.crps = mean(mn.crps)
      crps.df = rbind(crps.df, c("MV", year, mean.mn.crps))
    }

    if (nulls == "ALL" | nulls == "SM"){
      # Apply Stratified Mean Value Null
      sm.start = Sys.time()
      sm.result = apply.stratified.mean.null(train.data, year, n.draws, draw.vec)
      sm.end = Sys.time()
      time.df = rbind(time.df, c("SM", year, sm.start, sm.end))

      sm.data = sm.result[ ,3:ncol(sm.result)]
      sm.crps = scoringRules::crps_sample(as.numeric(year.in.data), as.matrix(sm.data))
      sm.lst = append(sm.lst, list(sm.crps))
      mean.sm.crps = mean(sm.crps)
      crps.df = rbind(crps.df, c("SM", year, mean.sm.crps))
    }

    if (nulls == "ALL" | nulls == "HN"){
      # Apply historical null model
      hn.start = Sys.time()
      hn.result = apply.historical.null(train.data, year, n.draws, draw.vec)
      hn.end = Sys.time()
      time.df = rbind(time.df, c("HN", year, hn.start, hn.end))

      hn.data = hn.result[ ,3:ncol(hn.result)]
      hn.crps = scoringRules::crps_sample(as.numeric(year.in.data), as.matrix(hn.data))
      hn.lst = append(hn.lst, list(hn.crps))
      mean.hn.crps = mean(hn.crps)
      crps.df = rbind(crps.df, c("HN", year, mean.hn.crps))
    }

    if (nulls == "ALL" | nulls == "PN"){
      # Apply prior-year null model
      pn.start = Sys.time()
      pn.result = apply.prior.year.null(train.data, year, n.draws, draw.vec)
      pn.end = Sys.time()
      time.df = rbind(time.df, c("PN", year, pn.start, pn.end))

      pn.data = pn.result[ ,3:ncol(pn.result)]
      pn.crps = scoringRules::crps_sample(as.numeric(year.in.data), as.matrix(pn.data))
      pn.lst = append(pn.lst, list(pn.crps))
      mean.pn.crps = mean(pn.crps)
      crps.df = rbind(crps.df, c("PN", year, mean.pn.crps))
    }

    if (nulls == "ALL" | nulls == "RI"){
      # Apply random draws based on human population based on US-wide incidence
      ri.start = Sys.time()
      ri.result = apply.random.incidence(train.data, year, n.draws, draw.vec)
      ri.end = Sys.time()
      time.df = rbind(time.df, c("RI", year, ri.start, ri.end))

      ri.data = ri.result[ ,3:ncol(ri.result)]
      ri.crps = scoringRules::crps_sample(as.numeric(year.in.data), as.matrix(ri.data))
      ri.lst = append(ri.lst, list(ri.crps))
      mean.ri.crps = mean(ri.crps)
      crps.df = rbind(crps.df, c("RI", year, mean.ri.crps))
    }

    if (nulls == "ALL" | nulls == "SI"){
      # Apply random draws based on human population based on stratified incidence
      si.start = Sys.time()
      si.result = apply.stratified.incidence(train.data, year, n.draws, draw.vec) # target.year
      si.end = Sys.time()
      time.df = rbind(time.df, c("SI", year, si.start, si.end))

      si.data = si.result[ ,3:ncol(si.result)]
      si.crps = scoringRules::crps_sample(as.numeric(year.in.data), as.matrix(si.data))
      si.lst = append(si.lst, list(si.crps))
      mean.si.crps = mean(si.crps)
      crps.df = rbind(crps.df, c("SI", year, mean.si.crps))
    }

    if (nulls == "ALL" | nulls == "UN"){
      # Uniform Null model
      un.start = Sys.time()
      un.result = apply.uniform(train.data, year, n.draws, draw.vec)
      un.end = Sys.time()
      time.df = rbind(time.df, c("UN", year, un.start, un.end))

      un.data = un.result[ ,3:ncol(un.result)]
      un.crps = scoringRules::crps_sample(as.numeric(year.in.data), as.matrix(un.data))
      un.lst = append(un.lst, list(un.crps))
      mean.un.crps = mean(un.crps)
      crps.df = rbind(crps.df, c("UN", year, mean.un.crps))
    }

    if (nulls == "ALL" | nulls == "NB"){
      # Do not apply if less than 3 years of training data
      # Apply negative binomial model
      nb.start = Sys.time()
      nb.result = apply.negative.binomial(train.data, year, n.draws, draw.vec)
      nb.end = Sys.time()
      time.df = rbind(time.df, c("NB", year, nb.start, nb.end))

      nb.data = nb.result[ ,3:ncol(nb.result)]
      nb.crps = scoringRules::crps_sample(as.numeric(year.in.data), as.matrix(nb.data))
      nb.lst = append(nb.lst, list(nb.crps))
      mean.nb.crps = mean(nb.crps)
      crps.df = rbind(crps.df, c("NB", year, mean.nb.crps))
    }

    if (nulls == "ALL" | nulls == "AR"){
      # AR(1) Null model
      ar.start = Sys.time()
      ar.result = apply.ar1(train.data, year, n.draws, draw.vec)
      ar.end = Sys.time()
      time.df = rbind(time.df, c("AR", year, ar.start, ar.end))

      ar.data = ar.result[ ,3:ncol(ar.result)]
      ar.crps = scoringRules::crps_sample(as.numeric(year.in.data), as.matrix(ar.data))
      ar.lst = append(ar.lst, list(ar.crps))
      mean.ar.crps = mean(ar.crps)
      crps.df = rbind(crps.df, c("AR", year, mean.ar.crps))
    }

    # Write the indiviudal year's output
    if (!is.na(out.path)){
      if (nulls == "ALL" | nulls == "HN"){
        write.table(hn.result, file = sprintf("%s/historical_null/hn_%s_%s.csv", out.path, year, n.draws),
                    sep = ',', row.names = FALSE, col.names = TRUE)}
      if (nulls == "ALL" | nulls == "MV"){
        write.table(mean.value, file = sprintf("%s/constant_nulls/mv_%s_%s.csv", out.path, year, n.draws),
                    sep = ',', row.names = FALSE, col.names = FALSE)}
      if (nulls == "ALL" | nulls == "SI"){
        write.table(si.result, file = sprintf("%s/stratified_incidence/si_%s_%s.csv", out.path, year, n.draws),
                    sep = ',', row.names = FALSE, col.names = TRUE)}
      if (nulls == "ALL" | nulls == "RI"){
        write.table(ri.result, file = sprintf("%s/random_incidence/ri_%s_%s.csv", out.path, year, n.draws),
                    sep = ',', row.names = FALSE, col.names = TRUE)}
      if (nulls == "ALL" | nulls == "UN"){
        write.table(un.result, file = sprintf("%s/uniform/un_%s_%s.csv", out.path, year, n.draws),
                    sep = ',', row.names = FALSE, col.names = TRUE)}
      if (nulls == "ALL" | nulls == "NB"){
        write.table(nb.result, file = sprintf("%s/neg_binomial/nb_%s_%s.csv", out.path, year, n.draws),
                    sep = ',', row.names = FALSE, col.names = TRUE)}
      if (nulls == "ALL" | nulls == "SM"){
        write.table(sm.result, file = sprintf("%s/constant_nulls/sm_%s_%s.csv", out.path, year, n.draws),
                    sep = ',', row.names = FALSE, col.names = TRUE)}
      if (nulls == "ALL" | nulls == "PN"){
        write.table(pn.result, file = sprintf("%s/prior_null/pn_%s_%s.csv", out.path, year, n.draws),
                    sep = ',', row.names = FALSE, col.names = TRUE)}
      if (nulls == "ALL" | nulls == "AR"){
        write.table(ar.result, file = sprintf("%s/autoregressive/ar_%s_%s.csv", out.path, year, n.draws),
                    sep = ',', row.names = FALSE, col.names = TRUE)}
    } # End of if !is.na block

  } # End of loop over years

  # Remove initialization rows
  crps.df = crps.df[2:nrow(crps.df), ]
  time.df = time.df[2:nrow(time.df), ]
  crps.df$CRPS = as.numeric(crps.df$CRPS)

  time.df$TIME.DIFF = as.numeric(time.df$end.time) - as.numeric(time.df$start.time)

  if (!is.na(out.path)){
    #crps.df$CRPS = sapply(crps.df$CRPS, round, 3) # Do not round - this will result in ties, which will prevent the Wilcoxon test from calculating an exact score.
    write.table(crps.df, file = sprintf("%s/crps_df_%s.csv", out.path, n.draws),
                sep = ',', row.names = FALSE, col.names = TRUE)

    write.table(time.df, file = sprintf("%s/time_df_%s.csv", out.path, n.draws),
                sep = ',', row.names = FALSE, col.names = TRUE)
  }

  model.lists = list(AA = aa.lst, MV = mn.lst, SM = sm.lst, HN = hn.lst,
                     PN = pn.lst, RI = ri.lst, SI = si.lst,
                     NB = nb.lst, AR = ar.lst, UN = un.lst)

  return(list(crps.df, time.df, sample.size.vec, model.lists))
}


#' Check Fields
#'
#' Check that input fields are correct in the in.data object. Stop execution
#' with an informative error if not.
#'
#' @param in.data An input data set, see apply.null.models documentation for details.
#'
check.fields = function(in.data, nulls){
  err.message = ""
  loc.error = 0
  year.error = 0
  count.error = 0
  pop.error = 0
  if (length(in.data$location) == 0){
    loc.error = 1
    err.message = sprintf("%sLocation field is mispecified. Needs to be exactly 'location'\n", err.message)
    }
  if (length(in.data$year) == 0){
    year.error = 1
    err.message = sprintf("%sYear field is misspecified. Needs to be exactly 'year'\n", err.message)
    }
  if (length(in.data$count) == 0){
    count.error = 1
    err.message = sprintf("%sCount field is misspecified. Needs to be exactly 'count'\n", err.message)
    }

  if (nulls == "ALL" | nulls == "RI" | nulls == "SI"){
    if (length(in.data$POP) == 0){
      pop.error = 1
      err.message = sprintf("%sPopulation field is misspecified. Needs to be exactly 'POP'\n", err.message)
    }
  }

  if (loc.error == 1 | year.error == 1 | count.error == 1 | pop.error == 1){

    stop(sprintf("One or more input errors detected:\n%s", err.message))
  }

}


#### Define functions to calculate each null model ####

#' Always Absent Null
#'
#' Make predictions of 0 for every location. Does not really warrant a stand-alone
#' function, but done to be parallel to the other functions
#'
#' @param train.data Training data to be used to set up the always absent null.
#' Must have a location field with locations
#' @param n.draws the number of draws per location. n.draws will not change the
#' outcome, as this is deterministic, but it be used to change the structure of
#' the matrix and make the output parallel to that of the other functions.
#'
#' @return out.df A data frame containing the predictions for each location for the target year
#' @export apply.always.absent.null
#'
apply.always.absent.null = function(train.data, target.year, n.draws = 1, draw.vec = c("DRAW_0001")){

  locations = unique(train.data$location)
  n.records = length(unique(train.data$location))
  aa.data = rep(0, n.draws * n.records)
  aa.data = matrix(aa.data, ncol = n.draws)

  out.df = cbind(locations, rep(target.year, n.records), aa.data)
  colnames(out.df) = c("location", "target_year", draw.vec)
  out.df = as.data.frame(out.df)
return(out.df)
}

#' Mean Value Null
#'
#' @param train.data Training data to be used to calculate the mean values.
#' Must have a location field with locations, and a count field with number of cases.
#' @param target.year The year for the prediction. Included in the output data
#' frame to indicate the target year, but otherwise unused
#' @param n.draws The number of probabilistic draws to include in the data matrix.
#' Retained here only for consistency in formatting with the probabilistic methods.
#' @param draw.vec A vector of column names for the probabilistic draws.
#'
#' @return out.df A data frame containing the predictions for each location for the target year
#'
#' @export apply.mean.value.null
apply.mean.value.null = function(train.data, target.year, n.draws = 1, draw.vec = c("DRAW_0001")){

  mean.value = mean(train.data$count)
  locations = unique(train.data$location)
  n.records = length(locations)
  mn.data = rep(mean.value, length(locations))
  mn.data = matrix(mn.data, ncol = n.draws)

  out.df = cbind(locations, rep(target.year, n.records), mn.data)
  colnames(out.df) = c("location", "target_year", draw.vec)
  out.df = as.data.frame(out.df)

  return(out.df)
}

#' Stratified Mean Null Model
#'
#' Calculate the mean number of West Nile virus cases for each location for the
#' target year.
#'
#' @param train.data Training data to be used to calculate the mean values.
#' Must have a location field with locations, and a count field with number of cases.
#' @param target.year The year for the prediction. Included in the output data
#' frame to indicate the target year, but otherwise unused
#' @param n.draws The number of probabilistic draws to include in the data matrix.
#' Retained here only for consistency in formatting with the probabilistic methods.
#' @param draw.vec A vector of column names for the probabilistic draws.
#'
#' @return out.df A data frame containing the predictions for each location for the target year
#'
#' @export apply.stratified.mean.null
apply.stratified.mean.null = function(train.data, target.year, n.draws, draw.vec){
  locations = unique(train.data$location)
  n.years = length(unique(train.data$year))

  out.df = data.frame(location = NA, target_year = NA)
  for (i in 1:length(draw.vec)){  out.df[[draw.vec[i]]] = NA  }


  for (j in 1:length(locations)){
    location = locations[j]
    loc.subset = train.data[train.data$location == location, ]
    # Add in 0's to historical counts
    if (nrow(loc.subset) < n.years){
      zeros = n.years - nrow(loc.subset)
      mean.value = mean(c(loc.subset$count, rep(0, zeros)))
    }else{
      mean.value = mean(loc.subset$count)
    }

    distribution.samples = rep(mean.value, n.draws)

    out.record = c(location, target.year, distribution.samples)
    out.df = rbind(out.df, out.record)
  }

  for (k in 3:ncol(out.df)){
    out.df[ ,k] = as.numeric(out.df[ ,k])
  }

  # Return out.df without the initialization row
  out.df = out.df[2:nrow(out.df), ]
  return(out.df)
}

#' #**# FILL IN HERE
#'
#' @param train.data Training data to be used to calculate the mean values.
#' Must have a location field with locations, and a count field with number of cases.
#' @param target.year The year for the prediction. Included in the output data
#' frame to indicate the target year, but otherwise unused
#' @param n.draws The number of probabilistic draws to include in the data matrix.
#' Retained here only for consistency in formatting with the probabilistic methods.
#' @param draw.vec A vector of column names for the probabilistic draws.
#'
#' @return out.df A data frame containing the predictions for each location for the target year
#'
#' @export apply.historical.null
apply.historical.null = function(train.data, target.year, n.draws, draw.vec){
  locations = unique(train.data$location)
  n.years = length(unique(train.data$year))

  out.df = data.frame(location = NA, target_year = NA)
  for (i in 1:length(draw.vec)){  out.df[[draw.vec[i]]] = NA  }


  for (j in 1:length(locations)){
    location = locations[j]
    loc.subset = train.data[train.data$location == location, ]
    historical.counts = stats::aggregate(loc.subset$count, by = list(loc.subset$year), sum, na.rm = TRUE)
    historical.counts = historical.counts$x
    # Add in 0's to historical counts
    if (length(historical.counts) < n.years){
      zeros = n.years - length(historical.counts)
      historical.counts = c(historical.counts, rep(0, zeros))
    }

    # If historical counts is a single entry, the behavior of sample changes - so need to make sure it just draws from that one value, instead of from seq(1,value)
    if (length(historical.counts) == 1){
      historical.counts = rep(historical.counts, 2) # Make it two entries instead of one
    }

    #historical.counts = dplyr::count(human.data, vars = year)[ ,2] # Tibble wasn't playing well with sample
    distribution.samples = sample(historical.counts, n.draws, replace = TRUE) # SAMPLE WITH REPLACEMENT FROM THE ANNUAL HUMAN CASES #**# LEFT OFF HERE

    out.record = c(location, target.year, distribution.samples)
    out.df = rbind(out.df, out.record)
  }

  for (k in 3:ncol(out.df)){
    out.df[ ,k] = as.numeric(out.df[ ,k])
  }

  # Return out.df without the initialization row
  out.df = out.df[2:nrow(out.df), ]
  return(out.df)
}

#' Prior Year Null Model
#'
#' Null model that uses the previous year's number of cases for a
#' county as the mean for this year
#'
#' @param train.data Training data to be used to calculate the mean values.
#' Must have a location field with locations, and a count field with number of cases.
#' @param target.year The year for the prediction. Included in the output data
#' frame to indicate the target year, but otherwise unused
#' @param n.draws The number of probabilistic draws to include in the data matrix.
#' Retained here only for consistency in formatting with the probabilistic methods.
#' @param draw.vec A vector of column names for the probabilistic draws.
#'
#' @return out.df A data frame containing the predictions for each location for the target year
#'
#' @export apply.prior.year.null
apply.prior.year.null = function(train.data, target.year, n.draws, draw.vec){
  locations = unique(train.data$location)
  n.years = length(unique(train.data$year))

  out.df = data.frame(location = NA, target_year = NA)
  for (i in 1:length(draw.vec)){  out.df[[draw.vec[i]]] = NA  }

  for (j in 1:length(locations)){
    location = locations[j]
    prior.year = train.data$count[train.data$location == location & train.data$year == (target.year - 1)]
    # NOTE: strictly speaking, the draws are not doing anything useful other than putting the model predictions into the same format as the other null models
    distribution.samples = sample(rep(prior.year, 2), n.draws, replace = TRUE)
    this.record = c(location, target.year, distribution.samples)
    out.df = rbind(out.df, this.record)
  }

  for (k in 3:ncol(out.df)){
    out.df[ ,k] = as.numeric(out.df[ ,k])
  }

  # Return out.df without the initialization row
  out.df = out.df[2:nrow(out.df), ]
  return(out.df)
}

#' Uniform Random Incidence
#'
#' Uses population data for prior year
#'
#' @param train.data Training data to be used to calculate the mean values.
#' Must have a location field with locations, and a count field with number of cases.
#' @param target.year The year for the prediction. Included in the output data
#' frame to indicate the target year, but otherwise unused
#' @param n.draws The number of probabilistic draws to include in the data matrix.
#' Retained here only for consistency in formatting with the probabilistic methods.
#' @param draw.vec A vector of column names for the probabilistic draws.
#'
#' @return out.df A data frame containing the predictions for each location for the target year
#'
#' @export apply.random.incidence
apply.random.incidence = function(train.data, target.year, n.draws, draw.vec){

  # Calculate US-wide incidence
  n.years = length(unique(train.data$year))
  cases.per.year = sum(train.data$count) / n.years
  total.population = sum(train.data$POP[train.data$year == (target.year - 1)]) # Pull population from previous year
  incidence = cases.per.year / total.population

  locations = unique(train.data$location)

  out.df = data.frame(location = NA, target_year = NA)
  for (i in 1:length(draw.vec)){  out.df[[draw.vec[i]]] = NA  }

  for (j in 1:length(locations)){
    location = locations[j]
    pop = train.data$POP[train.data$location == location & train.data$year == (target.year - 1)]

    sim.vec = c()
    for (i in 1:n.draws){
      random.numbers = runif(pop)
      sim.cases = length(random.numbers[random.numbers < incidence])
      sim.vec = c(sim.vec, sim.cases)
    }

    distribution.samples = sim.vec
    this.record = c(location, target.year, distribution.samples)
    out.df = rbind(out.df, this.record)
  }

  for (k in 3:ncol(out.df)){
    out.df[ ,k] = as.numeric(out.df[ ,k])
  }

  # Return out.df without the initialization row
  out.df = out.df[2:nrow(out.df), ]
  return(out.df)
}

#' Stratified Random Incidence
#'
#' @param train.data Training data to be used to calculate the mean values.
#' Must have a location field with locations, and a count field with number of cases.
#' @param target.year The year for the prediction. Included in the output data
#' frame to indicate the target year, but otherwise unused
#' @param n.draws The number of probabilistic draws to include in the data matrix.
#' Retained here only for consistency in formatting with the probabilistic methods.
#' @param draw.vec A vector of column names for the probabilistic draws.
#'
#' @return out.df A data frame containing the predictions for each location for the target year
#'
#' @export apply.stratified.incidence
apply.stratified.incidence = function(train.data, target.year, n.draws, draw.vec){

  n.years = length(unique(train.data$year))
  locations = unique(train.data$location)

  out.df = data.frame(location = NA, target_year = NA)
  for (i in 1:length(draw.vec)){  out.df[[draw.vec[i]]] = NA  }

  for (j in 1:length(locations)){
    location = locations[j]
    loc.subset = train.data[train.data$location == location, ]

    cases.per.year = sum(loc.subset$count) / n.years
    loc.population = sum(loc.subset$POP[loc.subset$year == (target.year - 1)]) # Pull population from previous year
    incidence = cases.per.year / loc.population

    if (incidence == 0){
      distribution.samples = rep(0, n.draws)
    }else{
      sim.vec = c()
      for (i in 1:n.draws){
        random.numbers = runif(loc.population)
        sim.cases = length(random.numbers[random.numbers < incidence])
        sim.vec = c(sim.vec, sim.cases)
      }

      distribution.samples = sim.vec
    }

    this.record = c(location, target.year, distribution.samples)
    out.df = rbind(out.df, this.record)
  }

  for (k in 3:ncol(out.df)){
    out.df[ ,k] = as.numeric(out.df[ ,k])
  }

  # Return out.df without the initialization row
  out.df = out.df[2:nrow(out.df), ]
  return(out.df)
}

#' Negative Binomial
#'
#'
#' @param train.data Training data to be used to calculate the mean values.
#' Must have a location field with locations, and a count field with number of cases.
#' @param target.year The year for the prediction. Included in the output data
#' frame to indicate the target year, but otherwise unused
#' @param n.draws The number of probabilistic draws to include in the data matrix.
#' Retained here only for consistency in formatting with the probabilistic methods.
#' @param draw.vec A vector of column names for the probabilistic draws.
#'
#' @return out.df A data frame containing the predictions for each location for the target year
#'
#' @export apply.negative.binomial
#'
apply.negative.binomial = function(train.data, target.year, n.draws, draw.vec){

  n.years = length(unique(train.data$year))
  locations = unique(train.data$location)

  # vectorized alternative that maintains n.draw samples
  draw.lst <- as.list(rep(NA_real_, n.draws))
  names(draw.lst) <- draw.vec
  out.df = data.frame(location = NA, target_year = NA, draw.lst)

  for (j in 1:length(locations)){
    #for (j in 1:10){ # FOR TESTING PURPOSES
    location = locations[j]
    loc.subset = train.data[train.data$location == location, ]

    # MASS::glm.nb(formula = count ~ , data = loc.subset) # This requires a formula, I just want a negative binomial fit with an intercept

    if (max(loc.subset$count) == 0){
      distribution.samples = rep(0, n.draws) # Using 0's if no cases in a county. Alternative would be to fit the negative binomial at a higher level of stratification (e.g., state)
    }else{
        error.output = tryCatch({
          fit = suppressWarnings(MASS::fitdistr(loc.subset$count, 'negative binomial'))
          # Getting a warning: In densfun(x, parm[1], parm[2], ...) : NaNs produced
          # But I don't see any NaN values, and it produces model parameters that can be used to generate a distribution.

        },
        error=function(cond){
          message(sprintf("Negative Binomial optimization failed for %s. Doubled the input data to obtain a fit", location))
          fit = try.again(loc.subset, location)
          #fit = MASS::fitdistr(rep(loc.subset$count, 2), 'negative binomial')
          return(list("YES", fit))
        })
      # Check first if it is a list
      if (is.list(error.output)){
        if (length(error.output[[1]]) == 1){
          # Check if it was an error
          if (error.output[[1]] == "YES"){
            fit = error.output[[2]]
          }
        }
      }
      rm(error.output)

      distribution.samples = rnbinom(n.draws, size = fit$estimate[1], mu = fit$estimate[2])
    }

    this.record = c(location, target.year, distribution.samples)
    out.df = rbind(out.df, this.record)
  }

  for (k in 3:ncol(out.df)){
    out.df[ ,k] = as.numeric(out.df[ ,k])
  }

  # Return out.df without the initialization row
  out.df = out.df[2:nrow(out.df), ]
  return(out.df)
}


#' Try to fit a negative binomial, progressively modifying the data to obtain a fit
#'
#' @param loc.subset the input data set with cases in a 'count' field
#' @param location The location of the cases
#'
#' @return fit the fit object from a negative binomial model
#'
try.again = function(loc.subset, location){

  # Try just doubling the data
  is.error = 1
  try2 = tryCatch({
    fit = MASS::fitdistr(rep(loc.subset$count, 2), 'negative binomial')
    is.error = 0
  },
  error=function(cond){
    was.error = "YES"
  })

  # 1 3 5 1 1 1 1  is a sequence that can cause this to fail (also it's double)
  # If that doesn't work, try resampling the doubled data, (up to 10 times)
  counter = 0
  while(is.error == 1){
    counter = counter + 1
    if (counter == 10){
      stop(sprintf("Could not find a fit or a work around for %s. Please remove from data set and try again with the rest of your data", location))
    }

    try2 = tryCatch({
      message(sprintf("Trying sampling the cases with replacement to allow a fit for %s", location))
      fit = MASS::fitdistr(sample(rep(loc.subset$count, 2), replace = TRUE), 'negative binomial')
      is.error = 0
    },
    error=function(cond){
      was.error = "YES"
    })

  }

  return(fit)
}

#' Apply Autoregressive 1 model
#'
#' Apply an Autoregressive 1 model assuming a normal distribution
#' (with negative values reclassified to be 0)
#'
#' @param train.data Training data to be used to calculate the mean values.
#' Must have a location field with locations, and a count field with number of cases.
#' @param target.year The year for the prediction. Included in the output data
#' frame to indicate the target year, but otherwise unused
#' @param n.draws The number of probabilistic draws to include in the data matrix.
#' Retained here only for consistency in formatting with the probabilistic methods.
#' @param draw.vec A vector of column names for the probabilistic draws.
#'
#' @return out.df A data frame containing the predictions for each location for the target year
#'
#' @export apply.ar1
apply.ar1 = function(train.data, target.year, n.draws, draw.vec){

  n.years = length(unique(train.data$year))
  n.predictions = 1 # For now, just allow predictions one time step into the future.
  locations = unique(train.data$location)

  out.df = data.frame(location = NA, target_year = NA)
  for (i in 1:length(draw.vec)){  out.df[[draw.vec[i]]] = NA  }

  for (j in 1:length(locations)){
    #for (j in 1:10){ # FOR TESTING PURPOSES
    location = locations[j]
    loc.subset = train.data[train.data$location == location, ]

    # Clear any prior distribution.samples objects to avoid accidental re-use
    # Note: the error block does not affect the scope outside the error function! But the tryCatch block does. That is weird.
    # Updating based on this post: https://stackoverflow.com/questions/38482937/variable-scope-in-r-trycatch-block-is-necessary-to-change-local-variable-de/38483499
    if(exists('distribution.samples')){rm(distribution.samples)} # This will give a warning if distribution.samples exists in the global scope but not the local scope!

    if (max(loc.subset$count) == 0){
      distribution.samples = rep(0, n.draws) # Using 0's if no cases in a county. Alternative would be to fit the negative binomial at a higher level of stratification (e.g., state)
    }else{
      error.output = tryCatch({
        fit = arima(loc.subset$count, order = c(1,0,0))
        next.prediction = predict(fit, n.predictions)
        distribution.samples = rnorm(n.draws, mean = next.prediction$pred, sd = next.prediction$se)
        distribution.samples[distribution.samples < 0] = 0 # Change any negative predictions to be 0.
        # Last object will be returned to function as a default
        #return(distribution.samp)
      },
      error=function(cond){
        message(sprintf("AR model fitting failed for %s. Fit a normal distribution without an auto-regressive term (negative values reclassified to 0)", location))
        loc.mean = mean(loc.subset$count)
        loc.sd = sd(loc.subset$count)
        distribution.samples = rnorm(n.draws, mean = loc.mean, sd = loc.sd)
        distribution.samples[distribution.samples < 0] = 0 # Change any negative predictions to be 0.
        return(list("YES", distribution.samples))
      })
      # Check first if it is a list
      if (is.list(error.output)){
        if (length(error.output[[1]]) == 1){
          # Check if it was an error
          if (error.output[[1]] == "YES"){
            distribution.samples = error.output[[2]]
          }
        }
      }
      rm(error.output)
    }

    this.record = c(location, target.year, distribution.samples)
    out.df = rbind(out.df, this.record)
  }

  for (k in 3:ncol(out.df)){
    out.df[ ,k] = as.numeric(out.df[ ,k])
  }

  # Return out.df without the initialization row
  out.df = out.df[2:nrow(out.df), ]
  return(out.df)

}

#' Uniform Null Model
#'
#' Apply a uniform distribution between 0 and the maximum number of cases observed
#' stratified by county.
#'
#' @param train.data Training data to be used to generate the null predictions
#' Must have a location field with locations, and a count field with number of cases.
#' @param target.year The year for the prediction. Included in the output data
#' frame to indicate the target year, but otherwise unused
#' @param n.draws The number of probabilistic draws to include in the data matrix.
#' @param draw.vec A vector of column names for the probabilistic draws.
#'
#' @return out.df A data frame containing the forecast for each location for the target year
#'
#' @export apply.uniform
apply.uniform = function(train.data, target.year, n.draws, draw.vec){

  n.years = length(unique(train.data$year))
  locations = unique(train.data$location)

  out.df = data.frame(location = NA, target_year = NA)
  for (i in 1:length(draw.vec)){  out.df[[draw.vec[i]]] = NA  }

  for (j in 1:length(locations)){
    #for (j in 1:10){ # FOR TESTING PURPOSES
    location = locations[j]
    loc.subset = train.data[train.data$location == location, ]

    if (max(loc.subset$count) == 0){
      distribution.samples = rep(0, n.draws) # Using 0's if no cases in a county. Alternative would be to fit the negative binomial at a higher level of stratification (e.g., state)
    }else{
      max.cases = max(loc.subset$count, na.rm = TRUE)
      distribution.samples = runif(n.draws, 0, max.cases)
      #**# SHOULD CASES BE ROUNDED TO NEAREST INTEGER? THERE WILL NEVER BE FRACTIONAL CASES, but an intermediate value might score partial credit on either bound.
    }
    this.record = c(location, target.year, distribution.samples)
    out.df = rbind(out.df, this.record)
  }


  for (k in 3:ncol(out.df)){
    out.df[ ,k] = as.numeric(out.df[ ,k])
  }

  # Return out.df without the initialization row
  out.df = out.df[2:nrow(out.df), ]
  return(out.df)

}
