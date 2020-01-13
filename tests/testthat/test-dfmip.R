# When set to 0, the tests that take several minutes to run are skipped
#0.8 s with Test_All = 0
#796.1 s with Test_All = 1 #**# FILL IN TIME ESTIMATE
Test_All = 0


test_that("forecasting model names are correct", {
  expect_error(check.models("NULL", "NULL.MODELS"))
  expect_equal(check.models("NULL.MODELS", "NULL.MODELS"), NULL)
  expect_equal(check.models('NULL.MODELS', c('NULL.MODELS', 'RF1_C')), NULL)
})


# Smaller test to see if I can get my code to work
test_that("My understanding of unit tests is correct", {

  hide.warnings.test = function(){
    b = 10
    warning("I don't want to see this")
    return(b)
  }

  #setup(a <- 5)
  #setup(b <- 6)
  a = 5
  expect_equal(a, 5)
  #expect_equal(a, 6) # Gives an error, as expected

  b = suppressWarnings(hide.warnings.test())
  expect_equal(b, 10)
  #teardown(rm(a))
  #expect_equal(exists('a'), FALSE)
  # Note: a is NOT in the workspace after I run test_that. So it looks like it IS doing it automatically, and I don't have to bother
  # with setup and teardown commands. I just plug in the code, and it looks like it handles it just fine.
})


test_that("Date handling functions perform as expected", {
  # Test get.DOY function
  expect_equal(get.DOY(2001, 1, 5), 5)
  expect_equal(get.DOY(2001, 2, 1), 32)
  expect_equal(get.DOY(2001,12,31), 365)
  expect_equal(get.DOY(2000, 3, 1), 61) # 2000 was a leap year, even though it was a new century
  expect_equal(get.DOY(2001, 3, 1), 60)
  expect_equal(get.DOY(2004, 3, 1), 61)

  # get.DOY may not throw an error if an improper date is given it
  expect_equal(get.DOY(2001,12,35), 369)
  expect_equal(get.DOY(2001,12,-1), 333)

  # Test get.days function
  expect_equal(get.days(2000), 366)
  expect_equal(get.days(1999), 365)
  expect_equal(get.days(2001), 365)
  expect_equal(get.days(2004), 366)
  expect_error(get.days(1900), "Not designed years <= 1900 or >= 2100")

  # Test get.start.week
  expect_equal(get.start.week(2021), 3) # In 2021, First sunday is Jan 3
  expect_equal(get.start.week(2020), 5) # In 2020, First sunday is Jan 5
  expect_equal(get.start.week(2019), 6) # In 2019, First sunday is Jan 6
  expect_equal(get.start.week(2018), 7) # In 2018, First sunday is Jan 7
  expect_equal(get.start.week(2017), 1) # in 2017, First sunday is Jan 1
  expect_equal(get.start.week(2016), 3) # in 2016, First sunday is Jan 3
  expect_error(get.start.week(1900), "get.start.week will not work for years before 1901 or after 2099")

  # Test date.subset function with two date formats
  date.df = data.frame(date = c("2019-01-01", "2020-04-25", "2016-12-31", "2021-12-24"))
  expect_equal(nrow(date.subset(date.df, 2020, 4, 24, 1)), 2) # Should be 2 rows remaining
  expect_equal(nrow(date.subset(date.df, 2020, 4, 25, 1)), 3) # Should be 3, because 4/25/2020 is now included
  expect_equal(nrow(date.subset(date.df, 2020, 4, 26, 1)), 3)
  expect_equal(nrow(date.subset(date.df, 2016, 12, 31, 1)), 1)
  expect_equal(nrow(date.subset(date.df, 2016, 12, 30, 1)), 0)
  expect_equal(nrow(date.subset(date.df, 2021, 12, 25, 1)), 4)

  date.df = data.frame(date = c("12/25/2016", "05/01/2019", "4/25/2020", "3/23/2021"))
  expect_equal(nrow(date.subset(date.df, 2020, 4, 24, 2)), 2)
  expect_equal(nrow(date.subset(date.df, 2020, 4, 25, 2)), 3)
  expect_equal(nrow(date.subset(date.df, 2022, 1, 1, 2)), 4)
  expect_equal(nrow(date.subset(date.df, 2015, 1, 1, 2)), 0)

  # Test get_sampling_weeks function
  out = get_sampling_weeks(2015, 7, 2, sample_frequency = 1)
  expect_equal(out[[1]], c(7,7))
  expect_equal(out[[2]], c(5,12))
  expect_equal(out[[3]], c(2015, 2015))
  out = get_sampling_weeks(2015, 7, 5, sample_frequency = 1)
  expect_equal(out[[1]], c(7,7,7,7,8))
  expect_equal(out[[2]], c(5,12,19,26, 2))
  expect_error(get_sampling_weeks(2015,7,5, sample_frequency = 2),"Currently only weekly sampling is scripted. Sorry!") # sample_frequency is not yet supported by the code

  # Test create.month.day.vecs function
  expect_equal(create.month.day.vecs(2000)[[1]][1], 1)
  expect_equal(create.month.day.vecs(2000)[[2]][1], 2)
  expect_equal(create.month.day.vecs(2000)[[1]][45], 11)
  expect_equal(create.month.day.vecs(2000)[[2]][45], 5)
  expect_equal(create.month.day.vecs(2020)[[1]][1], 1)
  expect_equal(create.month.day.vecs(2020)[[2]][1], 5)
  expect_equal(create.month.day.vecs(2020)[[1]][45], 11)
  expect_equal(create.month.day.vecs(2020)[[2]][45], 8)
  expect_equal(create.month.day.vecs(2019)[[2]][1], 6)
  expect_equal(create.month.day.vecs(2019)[[2]][45], 10)

})


test_that("Update functions work properly", {

  ## Test update.df function
  # Construct results object
  forecasts.df = NA
  results.object = list()
  results.object$model.name = "TEST"
  results.object$annual.human.cases = 5
  results.object$forecast.id = "TEST:2000-05-31"
  forecasts.df = update.df(c("annual.human.cases"), forecasts.df, results.object)
  expect_equal(forecasts.df$MODEL.NAME, "TEST")
  expect_equal(forecasts.df$annual.human.cases, 5)

  # Update an existing forecast.df object
  results.object = list()
  results.object$model.name = "TEST2"
  results.object$annual.human.cases = 3
  results.object$forecast.id = "TEST2:2001-07-22"
  forecasts.df = suppressWarnings(update.df(c("annual.human.cases"), forecasts.df, results.object)) #**# Gives a warning, it would be desirable to eliminate the warning
  expect_equal(nrow(forecasts.df), 2)
  expect_equal(forecasts.df$annual.human.cases[2], 3) # This fails after update. The update changes the format to character apparently.

  ## Test update.distributions function
  # Test single-value distribution with no previus distribution
  forecast.distributions = NA
  results.object$annual.human.cases = 5
  forecast.distributions = update.distribution(c("annual.human.cases"), "TEST", "TEST:2000-05-31", forecast.distributions, results.object)
  expect_equal(forecast.distributions$annual.human.cases[['TEST:TEST:2000-05-31']], 5)

  # Update with a proper sequence of distribution values
  set.seed(20200103)
  results.object$annual.human.cases = rnorm(1000, 0, 1)
  forecast.distributions = update.distribution(c("annual.human.cases"), "NEXTTEST", "TEST2:2001-07-22", forecast.distributions, results.object)
  expect_equal(forecast.distributions$annual.human.cases[['TEST:TEST:2000-05-31']], 5) # Expect first entry to be unchanged
  set.seed(20200103)
  a = rnorm(1000, 0, 1)
  expect_equal(forecast.distributions$annual.human.cases[["NEXTTEST:TEST2:2001-07-22"]], a)


})

test_that("assorted small functions all work as expected", {

  # Test splitter function
  expect_equal(splitter("5:15", ":", 2), 15)
  expect_equal(splitter("5:15", ":", 1), 5)
  expect_equal(splitter("1_3", "_", 1), 1)
  expect_equal(splitter("FIVE:2", ":", 1, 1), "FIVE")
  expect_equal(unname(vapply(c("1:2", "2:3", "4:5"), splitter, FUN.VALUE = numeric(1), ":", 1)), c(1,2,4))
  # sapply added the character vector as names to the output. The unname function removes those.
  # Incorporating the unname call into the function did not fix this, as sapply added the names, not splitter.
  # Unclear if vapply had the same behavior, I did not test it when I changed the code.

  # Test check.models function
  expect_equal(check.models(c("MY.MODEL"), c("MY.MODEL")), NULL)
  expect_error(check.models(c("MY.MODEL"), c("NOT.MY.MODEL")))

  # Test check for dependencies
  if (!requireNamespace('rf1')){
    skip('rf1 package must be installed to test check.dependencies function. You can do this with devtools::install_github("akeyel/rf1")')
  }
  expect_equal(check.dependencies('RF1', c('randomForest', 'psych')), NULL) # Should be no output if everything met. randomForest and psych must be installed on the machine in order to pass this test!
  expect_error(suppressWarning(check.dependencies('RF1', c("SomeMadeUpPackageThatDoesNotExist")))) # It will give a warning that the package does not exist, but we do not want to clutter things up.

  expect_equal(check.models.and.targets(c("RF1_C", "RF1_A", "NULL.MODELS"), c('annual.human.cases', 'seasonal.mosquito.MLE')), NULL)
  expect_message(check.models.and.targets(c("ArboMAP"), c('annual.human.cases', 'seasonal.mosquito.MLE')), "^seasonal.mosquito.MLE not supported for ArboMAP. Estimates will not be produced for this model")

})

# Should be in rf1 package tests, but done here because then only one copy of the example data is needed.
test_that("mosquito MLE estimates are calculated correctly",{

  # Check that rf1 is installed
  if(!requireNamespace('rf1')){
    skip('rf1 package must be installed to test MLE calculations. You can do this with devtools::install_github("akeyel/rf1")')
  }

  # Load example data to run the models (back out two directories to get into main package directory)
  load("dfmip_example_inputs.RData")
  #load("../../vignettes/dfmip_example_inputs.RData")
  estimate = rf1::calculate.MLE.v2(mosq.data)
  expect_equal(names(estimate), c("GROUP", "CI.lower", "CI.upper", "IR", "COUNTY", "abundance", "density", "YEAR", "county_year"))
  expect_equal(round(estimate$CI.lower[1],5), 0.00059)
  expect_equal(round(estimate$CI.upper[1],5), 0.01121)
  expect_equal(round(estimate$IR[1],5), 0.00364)
  expect_equal(nrow(estimate), 160)

  expect_equal(round(estimate$IR[160], 4), 0.0014)

  # Check that an informative error is given for a missing input
  districts = mosq.data$district # Allows recovery of NULL field if further testing is required
  mosq.data$district = NULL
  expect_error(rf1::calculate.MLE.v2(mosq.data))

})

test_that('the example data can be loaded properly', {
  # Load example data to run the models (back out two directories to get into main package directory)
  #stop(getwd())
  load("dfmip_example_inputs.RData")
  expect_equal(1,1)


})


#**# SKIP THIS ON CRAN - THIS WILL TAKE A WHILE TO RUN
test_that("ArboMAP model produces the expected outputs", {
  if (Test_All == 0 | Test_All == 2){
    skip_on_os('windows') #"Skipped testing ArboMAP model to save time") #**# Enable when testing code other than the main functions
  }

  # Load example data to run the models (back out two directories to get into main package directory)
  load("dfmip_example_inputs.RData")
  #load("../../vignettes/dfmip_example_inputs.RData")

  # Create a temporary results path
  results.path = "DFMIPTESTRESULTS/"
  dir.create(results.path)

  # Test ArboMAP Model for human cases
  dfmip.outputs = suppressWarnings(dfmip.forecast(c("annual.human.cases"), c("ArboMAP"), human.data, mosq.data, weather.data,
                                 districtshapefile, weekinquestion, week.id, results.path,
                                 arbo.inputs = arbo.inputs, observed.inputs = NA,
                                 population.df = NA, rf1.inputs = NA))

  forecasts.df = dfmip.outputs[[1]]
  forecast.distributions = dfmip.outputs[[2]]
  other.results = dfmip.outputs[[3]]

  # Test forecasts.df object
  expect_equal(round(forecasts.df$annual.human.cases, 0), 117)
  # Test distributions object
  expect_equal(round(forecast.distributions$annual.human.cases[[1]], 0), 117)
  #expect_equal(other.results, NULL) #**# Do not currently care about this output.

  #skip('Do not do hind casts until forecasts work')
  # Test ArboMAP hindcasts for human cases
  hindcasts = suppressWarnings(dfmip.hindcasts(c("annual.human.cases"), c("ArboMAP"), c(2015), human.data, mosq.data,
                              weather.data, districtshapefile, results.path, arbo.inputs = arbo.inputs,
                              population.df = NA, rf1.inputs = NA,
                              threshold = 1, percentage = 0.25, id.string = "test",
                              season_start_month = 7, weeks_in_season = 2))

  accuracy = hindcasts[[1]]
  forecasts.df = hindcasts[[2]]
  forecast.distributions = hindcasts[[3]]

  expect_equal(as.character(accuracy$model), "ArboMAP")
  expect_equal(accuracy$forecast.target, "annual.human.cases")
  expect_equal(accuracy$CRPS, 18.2)
  expect_equal(round(accuracy$RMSE, 1), 18.2) #**# Why is RMSE the same as CRPS? CRPS should be absolute error, not RMSE. Is only one data point being evaluated?
  expect_equal(round(accuracy$Scaled_RMSE, 3), 0.285)
  expect_equal(accuracy$within_percentage, 0)
  expect_equal(accuracy$within_threshold, 0)
  expect_equal(accuracy$within_threshold_or_percentage, 0)
  expect_equal(accuracy$AUC, NA)

  expect_equal(forecasts.df$MODEL.NAME[1], "ArboMAP")
  expect_equal(forecasts.df$FORECAST.ID[2], "test:2015-07-12")
  expect_equal(forecasts.df$UNIT[1], 'test')
  expect_equal(forecasts.df$DATE[1], '2015-07-05')
  expect_equal(forecasts.df$YEAR[1], 2015)
  expect_equal(forecasts.df$annual.human.cases[1], 83)
  expect_equal(forecasts.df$annual.human.cases[2], 81.4)

  expect_equal(forecast.distributions[['annual.human.cases']][[1]], 83)
  expect_equal(forecast.distributions[['annual.human.cases']][[2]], 81.4)

  # Remove files written to results path #**# Why isn't this removing the results.path directory at the end?
  # Maybe wrong working directory - need to look into this.
  unlink(results.path, recursive = TRUE)
})

# Test NULL model
test_that("NULL model produces the expected outputs", {
  if (Test_All == 0 | Test_All == 2){
    #skip_on_os('windows')
    skip("Skipped NULL model tests to save time") #**# Enable when testing code other than the main functions
  }

  # Load example data to run the models (back out two directories to get into main package directory)
  load("dfmip_example_inputs.RData")
  #load("../../vignettes/dfmip_example_inputs.RData")

  # Create a temporary results path
  results.path = "DFMIPTESTRESULTS/"
  dir.create(results.path)

  # Test Null Model for human cases
  dfmip.outputs = suppressWarnings(dfmip.forecast(c("annual.human.cases"), c("NULL.MODELS"), human.data, mosq.data, weather.data,
                                                  districtshapefile, weekinquestion, week.id, results.path,
                                                  arbo.inputs = arbo.inputs, observed.inputs = NA,
                                                  population.df = NA, rf1.inputs = NA))

  forecasts.df = dfmip.outputs[[1]]
  forecast.distributions = dfmip.outputs[[2]]
  other.results = dfmip.outputs[[3]]

  # Test forecasts.df object
  expect_equal(round(forecasts.df$annual.human.cases, 0), 95)
  # Test distributions object
  expect_equal(round(forecast.distributions$annual.human.cases[[1]], 0), 95)
  #expect_equal(other.results, NULL) #**# Do not currently care about this output.

  #skip('Do not do hind casts until forecasts work')
  # Test ArboMAP hindcasts for human cases
  hindcasts = suppressWarnings(dfmip.hindcasts(c("annual.human.cases"), c("NULL.MODELS"), c(2015), human.data, mosq.data,
                                               weather.data, districtshapefile, results.path, arbo.inputs = arbo.inputs,
                                               population.df = NA, rf1.inputs = NA,
                                               threshold = 1, percentage = 0.25, id.string = "test",
                                               season_start_month = 7, weeks_in_season = 2))

  accuracy = hindcasts[[1]]
  forecasts.df = hindcasts[[2]]
  forecast.distributions = hindcasts[[3]]

  expect_equal(as.character(accuracy$model), "NULL.MODELS")
  expect_equal(accuracy$forecast.target, "annual.human.cases")
  expect_equal(round(accuracy$CRPS,1), 24.7)
  expect_equal(round(accuracy$RMSE, 1), 24.7) #**# Why is RMSE the same as CRPS? CRPS should be absolute error, not RMSE. Is only one data point being evaluated?
  expect_equal(round(accuracy$Scaled_RMSE, 3), 0.386)
  expect_equal(accuracy$within_percentage, 0)
  expect_equal(accuracy$within_threshold, 0)
  expect_equal(accuracy$within_threshold_or_percentage, 0)
  expect_equal(accuracy$AUC, NA)

  expect_equal(forecasts.df$MODEL.NAME[1], "NULL.MODELS")
  #expect_equal(forecasts.df$FORECAST.ID[2], "test:2015-07-12") #**# This produces a weird result. It gives NA, but is not equal to NA.
  expect_equal(nrow(forecasts.df), 1) # Replace test above to set a clear expectation that only one row will be produced (as it only creates forecasts for the first week, as all subsequent weeks will be the same. #**# We can consider having the behavior produce an estimate for each week, even though they are all equal)
  expect_equal(forecasts.df$UNIT[1], 'test')
  expect_equal(forecasts.df$DATE[1], '2015-07-05')
  expect_equal(forecasts.df$YEAR[1], 2015)
  expect_equal(round(forecasts.df$annual.human.cases[1], 1), 88.7)
  #expect_equal(forecasts.df$annual.human.cases[2], NULL) #**# Same issue with FORECAST.ID[2] above. It's a weird NA that isn't an NA.

  expect_equal(round(forecast.distributions[['annual.human.cases']][[1]], 1), 88.7)
  expect_error(forecast.distributions[['annual.human.cases']][[2]], "subscript out of bounds")

  unlink(results.path, recursive = TRUE)
})

# Test NULL model
test_that("NULL model produces the expected outputs for mosquitoes", {
  if (Test_All == 0 | Test_All == 2){
    #skip_on_os('windows')
    skip("Skipped NULL model mosquito tests to save time") #**# Enable when testing code other than the main functions
  }

  # Load example data to run the models (back out two directories to get into main package directory)
  load("dfmip_example_inputs.RData")
  #load("../../vignettes/dfmip_example_inputs.RData")

  # Create a temporary results path
  results.path = "DFMIPTESTRESULTS/"
  dir.create(results.path)

  # Test Null Model for human cases
  dfmip.outputs = suppressWarnings(dfmip.forecast(c("seasonal.mosquito.MLE"), c("NULL.MODELS"), human.data, mosq.data, weather.data,
                                                  districtshapefile, weekinquestion, week.id, results.path,
                                                  arbo.inputs = arbo.inputs, observed.inputs = NA,
                                                  population.df = NA, rf1.inputs = NA))

  forecasts.df = dfmip.outputs[[1]]
  forecast.distributions = dfmip.outputs[[2]]
  other.results = dfmip.outputs[[3]]

  # Test forecasts.df object
  expect_equal(round(forecasts.df$seasonal.mosquito.MLE, 4), 0.0015)
  # Test distributions object
  expect_equal(round(forecast.distributions$seasonal.mosquito.MLE[[1]], 4), 0.0015)
  #expect_equal(other.results, NULL) #**# Do not currently care about this output.

  #skip('Do not do hind casts until forecasts work')
  # Test ArboMAP hindcasts for human cases
  hindcasts = suppressWarnings(dfmip.hindcasts(c("seasonal.mosquito.MLE"), c("NULL.MODELS"), c(2015), human.data, mosq.data,
                                               weather.data, districtshapefile, results.path, arbo.inputs = arbo.inputs,
                                               population.df = NA, rf1.inputs = NA,
                                               threshold = 1, percentage = 0.25, id.string = "test",
                                               season_start_month = 7, weeks_in_season = 1))

  accuracy = hindcasts[[1]]
  forecasts.df = hindcasts[[2]]
  forecast.distributions = hindcasts[[3]]

  expect_equal(as.character(accuracy$model), "NULL.MODELS")
  expect_equal(accuracy$forecast.target, "seasonal.mosquito.MLE")
  expect_equal(round(accuracy$CRPS,5), 0.00013)
  expect_equal(round(accuracy$RMSE, 5), 0.00013) #**# Why is RMSE the same as CRPS? CRPS should be absolute error, not RMSE. Is only one data point being evaluated?
  expect_equal(round(accuracy$Scaled_RMSE, 3), 0.097)
  expect_equal(accuracy$within_percentage, 1)
  expect_equal(accuracy$within_threshold, 0)
  expect_equal(accuracy$within_threshold_or_percentage, 1)
  expect_equal(accuracy$AUC, NA)

  expect_equal(forecasts.df$MODEL.NAME[1], "NULL.MODELS")
  #expect_equal(forecasts.df$FORECAST.ID[2], "test:2015-07-12") #**# This produces a weird result. It gives NA, but is not equal to NA.
  expect_equal(nrow(forecasts.df), 1) # Replace test above to set a clear expectation that only one row will be produced (as it only creates forecasts for the first week, as all subsequent weeks will be the same. #**# We can consider having the behavior produce an estimate for each week, even though they are all equal)
  expect_equal(forecasts.df$UNIT[1], 'test')
  expect_equal(forecasts.df$DATE[1], '2015-07-05')
  expect_equal(forecasts.df$YEAR[1], 2015)
  expect_equal(round(forecasts.df$seasonal.mosquito.MLE[1], 4), 0.0015)

  expect_equal(round(forecast.distributions[['seasonal.mosquito.MLE']][[1]], 4), 0.0015)
  expect_error(forecast.distributions[['seasonal.mosquito.MLE']][[2]], "subscript out of bounds")

  unlink(results.path, recursive = TRUE)
})


test_that("RF1 model produces the expected outputs", {
  if (Test_All == 0){
    #skip_on_os('windows')
    skip("Skipped RF1 model tests to save time") #**# Enable when testing code other than the main functions
  }

  # Load example data to run the models (back out two directories to get into main package directory)
  load("dfmip_example_inputs.RData")
  #load("../../vignettes/dfmip_example_inputs.RData")

  # Create a temporary results path
  results.path = "DFMIPTESTRESULTS/"
  dir.create(results.path)

  # Test Null Model for human cases
  dfmip.outputs = suppressWarnings(dfmip.forecast(c("annual.human.cases"), c("RF1_C"), human.data, mosq.data, weather.data,
                                                  districtshapefile, weekinquestion, week.id, results.path,
                                                  arbo.inputs = NA, observed.inputs = NA,
                                                  population.df = NA, rf1.inputs = rf1.inputs))

  forecasts.df = dfmip.outputs[[1]]
  forecast.distributions = dfmip.outputs[[2]]
  other.results = dfmip.outputs[[3]]

  # Test forecasts.df object
  expect_equal(round(forecasts.df$annual.human.cases, 0), 41)
  # Test distributions object
  expect_equal(round(forecast.distributions$annual.human.cases[[1]], 0), 41)
  #expect_equal(other.results, NULL) #**# Do not currently care about this output.

  #skip('Do not do hind casts until forecasts work')
  # Test ArboMAP hindcasts for human cases
  hindcasts = suppressWarnings(dfmip.hindcasts(c("annual.human.cases"), c("RF1_C"), c(2015), human.data, mosq.data,
                                               weather.data, districtshapefile, results.path, arbo.inputs = NA,
                                               population.df = NA, rf1.inputs = rf1.inputs,
                                               threshold = 1, percentage = 0.25, id.string = "test",
                                               season_start_month = 7, weeks_in_season = 2))

  accuracy = hindcasts[[1]]
  forecasts.df = hindcasts[[2]]
  forecast.distributions = hindcasts[[3]]

  expect_equal(as.character(accuracy$model), "RF1_C")
  expect_equal(accuracy$forecast.target, "annual.human.cases")
  expect_equal(round(accuracy$CRPS,1), 15.1)
  expect_equal(round(accuracy$RMSE, 1), 15.1) #**# Why is RMSE the same as CRPS? CRPS should be absolute error, not RMSE. Is only one data point being evaluated?
  expect_equal(round(accuracy$Scaled_RMSE, 3), 0.236)
  expect_equal(accuracy$within_percentage, 1)
  expect_equal(accuracy$within_threshold, 0)
  expect_equal(accuracy$within_threshold_or_percentage, 1)
  expect_equal(accuracy$AUC, NA)

  expect_equal(forecasts.df$MODEL.NAME[1], "RF1_C")
  #expect_equal(forecasts.df$FORECAST.ID[2], "test:2015-07-12") #**# This produces a weird result. It gives NA, but is not equal to NA.
  expect_equal(nrow(forecasts.df), 1) # Replace test above to set a clear expectation that only one row will be produced (as it only creates forecasts for the first week, as all subsequent weeks will be the same. #**# We can consider having the behavior produce an estimate for each week, even though they are all equal)
  expect_equal(forecasts.df$UNIT[1], 'test')
  expect_equal(forecasts.df$DATE[1], '2015-07-05')
  expect_equal(forecasts.df$YEAR[1], 2015)
  expect_equal(round(forecasts.df$annual.human.cases[1], 1), 48.9)
  #expect_equal(forecasts.df$annual.human.cases[2], NULL) #**# Same issue with FORECAST.ID[2] above. It's a weird NA that isn't an NA.

  expect_equal(round(forecast.distributions[['annual.human.cases']][[1]], 1), 48.9)
  expect_error(forecast.distributions[['annual.human.cases']][[2]], "subscript out of bounds")

  unlink(results.path, recursive = TRUE)
})

# Test multiple outputs
test_that("hindcasts works for all supported forecast targets simultaneously", {
  if (Test_All == 0 | Test_All == 2){
    #skip_on_os('windows')
    skip("Skipped test of all outputs") #**# Enable when testing code other than the main functions
  }

  # Load example data to run the models (back out two directories to get into main package directory)
  load("dfmip_example_inputs.RData")
  #load("../../vignettes/dfmip_example_inputs.RData")

  # Create a temporary results path
  results.path = "DFMIPTESTRESULTS/"
  dir.create(results.path)

  # Test hindcasts for multiple forecast targets simultaneously
  hindcasts = suppressWarnings(dfmip.hindcasts(c('annual.human.cases', "seasonal.mosquito.MLE"), c("NULL.MODELS"), c(2015), human.data, mosq.data,
                                               weather.data, districtshapefile, results.path, arbo.inputs = arbo.inputs,
                                               population.df = NA, rf1.inputs = NA,
                                               threshold = 1, percentage = 0.25, id.string = "test",
                                               season_start_month = 7, weeks_in_season = 1))

  accuracy = hindcasts[[1]]
  forecasts.df = hindcasts[[2]]
  forecast.distributions = hindcasts[[3]]

  expect_equal(as.character(accuracy$model[1]), "NULL.MODELS")
  expect_equal(accuracy$forecast.target[1], "annual.human.cases")
  expect_equal(accuracy$forecast.target[2], "seasonal.mosquito.MLE")
  expect_equal(round(accuracy$CRPS[1],0), 25)
  expect_equal(round(accuracy$CRPS[2],5), 0.00013)
  expect_equal(round(accuracy$RMSE[1], 0), 25)
  expect_equal(round(accuracy$RMSE[2], 5), 0.00013)
  expect_equal(round(accuracy$Scaled_RMSE[1], 3), 0.386)
  expect_equal(round(accuracy$Scaled_RMSE[2], 3), 0.097)
  expect_equal(accuracy$within_percentage[1], 0)
  expect_equal(accuracy$within_percentage[2], 1)
  expect_equal(accuracy$within_threshold[1], 0)
  expect_equal(accuracy$within_threshold[2], 0)
  expect_equal(accuracy$within_threshold_or_percentage[2], 1)
  expect_equal(accuracy$AUC[1], NA)

  expect_equal(forecasts.df$MODEL.NAME[1], "NULL.MODELS")
  #expect_equal(forecasts.df$FORECAST.ID[2], "test:2015-07-12") #**# This produces a weird result. It gives NA, but is not equal to NA.
  expect_equal(nrow(forecasts.df), 1) # Replace test above to set a clear expectation that only one row will be produced (as it only creates forecasts for the first week, as all subsequent weeks will be the same. #**# We can consider having the behavior produce an estimate for each week, even though they are all equal)
  expect_equal(forecasts.df$UNIT[1], 'test')
  expect_equal(forecasts.df$DATE[1], '2015-07-05')
  expect_equal(forecasts.df$YEAR[1], 2015)
  expect_equal(round(forecasts.df$seasonal.mosquito.MLE[1], 4), 0.0015)
  expect_equal(round(forecasts.df$annual.human.cases[1], 1), 88.7)

  expect_equal(round(forecast.distributions[['seasonal.mosquito.MLE']][[1]], 4), 0.0015)
  expect_equal(round(forecast.distributions[['annual.human.cases']][[1]], 1), 88.7)

  unlink(results.path, recursive = TRUE)
})


