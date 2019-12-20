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


#**# SKIP THIS ON CRAN - THIS WILL TAKE A WHILE TO RUN
test_that("Forecast models produce the expected outputs", {
  #skip("Skipping overall function tests to save time") #**# Enable when testing code other than the main functions

  # Load example data to run the models (back out two directories to get into main package directory)
  load("../../vignettes/dfmip_example_inputs.RData")

  # Create a temporary results path
  results.path = "DFMIPTESTRESULTS/"
  dir.create(results.path)

  # Test RF Model

  # Test Null Model

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

  #expect_equal(accuracy$model, "ArboMAP") #Failing, model name coming out as factor.
  expect_equal(as.character(accuracy$model), "ArboMAP") #Failing, model name coming out as factor.
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
