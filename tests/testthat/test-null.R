# Unit tests for the Null Model Comparison
#library(testthat)

create.training.data = function(start.year = 2000, end.year = 2018, n.counties = 3,
                                in.mean = 57.4, in.dispersion = 0.377){
  # Ensure the train.data set is ALWAYS THE SAME across unit tests
  set.seed(20210623)
  years = seq(start.year,end.year)
  n.years = length(years)
  county.names = sprintf("county%02d", seq(1,n.counties))
  # Make county populations a uniform random number between 100,000 and 1,000,000
  county.populations = runif(n.counties, 100000, 1000000)
  train.data = data.frame(year = rep(years,n.counties),
                          location = sort(rep(county.names ,n.years)),
                          count = rnbinom(n.years*n.counties, mu = in.mean, size = in.dispersion),
                          POP = sort(rep(county.populations, n.years)))

  return(train.data)
}

create.test.data = function(n.counties = 3, in.mean = 57.4, in.dispersion = 0.377){
  # Set starting seed to be different than training data to avoid an artificial exact match
  set.seed(20210624)
  county.names = sprintf("county%02d", seq(1,n.counties))
  test.data = data.frame(location = sort(county.names),
                          count = rnbinom(n.counties, mu = in.mean, size = in.dispersion))

  return(test.data)
}

test_that("Apply Null Models function works", {
  train.data = create.training.data()
  test.data = create.test.data()
  target.year = 2019
  n.draws = 3
  stuff = apply.null.models(train.data, n.draws, nulls = "ALL", out.path = NA, in.seed = 20210622)

  # Check that the correct number of outputs were generated
  expect_equal(length(stuff), 4)
  crps.df = stuff[[1]]
  time.df = stuff[[2]]
  sample.size.vec = stuff[[3]]
  expect_equal(sample.size.vec[4], 3)
  expect_equal(length(sample.size.vec), 16) # 3 years excluded - requires at least 3 years of training data and 1 year is used for testing
  extra.lists = stuff[[4]]
  expect_equal(length(extra.lists), 10)

  # Test mean CRPS against the CRPS calculated with the individual model functions to confirm that the correct
  # model outputs are in the correct locations and everything is as expected
  # Selecting arbitrary years to check for potential errors throughout the data set
  #**# THIS METHOD WON'T WORK - RANDOM SEEDS WILL VARY BETWEEN THE FIRST RUN AND THIS RUN,
  #**# AND MODEL RUN ORDER AFFECTS THE RANDOM DRAWS - WOULD BE COMPLICATED TO CONTROL FOR SEED EFFECTS
  #test.train = train.data[train.data$year <= 2005, ]
  #test.test = train.data[train.data$year == 2006, ]
  #test.df = apply.historical.null(test.train, 2006, n.draws = 3, draw.vec = c("DRAW_0001", "DRAW_0002", "DRAW_0003"))
  #my.crps = scoringRules::crps_sample(test.data$count, as.matrix(my.data))

  # Compare individual model lists to crps.df data frame values
  hn.2006.test = mean(extra.lists$HN[[4]])
  crps.df.test = crps.df$CRPS[crps.df$target.year == 2006 & crps.df$model == "HN"]
  expect_equal(hn.2006.test, crps.df.test)

  nb.2018.test = mean(extra.lists$NB[[16]])
  crps.df.test = crps.df$CRPS[crps.df$target.year == 2018 & crps.df$model == "NB"]
  expect_equal(nb.2018.test, crps.df.test)

  si.2003.test = mean(extra.lists$SI[[1]])
  crps.df.test = crps.df$CRPS[crps.df$target.year == 2003 & crps.df$model == "SI"]
  expect_equal(si.2003.test, crps.df.test)

  ri.2017.test = mean(extra.lists$RI[[15]])
  crps.df.test = crps.df$CRPS[crps.df$target.year == 2017 & crps.df$model == "RI"]
  expect_equal(ri.2017.test, crps.df.test)

  aa.2005.test = mean(extra.lists$AA[[3]])
  crps.df.test = crps.df$CRPS[crps.df$target.year == 2005 & crps.df$model == "AA"]
  expect_equal(aa.2005.test, crps.df.test)

  ar.2010 = mean(extra.lists$AR[[8]])
  crps.df.test = crps.df$CRPS[crps.df$target.year == 2010 & crps.df$model == "AR"]
  expect_equal(ar.2010, crps.df.test)

  # Stopped varying the years to expedite the coding
  test.2010 = mean(extra.lists$SM[[8]])
  crps.df.test = crps.df$CRPS[crps.df$target.year == 2010 & crps.df$model == "SM"]
  expect_equal(test.2010, crps.df.test)

  test.2010 = mean(extra.lists$PN[[8]])
  crps.df.test = crps.df$CRPS[crps.df$target.year == 2010 & crps.df$model == "PN"]
  expect_equal(test.2010, crps.df.test)

  test.2010 = mean(extra.lists$UN[[8]])
  crps.df.test = crps.df$CRPS[crps.df$target.year == 2010 & crps.df$model == "UN"]
  expect_equal(test.2010, crps.df.test)

  test.2010 = mean(extra.lists$MV[[8]])
  crps.df.test = crps.df$CRPS[crps.df$target.year == 2010 & crps.df$model == "MV"]
  expect_equal(test.2010, crps.df.test)

})

test_that("Historical Null Model Works", {
  train.data = create.training.data()
  test.data = create.test.data()
  target.year = 2019
  my.df = apply.historical.null(train.data, target.year, n.draws = 1, draw.vec = c("DRAW_0001"))
  my.data = my.df[ , 3:ncol(my.df)]
  my.data = as.numeric(my.data)
  my.data = matrix(my.data, ncol = 1)

  # The current-year predictions should be within the range of variation from prior years
  expect_true(my.data[1,1] %in% train.data$count[train.data$location == "county01"])
  expect_true(my.data[2,1] %in% train.data$count[train.data$location == "county02"])
  expect_true(my.data[3,1] %in% train.data$count[train.data$location == "county03"])

  my.crps = scoringRules::crps_sample(test.data$count, as.matrix(my.data))

  # CRPS is mean absolute error for a point estimate, and the error for a single draw should be equal to the predicted data minus the number of cases.
  expect_equal(abs(my.data[ ,1] - test.data$count), my.crps)
})

test_that("Negative Binomial Model Works",{
  train.data = create.training.data()
  test.data = create.test.data()
  target.year = 2019
  my.df = apply.negative.binomial(train.data, target.year, n.draws = 1, draw.vec = c("DRAW_0001"))
  my.data = my.df[ , 3:ncol(my.df)]
  my.data = as.numeric(my.data)
  my.data = matrix(my.data, ncol = 1)

  #**# A negative-binomial specific test would be nice, but I can't think of one.

  my.crps = scoringRules::crps_sample(test.data$count, as.matrix(my.data))

  # CRPS is mean absolute error for a point estimate, and the error for a single draw should be equal to the predicted data minus the number of cases.
  expect_equal(abs(my.data[ ,1] - test.data$count), my.crps)
})

test_that("Stratified Incidence Model Works",{
  train.data = create.training.data()
  test.data = create.test.data()
  target.year = 2019
  my.df = apply.stratified.incidence(train.data, target.year, n.draws = 1, draw.vec = c("DRAW_0001"))
  my.data = my.df[ , 3:ncol(my.df)]
  my.data = as.numeric(my.data)
  my.data = matrix(my.data, ncol = 1)

  #**# A stratified-incidence specific test would be nice, but I can't think of one.

  my.crps = scoringRules::crps_sample(test.data$count, as.matrix(my.data))

  # CRPS is mean absolute error for a point estimate, and the error for a single draw should be equal to the predicted data minus the number of cases.
  expect_equal(abs(my.data[ ,1] - test.data$count), my.crps)
})

test_that("Random Incidence Model Works",{
  train.data = create.training.data()
  test.data = create.test.data()
  target.year = 2019
  my.df = apply.random.incidence(train.data, target.year, n.draws = 1, draw.vec = c("DRAW_0001"))
  my.data = my.df[ , 3:ncol(my.df)]
  my.data = as.numeric(my.data)
  my.data = matrix(my.data, ncol = 1)

  #**# A random incidence specific test would be nice, but I can't think of one.

  my.crps = scoringRules::crps_sample(test.data$count, as.matrix(my.data))

  # CRPS is mean absolute error for a point estimate, and the error for a single draw should be equal to the predicted data minus the number of cases.
  expect_equal(abs(my.data[ ,1] - test.data$count), my.crps)
})

test_that("Always Absent Model works",{
  train.data = create.training.data()
  test.data = create.test.data()
  target.year = 2019
  aa.df = apply.always.absent.null(train.data, target.year)
  aa.data = aa.df[ , 3:ncol(aa.df)]
  aa.data = as.numeric(aa.data)
  aa.data = matrix(aa.data, ncol = 1)

  aa.crps = scoringRules::crps_sample(test.data$count, as.matrix(aa.data))

  # CRPS is mean absolute error for a point estimate, and the error for the always absent model should be equal to the number of cases.
  expect_equal(test.data$count, aa.crps)
})

test_that("Stratified Mean Model Works",{
  train.data = create.training.data()
  test.data = create.test.data()
  target.year = 2019
  my.df = apply.stratified.mean.null(train.data, target.year, n.draws = 1, draw.vec = c("DRAW_0001"))
  my.data = my.df[ , 3:ncol(my.df)]
  my.data = as.numeric(my.data)
  my.data = matrix(my.data, ncol = 1)

  # Check that returned means are correctly stratified
  expect_equal(my.data[1,1], mean(train.data$count[train.data$location == 'county01']))
  expect_equal(my.data[2,1], mean(train.data$count[train.data$location == 'county02']))
  expect_equal(my.data[3,1], mean(train.data$count[train.data$location == 'county03']))

  my.crps = scoringRules::crps_sample(test.data$count, as.matrix(my.data))

  # CRPS is mean absolute error for a point estimate, and the error for a single draw should be equal to the predicted data minus the number of cases.
  expect_equal(abs(my.data[ ,1] - test.data$count), my.crps)
})

test_that("Prior Year Model Works",{
  train.data = create.training.data()
  test.data = create.test.data()
  target.year = 2019
  my.df = apply.prior.year.null(train.data, target.year, n.draws = 1, draw.vec = c("DRAW_0001"))
  my.data = my.df[ , 3:ncol(my.df)]
  my.data = as.numeric(my.data)
  my.data = matrix(my.data, ncol = 1)

  # The current-year prediction should match the prior year from the training data set
  expect_equal(as.numeric(my.data), as.numeric(train.data$count[train.data$year == 2018]))

  my.crps = scoringRules::crps_sample(test.data$count, as.matrix(my.data))

  # CRPS is mean absolute error for a point estimate, and the error for the prior year null model should be equal to the prior-year minus the number of cases.
  expect_equal(abs(my.data[ ,1] - test.data$count), my.crps)
})

test_that("Mean Value Model Works",{
  train.data = create.training.data()
  test.data = create.test.data()
  target.year = 2019
  my.df = apply.mean.value.null(train.data, target.year)
  my.data = my.df[ , 3:ncol(my.df)]
  my.data = as.numeric(my.data)
  my.data = matrix(my.data, ncol = 1)

  # Each draw should match the mean of the training data
  expect_equal(as.numeric(my.df$DRAW_0001[2]), mean(train.data$count))
  # mean of the training data should match each value and the mean of the predicted data (only tested mean)
  expect_equal(mean(train.data$count), mean(my.data))

  my.crps = scoringRules::crps_sample(test.data$count, as.matrix(my.data))

  # CRPS is mean absolute error for a point estimate, and the error for the mean value model should be equal to the mean value minus the number of cases.
  expect_equal(abs(my.data[ ,1] - test.data$count), my.crps)
})

test_that("Uniform Null Model Works", {
  # Create a simulated test data set
  train.data = create.training.data()
  # Needs year, location, count and POP fields
  year = 2019
  n.draws = 100
  draw.vec = sprintf("DRAW_%04.0f", seq(1,n.draws))
  model.result = apply.uniform(train.data, year, n.draws, draw.vec)

  # Check that model predictions are always below the county max
  expect_lt(max(model.result[1,3:nrow(model.result)]), max(train.data$count[train.data$location == "county01"]))
  expect_lt(max(model.result[2,3:nrow(model.result)]), max(train.data$count[train.data$location == "county02"]))
  expect_lt(max(model.result[3,3:nrow(model.result)]), max(train.data$count[train.data$location == "county03"]))

})

test_that("AR(1) Normal Model Works", {
  # Create a simulated test data set
  # Needs year, location, count and POP fields
  set.seed(20210526)
  train.data = data.frame(year = rep(seq(2000,2018),3),
                          location = sort(rep(c('county1','county2','county3') ,19)),
                          count = rnbinom(19*3, mu = 57.4, size = 0.377),
                          POP = sort(rep(c(100000,200000,150000), 19)))
  year = 2019
  n.draws = 100
  draw.vec = sprintf("DRAW_%04.0f", seq(1,n.draws))
  model.result = apply.ar1(train.data, year, n.draws, draw.vec)

  # Check that model predictions are always above or equal to 0
  expect_gte(min(model.result[1:3,3:nrow(model.result)]), 0)

  # Not checked as part of the test, but visually checked
  # Check that model outputs are approximately normally distributed
  hist(as.numeric(model.result[1, 3:ncol(model.result)]))
  hist(as.numeric(model.result[2, 3:ncol(model.result)]))
  hist(as.numeric(model.result[3, 3:ncol(model.result)]))
  # THEY ARE NOT NORMAL - HEAPED AT 0.

  model.result.1 = as.numeric(model.result[1, 3:ncol(model.result)])
  model.result.1.no.zeros = model.result.1[model.result.1 > 0]
  hist(model.result.1.no.zeros) # Looks like a truncated normal distribution!

  model.result.2 = as.numeric(model.result[2, 3:ncol(model.result)])
  model.result.1.no.zeros = model.result.2[model.result.2 > 0]
  hist(model.result.1.no.zeros) # Looks like a truncated normal distribution!

  model.result.3 = as.numeric(model.result[3, 3:ncol(model.result)])
  model.result.1.no.zeros = model.result.3[model.result.3 > 0]
  hist(model.result.1.no.zeros) # Not super normal - what's going on here??? Looks like a really flat distribution with a lot of noise.


  # The training data set did not catch this error:
  #non-stationary AR part from CSS
  # This example replicated the error pre-code modification.
  train.2 = train.data[train.data$year <= 2003, ]
  year = 2004
  model.result = apply.ar1(train.2, year, n.draws, draw.vec)

  # The training data set did not catch this error:
  #Error in apply.ar1(train.data, year, n.draws, draw.vec) :
  #  object 'distribution.samples' not found (2018, for or after Alabama-Autauga)
  #TryCatch does not return values out of the error function by default (it's a function - I'd overlooked that!). Need to include a return statement and assign the TryCatch to output.
  # This issue has been corrected in the code now. Not quite sure how to set up tests so that it tests all three of the internal code loops.

  # New error now:
  #Error in checkNumeric(input) : Input with missing values: dat
  #In addition: There were 35 warnings (use warnings() to see them)
  #34: In apply.ar1(train.data, year, n.draws, draw.vec) :
  #  NAs introduced by coercion
  # Fixed - if tryCatch succeeds without error, it will return 0, not the last value.
  # Solution is to follow the original post approach, of having the error state return a list.

  #**# Think about a way to make these tests non-visual so they can be automatically tested with code updates.

})


# Add unit test for check.inputs function
test_that("check fields function works", {
  # Create a minimal data frame that will pass the basic check (note there is no type checking for fields)
  input.data = data.frame(location = "one", count = 1, year = 2004, POP = 10)
  nulls = "ALL"
  check.fields(input.data, nulls)

  # omit POP field
  input.data$POP = NULL
  expect_error(check.fields(input.data, nulls))

  input.data$pop = 10
  expect_error(check.fields(input.data, nulls))

  # Expect No error for a model that does not require the pop object
  nulls = "NB"
  check.fields(input.data, nulls)

  # Check omission of location field
  input.data$location = NULL
  expect_error(check.fields(input.data, nulls))
  input.data$location = 'two'

  # Check omission of count field
  input.data$count = NULL
  expect_error(check.fields(input.data, nulls))
  input.data$count = 22

  # Check omission of year field
  input.data$year = NULL
  expect_error(check.fields(input.data, nulls))
  input.data$year = 2015

})
