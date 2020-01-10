## ----LoadData-----------------------------------------------------------------

data("dfmip_example_inputs")

# # Load & compile weather data 
# weatherpathstr = "weather data/"
# weathersummaryfile = "weather data summary file.csv"
# weather.data = ArboMAP::read.weather.data(weatherpathstr, weathersummaryfile) # Compile the weather data into an R object for the code
# weather.data$district = ArboMAP::simplifynames(weather.data$district) # Convert the county names to match the mosquito and human data

data("weather_data")

# Show format of weather data (first 5 rows)
knitr::kable(weather_data[1:5, ])


data("weather.data")

# Show format of weather data (first 5 rows)
knitr::kable(weather.data[1:5, ])

# Load human data
# human.data.file = "human case data/simulated human case data.csv"
# human.data = read.csv(human.data.file)
# Show format of human data (first 5 rows)
knitr::kable(human.data[1:5, ])


# Load mosquito data
# mosq.data.file = "mosquito data/simulated mosquito tests.csv"
# mosq.data = read.csv(mosq.data.file)
# Show format of mosquito data (first 5 rows)
knitr::kable(mosq.data[1:5, ])

# Read in Shapefile with district polygons
# districtshapefile.file = "shapefile/cb_2014_us_county_5m - in EPSG 5070 - only SD.shp"
#**# NOT INCLUDED IN PACKAGE, DOWNLOAD FROM https://github.com/EcoGRAPH/ArboMAP/tree/master/shapefile/ (All 5 files are part of the shapefile)

# Load population data for calculation of incidence (optional)
# population.df = NA # This optional input  is omitted in this vignette

# Set up ArboMAP-specific inputs in the arbo.inputs object (if not using ArboMAP, arbo.inputs can be set to NA)
#**# IMPROVE DOCUMENTATION HERE
# stratafile.file = "strata/17-04-20 - classified strata - classic.csv"
# stratafile = read.csv(stratafile.file) #**# WATCH FOR ERRORS ASSOCIATED WITH THIS
# maxobservedhumandate = as.Date("2017-12-31", "%Y-%m-%d")
# var1name = "tmeanc"
# var2name = "vpd"
# compyear1 = 2012
# compyear2 = 2017
# arbo.inputs = list(stratafile = stratafile, maxobservedhumandate = maxobservedhumandate, var1name = var1name, var2name = var2name,
#                    compyear1 = compyear1, compyear2 = compyear2)


# Set up Random Forest Model-specific inputs in the rf1.inputs object (if not using RF1, rf1.inputs can be set to NA)
# No additional data layers will be input to the Random-Forest specific inputs will be used in this example, so this object will be relatively simple to set up.
#**# Improve documentation here
# analysis.counties = unique(read.csv(mosq.data)$district) # Get a list of counties for adding years with no human cases
# analysis.years = seq(2004,2018) # Get a list of years for correcting for years with no human cases
# files.to.add = c() # Needs to be an empty vector if there are no files to add
# merge.type.vec = c()
# user.drop.vars = c() # Additional variables from the environmental data to exclude from the analysis
# mosq.model = human.model = NA # If a random forest model has already been fit, please enter the path here
# analyze.mosquitoes = 1
# analyze.humans = 1
# rf1.inputs = list(files.to.add, merge.type.vec, analysis.counties, analysis.years, user.drop.vars, mosq.model, human.model,
#                   analyze.mosquitoes, analyze.humans)

# Code used to save the inputs once they are assembled 
# example.inputs = "dfmip_example_inputs.RData"
# save.image(file = example.inputs)




## ----RunModel-----------------------------------------------------------------

# Specify forecast week
weekinquestion = as.Date("2018-08-15", "%Y-%m-%d") #**# Is the as.Date part necessary?
week.id = "test"
results.path = "FORECAST_RESULTS/"

# Define what should be evaluated
forecast.targets = c("annual.human.cases") #, "seasonal.mosquito.MLE")

#models.to.run = c("RF1_C","ArboMAP", "NULL.MODELS")
models.to.run = c("NULL.MODELS")
# NOT RUN TO SAVE PROCESSING TIME & AVOID ERRORS if the ArboMAP or RF1 packages are not installed
#dfmip.outputs = dfmip.forecast(forecast.targets, models.to.run, human.data, mosq.data, weather.data,
#                          districtshapefile, weekinquestion, week.id, results.path,
#                          arbo.inputs = arbo.inputs, observed.inputs = observed.inputs,
#                          population.df = population.df, rf1.inputs = rf1.inputs)
#load('dfmip_outputs.RData')


#forecasts.df = dfmip.outputs[[1]]
#forecast.distributions = dfmip.outputs[[2]]
#other.results = dfmip.outputs[[3]]
#rf1.results = other.results$rf1
#arbomap.results = other.results$arbomap.results #**# Are there other Arbomap results to output?


## -----------------------------------------------------------------------------
# Set years for which hindcasts should be made
focal.years = c(2015, 2016, 2017)

# Set directory for results
results.path = "HINDCAST_RESULTS/"

# Remaining inputs were defined above
#**# NOT RUN TO SAVE TIME
#hindcasts = dfmip.hindcasts(forecast.targets, models.to.run, focal.years, human.data, mosq.data,
#                           weather.data, districtshapefile,
#                           results.path, arbo.inputs = arbo.inputs,
#                          population.df = population.df, rf1.inputs = rf1.inputs,
#                          threshold = 1, percentage = 0.25, id.string = "test")
#load('hindcasts.RData')




