# COVID19-Excess-Deaths-R-Example
An example of using time series to calculate the excess deaths from COVID19

###           Author:       Conrad Manske
###          Created:       12/17/2020
###        Last Edit:       

## Introduction:
When the pandemic started, I started an analysis on excess deaths from COVID19 (personal project, not professional).  After a few revisions, I wanted to outline my process.  At the same time, I decided to refresh the project.  I am outlining everything as I do it.  

I originally completed this analysis because I wanted to validate the excess deaths I have seen and if they could reasonably be attributed to COVID19.  I could go to the CDC and pull the excess death numbers they generate, but I have the knowledge, skills, and tools to check for myself.  So why not?

## Step 1:  House Cleaning
### Tabula RASA to clear anything in my workspace and clean memory
```
################################################## 
# TABULA RASA     
################################################## 
rm(list = ls(pattern = ''))
gc()
```
### Load the libraries required
```
################################################## 
# LOAD LIBRARIES     
################################################## 
library(bsts)
library(forecast)
library(ggplot2)
library(lubridate)
library(pracma)
```
`ggplot2` for graphing data; `lubridate` to tinker with dates; `pracma`, `bsts`, and `forecast` for time-series; 

## Step 2:  Find and clean
### Source 1 = Weekly Counts of Death by Jurisdiction and Select Causes of Death 
### ----->    (https://data.cdc.gov/NCHS/Weekly-Counts-of-Death-by-Jurisdiction-and-Select-/u6jv-9ijr)
### Source 2 = Weekly Counts of Deaths by Jurisdiction and Age 
### ----->    (https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-Jurisdiction-and-Age/y5bj-9g5w)
### Source 3 = Provisional COVID19 Death Counts by Week Ending Date and State 
### ----->    (https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Week-Ending-D/r8kw-7aab)
Future update:  Load directly to R, skip downloading to my PC
```
################################################## 
# LOAD DATA     
################################################## 
# LOAD FILES
Source1 <- read.csv(paste0('E:/R Projects/COVID Deaths - Time Series/',
                           'Weekly_Counts_of_Death_by_Jurisdiction_and_Select_Causes_of_Death.csv'),
                    header = T, sep = ',')
Source2 <- read.csv(paste0('E:/R Projects/COVID Deaths - Time Series/',
                           'Weekly_Counts_of_Deaths_by_Jurisdiction_and_Age.csv'),
                    header = T, sep = ',', quote = '\"')
Source3 <- read.csv(paste0('E:/R Projects/COVID Deaths - Time Series/',
                           'Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State.csv'),
                    header = T, sep = ',', quote = '\"')
```
For Source 1, I'm looking for unique/distinct jurisdictions (although my analysis will be at the federal level) and dates.  I am filtering the data to unweighted types, because the alternative is predicted/weighted estimates (predicted/weighted is used for newer data that might not be fully reported yet).  I am taking the sum of the number of deaths because those are broken into various causes of death.  If they included categories like accidents or suicides I would have included the other causes, but those are not included in the statistics.  
```
# CLEAN SOURCE 1 
# Aggregate for Total Deaths
sourceOneData <- aggregate(Number.of.Deaths ~ ï..Jurisdiction + Week.Ending.Date, 
                           data = Source1[Source1$Type == 'Unweighted', ], 
                           FUN = sum)
# Fix column names
colnames(sourceOneData)[colnames(sourceOneData) == 'ï..Jurisdiction'] <- 'Jurisdiction'
colnames(sourceOneData)[colnames(sourceOneData) == 'Week.Ending.Date'] <- 'Date'
colnames(sourceOneData)[colnames(sourceOneData) == 'Number.of.Deaths'] <- 'TotalDeaths_Source1'

# Convert date field to DATE type
sourceOneData$Date <- as.Date(sourceOneData$Date, format = '%Y-%m-%d')
```

For Source 2, I am filtering the data to unweighted again, then I take the sum of deaths by unique/distinct jurisdictions and dates.  This data set breaks the deaths by age cohort, but I am not interested in that analysis.  It may be interesting for a future analytic.  
```
# CLEAN SOURCE 2
# Convert CHARs to INTs
Source2$Number.of.Deaths <- as.numeric(gsub(',', '', Source2$Number.of.Deaths))

# Replace NA's with zero
Source2$Number.of.Deaths[is.na(Source2$Number.of.Deaths)] <- 0

# Aggregate for Total Deaths and COVID Deaths
sourceTwoData <- aggregate(Number.of.Deaths ~ ï..Jurisdiction + Week.Ending.Date, 
                           data = Source2[Source2$Type == 'Unweighted', ], 
                           FUN = sum)

# Fix column names 
colnames(sourceTwoData)[colnames(sourceTwoData) == 'ï..Jurisdiction'] <- 'Jurisdiction'
colnames(sourceTwoData)[colnames(sourceTwoData) == 'Week.Ending.Date'] <- 'Date'
colnames(sourceTwoData)[colnames(sourceTwoData) == 'Number.of.Deaths'] <- 'TotalDeaths_Source2'

# Convert date field to DATE type
sourceTwoData$Date <- as.Date(sourceTwoData$Date, format = '%m/%d/%Y')
```

For Source 3, I filter for distinct jurisdictions and dates again.  I filter the data for the "by week" data, and I pull the Total Deaths and COVID19 Deaths. 
```
# CLEAN SOURCE 3
# Convert CHARs to INTs
Source3$Total.Deaths <- as.numeric(gsub(',', '', Source3$Total.Deaths))
Source3$COVID.19.Deaths <- as.numeric(gsub(',', '', Source3$COVID.19.Deaths))

# Replace NA's with zero
Source3$Total.Deaths[is.na(Source3$Total.Deaths)] <- 0
Source3$COVID.19.Deaths[is.na(Source3$COVID.19.Deaths)] <- 0

# Aggregate for Total Deaths and COVID Deaths
sourceThreeData <- aggregate(cbind(Total.Deaths, COVID.19.Deaths) ~ State + Week.Ending.Date, 
                             data = Source3[Source3$Group == 'By Week', ], 
                             FUN = sum)

# Fix column names 
colnames(sourceThreeData)[colnames(sourceThreeData) == 'State'] <- 'Jurisdiction'
colnames(sourceThreeData)[colnames(sourceThreeData) == 'Week.Ending.Date'] <- 'Date'
colnames(sourceThreeData)[colnames(sourceThreeData) == 'Total.Deaths'] <- 'TotalDeaths_Source3'
colnames(sourceThreeData)[colnames(sourceThreeData) == 'COVID.19.Deaths'] <- 'CovidDeaths_Source3'

# Convert date field to DATE type
sourceThreeData$Date <- as.Date(sourceThreeData$Date, format = '%m/%d/%Y')
```

I compared the deaths from Source 1 and Source 3 and from Source 2 and Source 3 for the overlapping periods.  There were a lot of missing deaths from Source 1.  After re-reading the "technical notes" for Source 1, I found it excludes COVID19 and accidents, but Source 2 and Source 3 were very much aligned (99.94% match).
```
# MERGE DATA FRAMES
inputData <- merge(sourceTwoData, 
                   sourceThreeData, 
                   by = c('Jurisdiction', 'Date'), 
                   all.x = T)
```

This section can be used to quickly filter the jurisdiction.  For this example, I will use the federal level ('United States')
```
# CLEAN DATA FRAME
 inputData <- inputData[inputData$Jurisdiction == 'United States',]
# inputData <- inputData[inputData$Jurisdiction == 'New York',]
# inputData <- inputData[inputData$Jurisdiction == 'California',]
# inputData <- inputData[inputData$Jurisdiction == 'Texas',]
# inputData <- inputData[inputData$Jurisdiction == 'Florida',]
# inputData <- inputData[inputData$Jurisdiction == 'Kansas',]
# inputData <- inputData[inputData$Jurisdiction == 'Missouri',]
# inputData <- inputData[inputData$Jurisdiction == 'North Dakota',]

# CLEAN ENVIRONMENT
rm(Source1)
rm(Source2)
rm(Source3)
rm(sourceOneData)
rm(sourceTwoData)
rm(sourceThreeData)
```

Now that I have a data frame with the base data, I will start to format data in ways that will be useful (Years, Months, time_period).  
```
################################################## 
# CREATE DATA FRAME TO STORE RESULTS    
################################################## 
# NEW DATA FRAME
forecastData <- inputData

# SORT DATA 
forecastData <- forecastData[order(forecastData$Date),]

# RENAME COLUMNS & DROP REDUNDANT COLUMN
colnames(forecastData)[colnames(forecastData) == 'TotalDeaths_Source2'] <- 'TotalDeaths'
colnames(forecastData)[colnames(forecastData) == 'CovidDeaths_Source3'] <- 'CovidDeaths'
forecastData <- forecastData[, -which(colnames(forecastData) == 'TotalDeaths_Source3')]
forecastData$Volume <- forecastData$TotalDeaths

# ERASE VOLUME FROM COVID PERIOD
forecastData[forecastData$Date >= '2020-03-15', 'Volume'] <- NA

# ADD TIME PERIOD
forecastData$time_period <- 1:nrow(forecastData)

# ADD YEAR AND MONTH DUMMY VARS
forecastData$Year <- format(as.Date(forecastData$Date), format = '%Y')
forecastData$Year <- as.integer(forecastData$Year)

forecastData$Month <- format(as.Date(forecastData$Date), format = '%B')
forecastData$Month <- as.factor(forecastData$Month)
```

## Step 3:  Plot some data
```
################################################## 
# PLOT DATA SERIES    
##################################################
```
First plot is a very basic check.  Look for weird outliers, especially at the beginning and end of the dataset.  The first iteration of this project had an incomplete first week of data.  The CDC has updated their files.  
```
ggplot(data = forecastData, aes(x = Date, y = Volume)) +
     geom_line() +
     scale_x_date(date_labels = '%B-%Y') +
     geom_point()
```
![Plot1](https://user-images.githubusercontent.com/14900746/149211444-6d1f08ab-2dc5-4452-8f82-4ae195d88ea1.png)

The next few steps are to check for seasonality in the data. 
```
# EXPLORE SEASONALITY
# Plot Overlapping Years by Month
ggplot(data = forecastData, aes(x = month(Date, label = T, abbr = T),
                                y = Volume,
                                group = factor(year(Date)),
                                colour = factor(year(Date)))) +
                                geom_line() +
                                geom_point() +
                                labs(x = 'Month', colour = 'Year') +
                                theme_classic()
```
![Plot2](https://user-images.githubusercontent.com/14900746/149211663-8802c27a-cf2a-4737-8a04-c38365b73852.png)

```
# Plot Overlapping Years by Week
ggplot(data = forecastData, aes(x = week(Date),
                                y = Volume,
                                group = factor(year(Date)),
                                colour = factor(year(Date)))) +
                                geom_line() +
                                geom_point() +
                                labs(x = 'Week', colour = 'Year') +
                                theme_classic()
```
![Plot3](https://user-images.githubusercontent.com/14900746/149211707-d28b448b-d4a6-4adc-8b7d-70f3e25bfb50.png)


## Step 4:  Build a lot of models
This step is the meat & potatoes of the project.  I will build a lot of models in this section.  

Some models need to be built with a vector and some need to use a time series object
```
################################################## 
# CREATE DAILY VECTOR OBJECT (FORCED > 0.00)    
################################################## 
vectorTemp <- as.vector(forecastData[!is.na(forecastData[, 'Volume']), 'Volume'])
# vectorTemp <- vectorTemp + 0.000000000001


################################################## 
# CREATE DAILY TIME SERIES OBJECT    
################################################## 
tsObjectDaily <- ts(vectorTemp, start = as.Date('2015-01-10', format = '%Y-%m-%d'), frequency = 52)
```

### Moving Average
https://www.rdocumentation.org/packages/pracma/versions/1.9.9/topics/movavg
```
################################################## 
# MOVING AVERAGES    
################################################## 
```
Moving Averages are not going to be great models for this project.  They have many strengths for smoothing and extrapolating data, but with the seasonality of the data, we will save time by skipping these.  
```
# NOTES:  Different types of moving average of a time series.
# ARGUMENTS:
#    x    == time series as numeric vector.
#    n    == backward window length.
#    type == c('s', 't', 'w', 'm', 'e', 'r')
#              's' = Simple Moving Average
#              't' = Triangular Moving Average
#              'w' = Weighted Moving Average
#              'e' = Exponential Moving Average
#              'r' = Running Moving Average

# RUN MODELS (4 WEEKS)
# fcst_MovingAvg_04week_Simple <- movavg(tsObjectDaily, n = 4, type = 's')
# fcst_MovingAvg_04week_Triangle <- movavg(tsObjectDaily, n = 4, type = 't')
# fcst_MovingAvg_04week_Weighted <- movavg(tsObjectDaily, n = 4, type = 'w')
# fcst_MovingAvg_04week_Exponential <- movavg(tsObjectDaily, n = 4, type = 'e')
# fcst_MovingAvg_04week_Running <- movavg(tsObjectDaily, n = 4, type = 'r')

# RUN FORECASTS (4 WEEKS)
# prdct_MovingAvg_04week_Simple <- forecast(fcst_MovingAvg_04week_Simple, sum(is.na(forecastData$Volume)))
# prdct_MovingAvg_04week_Triangle <- forecast(fcst_MovingAvg_04week_Triangle, sum(is.na(forecastData$Volume)))
# prdct_MovingAvg_04week_Weighted <- forecast(fcst_MovingAvg_04week_Weighted, sum(is.na(forecastData$Volume)))
# prdct_MovingAvg_04week_Exponential <- forecast(fcst_MovingAvg_04week_Exponential, sum(is.na(forecastData$Volume)))
# prdct_MovingAvg_04week_Running <- forecast(fcst_MovingAvg_04week_Running, sum(is.na(forecastData$Volume)))

# PLOT RESULTS
# autoplot(prdct_MovingAvg_04week_Simple)
# autoplot(prdct_MovingAvg_04week_Triangle)
# autoplot(prdct_MovingAvg_04week_Weighted)
# autoplot(prdct_MovingAvg_04week_Exponential)
# autoplot(prdct_MovingAvg_04week_Running)
```

### Season and Trend Decomposition
https://www.rdocumentation.org/packages/forecast/versions/8.15/topics/forecast.stl
```
################################################## 
# SEASON AND TREND DECOMPOSITION MODEL    
##################################################
# NOTES:  Returns forecasts obtained by either ETS or ARIMA models applied to 
#         the seasonally adjusted data from an STL decomposition
# ARGUMENTS:
#    x         == A univariate numeric time series of class 'ts'
#    h         == Number of periods for forecasting.
#    s.window  == Either the character string 'periodic' or the span (in lags)
#                   of the loess window for seasonal extraction
#    robust    == If TRUE, robust fitting will used in the loess procedure 
#                   within stl
#    method    == Method to use for forecasting the seasonally adjusted series
#                   c('ets', 'arima', 'naive', 'rwdrift')
#    lambda    == Box - Cox transformation parameter. Ignored if NULL. 
#                   Otherwise, data transformed before model is estimated and 
#                   back - transformed after forecasts are computed.
#    xreg      == Historical regressors to be used in auto.arima()
#    newxreg   == Future regressors to be used in forecast.Arima()

# RUN SEASON AND TREND DECOMPOSITION MODELS 
fcst_STDecomp_ETS <- stlf(tsObjectDaily, h = sum(is.na(forecastData$Volume)), method = 'ets')
fcst_STDecomp_ARIMA <- stlf(tsObjectDaily, h = sum(is.na(forecastData$Volume)), method = 'arima')
fcst_STDecomp_Naive <- stlf(tsObjectDaily, h = sum(is.na(forecastData$Volume)), method = 'naive')
fcst_STDecomp_RandomWalk <- stlf(tsObjectDaily, h = sum(is.na(forecastData$Volume)), method = 'rwdrift')

# RUN SEASON AND TREND DECOMPOSITION FORECASTS 
prdct_STDecomp_ETS <- forecast(fcst_STDecomp_ETS, sum(is.na(forecastData$Volume)))
prdct_STDecomp_ARIMA <- forecast(fcst_STDecomp_ARIMA, sum(is.na(forecastData$Volume)))
prdct_STDecomp_Naive <- forecast(fcst_STDecomp_Naive, sum(is.na(forecastData$Volume)))
prdct_STDecomp_RandomWalk <- forecast(fcst_STDecomp_RandomWalk, sum(is.na(forecastData$Volume)))

# PLOT RESULTS
# autoplot(prdct_STDecomp_ETS)
# autoplot(prdct_STDecomp_ARIMA)
# autoplot(prdct_STDecomp_Naive)
# autoplot(prdct_STDecomp_RandomWalk)
```

### Autoregressive Integrated Moving Average (ARIMA) 
https://www.rdocumentation.org/packages/forecast/versions/8.15/topics/auto.arima
```
################################################## 
# AUTO ARIMA    
################################################## 
# NOTES:  Returns best ARIMA model according to either AIC, AICc or BIC value. 
#         The function conducts a search over possible model within the order 
#         constraints provided.
# ARGUMENTS:
#    y          == a univariate time series
#    stationary == If TRUE, restricts search to stationary models.
#    seasonal   == If FALSE, restricts search to non-seasonal models.
#    ic         == Information criterion to be used in model selection.
#    stepwise   == If TRUE, will do stepwise selection(faster). Otherwise, it 
#                   searches over all models. Non-stepwise selection can be 
#                   very slow, especially for seasonal models.
#    nmodels    == Maximum number of models considered in the stepwise search.

# RUN AUTO ARIMA MODELS 
fcst_ARIMA_Seasonal <- auto.arima(tsObjectDaily, stepwise = F, stationary = F, seasonal = TRUE)
fcst_ARIMA_NonSeasonal <- auto.arima(tsObjectDaily, stepwise = F, stationary = F, seasonal = FALSE)

# RUN AUTO ARIMA FORECASTS 
prdct_ARIMA_Seasonal <- forecast(fcst_ARIMA_Seasonal, sum(is.na(forecastData$Volume)))
prdct_ARIMA_NonSeasonal <- forecast(fcst_ARIMA_NonSeasonal, sum(is.na(forecastData$Volume)))

# PLOT RESULTS
# autoplot(prdct_ARIMA_Seasonal)
# autoplot(prdct_ARIMA_NonSeasonal)
```


### Holt-Winters Exponential Smoothing
https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/HoltWinters
```
################################################## 
# HOLT-WINTERS EXPONENTIAL SMOOTHING    
################################################## 
# NOTES:  Computes Holt-Winters Filtering of a given time series. Unknown 
#         parameters are determined by minimizing the squared prediction error.
# ARGUMENTS:
#    x         == An object of class ts
#    seasonal  == c('additive', 'multiplicative')
#                   additive = the magnitude of seasonality does not change over time
#                   multiplicative = seasonal pattern depends on the magnitude of the data

# RUN HOLT-WINTERS MODELS (AUTO FIT)     
fcst_HoltWinters_Add <- HoltWinters(x = tsObjectDaily, seasonal = 'additive')
fcst_HoltWinters_Mult <- HoltWinters(x = tsObjectDaily, seasonal = 'multiplicative')

# RUN HOLT-WINTERS FORECASTS   
prdct_HoltWinters_Add <- forecast(fcst_HoltWinters_Add, sum(is.na(forecastData$Volume)))
prdct_HoltWinters_Mult <- forecast(fcst_HoltWinters_Mult, sum(is.na(forecastData$Volume)))

# PLOT RESULTS
# autoplot(prdct_HoltWinters_Add)
# autoplot(prdct_HoltWinters_Mult)
```

### Double Seasonal Holt-Winters
https://www.rdocumentation.org/packages/forecast/versions/8.15/topics/dshw
```
################################################## 
# DOUBLE SEASONAL HOLT-WINTERS    
################################################## 
# NOTES:  Returns forecasts using Taylor's (2003) Double-Seasonal HW method.
# ARGUMENTS:
#    x         == Numeric vector
#    period1   == Period of the shorter seasonal period
#    period2   == Period of the longer seasonal period
#    h         == Number of periods for forecasting

# RUN DOUBLE SEASONAL HOLT-WINTERS MODELS     
fcst_DSHoltWinters1 <- dshw(vectorTemp, period1 = 4, period2 = 4 * 13, h = sum(is.na(forecastData$Volume)))
fcst_DSHoltWinters2 <- dshw(vectorTemp, period1 = 13, period2 = 4 * 13, h = sum(is.na(forecastData$Volume)))

# RUN DOUBLE SEASONAL HOLT-WINTERS FORECASTS     
prdct_DSHoltWinters1 <- forecast(fcst_DSHoltWinters1, sum(is.na(forecastData$Volume)))
prdct_DSHoltWinters2 <- forecast(fcst_DSHoltWinters2, sum(is.na(forecastData$Volume)))

# PLOT RESULTS
# autoplot(prdct_DSHoltWinters1)
# autoplot(prdct_DSHoltWinters2)
```

### BATS & TBATS
https://www.rdocumentation.org/packages/forecast/versions/8.15/topics/tbats
```
################################################## 
# BOX-COX, ARMA, TREND, SEASONALITY (TBATS)   
################################################## 
# NOTES:  Fits a TBATS model applied to y, as described in De Livera, 
#         Hyndman & Snyder (2011). 
# ARGUMENTS:
#    y           == The time series to be forecast.
#    use.box.cox == Use the Box-Cox transformation. If NULL then both are tried. 
#                   Best fit is selected by AIC.
#    use.trend   == Include a trend. If NULL then both are tried. Best fit 
#                   is selected by AIC.
#    use.parallel == Indicates whether or not to use parallel processing

# RUN BATS/TBATS MODELS     
fcst_TBATS_Trigonometric <- tbats(tsObjectDaily, use.parallel = FALSE)

# RUN BATS/TBATS FORECASTS     
prdct_TBATS_Trigonometric <- forecast(fcst_TBATS_Trigonometric, sum(is.na(forecastData$Volume)))

# PLOT RESULTS
# autoplot(prdct_TBATS_Trigonometric)
```

### Time Series Neural Network
https://www.rdocumentation.org/packages/forecast/versions/8.4/topics/nnetar
```
################################################## 
# NEURAL NETWORK TIME SERIES     
################################################## 
# NOTES:  Feed-forward neural networks with a single hidden layer and lagged 
#         inputs for forecasting univariate time series 
# ARGUMENTS:
#    y         == A numeric vector or time series of class ts.
#    P         == Number of seasonal lags used as inputs.
#    size      == Number of nodes in the hidden layer. Default is half of the 
#                   number of input nodes plus 1.
#    repeats   == Number of networks to fit with different random starting 
#                   weights. These are then averaged when producing forecasts.
#    xreg      == Optionally, a vector or matrix of external regressors.  
#                   Must have the same number of rows as y. Must be numeric.
#    lambda    == Box-Cox transformation parameter. If lambda = 'auto', then a 
#                   transformation is automatically selected using BoxCox.lambda

# RUN NEURAL NETWORK MODELS     
fcst_Neural_TimeSeries <- nnetar(tsObjectDaily, lambda = 'auto')

# RUN NEURAL NETWORK FORECASTS     
prdct_Neural_TimeSeries <- forecast(fcst_Neural_TimeSeries, sum(is.na(forecastData$Volume)))

# PLOT RESULTS
# autoplot(prdct_Neural_TimeSeries)
```

This next section exports all of the time-series results.  The predictions are added to the forecastData dataframe and then exported into their own dataframes.  
```
################################################## 
# EXPORT TIME-SERIES RESULTS (DAILY)    
################################################## 
# CREATE LIST OF FORECASTS     
forecastList <- ls(pattern = '^prdct_')
forecastDFs <- lapply(ls(pattern = '^prdct_'), function(x) get(x))

# LOOP FORECASTS, FIT IN DATAFRAME     
for (i in 1:length(forecastList)) {
     assign(paste0('df_', paste0(forecastList[i], '_Results')),
            data.frame(Date = forecastData$Date,
                       Volume = forecastData[, 'TotalDeaths'],
                       Fitted = c(forecastDFs[[i]]$fitted, forecastDFs[[i]]$mean),
                       lower95 = if (is.null(forecastDFs[[i]]$lower[, 2])) {
                         rep(NA, length(forecastData$Volume))
                       } else { c(rep(NA, sum(!is.na(forecastData$Volume))), forecastDFs[[i]]$lower[, 2]) },
                       upper95 = if (is.null(forecastDFs[[i]]$lower[, 2])) {
                         rep(NA, length(forecastData$Volume))
                       } else { c(rep(NA, sum(!is.na(forecastData$Volume))), forecastDFs[[i]]$upper[, 2]) }
            )
     )
}

# LOOP FORECASTS, FIT IN DATAFRAME     
for (i in 1:length(forecastList)) {
     forecastData[, forecastList[i]] <- c(forecastDFs[[i]]$fitted, forecastDFs[[i]]$mean)
}

# CLEAN ENVIRONMENT     
rm(i)
rm(forecastDFs)
rm(forecastList)
rm(list = ls(pattern = '^prdct_'))
rm(list = ls(pattern = '^fcst_'))
```

### Regression
This section creates different time series regressions, tinkering with the strength of the time_period variable.
```
################################################## 
# TIME SERIES REGRESSIONS    
################################################## 
# REQUIRE > 1 YEAR OF DAILY DATA
# CREATE TIME SERIES REGRESSION MODELS     
tsReg_WithoutTime <- lm(Volume ~ Year + Month, data = forecastData)
tsReg_NormalTime <- lm(Volume ~ time_period + Year + Month, data = forecastData)
tsReg_TimeSquared <- lm(Volume ~ I(time_period ^ 2) + Year + Month, data = forecastData)
tsReg_RootTime <- lm(Volume ~ sqrt(time_period) + Year + Month, data = forecastData)
tsReg_LogTime <- lm(Volume ~ log(time_period, base = exp(1)) + Year + Month, data = forecastData)

# LIMIT MODELS USING STEPWISE SELECTION     
# if (AIC(tsReg_WithoutTime) != -Inf) {
#      tsReg_WithoutTime <- stepAIC(tsReg_WithoutTime, direction = 'both', trace = FALSE)
# }
# if (AIC(tsReg_NormalTime) != -Inf) {
#      tsReg_NormalTime <- stepAIC(tsReg_NormalTime, direction = 'both', trace = FALSE)
# }
# if (AIC(tsReg_TimeSquared) != -Inf) {
#      tsReg_TimeSquared <- stepAIC(tsReg_TimeSquared, direction = 'both', trace = FALSE)
# }
# if (AIC(tsReg_RootTime) != -Inf) {
#      tsReg_RootTime <- stepAIC(tsReg_RootTime, direction = 'both', trace = FALSE)
# }
# if (AIC(tsReg_LogTime) != -Inf) {
#      tsReg_LogTime <- stepAIC(tsReg_LogTime, direction = 'both', trace = FALSE)
# }
```

This section exports the time series regressions.  Like the section above, they are put into the forecastData dataframe, but they are not added to their own dataframes.
```
################################################## 
# EXPORT REGRESSION RESULTS (DAILY)    
################################################## 
# CREATE LIST OF REGRESSIONS     
tsRegressionList <- ls(pattern = '^tsReg_')
tsRegressionData <- lapply(ls(pattern = '^tsReg_'), function(x) get(x))

# LOOP REGRESSIONS, FIT IN DATAFRAME     
for (i in 1:length(tsRegressionList)) {
     forecastData[, tsRegressionList[i]] <- as.vector(predict.lm(tsRegressionData[[i]],
                                                      newdata = forecastData,
                                                      n.ahead = sum(is.na(forecastData$Volume))))
}

# CLEAN ENVIRONMENT     
rm(i)
rm(tsRegressionData)
rm(tsRegressionList)
rm(list = ls(pattern = '^tsReg_'))
```

### Bayesian Structural Time Series
https://www.rdocumentation.org/packages/bsts/versions/0.9.7/topics/bsts
```
################################################## 
# BAYESIAN STRUCTURAL TIME SERIES     
################################################## 
```
This section creates Bayesian structural time series models.  This package appears to be very powerful and you can create a lot of customized components for your model.  I spent a little time on these, but did not go overboard.  
```
# NOTES:  Uses MCMC to sample from the posterior distribution of a 
#         Bayesian structural time series model. 
# ARGUMENTS:
#    formula   == formula of model or numeric vector
#    family    == model family for observation equation
#    niter     == number of MCMC draws
#    ping      == frequency of status messages
#    State.specification 
#         AddAutoAr     =       Adds sparse AR component
#                               AR with spike-and-slab regression
#         AddLocalLevel =       Adds local linear state model component
#                               Assumes trend is random walk
#         AddLocalLinearTrend = Adds local linear trend model component
#                               Assumes mean and trend follow random walks
#         AddSeasonal =         Adds a seasonal model component
#         AddTrig =             Adds a trigonometric seasonal model component

# TRY/CATCH TO AVOID CRITICAL ERRORS
tryCatch({
     # BUILD THE MODEL COMPONENETS
     modelComponents1 <- AddLocalLevel(list(), vectorTemp)
     modelComponents1 <- AddSeasonal(modelComponents1, vectorTemp, nseasons = 52)

     # EXECUTE THE FORECAST(S)
     fcst_BayesianTimeSeries1 <- bsts(vectorTemp,
                                       state.specification = modelComponents1,
                                       family = 'gaussian',
                                       niter = 10000, ping = 0)

     # PREDICT FUTURE VALUES
     prdct_BayesianTimeSeries1 <- predict(fcst_BayesianTimeSeries1, horizon = sum(is.na(forecastData$Volume)),
                                          burn = SuggestBurn(0.25, fcst_BayesianTimeSeries1),
                                          quantiles = c(.20, .80))

     # ADD TO OUTPUT DATA
     fitted_BayesianTimeSeries1 <- (-colMeans(fcst_BayesianTimeSeries1$one.step.prediction.errors[-(1:SuggestBurn(0.25, fcst_BayesianTimeSeries1)),]) + vectorTemp)
     forecastData[, 'BayesianTSMedian1'] <- c(as.vector(fitted_BayesianTimeSeries1), as.vector(prdct_BayesianTimeSeries1$median))

     # CLEAN ENVIRONMENT
     rm(modelComponents1)
     rm(fcst_BayesianTimeSeries1)
     rm(prdct_BayesianTimeSeries1)
     rm(fitted_BayesianTimeSeries1)
}, error = function(e) { })

# TRY/CATCH TO AVOID CRITICAL ERRORS
tryCatch({
     # BUILD THE MODEL COMPONENETS
     modelComponents2 <- AddLocalLevel(list(), vectorTemp)
     modelComponents2 <- AddTrig(modelComponents2, vectorTemp, period = 52, frequencies = 1:4)

     # EXECUTE THE FORECAST(S)
     fcst_BayesianTimeSeries2 <- bsts(vectorTemp,
                                      state.specification = modelComponents2,
                                      family = 'gaussian',
                                      niter = 10000, ping = 0)

     # PREDICT FUTURE VALUES
     prdct_BayesianTimeSeries2 <- predict(fcst_BayesianTimeSeries2, horizon = sum(is.na(forecastData$Volume)),
                                          burn = SuggestBurn(0.25, fcst_BayesianTimeSeries2),
                                          quantiles = c(.20, .80))

     # ADD TO OUTPUT DATA
     fitted_BayesianTimeSeries2 <- (-colMeans(fcst_BayesianTimeSeries2$one.step.prediction.errors[-(1:SuggestBurn(0.25, fcst_BayesianTimeSeries2)),]) + vectorTemp)
     forecastData[, 'BayesianTSMedian2'] <- c(as.vector(fitted_BayesianTimeSeries2), as.vector(prdct_BayesianTimeSeries2$median))

     # CLEAN ENVIRONMENT
     rm(modelComponents2)
     rm(fcst_BayesianTimeSeries2)
     rm(prdct_BayesianTimeSeries2)
     rm(fitted_BayesianTimeSeries2)
}, error = function(e) { })

# TRY/CATCH TO AVOID CRITICAL ERRORS
tryCatch({
     # BUILD THE MODEL COMPONENETS
     modelComponents3 <- AddLocalLevel(list(), vectorTemp)
     modelComponents3 <- AddAutoAr(modelComponents3, vectorTemp)
     modelComponents3 <- AddTrig(modelComponents3, vectorTemp, period = 52, frequencies = 1:4)

     # EXECUTE THE FORECAST(S)
     fcst_BayesianTimeSeries3 <- bsts(vectorTemp,
                                      state.specification = modelComponents3,
                                      family = 'gaussian',
                                      niter = 10000, ping = 0)

     # PREDICT FUTURE VALUES
     prdct_BayesianTimeSeries3 <- predict(fcst_BayesianTimeSeries3, horizon = sum(is.na(forecastData$Volume)),
                                          burn = SuggestBurn(0.25, fcst_BayesianTimeSeries3),
                                          quantiles = c(.20, .80))

     # ADD TO OUTPUT DATA
     fitted_BayesianTimeSeries3 <- (-colMeans(fcst_BayesianTimeSeries3$one.step.prediction.errors[-(1:SuggestBurn(0.25, fcst_BayesianTimeSeries3)),]) + vectorTemp)
     forecastData[, 'BayesianTSMedian3'] <- c(as.vector(fitted_BayesianTimeSeries3), as.vector(prdct_BayesianTimeSeries3$median))

     # CLEAN ENVIRONMENT
     rm(modelComponents3)
     rm(fcst_BayesianTimeSeries3)
     rm(prdct_BayesianTimeSeries3)
     rm(fitted_BayesianTimeSeries3)
}, error = function(e) { })
```

This section is to find the best model.  The "goodness" of a model is always relative to another model, so I started by creating a dummy model that lags the row by one.  This is because t0 is usually correlated with t1, so the models should be at least as good as saying "tomorrow will be like today."
```
################################################## 
# IDENTIFY BEST FORECAST    
##################################################
# NOTE:  To identify the best forecast we will assume t0 is highly 
#        correlated with t1.  We will reject all forecasts with a fit worse
#        than a model that lags inputs by one.

# CREATE LAGGED DATA   
laggedData <- forecastData[, c(which(colnames(forecastData) == 'Date'),
                            which(colnames(forecastData) == 'Volume'))]
laggedData[, 'Lag'] <- c(c(NA), c(laggedData[1:(nrow(forecastData) - 1), 'Volume']))

# CALCULATE LAGGED ROOT MEAN SQUARED ERROR (RMSE) 
# Lagged One Time Period 
rmserr(as.vector(laggedData[218:269, 'Lag']),
       as.vector(forecastData[218:269, 'Volume']),
       summary = T)

# PLOT ACTUAL VOLUME VS PREDICTED VOLUME
ggplot() +
     geom_point(data = forecastData, aes(x = Date, y = Volume, color = 'Data')) +
     geom_line(data = forecastData, aes(x = Date, y = Volume, color = 'Data')) +
     geom_point(data = laggedData, aes(x = Date, y = Lag, color = 'Fitted')) +
     geom_line(data = laggedData, aes(x = Date, y = Lag, color = 'Fitted')) +
     scale_color_manual(values = c('Data' = 'black', 'Fitted' = 'red')) +
     labs(color = 'Legend') +
     theme_classic() +
     ylab('Weekly Deaths') +
     xlab('Date') +
     labs(title = 'Weekly Deaths vs Predicted Weekly Deaths (Lagged)')
```
![Plot4](https://user-images.githubusercontent.com/14900746/149235283-37d84ce7-415c-496c-8571-24cee62e09a3.png)


## Step 5: Compare all of the models
This section creates a dataframe with the model comparisons.  If anyone has a suggestion to clean the code here, I'm very open to suggestions. 
```
################################################## 
# CALCULATE RMSE FOR ALL FORECASTS    
##################################################
fitStats <- data.frame(list(
     Model_Name = c('Lagged (t-1)', 'ARIMA_NonSeasonal', 'ARIMA_Seasonal', 'DSHoltWinters1', 
                    'DSHoltWinters2', 'HoltWinters_Add', 'HoltWinters_Mult', 'Neural_TimeSeries', 
                    'STDecomp_ARIMA', 'STDecomp_ETS', 'STDecomp_Naive', 'STDecomp_RandomWalk', 
                    'TBATS_Trigonometric', 'TSReg-WithoutTime', 'TSReg-NormalTime', 
                    'TSReg-TimeSquared', 'TSReg-RootTime', 'TSReg-LogTime', 'BayesianTSMedian1', 
                    'BayesianTSMedian2', 'BayesianTSMedian3'),
     Seasonal = c('N/A', 'No', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes',
                  'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes',
                  'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'),
     RMSE = c(rmserr(as.vector(laggedData[218:269, 'Lag']),as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'prdct_ARIMA_NonSeasonal']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'prdct_ARIMA_Seasonal']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'prdct_DSHoltWinters1']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'prdct_DSHoltWinters2']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'prdct_HoltWinters_Add']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'prdct_HoltWinters_Mult']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'prdct_Neural_TimeSeries']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'prdct_STDecomp_ARIMA']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'prdct_STDecomp_ETS']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'prdct_STDecomp_Naive']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'prdct_STDecomp_RandomWalk']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'prdct_TBATS_Trigonometric']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'tsReg_WithoutTime']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'tsReg_NormalTime']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'tsReg_TimeSquared']),as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'tsReg_RootTime']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'tsReg_LogTime']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'BayesianTSMedian1']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'BayesianTSMedian2']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse,
              rmserr(as.vector(forecastData[218:269, 'BayesianTSMedian3']), as.vector(forecastData[218:269, 'Volume']), summary = F)$rmse),
     MAPE = c(rmserr(as.vector(laggedData[218:269, 'Lag']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'prdct_ARIMA_NonSeasonal']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'prdct_ARIMA_Seasonal']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'prdct_DSHoltWinters1']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'prdct_DSHoltWinters2']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'prdct_HoltWinters_Add']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'prdct_HoltWinters_Mult']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'prdct_Neural_TimeSeries']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'prdct_STDecomp_ARIMA']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'prdct_STDecomp_ETS']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'prdct_STDecomp_Naive']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'prdct_STDecomp_RandomWalk']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'prdct_TBATS_Trigonometric']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'tsReg_WithoutTime']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'tsReg_NormalTime']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'tsReg_TimeSquared']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'tsReg_RootTime']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'tsReg_LogTime']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'BayesianTSMedian1']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'BayesianTSMedian2']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape,
              rmserr(as.vector(forecastData[218:269, 'BayesianTSMedian3']), as.vector(forecastData[218:269, 'Volume']), summary = F)$mape)))


write.table(fitStats,
            file = 'clipboard-128',
            sep = '\t',
            row.names = F,
            col.names = T)
```

This is where I'd normally dump the results into Excel.  My favored comparisons are the root mean squared error (RMSE) and mean absolute percent error (MAPE)
```
################################################## 
# SUMMARY AND RESULTS    
##################################################
```

| MODEL NAME | SEASONAL | RMSE | MAPE | BEATS LAG | BEST |
| ------------- | ------------- | ------------- | ------------- | ------------- | ------------- |
| Lagged | N/A | 685 | 0.96% | N/A
| ARIMA_NonSeasonal | No | 654 | 0.94% | Yes |  | 
| ARIMA_Seasonal | Yes | 714 | 1.02% | No |  | 
| DSHoltWinters1 | Yes | 616 | 0.84% | Yes |  | 
| DSHoltWinters2 | Yes | 583 | 0.79% | Yes |  | 
| HoltWinters_Add | Yes | 575 | 0.78% | Yes |  | 
| HoltWinters_Mult | Yes | 577 | 0.78% | Yes |  | 
| Neural_TimeSeries | Yes | 654 | 0.90% | Yes |  | 
| STDecomp_ARIMA | Yes | 490 | 0.67% | Yes | Yes | 
| STDecomp_ETS | Yes | 499 | 0.69% | Yes |  | 
| STDecomp_Naive | Yes | 513 | 0.71% | Yes |  | 
| STDecomp_RandomWalk | Yes | 513 | 0.71% | Yes |  | 
| TBATS_Trigonometric | Yes | 563 | 0.79% | Yes |  | 
| TSReg-WithoutTime | Yes | 790 | 1.12% | No |  | 
| TSReg-NormalTime | Yes | 787 | 1.11% | No |  | 
| TSReg-TimeSquared | Yes | 779 | 1.10% | No |  | 
| TSReg-RootTime | Yes | 794 | 1.12% | No |  | 
| TSReg-LogTime | Yes | 794 | 1.13% | No |  | 
| BayesianTSMedian1 | Yes | 633 | 0.89% | Yes |  | 
| BayesianTSMedian2 | Yes | 623 | 0.90% | Yes |  | 
| BayesianTSMedian3 | Yes | 619 | 0.89% | Yes |  | 

## Step 6:  Conclusions
We now have all of the elements we need to determine the excess deaths.  We have selected our model (STDecomp_ARIMA), the confidence interval for the model, and the actual number of deaths and COVID19-related deaths
```
################################################## 
# SUMMARY AND PLOTS    
##################################################`
```
Some breif setup
```
# ADD COVID ACTUALS INTO DF
finalData <- df_prdct_STDecomp_ARIMA_Results
finalData$CovidDeaths <- forecastData$CovidDeaths
finalData$CovidDeaths[is.na(finalData$CovidDeaths)] <- 0

# CHANGE COLUMN NAME
colnames(finalData)[colnames(finalData) == 'Volume'] <- 'TotalDeaths'

# PREPARE COVID-ADJUSTED PREDICTED VOLUME
finalData$VlessC <- finalData$TotalDeaths - finalData$CovidDeaths
finalData$FitToActualRatio <- finalData$Fitted / finalData$TotalDeaths
finalData$FitToVLCRatio <- finalData$Fitted / finalData$VlessC

# REMOVE LAST SEVERAL ROWS OF DATA
# Potentially incomplete data
finalData <- finalData[1:(nrow(finalData) - 8), ]
```

The first plot will show the actual deaths vs the predicted deaths.  The black line shows actual deaths, the red line shows predicted deaths, and the gray shaded region shows the confidence interval.  
```
# PLOT ACTUAL VS PREDICTED (+FITTED) 
ggplot() +
     geom_point(data = finalData[finalData$Date >= '2019-01-01', ], aes(x = Date, y = TotalDeaths, color = 'Data')) +
     geom_line(data = finalData[finalData$Date >= '2019-01-01', ], aes(x = Date, y = TotalDeaths, color = 'Data')) +
     geom_point(data = finalData[finalData$Date >= '2019-01-01', ], aes(x = Date, y = Fitted, color = 'Fitted')) +
     geom_line(data = finalData[finalData$Date >= '2019-01-01', ], aes(x = Date, y = Fitted, color = 'Fitted')) +
#     geom_vline(xintercept = as.numeric(as.Date('2020-03-30')), color = 'blue', linetype = 2) +
#     geom_vline(xintercept = as.numeric(as.Date('2020-05-03')), color = 'blue', linetype = 2) +
#     geom_rect(aes(xmin = as.Date('2020-03-30'), xmax = as.Date('2020-12-31'), ymin = -Inf, ymax = Inf), alpha = 0.25) +
#     geom_vline(xintercept = as.numeric(as.Date('2020-08-08')), color = 'red', linetype = 2) +
#     geom_vline(xintercept = as.numeric(as.Date('2020-10-10')), color = 'red', linetype = 2) +
     geom_ribbon(data = finalData[finalData$Date >= '2019-01-01', ], alpha = 0.25, 
                 aes(x = Date, ymin = lower95, ymax = upper95)) +
     scale_color_manual(values = c('Data' = 'black', 'Fitted' = 'red')) +
     scale_x_date(date_breaks = '2 months', date_labels = '%m/%y') +
     labs(color = 'Legend') + theme_classic() +
     ylab('Weekly Deaths') + xlab('Date') +
     labs(title = 'Weekly Deaths (USA) vs Predicted Weekly Deaths (STDecomp + ARIMA)',
          subtitle = 'Shaded area is the 95% CI')
```
![Plot5](https://user-images.githubusercontent.com/14900746/149235923-b1eb0e6e-2dcb-4437-b873-3b08c7527a9e.png)

As you can see, the actual deaths far outpace the predicted deaths and exceed the confidence interval.  We can say with confidence that there were excess deaths during this time period.

The second plot shows the actual deaths without COVID19 compared to the predicted deaths.  
```
# PLOT ACTUAL LESS COVID VS PREDICTED (+FITTED) 
ggplot() +
     geom_point(data = finalData[finalData$Date >= '2019-01-01', ], aes(x = Date, y = VlessC, color = 'Data')) +
     geom_line(data = finalData[finalData$Date >= '2019-01-01', ], aes(x = Date, y = VlessC, color = 'Data')) +
     geom_point(data = finalData[finalData$Date >= '2019-01-01', ], aes(x = Date, y = Fitted, color = 'Fitted')) +
     geom_line(data = finalData[finalData$Date >= '2019-01-01', ], aes(x = Date, y = Fitted, color = 'Fitted')) +
#     geom_vline(xintercept = as.numeric(as.Date('2020-03-30')), color = 'blue', linetype = 2) +
#     geom_vline(xintercept = as.numeric(as.Date('2020-05-03')), color = 'blue', linetype = 2) +
#     geom_rect(aes(xmin = as.Date('2020-03-30'), xmax = as.Date('2020-05-03'), ymin = -Inf, ymax = Inf), alpha = 0.25) +
#     geom_vline(xintercept = as.numeric(as.Date('2020-08-08')), color = 'red', linetype = 2) +
#     geom_vline(xintercept = as.numeric(as.Date('2020-10-10')), color = 'red', linetype = 2) +
     geom_ribbon(data = finalData[finalData$Date >= '2019-01-01', ], alpha = 0.25, 
                 aes(x = Date, ymin = lower95, ymax = upper95)) +
     scale_color_manual(values = c('Data' = 'black', 'Fitted' = 'red')) +
     scale_x_date(date_breaks = '2 months', date_labels = '%m/%y') +
     labs(color = 'Legend') + theme_classic() +
     ylab('Weekly Deaths') + xlab('Date') +
     labs(title = 'Weekly Deaths (USA) less COVID vs Predicted Weekly Deaths (STDecomp + ARIMA)',
          subtitle = 'Shaded area is the 95% CI')
```
![Plot6](https://user-images.githubusercontent.com/14900746/149236055-76f6bf31-0a7e-49a5-93f0-fc796fa4c42d.png)

As you can see, most of the outliers have cleared away.  This means we can confidently attribute these excess deaths to COVID19.  However, from 03/28/2020 - 04/25/2020 , 07/04/2020 - 09/26/2020, 10/10/2020, 10/17/2020, 11/07/2020, 12/12/2020, 07/03/2021, and 07/31/2021 - 10/02/2021, COVID19 alone does not explain the excess number of deaths.  

## Potential Improvements
In the future, I could make several improvements.  We could add exogenous factors that flag the earliest appearance of COVID19, the Delta Variant, and the Omicron Variant.  I could make the time-series and Bayesian structural time series models more robust.  We could also test additional models.  

Other areas to explore would be looking for the impact of lockdown and stay-at-home orders (however, these typically happen locally and would be hard to extrapolate to entire states).  We could look for additional data to explore accidents and suicides, or the trends with different causes of death (cardiac, pulmonary, etc.).  
