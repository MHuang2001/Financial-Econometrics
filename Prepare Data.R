#---------------------------------------------------------------------------------------------------------------------------
# ECMT2130: Assignment script 2
#
# Script to generate comparable continuously compounded rates of return from the raw data
#---------------------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------------------
# Load all of these libraries into the environment before running any of the scripts in this assignment!!!
#---------------------------------------------------------------------------------------------------------------------------

# Log progress to help with debugging.
# install.packages("futile.logger")
library(futile.logger)

# Ensure that the logger reports debug messages.
flog.threshold(DEBUG)

# Make it easier to handle dates
# install.packages("lubridate")
library(lubridate)

# Make it easier to get data from Yahoo Finance and other sources.
# install.packages("quantmod")
library(quantmod)

# read in interest rates from Excel
# install.packages("readxl")
library(readxl)

# Work with extended time series
# install.packages("xts")
library(xts)

# Plot correlation matrices
# install.packages("corrplot")
library(corrplot)

# Do portfolio optimisation
# install.packages(c("PortfolioAnalytics", "ROI", "ROI.plugin.quadprog"))
library(ROI)
library(ROI.plugin.quadprog)
library(PortfolioAnalytics)

# Just show one chart per page
par(mfrow = c(1, 1)) 

#---------------------------------------------------------------------------------------------------------------------------
# Data preparation
#---------------------------------------------------------------------------------------------------------------------------


# Restore the object
data <- readRDS(file = "rawAdjustedPriceData.rds")

# Get the names of the stock prices.
stockPriceNames <- colnames(data)[grep(".AX$", colnames(data))]

# Plot the adjusted stock prices on the one timeseries plot.
plot(data[,stockPriceNames])

# Plot the risk free rate of return as a timeseries.
plot(data$rf)

# Difference the stock price data
stockPriceChangesData <- diff(data[,stockPriceNames], lag = 1, differences = 1, arithmetic = TRUE, na.pad = TRUE)

# Get the lagged stock price data
stockPriceLagData <- lag(data[,stockPriceNames], lag = 1, na.pad = TRUE)

# Compute simple monthly stock price returns - in percent
stockPriceReturnsData <- 100 * stockPriceChangesData / stockPriceLagData

stockPriceReturnsData <- na.omit(stockPriceReturnsData)

# Split the data into a training set and a held back set.
trainingTimespan <- "20190101/"


# Plot the stock returns data
plot(stockPriceReturnsData[trainingTimespan])
plot(stockPriceReturnsData$CSL.AX[trainingTimespan])
# Plot the average monthly stock price returns
averageMonthlyStockReturns <- colMeans(stockPriceReturnsData[trainingTimespan])
barplot(averageMonthlyStockReturns, names.arg=rownames(averageMonthlyStockReturns), main="Average monthly returns", xlab = "Stocks", las=2, cex.names=0.5)



# Plot the standard deviations
averageMonthlyStockReturnStdDev <- StdDev(stockPriceReturnsData[trainingTimespan])
barplot(averageMonthlyStockReturnStdDev, names.arg=colnames(averageMonthlyStockReturnStdDev), main="Monthly return standard deviations", xlab = "Stocks", las=2, cex.names=0.5)

# na.omit removes any row (observation) from the data set where there is a missing value in any column.
# This ensures that rebalances are done as close to month end as is possible, given constraints on data availability.
preparedData = na.omit(merge(data$rf,stockPriceReturnsData))




saveRDS(preparedData[trainingTimespan], file = "trainingData.rds")
saveRDS(preparedData, file = "allData.rds")





