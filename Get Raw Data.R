#---------------------------------------------------------------------------------------------------------------------------
# ECMT2130: Assignment script 1
#
# Script to generate the data that you want to work with. Adapt it to restrict yourself to the portfolio 
# you will be working with based upon the assignment instructions.
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

#---------------------------------------------------------------------------------------------------------------------------
# Retrieve data from Yahoo Finance for a list of symbols using quantmod library.
# https://rstudio-pubs-static.s3.amazonaws.com/288543_ca65993139974727bc5accc3a5acd290.html#make-getsymbols-return-the-data-it-retrieves
# https://cran.r-project.org/web/packages/quantmod/quantmod.pdf
#---------------------------------------------------------------------------------------------------------------------------

# Select the stocks to download - revise this to reflect the last 3 digits of your SIC.
symbols <- c(
  "CBA.AX", "CSL.AX", "BHP.AX", "WBC.AX", "NAB.AX", "ANZ.AX", "WOW.AX", "TLS.AX", "WES.AX", 
 "QAN.AX", "SUN.AX")

# Do the download for each symbol (lapply calls the function and passes in each value in the symbols list)
stocks = lapply(symbols, function(symbol) {
  info <- getSymbols(symbol,
                     src="yahoo",
                     .GlobalEnv,
                     return.class = 'xts',
                     index.class  = 'Date',
                     from = "2000-11-01",
                     to = "2020-06-30",
                     periodicity = "daily",
                     curl.options = list(),
                     auto.assign = FALSE)
})

# Combine the data for all the different symbols.
equityData <- do.call(merge, stocks)

# Remove all the columns except the closing prices and rename them
equityClosingPriceData <- equityData[,grep(".Close$", colnames(equityData))]
names(equityClosingPriceData) <- gsub(".Close", "", colnames(equityClosingPriceData))

# Remove all the columns except the adjusted prices and rename them.
equityAdjustedPriceData <- equityData[,grep(".Adjusted$", colnames(equityData))]
names(equityAdjustedPriceData) <- gsub(".Adjusted", "", colnames(equityAdjustedPriceData))

# Read data from spreadsheet in same folder as this R script and convert to an XTS
rf <- read_excel("30dayBBSW.xlsx")
rf <- xts(rf$FIRMMBAB30D, order.by = as.Date(rf$date))
rf <- rf["20001101/20200630"]

# RF CHANGING TIME SERIES

# Replace all missing risk free rate values with the value from the preceding day - they don't move fast so this should be OK.
rf <- na.locf(rf, fromLast = TRUE)

# Eyeball the annualised risk free rate of return (in percent)
plot(rf)

# De-annualise the risk free rate of return 
rf <- rf / 12

# Merge the 3 datasets, matching rows by date.
dates <- seq(as.Date("2000-11-01"), as.Date("2020-06-30"), by = "day")
zeros <- rep(0, length(dates))
allDaysData <- xts(zeros, order.by = dates)
names(allDaysData) <- "zeros"

# Merge the risk free data with the equity price data sets.
combinedAdjustedPriceData <- merge(allDaysData, rf, equityAdjustedPriceData)
combinedClosingPriceData <- merge(allDaysData, rf, equityClosingPriceData)

# Remove the column of zeros in the data set.
combinedAdjustedPriceData$zeros <- NULL
combinedClosingPriceData$zeros <- NULL

# Remove the rows that are for weekends (redundant given next line that removes all 'na' values)
combinedAdjustedPriceData <- combinedAdjustedPriceData[!weekdays(index(combinedAdjustedPriceData)) %in% c("Saturday", "Sunday")]
combinedClosingPriceData <- combinedClosingPriceData[!weekdays(index(combinedClosingPriceData)) %in% c("Saturday", "Sunday")]

# Remove dates (rows) where there is missing equity return data.
combinedAdjustedPriceData <- na.omit(combinedAdjustedPriceData)
combinedClosingPriceData <- na.omit(combinedClosingPriceData)

# Get the data for the last trading day in each month
combinedAdjustedPriceData <- combinedAdjustedPriceData[c(diff(as.numeric(substr(index(combinedAdjustedPriceData), 9, 10))) < 0, TRUE), ]
combinedClosingPriceData <- combinedClosingPriceData[c(diff(as.numeric(substr(index(combinedClosingPriceData), 9, 10))) < 0, TRUE), ]

# Save the data ready to feed your trading algorithm
saveRDS(combinedAdjustedPriceData, file = "rawAdjustedPriceData.rds")
saveRDS(combinedClosingPriceData, file = "rawClosingPriceData.rds")





