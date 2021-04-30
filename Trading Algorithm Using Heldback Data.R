#---------------------------------------------------------------------------------------------------------------------------
# ECMT2130: Assignment script 5
#
# Script to run the trading algorithm including Markowitz portfolio optimisation.
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
# Algorithm implementation using training data
#---------------------------------------------------------------------------------------------------------------------------

# Access the training data
data <- readRDS(file = "allData.rds")

# Get the list of dates on which we will try to form a portfolio
endDates <- as.Date(index(data["20190101/"]))

# Partition variable names into the risk free return and all the risky returns.
# help(grepl)
# Get all variable names except for BBSW.Return.
riskFreeAssetName <- c("rf")
riskyAssetNames <- colnames(data)[!grepl(riskFreeAssetName, colnames(data))]

plot(data$rf)

# Create the timespan variable to store the timespan in the global environment
timespan <- NULL

# Get starting weights

#' Initialise a new portfolio optimisation configuration
#' @param The names of the risky assets to form the portfolio from
#' @return the base portfolio definition
basePortfolio <- function(riskyAssetNames) {
  portfolio <- portfolio.spec(assets=riskyAssetNames)
  portfolio <- add.constraint(portfolio, type="full_investment")
  portfolio <- add.constraint(portfolio=portfolio, type="long_only")
  portfolio <- add.objective(portfolio=portfolio, type="risk", name="StdDev")
  portfolio
}

#' Define the function to compute the negative sharpe ratio given a required expected return
#' @param expectedReturn the expected return of the portfolio
#' @return the negative sharpe ratio of the efficient portfolio with the given expected return.
negativeSharpeRatio <- function(expectedReturn) {
  
  # flog.debug("Required expected return when computing the sharpe ratio = %s", expectedReturn)
  
  portfolio <- basePortfolio(riskyAssetNames)
  portfolio <- add.constraint(portfolio=portfolio, type="return", return_target=expectedReturn)
  
  optimisedPortfolio <- optimize.portfolio(R=data[timespan], portfolio=portfolio, optimize_method="ROI", trace=TRUE)
  #flog.debug("sharpe ratio weights: %s", toString(optimisedPortfolio$weights))

  standardDeviation <- as.double(optimisedPortfolio$objective_measures$StdDev)
  #flog.debug("Standard deviation of portfolio returns = %s", standardDeviation)
  
  # Handle cases where the expected return cannot be achieved given portfolio constraints
  if (is.na(standardDeviation)) { # Tends to happen for extreme expected returns when a box constraint is binding.
    return(expectedReturn^2) # Penalise impossible expected return choices more as they become larger 
  }
  
  riskFreeReturn = as.double(last(data[,riskFreeAssetName][timespan])) # Use the most recent risk free return.
  #flog.debug("Risk free return = %s", riskFreeReturn)
  
  sharpeRatio = (expectedReturn - riskFreeReturn) / standardDeviation
  #flog.debug("Sharpe ratio = %s", sharpeRatio)
  
  -sharpeRatio
}

# Define the newWeights function:
#' Update the tangency portfolio
#' 
#' @param newTimespan the timespan over which to do the analysis needed for the new weights
#' @return the details of the tangency portfolio

newTangencyPortfolio <- function() {

    #flog.debug("Getting tangency portfolio weights for %s", newTimespan)
    portfolio <- basePortfolio(riskyAssetNames)
    minimumVariancePortfolio <- optimize.portfolio(R=data[timespan], portfolio=portfolio, optimize_method="ROI", trace=TRUE) 
    initialMeanReturnValue <- as.double(t(minimumVariancePortfolio$weights) %*% colMeans(data[,riskyAssetNames][timespan],na.rm = TRUE))
    #flog.debug("Initial required expected return = %.2f", initialMeanReturnValue)
    optimisationResults <- optim(initialMeanReturnValue, negativeSharpeRatio, method="BFGS")
    tangencyPortfolioDefinition <- add.constraint(portfolio=portfolio, type="return", return_target=optimisationResults$par)
    optimisedTangencyPortfolio <- optimize.portfolio(R=data[timespan], portfolio=tangencyPortfolioDefinition, optimize_method="ROI", trace=TRUE)
    c(optimisedTangencyPortfolio, optimisationResults$par)
}

#---------------------------------------------------------------------------------------------------------------------------
# Iterate the portfolio re-evaluation days - recomputing portfolio weights and reporting them each time.
#---------------------------------------------------------------------------------------------------------------------------

# Set the portfolio criteria - target return subject to a ceiling on leverage (how much can be borrowed to fund equity)
targetReturn = 3 # percent per month...
maximumLeverage = 0

# Initialise variable to store the portfolio weights over time.
weightsXTS = NULL
for (i in 1:(length(endDates)-1)) {
  
  result = tryCatch({
    
    endDate <- as.Date(endDates[i])
    startDate <- as.Date(endDate) %m-% months(36)

    # timeperiod used for return analysis unlying the portfolio optimisation that drives rebalances
    timespan <- paste(format(startDate, "%Y%m%d"),"/",format(as.Date(endDate), "%Y%m%d"), sep = "")
    
    newTangencyPortfolioDetails <- newTangencyPortfolio()
    newTangencyPortfolioWeights <- newTangencyPortfolioDetails[1]$weights
    
    newTangencyPortfolioStdDev <- newTangencyPortfolioDetails$objective_measures$StdDev
    newTangencyPortfolioExpectedReturn <- as.double(newTangencyPortfolioDetails[[11]])

    # Get timespan for single observation at rebalance date so we can get the risk free rate at that date.
    endDateTimeSpan <- paste(format(endDate, "%Y%m%d"),"/",format(as.Date(endDate), "%Y%m%d"), sep = "")
    riskFreeReturn <- data[endDateTimeSpan]$rf
    
    flog.debug("For %s, E(r_tangency) = %.2f and r_f = %.2f", timespan, newTangencyPortfolioExpectedReturn, riskFreeReturn)

    # Base risk free asset holdings on target return  but impose a maximum amount that can be borrowed.  
    riskFreeAssetWeight = max(maximumLeverage, (newTangencyPortfolioExpectedReturn - targetReturn) / (newTangencyPortfolioExpectedReturn - riskFreeReturn))
    riskyAssetsCombinedWeight = 1 - riskFreeAssetWeight

    # Zero out weights that are sufficiently close to zero.
    newTangencyPortfolioWeights <- newTangencyPortfolioWeights * as.double(riskyAssetsCombinedWeight)

    (allAssetWeights <- c(as.double(riskFreeAssetWeight), newTangencyPortfolioWeights))
    names(allAssetWeights)[1] <- "rf"

    allAssetWeights[abs(allAssetWeights) < 0.001] <- 0
    allAssetWeights <- allAssetWeights / sum(allAssetWeights)

    tempXTS <- xts(t(allAssetWeights), order.by=endDate)
    if (is.null(weightsXTS)) {
      weightsXTS <-tempXTS
    } else {
      weightsXTS <- rbind(weightsXTS, tempXTS )
    }

  }, finally = {
    
    # clean up code.
    
  })
  
}

# Function to generate a colour palette to use in the graph of the target portfolio weights over time.
myColors <- function(g){
  d <- 360/g
  h <- cumsum(c(15, rep(d,g - 1)))
  hcl(h = h, c = 100, l = 65)
}

# Plot the portfolio weights over time.
chart.StackedBar(weightsXTS, date.format="%Y", cex.legend = 0.7, colorset=myColors(20))

saveRDS(weightsXTS, file = "heldBackPortfolioWeights.rds")

