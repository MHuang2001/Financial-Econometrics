#---------------------------------------------------------------------------------------------------------------------------
# ECMT2130: Assignment script 6
#
# Script to Assess trading algorithm profitability
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
# Compute profits from trading
#---------------------------------------------------------------------------------------------------------------------------

# Access the actual prices data and risk free return data
prices <- readRDS(file = "rawClosingPriceData.rds")

# Access the risk free return and monthly adjusted equity return data.
adjustedReturns <- readRDS(file = "allData.rds")

# Make sure we are using the same names for the columns in the price data that we are using for the weights
# The names for the prices data now seem a bit misleading but this change makes it possible to 
# easily match up prices with portfolio weights.
riskFreeReturnName <- c("rf")
stockNames <- colnames(prices)[!grepl(riskFreeReturnName, colnames(prices))]

# Load the portfolio weights data (one row per rebalance date) - weights add to 1 across the row.
portfolioWeights <- readRDS(file = "heldbackPortfolioWeights.rds")

# Determine the list of rebalance dates (one for each row in the history of portfolio weights)
rebalanceDates <- as.Date(index(portfolioWeights))

# Define the transaction cost for buying/selling a dollar of any of the risky assets.
transactionCost = 0.0012

# Define the initial value of the portfolio - required for transaction cost calculation.
portfolioValue <- 10000000

# Declare and set the dimensions of variables that will track investments over time.
# These will be updated at each rebalance date.
weights <- portfolioWeights[1,] * 0 # Just use the first row of the portfolio weights to get the dimensions right.
shareHoldings <- portfolioWeights[1,stockNames] * 0
shareEarnings <- portfolioWeights[1,stockNames] * 0
debt <- 0
debtEarnings <- 0

# Declare the extended time-series variables that will be used to 
# store investment performance data for all of the rebalance dates.
transactionCostsXTS <- NULL
shareHoldingsXTS <- NULL
shareEarningsXTS <- NULL
debtXTS <- NULL
debtEarningsXTS <- NULL
portfolioValueXTS <- NULL

# At each balance date:
# - update the portfolio value
# - determine the new balances required for the various assets
# - determine the amounts to be bought and sold for each asset to get to new weights.
# - determine the transaction costs associated with the trades.
for (i in 1:length(rebalanceDates)) {
  
  # Determine the rebalance date
  rebalanceDate <- as.Date(rebalanceDates[i])
  
  # Get the adjusted stock returns over the previous month - as decimals rather than percentages.
  currentAdjustedReturns <- adjustedReturns[rebalanceDate,stockNames] / 100
  
  # Get the actual closing prices of the stocks at the rebalance date.
  currentPrices <- prices[rebalanceDate,stockNames]

  # Update the portfolio value for earnings over the last month if we are already invested (if i > 1)
  if (i > 1) {
    
    # Get the date of the previous rebalance - earnings will be based on time elapsed since then.
    previousDate <- as.Date(rebalanceDates[i-1])
    
    # Get the prices at the start of the earnings computation period.
    previousEquityPrices <- prices[as.Date(rebalanceDates[i-1]),stockNames]
    
    # Get the $ values invested in each stock at the start of the earnings computation period
    initialStockValues = (as.matrix(shareHoldings) * as.matrix(previousEquityPrices))
    
    # Apply the adjusted rate of return (to include dividends etc.) to the initial stock values.
    shareEarnings = as.matrix(initialStockValues) * (as.matrix(currentAdjustedReturns))
    
    # Compute the earnings from lending/borrowing at the risk-free rate of return    
    previousRiskFreeReturn <- prices[as.Date(rebalanceDates[i-1]),]$rf / 100
    debtEarnings <- debt * previousRiskFreeReturn
    
    # Compute the value of the portfolio at the end of the earnings computation period
    portfolioValue =  as.double(debt + debtEarnings) + sum(as.matrix(initialStockValues)) + sum(as.matrix(shareEarnings))
    
  }
  
  # Get the weights required by the trading algorithm as at the portfolio rebalance date
  weights <- portfolioWeights[rebalanceDate]

  # Compute how many shares are to be held in each stock in the rebalanced portfolio
  # Ignore fractions of shares - they are too small to have a material influence on results.
  newEquityValues <- (as.matrix(weights[,stockNames]) * as.double(portfolioValue))
  newShareHoldings <- newEquityValues / as.matrix(currentPrices)

  # Compute the transaction costs involved in the purchases and sales of shares required to achieve rebalancing 
  transactionCosts <- transactionCost * (abs(as.matrix(newShareHoldings) - as.matrix(shareHoldings)) * as.matrix(currentPrices))

  # Compute the new debt level (Not that a negative value means borrowing and a positive value means lending)  
  # and paying for the transactions costs out of cash that would otherwise return the risk free rate
  debt <- as.double(weights$rf) * as.double(portfolioValue) - sum(as.matrix(transactionCosts))

  portfolioValue <- portfolioValue - sum(as.matrix(transactionCosts))
    
  # Check that everythings is being reinvested or spent on transaction costs (to within 1 cent)
  stopifnot(abs(portfolioValue - debt - sum(newEquityValues)) < 0.01)
  
  # Update the weights to reflect their new values, ready to step forward to the next rebalance date
  shareHoldings <- newShareHoldings
  
  # Store the performance data in extended time series objects for detailed analysis - e.g. transaction costs by stock.
  if (i == 1) {
    transactionCostsXTS <- xts(transactionCosts, order.by=rebalanceDate)
    shareHoldingsXTS <- xts(shareHoldings, order.by=rebalanceDate)
    debtXTS <- xts(debt, order.by=rebalanceDate)
    shareEarningsXTS <- xts(shareEarnings, order.by=rebalanceDate)
    debtEarningsXTS <- xts(debtEarnings, order.by=rebalanceDate)
    portfolioValueXTS <- xts(portfolioValue, order.by=rebalanceDate)
  } else {
    transactionCostsXTS <- rbind(transactionCostsXTS, xts(transactionCosts, order.by=rebalanceDate) )
    shareHoldingsXTS <- rbind(shareHoldingsXTS, xts(shareHoldings, order.by=rebalanceDate) )
    shareEarningsXTS <- rbind(shareEarningsXTS, xts(shareEarnings, order.by=rebalanceDate) )
    debtXTS <- rbind(debtXTS, xts(debt, order.by=rebalanceDate) )
    debtEarningsXTS <- rbind(debtEarningsXTS, xts(debtEarnings, order.by=rebalanceDate) )
    portfolioValueXTS <- rbind(portfolioValueXTS, xts(portfolioValue, order.by=rebalanceDate) )
  }
  
}

# View transaction costs (in $ by stock)
plot(transactionCostsXTS)

# View interest earnings on loans at risk free rate (or interest expenses on borrowings, again at risk free rate)
plot(debtEarningsXTS)

# View the portfolio value over the analysis period
plot(portfolioValueXTS)

# Compute and plot monthly portfolio return
portfolioReturnsXTS <- 100 * diff(portfolioValueXTS, lag = 1, differences = 1, arithmetic = TRUE, na.pad = TRUE) / lag(portfolioValueXTS, lag = 1, na.pad = TRUE)
plot(portfolioReturnsXTS)

# Compute the average monthly return on the portfolio
mean(portfolioReturnsXTS, na.rm = TRUE)

# Compute the portfolio return standard deviation
sd(portfolioReturnsXTS, na.rm = TRUE)

