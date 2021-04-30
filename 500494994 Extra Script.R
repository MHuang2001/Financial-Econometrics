# SID 500494994

#INPUT DATA
##RANK THE INDIVIDUAL STOCK BY THEIR AVERAGE MONTHLY EXCESS RETURNS OVER THE RISK FREE RATE
xsReturns <- stockPriceReturnsData
for (i in stockPriceNames) {
  xsReturns[, i] <- averageMonthlyStockReturns - averageRF
}
xsReturns

averageExcess <- colMeans(xsReturns)

averageExcessStdDev <- StdDev(xsReturns)

sharperatio = averageExcess/averageExcessStdDev
barplot(sort(sharperatio))

##RANK THE INDIVIDUAL STOCK BY THEIR MONTHLY EXCESS RETURN STANDARD DEVIATIONS
averageMonthlyStockReturnStdDev <- StdDev(stockPriceReturnsData[trainingTimespan])
barplot(sort(averageExcessStdDev), names.arg=colnames(averageExcessStdDev), main="SD Excess Return", xlab = "Stocks", las=1.5, cex.names=1)

barplot(sort(averageExcess), names.arg=colnames(averageExcess), main="Average Monthly Excess Return", xlab = "Stocks", las=1.5, cex.names=1)

barplot(sort(sharperatio), names.arg=colnames(sharperatio), main="Sharpe Ratio", xlab = "Stocks", las=1.5, cex.names=1)




#Correlation
(correlations <- cor(data))
corrplot(correlations, method = "number", col=col4(20))
title("Correlations 2000-2010", line = 3)  

correlations2 <- cor(preparedData)
corrplot(correlations2, method = 'number', col = col4(20))
title("Correlations 2000-2020", line = 3)

col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue")) 
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))
whiteblack <- c("white", "black")

corrdifference <- cor(preparedData) - cor(data)
corrplot(corrdifference, method = 'number', col = col4(20))
title("Correlations Difference", line = 3)

sd(data$rf)

#Stability over time of the distribution of excess returns
barplot(AverageExcess)
AverageRF2 <- colMeans(data$rf10)
AverageExcess2 <- averageMonthlyStockReturns - AverageRF2
barplot(AverageExcess2)
#prepared data 2000-2020
#data 2000-2010

#How many missing rows
sum(is.na(preparedData2))

#QQPLOT

qqnorm(xsReturns$CBA.AX) #1
qqline(xsReturns$CSL.AX, main = "Normal Q-Q Plot CSL.AX") 
qqnorm(xsReturns$CSL.AX, main = "Normal Q-Q Plot CSL.AX Excess Returns")
qqnorm(xsReturns$BHP.AX)
qqnorm(xsReturns$SUN.AX)
qqnorm(xsReturns$QAN.AX)
qqnorm(xsReturns$WES.AX)
qqnorm(xsReturns$TLS.AX) 
qqnorm(xsReturns$WOW.AX)
qqnorm(xsReturns$ANZ.AX)
qqnorm(xsReturns$NAB.AX)
qqline(xsReturns$WBC.AX, main = "Normal Q-Q Plot WBC.AX Excess Returns")




# Read data from spreadsheet in same folder as this R script and convert to an XTS
rf10 <- read_excel("30dayBBSW.xlsx")
rf10 <- xts(rf$FIRMMBAB30D, order.by = as.Date(rf$date))
rf10 <- rf["20101101/20200630"]

plot(rf10)

#Histogram plus normal curve
hist(averageMonthlyStockReturns, freq=FALSE, col="gray", xlab="Average Monthly Stock Return", main="Average Monthly Stock Return Distribution (2000-2020)")
curve(dnorm(x, mean=mean(averageMonthlyStockReturns), sd=sd(averageMonthlyStockReturns)), add=TRUE, col="blue")


hist(averageExcess, freq=FALSE, col="gray", xlab="Average Monthly Excess Return", main="Average Monthly Excess Return Distribution (2000-2020)")
curve(dnorm(x, mean=mean(averageExcess), sd=sd(averageExcess)), add=TRUE, col="blue")
qqnorm(averageExcess)



#T Test
t.test(averageMonthlyStockReturns)


#Shapiro Wilk Test for Normality
shapiro.test(averageMonthlyStockReturns)
hist(data)
AverageExcess

20191231

#VAR==========================================================
#Historical Simulation

portfolioWeights <- readRDS(file = "heldbackPortfolioWeights.rds")
varWeights <- tail(portfolioWeights, 1)
adjustedReturns <- readRDS(file = "allData.rds")
timespan <- "20100101/20191231"
historicalReturns <- adjustedReturns[timespan]

# Use the seq function to get the list of possible row indices. We will randomly draw indices next. 
sample.space <- seq(from=1, to=nrow(historicalReturns))

# Randomly choose days from history using a uniform distribution across possible days in the available history.
# This gives us the indices in our simulation - use each one to get the list of rates of return for the various investments.
sampleIndices <- sample(sample.space, 
                        size = simulations, 
                        replace = TRUE, 
                        prob = rep((1/length(sample.space)), length(sample.space))
)

# Define vector where historical simulation results will be held.
simulatedPortfolioValues = vector(length = simulations)

i <- 1
for (t in sampleIndices) {
  # Get the randomly chosen monthly returns
  randomDrawOfHistoricalReturns <- historicalReturns[t,]
  # Note that the colProds function has been removed because we are not needing to compound daily returns to form a return over a longer period.
  # Note the matrix transpose used to multiply returns by weights
  simulatedPortfolioValues[i] <- ((1+randomDrawOfHistoricalReturns/100) %*% t(varWeights)) * 10000000
  i <- i + 1
}
hist(simulatedPortfolioValues)

(historicalSimulationVaR <- quantile(x = simulatedPortfolioValues, probs = c(0.01)))

#Portfolio Value

plot(portfolioValueXTS)
 

