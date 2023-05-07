# Script leads to large objects and so was not saved as RData file.
library(xts)
library(PerformanceAnalytics)
library(quantmod)
library(Quandl)
library(magrittr)
library(tseries)

## INTRODUCTION TO PORTFOLIO ANALYSIS
# Create the data in DC exercise:
getSymbols(Symbols = c("KO", "PEP"), 
           src = "yahoo", 
           auto.assign = T, 
           from = "2003-01-01", to = "2016-08-31", periodicity = "daily") # Use the full "Date" character.
# With multiple Symbols, auto.assign = T

# Create a list with both xts objects as elements:
KO_PEP <- list("KO" = KO, "PEP" = PEP)

head(KO_PEP$KO) # Check that we have OHLC data
head(KO_PEP$PEP)

# Create a list with each element an xts object with equity prices normalized to 1.00 at 2003-01-02. 
lst <- lapply(X = KO_PEP, FUN = function(x){
   Ad(x) / as.numeric(Ad(x)["2003-01-02"])
})

ko_pep_adj <- do.call(what = merge, args = lst) # xts, zoo

plot.zoo(x = ko_pep_adj, plot.type = "single", main = "Normalized stock prices", xlab = "Date", 
         ylab = "Normalized price", col = c("red", "blue"))
legend(x = "topleft", legend = c("Coca-Cola (KO)", "PepsiCo (PEP)"), lty = 1, col = c("red", "blue"))

# Using the normalized adjusted prices of KO and PEP, calculate KO / PEP and plot:
ko_pep_adj$KO_PEP_ratio <- ko_pep_adj$KO.Adjusted / ko_pep_adj$PEP.Adjusted

plot.zoo(ko_pep_adj$KO_PEP_ratio, col = "blue", lwd = 1.5, main = "KO - PEP Ratio")
abline(h = 1, col = "red", lwd = 2)

# Therefore, KO is a better investment than PEP. 
# Symbols
symbols <- c("AAPL", "MSFT", "IBM")

## Another algorithm for data wrangling is create new environment -> eapply -> do.call. This is helpful when you
## load data from multiple sources, extract the same column/row/list from the data and then cbind, rbind or
## merge the extracted elements.
# Create new environment 
data_env <- new.env() # The benefits of using new.env() is that changes will be consistent throughout. 

# Load symbols into data_env
getSymbols(Symbols = symbols, src = "yahoo", auto.assign = TRUE, env = data_env)

# Extract the close column from each object and combine into one xts object
close_data <- do.call(merge, eapply(data_env, Cl))

# View the head of close_data
head(close_data)

## Practice using the quantmod and PerformanceAnalytics package:
# Get OHLF data using quantmod
getSymbols(Symbols = c("AAPL", "MSFT"), src = "yahoo", auto.assign = T, 
           from = "2006-01-03", to = "2016-08-31", periodicity = "daily")

# Extract closing price and merge into single dataset
AAPL_MSFT <- list("AAPL" = AAPL, "MSFT" = MSFT)
lst <- lapply(X = AAPL_MSFT, FUN = Cl)
aapl_msft <- do.call(what = merge, args = lst)

# Use Performance Analytics package to get returns for AAPL and MSFT and for a portfolio with AAPL and MSFT:
aapl_msft_returns <- Return.calculate(aapl_msft) # CalculateReturns() is the same function
head(aapl_msft_returns)
aapl_msft_returns <- aapl_msft_returns[-1, ]

# Portfolio returns with equal weight and no rebalancing:
ptfl_bh <- Return.portfolio(R = aapl_msft_returns, weights = c(.5, .5))

# Portfolio returns with equal weight and monthly rebalancing:
ptfl_mnth_rebal <- Return.portfolio(R = aapl_msft_returns, weights = c(.5, .5), rebalance_on = "month")

# Plot: Notice that the rebalanced monthly plot has less volatility in its returns
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
plot.zoo(x = ptfl_bh, main = "Balance and Hold Portfolio")
plot.zoo(x = ptfl_mnth_rebal, main = "Monthly Rebalanced Portfolio")
par(mfrow = c(1, 1))

## By setting the argument verbose = TRUE in Return.portfolio() you can create a list of beginning of period
## (BOP) and end of period (EOP) weights and values in addition to the portfolio returns, and contributions.
## View the str() of the object created by Return.portfolio() to see what is available.
ptfl_bh <- Return.portfolio(R = aapl_msft_returns, weights = c(.5, .5), verbose = T)
str(ptfl_bh) # List of 6

ptfl_mnth_rebal <- Return.portfolio(R = aapl_msft_returns, weights = c(.5, .5), 
                                    rebalance_on = "month", verbose = T)

# Plot the end of period (EOP) weights: Notice that with no rebalancing, the weights of the AAPl increase
# since its relative value increases over time, while with monthly rebalancing the weights oscillate around 0.5. 
plot.zoo(ptfl_bh$EOP.Weight, plot.type = "single", col = c("blue", "red"), 
         main = "Portfolio End of Period Weights (no rebalancing)")
legend(x = "topleft", legend = c("AAPL End of Period Weights", "MSFT End of Period Weights"), 
       col = c("blue", "red"), lty = 1, cex = .7)

plot.zoo(ptfl_mnth_rebal$EOP.Weight, plot.type = "single", col = c("blue", "red"), 
         main = "Portfolio End of Period Weights (monthly rebalancing)")
legend(x = "topright", legend = c("AAPL End of Period Weights", "MSFT End of Period Weights"), 
       col = c("blue", "red"), lty = 1, cex = .7)

## The reward and risk of a portfolio are measured by the mean (arithmetic, geometric) and the sd. Use the
## S&P500 data to calculate.
sp500_mnth_ohlc <- getSymbols(Symbols = "^GSPC", src = "yahoo", auto.assign = F,
                         from = "1986-01-01", to = "2016-08-31", periodicity = "monthly") 
# Sets the time stamp to the first of the month but uses the last of the months' obs.

sp500_ohlc <- getSymbols(Symbols = "^GSPC", src = "yahoo", auto.assign = F,
                         from = "1986-01-01", to = "2016-08-31") 
sp500_mnth_cl <- Cl(to.monthly(x = sp500_ohlc, indexAt = "yearmon"))
# Time stamp is "yearmon" and the last obs of the month is used. 

# Calculate and plot the returns:
sp500_mnth_rtns <- Return.calculate(prices = sp500_mnth_cl)
names(sp500_mnth_rtns) <- "SP500_Returns"
plot.zoo(sp500_mnth_rtns, main = "S&P 500 Monthly Returns January 1986 - August 2016")

# For annual returns by month (cf Jan 2015 and Jan 2016) and by the entire year:
table.CalendarReturns(R = sp500_mnth_rtns)

# Now using the sp500_mnth_rtns, calculate the mean (geometric and arithmetic) and sd:
mean(sp500_mnth_rtns, na.rm = T)
mean.geometric(sp500_mnth_rtns)
sd(sp500_mnth_rtns, na.rm = T)

## Calculate the Sharpe ratio on the portfolio using 1 month US Treasury rate
## Download the risk free rate = One month US Treasury rate
one_mnth_ust <- Quandl(code = "FRED/DGS1MO", type = "xts", collapse = "month")
# The Quandl historical data goes only as far as July 1, 2001. Use the data from DC: 
library(readr)
one_mnth_ust <- read_csv(file = "us_treasury.csv", col_names = T, 
                        col_types = cols(Index = col_date(format = "%Y-%m-%d")))
one_mnth_ust <- xts(x = one_mnth_ust$Value, order.by = as.yearmon(one_mnth_ust$Index))

# Add the Jan 1986 returns data from DC to the S&P 500 monthly returns data:
sp500_mnth_rtns[1, 1] <- 0.002366528

# Calculate the excess returns, or risk premium, of the risky portfolio: S&P 500, over the risk free asset: 
# 1 month US Treasury, then calculate the arithmetic mean 
rsk_prem <- sp500_mnth_rtns - one_mnth_ust
mean(rsk_prem); mean(sp500_mnth_rtns)

# Now compute the Sharpe ratio:
sp500_mnth_sharpe <- mean(rsk_prem) / sd(sp500_mnth_rtns)

# In portfolio analysis, we prefer annualized rates. Transform the monthly rates to annual rates. 
one_mnth_ust_annlzd <- (1 + one_mnth_ust)^12 - 1

## Compute the annualized return, volatility and Sharpe ratio of the S&P 500 monthly returns 
# Annualized returns from monthly data:
mean.geometric(x = sp500_mnth_rtns + 1)^12 - 1
prod(1 + sp500_mnth_rtns)^(12 / length(sp500_mnth_rtns)) - 1
# Strangely, the two codes give different results. The latter matches the results of Return.annualized().

# Annualized volatility from monthly data:
sd(sp500_mnth_rtns) * sqrt(12)

# Annualized Sharpe ratio:
((prod(1 + sp500_mnth_rtns)^(12 / length(sp500_mnth_rtns)) - 1) - 
   (prod(1 + one_mnth_ust)^(12 / length(one_mnth_ust)) - 1)) / (sd(sp500_mnth_rtns) * sqrt(12))

## Now using the PerformanceAnalytics package calculate annualized returns, volatility, and the Sharpe ratio:
(sp500_annl_rtns <- Return.annualized(sp500_mnth_rtns, scale = 12))
(sp500_annl_vol <- StdDev.annualized(sp500_mnth_rtns))
(Return.annualized(sp500_mnth_rtns, scale = 12) - Return.annualized(one_mnth_ust, scale = 12)) / 
   StdDev.annualized(sp500_mnth_rtns)
(sp500_annl_ShR <- SharpeRatio.annualized(R = sp500_mnth_rtns, Rf = one_mnth_ust, scale = 12))
# Note that the calculated Sharpe Ratio and the result using the function are different. 

# Use a PerformanceAnalytics function to get the same result:
table.AnnualizedReturns(R = sp500_mnth_rtns, scale = 12, Rf = one_mnth_ust)

## Rolling annualized returns and volatility. Recall that we can use the rollapply() function.
# Create a plot with the rolling annualized returns, annualized volatility and Sharpe ratio
chart.RollingPerformance(R = sp500_mnth_rtns, width = 12, FUN = "Return.annualized", scale = 12)
chart.RollingPerformance(R = sp500_mnth_rtns, width = 12, FUN = "StdDev.annualized")
chart.RollingPerformance(R = sp500_mnth_rtns, widht = 12, FUN = "SharpeRatio.annualized", 
                         Rf = one_mnth_ust, scale = 12)

## Working with daily S&P 500 data, look at how the S&P 500 performed in the year 2008
# Create the daily returns and add January 2, 1986 data via DC
sp500_dly_rtns <- Return.calculate(prices = Cl(sp500_ohlc))
sp500_dly_rtns[1, 1] <- -0.007998878

# Create two subsets: 2008 and 2014 and plot them using the chart.Histogram() function
par(mfrow = c(1, 2))
chart.Histogram(R = sp500_dly_rtns["2008"], breaks = 40, main = "S&P 500 Daily Returns for 2008", 
                xlab = "Returns", ylab = "Frequency", methods = c("add.density", "add.normal"))
# Note that the methods = "add.density" added a non-parametric density plot.
chart.Histogram(R = sp500_dly_rtns["2014"], breaks = 40, main = "S&P 500 Daily Returns for 2014", 
                xlab = "Returns", ylab = "Frequency", methods = c("add.density", "add.normal"))
par(mfrow = c(1, 1))

## The problem with the above analysis is the use of sd() and the assumption of normality in returns.
## Portfolio assets tend to increase and have thicker tails; in short, they are left-skewed and have non
## negative excess kurtosis. We quantify these "downside risk measures" via the semi-deviation, 5% value at
## risk and the 5% expected shortfall.

# Before looking at downside measures of risk, let's look at some measures of the distribution of returns,
# namely, skewness and kurtosis (all univariate normal distros have kurtosis = 3).
# First, plot density of the daily and monthly S&P 500 returns:
par(mfrow = c(1, 2))
chart.Histogram(R = sp500_dly_rtns, breaks = 40, main = "S&P 500 Daily Returns", 
                xlab = "Returns", ylab = "Frequency", methods = c("add.density", "add.normal"))
chart.Histogram(R = sp500_mnth_rtns, breaks = 40, main = "S&P 500 Monthly Returns", 
                xlab = "Returns", ylab = "Frequency", methods = c("add.density", "add.normal"))
par(mfrow = c(1, 1))

# Check the overall distribution of daily and monthly returns for the S&P 500
# Check the skewness (perfectly symmetric would have skewness = 0)
skewness(x = rnorm(2000)) # Close to 0
skewness(x = sp500_dly_rtns)
skewness(x = sp500_mnth_rtns)

# Check the kurtosis (excess kurtosis is reported, meaning 3 has already been subtracted = normal distribution
# would have a kurtosis of 0):
kurtosis(x = rnorm(2000)) # Close to 0.
kurtosis(x = sp500_dly_rtns) # 20.88228
kurtosis(x = sp500_mnth_rtns) # 2.436516

# The standard deviation gives equal weight to positive and negative returns in calculating the return
# variability. When the return distribution is asymmetric (skewed), investors use additional risk measures
# that focus on describing the potential losses. One such measure of downside risk is the Semi-Deviation. The
# Semi-Deviation is the calculation of the variability of returns below the mean return.
SemiDeviation(R = sp500_mnth_rtns)

# Another more popular measure is the so-called Value-at-Risk (or VaR). Loosely speaking, the VaR corresponds
# to the 5% quantile of the return distribution, meaning that a more negative return can only happen with a
# probability of 5%.
VaR(R = sp500_mnth_rtns, p = .05)

# The expected shortfall is another measure of risk that focuses on the average loss below the 5% VaR quantile.
ES(R = sp500_mnth_rtns, p = .05)

# The volatility, semi-deviation, value-at-risk, and expected shortfall are all measures that describe risk
# over 1 period. These metrics do not do a great job at describing the worst case risk of buying at a peak,
# and selling at a trough. This sort of risk can be quantified by analyzing the portfolio's drawdowns, or
# peak-to-trough decline in cumulative returns.

# To use table.Drawdowns() and chart.Drawdown(), the rownames must have standard date formats. 
index(sp500_mnth_rtns) <- as.Date(index(sp500_mnth_rtns))
table.Drawdowns(R = sp500_mnth_rtns) # Five largest drawdown episodes over the sample period. 
chart.Drawdown(R = sp500_mnth_rtns) # Visualize the drawdowns. 

## Use the components of a portfolio to evaluate portfolio performance. 
bond_prices <- readRDS("bond_prices.RData")
eq_prices <- readRDS("eq_prices.RData")
comm_prices <- readRDS("comm_prices.RData")
re_prices <- readRDS("re_prices.RData")

# First, portfolio performance is determined by the returns of the individual assets in the portfolio.
# Calculate bond and equity returns and a portfolio of 40% bonds and 60% equities returns:
bond_rtns <- Return.calculate(prices = bond_prices) %>%
   na.omit()
eq_rtns <- Return.calculate(prices = eq_prices) %>%
   na.omit()
bond_eq_4060_rtns <- .4 * bond_rtns + .6 * eq_rtns

# Check to see that the portfolio returns are equal to the weighted component returns:
all.equal(mean(bond_eq_4060_rtns), .6 * mean(eq_rtns) + .4 * mean(bond_rtns)) # TRUE

# Second, weights of the individual assets determine the performance of a portfolio: there exists, given
# historic returns, the optimal weight which maximizes risk adjusted returns, or the Sharpe ratio.
# Calculate the optimal weight for two assets and with a riskfree comparison equal to 0
grid <- seq(from = 0, to = 1, by = .01)
vec_ShR <- vector(mode = "numeric", length = length(grid))
for (i in 1:length(grid)) {
   weight <- grid[i]
   prtfl_rtns <- weight * eq_rtns + (1 - weight) * bond_rtns
   vec_ShR[i] <- SharpeRatio.annualized(R = prtfl_rtns, Rf = 0, scale = 12)
}
plot(x = grid, y = vec_ShR, xlab = "Weights", ylab = "Annualized Sharpe Ratio")
abline(v = grid[vec_ShR == max(vec_ShR)], col = "red", lwd = 3)
grid[vec_ShR == max(vec_ShR)] # 0.09

grid <- seq(from = 0, to = 1, by = .01)
vec_ShR <- vector(mode = "numeric", length = length(grid))
for (i in 1:length(grid)) {
   weight <- grid[i]
   prtfl_rtns <- weight * eq_rtns + (1 - weight) * bond_rtns
   vec_ShR[i] <- SharpeRatio.annualized(R = prtfl_rtns, Rf = one_mnth_ust, scale = 12)
}
plot(x = grid, y = vec_ShR, xlab = "Weights", ylab = "Annualized Sharpe Ratio")
abline(v = grid[vec_ShR == max(vec_ShR)], col = "red", lwd = 3)
grid[vec_ShR == max(vec_ShR)] # 0.77

# Third, portfolio performance is determined by the covariance of the asset returns. The greater the
# covariance, the greater the portfolio volatility.
# Check the correlation of the two assets using a plot:
chart.Scatter(x = bond_rtns, y = eq_rtns, xlab = "Bond returns", ylab = "Equity returns")

# There doesn't seem to be a strong correlation, but use cor() to quantify:
cor(x = bond_rtns, y = eq_rtns) # .06

# Chart the correlations:
chart.Correlation(R = merge(bond_rtns, eq_rtns), method = "pearson", histogram = T)

# Instead of static correlations, let's look at rolling correlations:
chart.RollingCorrelation(Ra = bond_rtns, Rb = eq_rtns, width = 24)

## Now extend our analysis to a portfolio with n assets: 
# We'll be dealing with four assets: US equity ETS (SPY), US bond ETS (AGG), US real estate ETS (VNQ) and GSCI
# commodities index (GSG).

# First plot the relative values of the four assets. Note that all of the four price data have been adjusted
# s.t. the first month's value is equal to 1.
becr_adj_cl <- merge(bond_prices, eq_prices, comm_prices, re_prices)
# Note that there are four NAs because of the mismatch in time stamps at the beginning of the data.
becr_adj_cl[1, 3] <- 1.00
becr_adj_cl <- becr_adj_cl[-2, ]
which(is.na(becr_adj_cl)) # integer(0)

plot.zoo(x = becr_adj_cl, plot.type = "single", xlab = "Date", ylab = "Relative price", 
         main = "Relative price of AGG, SPY, VNQ and GSG", col = c("black", "blue", "red", "orange"))
legend(x = "topleft", legend = c("Bonds (AGG)", "Equities (SPY)", "Commodities (GSG)", "Real estate (VNQ)"), 
       col = c("black", "blue", "red", "orange"), lty = 1, cex = .7)

# Create a single xts, zoo object with the returns of all four assets:
becr_rtns <- Return.calculate(becr_adj_cl) %>%
   na.omit()
becr_rtns <- as.xts(becr_rtns)
colnames(becr_rtns) <- c("bonds", "equities", "commodities", "real_estate")

# As a first pass, check the static returns and volatility over the course of the data:
(becr_mean_rtns <- sapply(X = becr_rtns, FUN = mean)) # vector of length 4
(becr_vol <- sapply(X = becr_rtns, FUN = sd))

# Create a risk-reward plot:
plot(x = becr_vol, y = becr_mean_rtns, ylim = c(-0.01, 0.01))
text(x = becr_vol, y = becr_mean_rtns, labels = names(becr_rtns), cex = .8, pos = 1)
abline(h = 0, lty = 2, col = "blue")

# Create a covariance and correlation matrices for becr_rtns
becr_cov <- cov(becr_rtns)
becr_cor <- cor(becr_rtns)

# Check that cov(asset_1, asset_2) = sd(asset_1) * sd(asset_2) * cor(asset_1, asset_2)
becr_vol %*% t(becr_vol) * becr_cor
becr_cov
# The two matrices are the same, however, all.equal() function does not work here. 

# Set the weights and calculate the mean return and volatility of the portfolio:
wgts <- as.matrix(c(.4, .4, .1, .1))
(becr_ptfl_rtn <- t(wgts) %*% becr_mean_rtns)
sqrt((becr_ptfl_vol <- t(wgts) %*% becr_cov %*% wgts)) # sqrt of the portfolio variance.

# Calculate each asset's risk contribution to the portfolio. Note that if the risk contribution is greater
# than the asset's portfolio weight, then it is relatively more risky asset.
# See https://faculty.washington.edu/ezivot/econ424/riskbudgetingslides.pdf for very useful review of risk
# budgeting and the calculation of risk contributions behind PerformanceAnalytics' StdDev() call.
becr_rsk_budget <- StdDev(R = becr_rtns, portfolio_method = "component", weights = t(wgts))
# weights = transpose because the number of columns must equal the ncols of R. 
str(becr_rsk_budget)
capital_riskpct_budget <- cbind(wgts, becr_rsk_budget$pct_contrib_StdDev)
colnames(capital_riskpct_budget) <- c("capital_budget", "risk_budget_perc")
print(capital_riskpct_budget)

## Portfolio optimization using the DJIA. Invest in a portfolio made up of the DJIA composite stocks. Optimize
## return and risk whilst fully investing in the portfolio (weights should sum to 1).
djia <- readRDS("prices.rds")
djia_rtns <- Return.calculate(prices = djia) %>%
   na.omit()

# Plot the risk-return of each of the component DJIA stocks:
djia_comp_mean_rtns <- sapply(X = djia_rtns, FUN = mean)
djia_comp_sds <- sapply(X = djia_rtns, FUN = sd)
plot(x = djia_comp_sds, y = djia_comp_mean_rtns, main = "DJIA Component Equity Risk vs Return", 
     xlab = "Standard deviation (monthly)", ylab = "Average return (monthly)", xlim = c(0.03, .15), 
     ylim = c(0.005, 0.025))
text(x = djia_comp_sds, y = djia_comp_mean_rtns, labels = colnames(djia_rtns), pos = 1, cex = .6)

# Create a vector of row means:
djia_eqwgt_rtns <- apply(X = djia_rtns, MARGIN = 1, FUN = mean)
djia_eqwgt_rtns <- xts(x = djia_eqwgt_rtns, order.by = index(djia_rtns))
mean(djia_eqwgt_rtns) # 0.01249652
plot.zoo(djia_eqwgt_rtns, xlab = "Months", ylab = "Returns", main = "DJIA equal weight portfolio returns")

## A mean-variance efficient portfolio can be obtained as the solution of MINIMIZING the portfolio variance
## under the constraint that the portfolio expected return equals a target return. A convenient R function for
## doing so is the function portfolio.optim() in the R package tseries. Its default implementation finds the
## mean-variance efficient portfolio weights under the constraint that the portfolio return equals the return
## on the equally-weighted portfolio.
# Use the default (equal weight portfolio return = target return; minimize volatility):
opt <- portfolio.optim(x = djia_rtns)
str(opt) 
opt$pm # .01249652 NOTE THAT THIS IS EQUAL TO mean(djia_eqwgt_rtns) above. 
opt$ps # .0355865
# $pw: portfolio weights; $px: portfolio returns; $pm: expected portfolio return; $ps: sd of portfolio returns

djia_wgts <- opt$pw
names(djia_wgts) <- colnames(djia_rtns)
djia_opt_wgts <- djia_wgts[djia_wgts >= 0.01]
barplot(height = djia_opt_wgts, ylab = "Weight", names.arg = names(djia_opt_wgts))

# Note that above, the target return was the return equal to an equally weighted portfolio. What happens when
# you increase the portfolio's target return?
# Note that mean(djia_rtns) = mean(djia_eqwgt_rtns) = opt$pm. 
pf_mean <- portfolio.optim(x = djia_rtns, pm = mean(djia_rtns))
pf_10plus <- portfolio.optim(x = djia_rtns, pm = mean(djia_rtns) * 1.1)
# Calculate the change in volatility:
(pf_10plus$ps - pf_mean$ps) / pf_mean$ps # Volatility increased by almost 8%

## Investors are often constrained by the maximum values allowed for the portfolio weights. The advantage of a
## maximum weight constraint is that the subsequent portfolio will be less concentrated in certain assets.
## The disadvantage is that the same target return may no longer be possible or will be obtained at the expense
## of a higher volatility.
# Set the maximum weights. Recall that djia_opt_wgts had several assets with weights greater than 0.1 and many
# greater than .05
max_wgts1 <- rep(x = 1, ncol(djia_rtns))
max_wgts2 <- rep(x = .1, ncol(djia_rtns))
max_wgts3 <- rep(x = .05, ncol(djia_rtns))

# Create optimum portfolios with the above weights (return target set to equal weighting returns):
opt1 <- portfolio.optim(x = djia_rtns, pm = mean(djia_rtns), reshigh = max_wgts1)
opt2 <- portfolio.optim(x = djia_rtns, pm = mean(djia_rtns), reshigh = max_wgts2)
opt3 <- portfolio.optim(x = djia_rtns, pm = mean(djia_rtns), reshigh = max_wgts3)
opt1$pm; opt2$pm; opt3$pm # equal to mean(djia_rtns)

# How many assets in each portfolio have a weight greater than 1%? 
sum(opt1$pw > .01); sum(opt2$pw > .01); sum(opt3$pw > .01)
# As the maximum weight decreases, the number of assets that are invested in increases.

opt1$ps; opt2$ps; opt3$ps
# Portfolio standard deviation / volatility increases. 

# Create the risk-return efficient frontier:
# Create a grid of returns from min to max of DJIA average returns:
grid <- seq(from = .01, to = max(djia_comp_mean_rtns), length.out = 50)

# Create empty vectors for calculated means and sds and empty matrix for calculated weights:
vec_pm <- vector(mode = "numeric", length = length(grid))
vec_psd <- vector(mode = "numeric", length = length(grid))
wgt_matrix <- matrix(data = 0, nrow = 50, ncol = 30)

# Create a for loop:
for (i in 1:length(grid)) {
   opt <- portfolio.optim(x = djia_rtns, pm = grid[i])
   vec_pm[i] <- opt$pm
   vec_psd[i] <- opt$ps
   wgt_matrix[i, ] <- opt$pw
}

all.equal(vec_pm, grid) 
# TRUE. Note that we didn't need to create the empty vector for portfolio mean because we set the target
# return in the portfolio.optim() call.

plot(x = vec_psd, y = vec_pm, type = "l", main = "Efficient frontier for DJIA constituent stocks", 
     xlab = "Standard deviation / volatility (monthly)", ylab = "Average return (monthly)", 
     ylim = c(0.005, .025))
text(x = djia_comp_sds, y = djia_comp_mean_rtns, labels = colnames(djia_rtns), pos = 1, cex = .6)

# The optimized portfolios often are also not realistic because they tend to invest large weights in only a
# few assets. A practical solution to avoid this is to impose weight constraints.

# Using vec_pm, vec_psd and wgt_matrx, identify the portfolio with the least volatility and the portfolio with
# the greatest Sharpe ratio.
# Find minimum volatility portfolio:
min_sd_prtfl <- wgt_matrix[vec_psd == min(vec_psd), ]
names(min_sd_prtfl) <- colnames(djia_rtns)


# Find the maximum Sharpe Ratio assuming that the risk free rate is .75%:
vec_SR <- (vec_pm - .0075) / vec_psd
max_SR_prtfl <- wgt_matrix[vec_SR == max(vec_SR), ]
names(max_SR_prtfl) <- colnames(djia_rtns)

# Use barplots to plot the two portfolios (where the weights are greater than 1%) and compare:
par(mfrow = c(2, 1))
barplot(height = min_sd_prtfl[min_sd_prtfl > .01], col = "lightblue", 
        main = "Portfolio weights for minimum volatility")
barplot(height = max_SR_prtfl[max_SR_prtfl > .01], col = "lightgreen", 
        main = "Portfolio weights for maximum Sharpe ratio")
par(mfrow = c(1, 1))
# As noted above, the portfolios on the efficient frontier with greater returns, tend to have more
# concentrated asset allocations.

## To check the validity / error of the calculated portfolio weights, use a training sample and a test sample.
## Recall that there is a cross validation procedure in the forecast package.
periodicity(djia_rtns) # Monthly from 1991-01-31 to 2015-12-31
train_djia <- djia_rtns["1991-01-01/2003-12-31"]
test_djia <- djia_rtns["2004-01-01/2015-12-31"]
djia_train_prtfl <- portfolio.optim(x = train_djia, reshigh = max_wgts2)
djia_test_prtfl <- portfolio.optim(x = test_djia, reshigh = max_wgts2)

# To test the errors, we could calculate the RMSE, however, without a comparison, the exercise is not very
# useful. Therefore, plot the weights.
djia_train_prtfl$pw; djia_test_prtfl$pw
plot(x = djia_train_prtfl$pw, y = djia_test_prtfl$pw, type = "p", xlab = "Training set weights", 
     ylab = "Testing set weights", main = "Training versus testing dataset optimum portfolio weights", 
     ylim = c(-.02, .11), xlim = c(-.01, .11))
text(x = djia_train_prtfl$pw, y = djia_test_prtfl$pw, labels = colnames(djia_rtns), pos = 1, cex = .6)
abline(a = 0, b = 1, col = "red", lwd = 2)
# Note that the training and testing set optimum weights differ markedly. 

# Check the performance of the optimum weights created on the training data, djia_train_prtfl. See how much
# the mean returns differ from another model. We'd use the predict function for statistical models. Here we
# use Return.portfolio() with monthly rebalancing to calculate the returns.
djia_train_rtns <- Return.portfolio(R = train_djia, weights = djia_train_prtfl$pw, 
                                    rebalance_on = "months", verbose = T)
str(djia_train_rtns) 
djia_test_rtns <- Return.portfolio(R = test_djia, weights = djia_train_prtfl$pw, 
                                   rebalance_on = "months", verbose = T)

# Compare the annualized returns:
table.AnnualizedReturns(R = djia_train_rtns$returns, scale = 12) 
# .194, annualized returns, .1325 SD, and 1.4634 SR
table.AnnualizedReturns(R = djia_test_rtns$returns, scale = 12)
# .0864, annualized returns, .1242 SD, and .6954 SR
# As expected the optimal weights result in lower returns and lower SR. 

## INTERMEDIATE PORTFOLIO ANALYSIS
## The first chapter covers classic portfolio theory (Markowitz, 1952) using the PortfolioAnalytics package:
library(PortfolioAnalytics)
data(package = "PortfolioAnalytics") # Provides list of datasets in a given package. 
data("indexes", package = "PortfolioAnalytics") # Load data of 6 indices.
index_sub <- indexes[, 1:4] # "US Bonds, US Equities, Int'l Equities, and Commodities" indices. 

# Solve for the optimum portfolio given index_sub. To use PortfolioAnalytics, we have to create a
# portfolio specification object using the portfolio.spec() and add.constraint(), add.objective() calls:
# Once created, we can use the optimize.portfolio() call. 

# Add the assets to the portfolio:
port_spec <- portfolio.spec(assets = colnames(index_sub))
class(port_spec) # portfolio.spec and portfolio

# Constraints of the optimization problem:
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")

# Optimization choice variables:
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev") 
# name argument should be a function.
port_spec

# Solve the optimization:
library(ROI); library(ROI.plugin.glpk); library(ROI.plugin.quadprog)
opt <- optimize.portfolio(index_sub, portfolio = port_spec, optimize_method = "ROI")
opt
chart.RiskReward(object = opt, return.col = "mean", risk.col = "StdDev", chart.assets = T)

# Get the optimal weights: The weights are considered optimal in the sense that the set of weights minimizes
# the objective value, portfolio standard deviation, and satisfies the full investment and long only
# constraints based on historical data.
opt$weights # extractWeights() call works as well
chart.Weights(opt)

## Now let's look at what goes under the hood of the optimize.portfolio() function. 
## Solvers: 1) closed form (e.g. quadratic programming); 2) global (e.g. differential evolution optimization). 

# Quadratic programming: we can use the quadprog package's solve.QP() call, but have to set up the problem s.t.
# solve.QP(Dmat, dvec, Amat, bvec, meq=0, factorized=FALSE), where:
# \min(-\vec(d)^T \vec(b) + \frac{1}{2} \vec(b)^T D \vec(b)) s.t.
# A^T \vec(b) \geq b_0
# This will solve for the \vec(b)

# Using the index_sub data, set up the arguments to solve.QP(). Note that the optimum weights vector, 
# \vec(b)^* is 4 x 1 vector. 
Amat <- cbind(1, diag(ncol(index_sub)), -diag(ncol(index_sub)))
t(Amat) 
bvec <- b_nought <- c(1, rep(0, ncol(index_sub)), -rep(1, ncol(index_sub)))
# result is that b_i \geq 0 and -w_i \geq -1 => w_i \leq 1, which means that w_i \in [0, 1] \forall i. Further
# b_1 + ... + b_4 \geq 1
dvec <- colMeans(index_sub)
Dmat <- 10 * cov(index_sub) # This means that \lambda (the risk aversion factor is set to 5). 
meq <- 1 # The first constraint is an inequality constraint; b_1 + ... + b_4 = 1
# I've set up the problem, but haven't called solve.QP(), which would require installing package.

# Now use PortfolioAnalytics package to solve the quadratic programming:
# Create a portfolio specification object:
port_spec2 <- portfolio.spec(assets = colnames(x = index_sub))

# Add the full investment and long only constraints:
port_spec2 <- add.constraint(portfolio = port_spec2, type = "full_investment")
port_spec2 <- add.constraint(portfolio = port_spec2, type = "long_only")

# Add the objectives:
port_spec2 <- add.objective(portfolio = port_spec2, type = "return", name = "mean")
port_spec2 <- add.objective(portfolio = port_spec2, type = "risk", name = "var", risk_aversion = 10) 
# Note that the "risk" measure is the var() call and not StdDev() call. 

# Optimize: This problem can be solved by a quadratic programming solver so we specify "ROI". 
opt2 <- optimize.portfolio(R = index_sub, portfolio = port_spec2, optimize_method = "ROI", trace = T)
chart.Weights(object = opt, main = "Optimal weights")

# Now change the risk_aversion to 1 (less risk averse):
port_spec2_2 <- portfolio.spec(assets = colnames(x = index_sub))

# Add the constraints:
port_spec2_2 <- add.constraint(portfolio = port_spec2_2, type = "full_investment")
port_spec2_2 <- add.constraint(portfolio = port_spec2_2, type = "long_only")

# Add the objectives:
port_spec2_2 <- add.objective(portfolio = port_spec2_2, type = "return", name = "mean")
port_spec2_2 <- add.objective(portfolio = port_spec2_2, type = "risk", name = "var", risk_aversion = 1) 
# Note that the "risk" measure is the var() call and not StdDev() call. 

# Optimize: This problem can be solved by a quadratic programming solver so we specify "ROI". 
opt2_2 <- optimize.portfolio(R = index_sub, portfolio = port_spec2_2, optimize_method = "ROI", trace = T)
chart.Weights(object = opt2_2, main = "Optimal weights")
# With risk_aversion = 10, US equities weight decreases markedly and US Bonds increases compared to when
# risk_aversion = 1.

## Portfolio optimization workflow using PortfolioAnalytics package:
## 1) Create the portfolio specification; 2) add constraints; 3) add objectives; 4) optimization; 5) analyze
## optimization output.

# For this chapter, use the edhec dataset from PerformanceAnalytics package, which contains monthly returns 
# for 13 assets:
data(package = "PerformanceAnalytics")
data("edhec", package = "PerformanceAnalytics")

## 1) Create the portfolio spec with portfolio.spec()
port_spec3 <- portfolio.spec(assets = colnames(edhec)) # 13 different assets

## 2) Add constraints 
port_spec3 <- add.constraint(portfolio = port_spec3, type = "weight_sum", min_sum = 1, max_sum = 1) 
# type = "full_investment"
port_spec3 <- add.constraint(portfolio = port_spec3, type = "box", 
                            min = c(rep(.1, 5), rep(.05, ncol(edhec) - 5)), max = .4)
port_spec3 <- add.constraint(portfolio = port_spec3, type = "group", groups = list(c(1, 5, 7, 9, 10, 11), 
                                                                                 c(2, 3, 4, 6, 8, 12)), 
                            group_min = .4, group_max = .6)
print(port_spec3)

## 3) Add objectives. Recall that the name = argument must be a valid R function. The functions will usually
## come from base R or PerformanceAnalytics, but can also be user defined functions.
port_spec4 <- portfolio.spec(assets = colnames(edhec))
port_spec4 <- add.objective(portfolio = port_spec4, type = "return", name = "mean")
port_spec4 <- add.objective(portfolio = port_spec4, type = "risk", name = "StdDev")
port_spec4 <- add.objective(portfolio = port_spec4, type = "risk_budget", name = "StdDev", 
                            min_prisk = .05, max_prisk = .1)
port_spec4
# Recall that StdDev(R, portfolio_method = "component", weights = ) will result in a list with $contribution
# of each asset to the portfolio sd and $pct_contrib_StdDev to portfolio sd.

## 4) Optimize. In this example, use a global solver ("random"). In order to use the "random" solver, you need
## to create random portfolios. Note that this method is akin to the Monte Carlo method.
port_spec5 <- portfolio.spec(assets = colnames(edhec))
port_spec5 <- add.constraint(portfolio = port_spec5, type = "full_investment")
port_spec5 <- add.constraint(portfolio = port_spec5, type = "long_only")
port_spec5 <- add.objective(portfolio = port_spec5, type = "return", name = "mean")
port_spec5 <- add.objective(portfolio = port_spec5, type = "risk", name = "StdDev")
port_spec5 <- add.objective(portfolio = port_spec5, type = "risk_budget", name = "StdDev", 
                            min_prisk = .05, max_prisk = .1)
port_spec5

# Create the random portfolios. Note that for the full_investment and long_only constraints, we need to use
# the "simplex" random portfolio method. 
rp5 <- random_portfolios(portfolio = port_spec5, permutations = 500, rp_method = "simplex")
# Creates a matrix

# Optimize:
opt5 <- optimize.portfolio(R = edhec, 
                           portfolio = port_spec5, 
                           optimize_method = "random", 
                           rp = rp5, 
                           trace = T)
print(opt5)
# Results are very different from DC. 
# The call also results in the following warning:
# Leverage constraint min_sum and max_sum are restrictive, 
# consider relaxing. e.g. 'full_investment' constraint should be min_sum=0.99 and max_sum=1.01

# Therefore, relax the full_investment constraint:
port_spec5_2 <- port_spec5
port_spec5_2 <- add.constraint(portfolio = port_spec5_2, 
                               type = "weight_sum", min_sum = .99, max_sum = 1.01, 
                               indexnum = 1)
opt5_2 <- optimize.portfolio(R = edhec, portfolio = port_spec5_2, 
                             optimize_method = "random", rp = rp5, trace = T)
print(opt5_2)
## The results do not match DC results, which have equal weights on all 13 assets. Furthermore, relaxing the 
## "full investment" condition does not change the optimal weights. Therefore, in the future, ignore warning.

opt5_rebal <- optimize.portfolio.rebalancing(R = edhec, portfolio = port_spec5, 
                                             optimize_method = "random", rp = rp5, 
                                             trace = T, 
                                             search_size = 1000, 
                                             rebalance_on = "quarters", 
                                             training_period = 60, 
                                             rolling_window = 60)
# search_size = 20000 is the default. DC reduced to 1000. 
# Note that opt and opt_rebalance are huge objects. 
print(opt5_rebal) # Again the solutions differ from DC. 
head(extractWeights(object = opt5_rebal), 20)

## 5) Analyze: We need to analyze via extracting measures and plotting values from opt and opt_rebalance.
# Extact the objective measures for both the static and the quarterly rebalancing portfolios:
extractObjectiveMeasures(object = opt5)
head(extractObjectiveMeasures(object = opt5_rebal))
# port_spec specified three objectives: mean monthly returns, risk as measured by sd, and risk_budgeting

# Extract the weights for the static and quarterly rebalancing portfolios:
extractWeights(object = opt5)
chart.Weights(object = opt5, plot.type = "line") # "line" is the default

extractWeights(object = opt5_rebal)
chart.Weights(object = opt5_rebal)

## Moment estimation and objective functions: 
## The default method for estimating portfolio moments is the sample method. The moments are calculated in
## optimize.portfolio() by evaluating the function passed to the momentFUN argument. The default for momentFUN
## is set.portfolio.moments() which defaults to calculating the sample moments. The moments are then used as
## inputs to the objective functions. The moments that must be estimated depend on the objectives. For
## example, an objective to minimize portfolio standard deviation requires only an estimate of the second
## moment. Compare that to the objective to maximize Sharpe Ratio which requires the first and second moments
## to be estimated.

# Create a new portfolio specification:
port_spec6 <- portfolio.spec(assets = names(edhec))

# If we set the objective to be maximizing mean returns, then the set.portfolio.moment() call will calculate
# only the first moment of the assets using the sample method.
port_spec6 <- add.objective(portfolio = port_spec6, type = "return", name = "mean")
moments6 <- set.portfolio.moments(R = edhec, portfolio = port_spec6, method = "sample") # method is the default
str(moments6) # list of 1: $mu

# Check that the first moment calculated by set.portfolio.moments() is equal to that calculated manually:
colMeans(edhec) == moments6$mu

# Add a risk objective measured by StdDev():
port_spec6 <- add.objective(portfolio = port_spec6, type = "risk", name = "StdDev")
moments6_2 <- set.portfolio.moments(R = edhec, portfolio = port_spec6, method = "sample")
str(moments6_2) # list of 2: mu and sigma (covariance matrix)
# Check that the second moment calculated by set.portfolio.moments() is equal to that calculated manually:
moments6_2$sigma == cov(edhec)

# Let's create new portfolio specifications to practice the Boudt method:
port_spec_boudt <- portfolio.spec(assets = colnames(edhec))
port_spec_boudt <- add.objective(portfolio = port_spec_boudt, type = "risk", name = "StdDev")
moments_boudt <- set.portfolio.moments(R = edhec, portfolio = port_spec_boudt, method = "boudt", k = 3)
str(moments_boudt) # list of 2: mu and sigma

# Check that the output of set.portfolio.moments() is equal to the PCA output from statistical.factor.model()
fit <- statistical.factor.model(R = edhec, k = 3) # Uses principal component analysis and outputs a list
moments_boudt$sigma == extractCovariance(fit)

# Custom moment functions using the MASS package's robust covariance function: cov.rob() with methods "mve" or
# "mcd".
# Create the portfolio specifications:
port_spec7 <- portfolio.spec(assets = colnames(edhec))
port_spec7 <- add.objective(portfolio = port_spec7, type = "return", name = "mean")
port_spec7 <- add.objective(portfolio = port_spec7, type = "risk", name = "StdDev")

library(MASS)

moments_robust <- function(R, portfolio) {
   output <- list()
   output$sigma <- cov.rob(x = R, method = "mcd")$cov
   output
}

moments7 <- moments_robust(R = edhec, portfolio = port_spec7)

cov_rob_mcd <- cov.rob(x = edhec, method = "mcd")
cov_rob_mcd$cov
moments7$sigma
# The cov.rob() is not a consistent function. The covariance matrix it outputs is not always the same. 

# Using the custom moment function, optimize the edhec assets. Compare with a portfolio optimization using
# sample moments:
# Confirm that our portfolio objective is minimizing risk. For purposes of optimization, we need to add 
# constraints as well: 
port_spec8 <- portfolio.spec(assets = colnames(edhec))
port_spec8 <- add.constraint(portfolio = port_spec8, type = "full_investment")
port_spec8 <- add.constraint(portfolio = port_spec8, type = "long_only")
port_spec8 <- add.objective(portfolio = port_spec8, type = "risk", name = "StdDev")
port_spec8

# Create 500 random portfolios: # Note that we set rp or the optimization call will search through 20000 random
# portfolios.
rp8 <- random_portfolios(portfolio = port_spec8, permutations = 500, rp_method = "simplex")

# Optimize:
opt8_custom_f_mmt <- optimize.portfolio(R = edhec, 
                                        portfolio = port_spec8, 
                                        optimize_method = "random", 
                                        rp = rp8, 
                                        momentFUN = "moments_robust") # trace = T required for chart.Weights()
print(opt8_custom_f_mmt)

# optimize.portfolio() will use set.portfolio.moment() call as the momentFUN default. 
# Note that, in turn, set.portfolio.moment() uses "sample" as default.
opt8_default_f_mmt <- optimize.portfolio(R = edhec, 
                                         portfolio = port_spec8, 
                                         optimize_method = "random", 
                                         rp = rp8)
print(opt8_default_f_mmt)
# The results are completely different. Furthermore, results are not similar to DC.

## Recall that optimize.portfolio() function needs a portfolio argument and that the portfolio has an
## objective function. This function can be any R function or any custom R function. Create a custom function
## and use optimize.portfolio():
# Create a function that calculates the portfolio annualized standard deviation:
pasd <- function(R, weights, sigma, scale = 12) {
   sqrt(as.numeric(t(weights) %*% sigma %*% weights)) * sqrt(scale)
}
# We use sigma instead of cov(R) since, optimize.portfolio() needs arguments for each of the four moments
# named as mu, sigma, m3, and m4. Note that optimize.portfolio also needs "weights" and "R". 

# Create a new port_spec and use the pasd as the objective function:
port_spec9 <- portfolio.spec(assets = colnames(edhec))
port_spec9 <- add.constraint(portfolio = port_spec9, type = "full_investment")
port_spec9 <- add.constraint(portfolio = port_spec9, type = "long_only")
port_spec9 <- add.objective(portfolio = port_spec9, type = "risk", name = "pasd")
port_spec9

# With port_spec objective set to minimizing "pasd" we do not have an argument for sigma. Recall that
# optimize.portfolio() uses set.portfolio.moments(). However, set.portfolio.moments() will not recognize pasd.
# Therefore, we need to create a new function for creating a list with the name sigma.
set_sigma <- function(R){
   output <- list()
   output$sigma <- cov(R)
   output
}

# Now let's run optimize.portfolio:
rp9 <- random_portfolios(portfolio = port_spec9, permutations = 500, rp_method = "simplex")
opt9 <- optimize.portfolio(R = edhec, 
                           portfolio = port_spec9, 
                           optimize_method = "random", 
                           rp = rp9, 
                           momentFUN = "set_sigma")
print(opt9) # Again results differ markedly from DC. 

## Case study using the edhec dataset:

## Create a benchmark with equal weight and quarterly rebalancing:
asset_returns <- edhec
equal_weights <- rep(1/ncol(x = asset_returns), ncol(x = asset_returns))
r_benchmark <- Return.portfolio(R = asset_returns, weights = equal_weights, rebalance_on = "quarters")
colnames(r_benchmark) <- "benchmark"
class(r_benchmark) # xts, zoo
plot.zoo(r_benchmark, plot.type = "single", main = "Benchmark returns")

## Specify the case study portfolio constraints and objectives (minimize risk):
port_spec_cs <- portfolio.spec(assets = colnames(asset_returns))
port_spec_cs <- add.constraint(portfolio = port_spec_cs, type = "long_only")
port_spec_cs <- add.constraint(portfolio = port_spec_cs, type = "full_investment")
port_spec_cs <- add.objective(portfolio = port_spec_cs, type = "risk", name = "StdDev")
port_spec_cs

## Run the optimization with backtesting and rebalancing; print the results and plot the weights. 
opt_rebal_base <- optimize.portfolio.rebalancing(R = asset_returns, 
                                                 portfolio = port_spec_cs, 
                                                 optimize_method = "ROI", 
                                                 rebalance_on = "quarters", 
                                                 training_period = 60, 
                                                 rolling_window = 60)
print(opt_rebal_base) # Matches DC. 
chart.Weights(object = opt_rebal_base)

# Calculate the portfolio returns with the optimized base weights. 
returns_opt_base_wgts <- Return.portfolio(R = asset_returns, 
                                          weights = extractWeights(opt_rebal_base)) # No need for "rebalance_on"
# since the weights are rebalanced during optimization. 
colnames(returns_opt_base_wgts) <- "base"
plot(returns_opt_base_wgts)

## Consider two upgrades to the portfolio:
## 1) Redefine the objectives by adding a risk budgeting objective: .05 to .1.
port_spec_cs <- add.objective(portfolio = port_spec_cs, type = "risk_budget", name = "StdDev",
                              min_prisk = .05, max_prisk = .1)

# Rerun the optimization using the "random" solver; print the results and plot weights.
rp_rebal_rkbdg <- random_portfolios(portfolio = port_spec_cs, permutations = 500, rp_method = "simplex")
opt_rebal_rkbdg <- optimize.portfolio.rebalancing(R = asset_returns, 
                                                  portfolio = port_spec_cs, 
                                                  optimize_method = "random", 
                                                  rp = rp_rebal_rkbdg, 
                                                  rebalance_on = "quarters", 
                                                  training_period = 60, 
                                                  rolling_window = 60)
print(opt_rebal_rkbdg) # Differs from DC probably because of the different random portfolios. 
chart.Weights(object = opt_rebal_rkbdg)
chart.RiskBudget(object = opt_rebal_rkbdg, match.col = "StdDev", risk.type = "percentage")

# Calculate the portfolio returns with the new optimized base weights. 
returns_rkbdg <- Return.portfolio(R = asset_returns, 
                                  weights = extractWeights(object = opt_rebal_rkbdg))
colnames(returns_rkbdg) <- "risk_budget"
plot(returns_rkbdg)

# 2) Assume that using a custom moment function: moments_robust will give more precise estimates and so will
# improve our portfolio performance. Rerun the entire process from above.
rp_rebal_rkbdg_mmntrbst <- random_portfolios(portfolio = port_spec_cs, 
                                             permutations = 500, 
                                             rp_method = "simplex")

opt_rebal_rkbdg_mmntrbst <- optimize.portfolio.rebalancing(R = asset_returns, 
                                                           portfolio = port_spec_cs, 
                                                           optimize_method = "random", 
                                                           rp = rp_rebal_rkbdg_mmntrbst, 
                                                           rebalance_on = "quarters", 
                                                           training_period = 60, 
                                                           rolling_window = 60, 
                                                           momentFUN = "moments_robust")
print(opt_rebal_rkbdg_mmntrbst) # Again differs from DC.
chart.Weights(opt_rebal_rkbdg_mmntrbst)
chart.RiskBudget(object = opt_rebal_rkbdg_mmntrbst, match.col = "StdDev", risk.type = "percentage")
returns_rkbdg_mmntrbst <- Return.portfolio(R = asset_returns, 
                                           weights = extractWeights(object = opt_rebal_rkbdg_mmntrbst))
colnames(returns_rkbdg_mmntrbst) <- "moment_robust"
plot(returns_rkbdg_mmntrbst)

# Combine the results into new object "returns" and pull summary data:
returns <- cbind(r_benchmark, returns_opt_base_wgts, returns_rkbdg, returns_rkbdg_mmntrbst)

plot.zoo(returns, plot.type = "single", main = "Returns on Portfolios", 
         col = c("black", "red", "blue", "orange"))
legend(x = "bottomleft", legend = c("Benchmark", "Base Optimum Weights", "Risk Budgeting", 
                                    "Robust Moment Function"), 
       col = c("black", "red", "blue", "orange"), lty = 1, cex = .6)
table.AnnualizedReturns(R = returns)
charts.PerformanceSummary(R = returns)