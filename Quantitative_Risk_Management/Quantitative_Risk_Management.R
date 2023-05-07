library(qrmdata)
library(xts)
library(quantmod)

## To engage in risk management, one has to be able to quantify risk. This is the first step in the analysis.
## The risk factors in a portfolio, unlike the legal definition, are the prices, returns and values of the underlying 
## assets.

# Explore the various types of data in qrmdata:
# 1) EQUITY INDEX. Get the Dow Jones Industrial Average historical data:
DJ <- getSymbols(Symbols = "^DJI", src = "yahoo", auto.assign = F)
periodicity(DJ) # Daily periodicity from 2007-01-03 to 2018-12-21 
head(DJ)
tail(DJ)
plot.zoo(x = Cl(DJ), plot.type = "single", xlab = "Date", ylab = "Level", main = "Dow Jones Industrial Average (^DJI)",
         col = "blue")

# DC uses data from 1985-01-29:
data(package = "qrmdata")
data("DJ", package = "qrmdata")
periodicity(x = DJ) # Daily periodicity from 1985-01-29 to 2015-12-31 
plot.zoo(x = DJ, plot.type = "single", xlab = "Date", ylab = "Level", main = "Dow Jones Industrial Average (^DJI)",
         col = "blue")

# 2) EQUITIES:
data("DJ_const", package = "qrmdata")
periodicity(DJ_const) # Daily periodicity from 1962-01-02 to 2015-12-31 
names(DJ_const)
head(DJ_const)
plot.zoo(x = DJ_const["2008/2015", c("AAPL", "BA", "DIS", "JPM", "PG", "WMT", "XOM")], plot.type = "single",
         xlab = "Date", ylab = "Closing price", 
         col = c("blue", "red", "green", "orange", "black", "chartreuse", "cyan"))
legend(x = "topleft", legend = c("AAPL", "BA", "DIS", "JPM", "PG", "WMT", "XOM"), lty = 1, cex = .6,
       col = c("blue", "red", "green", "orange", "black", "chartreuse", "cyan"))

# 3) EXCHANGE RATES
data("EUR_USD", package = "qrmdata"); periodicity(x = EUR_USD) # Daily periodicity from 2000-01-01 to 2015-12-31 
data("GBP_USD", package = "qrmdata"); periodicity(x = GBP_USD) # Daily periodicity from 2000-01-01 to 2015-12-31 
fx <- merge(EUR_USD, GBP_USD, all = T) # Setting all = T is equal to full_join(). Default: all = F <=> inner_join()
plot.zoo(x = fx, plot.type = "single", col = c("blue", "red"), main = "Foreign Exchange Rates",
         xlab = "Date", ylab = "Rate")
legend(x = "topleft", legend = c("EUR-USD", "GBP-USD"), col = c("blue", "red"), lty = 1, cex = .6)

# You can plot the USD-GBP exchange rate by taking the inverse:
plot.zoo(x = 1 / GBP_USD, main = "USD-GBP Exchange Rate", xlab = "Date", ylab = "Rate")

# We use difference in log values, or log-returns, when analyzing risk. One reason is that log-returns are log
# normally distributed for purposes of using the Black_Scholes model.
# Plot log-returns for 2008-2009 Dow Jones Industrial and the GBP-USD exchange rate:
plot.zoo(diff(x = log(DJ["2008/2009"]), lag = 1), main = "Dow Jones log returns, 2008-09", xlab = "Date",
         ylab = "log returns")

# You don't have to take the log directly, you can use the logical argument within the diff() call.
plot.zoo(diff(x = DJ["2008/2009"], lag = 1, log = T), main = "Dow Jones log returns, 2008-09", xlab = "Date",
         ylab = "log returns")

plot.zoo(diff(x = GBP_USD, lag = 1, log = T), main = "GBP-USD Exchange Rate log Returns", xlab = "Date",
         ylab = "log returns") 

# Plot the closing price and log returns of the first four stocks of the Dow Jones Industrial Average on a single plot:
plot.zoo(DJ_const["2008/2009", 1:4], plot.type = "single", xlab = "Date", ylab = "Closing price",
         main = "Closing Price for AAPL, AXP, BA, and CAT, 2008-09", col = 1:4)
legend(x = "top", legend = names(DJ_const)[1:4], fill = 1:4, cex = .6) # Different type of legend. 
# type = h => Use bars instead of lines. 
plot.zoo(diff(x = DJ_const["2008/2009", 1:4], lag = 1, log = T), plot.type = "multiple", xlab = "Date",
         main = "Log Returns for AAPL, AXP, BA, and CAT, 2008-09", type = "h")

## Aggregate log-returns data: Note that since log-returns: X_t = log(Z_(t+1)) - log(Z_(t)), we can aggregate
## the  returns over longer time periods simply by summing.
# Plot the weekly and monthly log returns of the DJIA. Use vertical bars.
DJ_wk_lnrtn <- period.apply(x = diff(x = DJ, lag = 1, log = T)[-1, ], 
                           INDEX = endpoints(x = diff(x = DJ, lag = 1, log = T)[-1, ], on = "weeks"), 
                           FUN = sum)
plot.zoo(DJ_wk_lnrtn, plot.type = "single",  col = "blue", main = "DJIA, Weekly Log Returns", xlab = "Date",
         ylab = "Level", type = "h")

DJ_mnth_lnrtn <- period.apply(x = diff(x = DJ, lag = 1, log = T)[-1, ],
                              INDEX = endpoints(x = diff(x = DJ, lag = 1, log = T)[-1, ], on = "months"), 
                              FUN = sum)
plot.zoo(DJ_mnth_lnrtn, plot.type = "single",  col = "blue", main = "DJIA, Monthly Log Returns", xlab = "Date",
         ylab = "Level", type = "h")

# Plot the daily log returns of the first four equities of DJ_const from 2000-2015; then plot the monthly
# log-returns.
plot.zoo(x = diff(x = DJ_const["2000/2015", 1:4], lag = 1, log = T)[-1, ], plot.type = "multiple",
         col = 1:4, xlab = "Date", main = "Daily log returns for AAPL, AXP, BA, and CAT")
plot.zoo(x = period.apply(x = diff(x = DJ_const["2000/2015", 1:4], lag = 1, log = T)[-1, ], 
                          INDEX = endpoints(x = diff(x = DJ_const["2000/2015", 1:4], lag = 1, log = T)[-1, ], 
                                            on = "months"), 
                          FUN = colSums), plot.type = "multiple", 
         col = 1:4, 
         xlab = "Date", main = "Monthly log returns for AAPL, AXP, BA, and CAT", 
         type = "h")

## Now let's look at commodities data: 
data("GOLD", package = "qrmdata")
periodicity(GOLD) # Daily periodicity from 1970-01-01 to 2015-12-31 
data("OIL_Brent", package = "qrmdata")
periodicity(OIL_Brent) # Daily periodicity from 1987-05-20 to 2015-12-28 

plot.zoo(x = GOLD, col = "blue", main = "Gold Prices, 1970-2015", xlab = "Date", ylab = "Closing price")
plot.zoo(x = OIL_Brent, col = "green", main = "Brent Oil Prices, 1987-2015", xlab = "Date", ylab = "Closing price")

# Calculate the daily and monthly log returns for gold and Brentwood Oil. Merge the monthly log returns and bar plot. 
gold_mnth_lnrtn <- apply.monthly(x = diff(x = GOLD, lag = 1, log = T)[-1, ], FUN = sum)
oil_mnth_lnrtn <- apply.monthly(x = diff(x = OIL_Brent, lag = 1, log = T)[-1, ], FUN = sum)
cmmdties <- merge(gold_mnth_lnrtn["1990/2015"], oil_mnth_lnrtn["1990/2015"])
class(cmmdties)
plot.zoo(x = cmmdties, 
         plot.type = "multiple", col = 1:2, xlab = "Date", main = "Monthly Log Returns for Gold and Brent Oil",
         type = "h")

# Create a scatterplot of the two commodities' monthly log returns:
library(ggplot2)
ggplot(data = cmmdties, aes(x = GOLD, y = OIL_Brent)) + 
   geom_point()

# Or we could use:
pairs(as.zoo(cmmdties))

## Interest rate data:
data("ZCB_CA", package = "qrmdata") # Canadian zero coupon bond yields in percentages. 
periodicity(ZCB_CA) # Daily periodicity from 1991-01-02 to 2015-08-31 
zcb_dy_lnrtn <- diff(x = ZCB_CA, lag = 1, log = T)
zcb_dy_rtn <- diff(x = ZCB_CA, lag = 1)

plot.zoo(x = zcb_dy_lnrtn[, c("1.00y", "5.00y", "10.00y")], plot.type = "multiple", xlab = "Date",
         main = "Daily Log Returns of 1, 5, and 10 Year Zero Coupon Bond Yields")

plot.zoo(x = zcb_dy_rtn[, c("1.00y", "5.00y", "10.00y")], plot.type = "multiple", xlab = "Date",
         main = "Daily Simple Returns of 1, 5, and 10 Year Zero Coupon Bond Yields")

# Let y(t,T) denote the yield of a zero coupon bond for a T-year bond at time t. The yield curve is y(T; t)
# for fixed t and variable maturity T.
library(stringr)
maturity <- as.numeric(x = str_replace(string = names(ZCB_CA), pattern = "y", replacement = ""))
plot(x = maturity, y = first(ZCB_CA), type = "l", main = "Zero Coupon Bond Yield Curve", xlab = "Maturity", 
     ylab = "Yield (%)", col = "blue", ylim = range(ZCB_CA))
lines(x = maturity, y = last(ZCB_CA), col = "red")

## Recall that normality of log returns is a key assumption of Black-Scholes. Therefore, we need to be able to
## ascertain whether a given set of returns are normally distributed.
DJ_lnrtn_0809 <- diff(x = DJ["2008/2009"], lag = 1, log = T)[-1, ]

# To create a normal distribution overlay, you need to order the values of log-returns and calculate the
# normal density. Note that DC code creates an ordered vector "djx". 
DJ_lnrtn_0809_ordrd <- as.numeric(DJ_lnrtn_0809)[order(as.numeric(DJ_lnrtn_0809))]

hist(DJ_lnrtn_0809_ordrd, 
     breaks = seq(from = range(DJ_lnrtn_0809)[1], 
                  to = range(DJ_lnrtn_0809)[2], 
                  length.out = 50),
     probability = T,
     main = "DJIA Daily Log Returns, 2008-09", xlab = "log return")
lines(x = DJ_lnrtn_0809_ordrd, 
      y = dnorm(x = DJ_lnrtn_0809_ordrd, 
                mean = mean(DJ_lnrtn_0809_ordrd), 
                sd = sd(DJ_lnrtn_0809_ordrd)), 
      col = "red")

# Create a kernel density estimate, which makes no assumptions about the underlying distribution, and plot
# with a normal density overlay.
(DJ_lnrtn_0809_kde <- density(DJ_lnrtn_0809_ordrd)) # Creates a "density" class with x and y columns.
plot(DJ_lnrtn_0809_kde, main = "DJIA Daily Log Returns Density, 2008-09", xlab = "log return")
lines(x = DJ_lnrtn_0809_ordrd, 
      y = dnorm(x = DJ_lnrtn_0809_ordrd, mean = mean(DJ_lnrtn_0809_ordrd), sd = sd(DJ_lnrtn_0809_ordrd)),
      col = "red")
# The histogram and normal distribution overlay; and the KDE plot and normal overlay shows that the
# DJ_lnrtn_0809 returns are not normally distributed. It has a higher peak in the center and has fatter
# tailes.

## Let's be more precise. There are two tests for normality of a distribution. 
## First, qqplots: qqplots compare sample quantiles of data with theoretical quantiles of a normal
## distribution. When comparing with theoretical normal, we use the standard normal distribution. We can, of
## course, use other distributions as the theoretical reference distribution.
# Since we're dealing with distributions, we need to use the DJ_lnrtn_0809_ordrd data. Use qqnorm to plot the
# distribution y compared to a standard normal.
qqnorm(y = DJ_lnrtn_0809_ordrd, main = "DJIA Log Returns QQ Plot, 2008-09")
qqline(y = DJ_lnrtn_0809_ordrd, col = "red")

# Compare randomly generated samples against the standard normal using the qqplot:
y1 <- rnorm(n = length(DJ_lnrtn_0809_ordrd)) # Standard normal
qqnorm(y = y1); qqline(y = y1, col = "red")

y2 <- rt(n = length(DJ_lnrtn_0809_ordrd), df = 4)
qqnorm(y = y2); qqline(y = y2, col = "red")

y3 <- runif(n = length(DJ_lnrtn_0809_ordrd)) # Defaults are min = 0, max = 1.
qqnorm(y = y3); qqline(y = y3, col = "red")

## Second, we can check the distributions skewness (3rd moment) and kurtosis (4th moment) to see whether a
## distribution is normal or not. A normal distribution will have a skewness of 0 and kurtosis of 3.
# We will use the moments package's Jarque-Bera test which computes a test statistic assumed to have a Chi
# squared  distribution with 2 df.
library(moments)
DJ_lnrtn_0811 <- diff(x = DJ["2008/2011"], lag = 1, log = T)[-1, ]
skewness(x = DJ_lnrtn_0811) # Close to 0
kurtosis(x = DJ_lnrtn_0811) # 9.058875 > 3
jarque.test(x = as.numeric(DJ_lnrtn_0811)) # jarque.test() call requires a numeric vector.
# Test stat, JB = 1541.8 and Chi square test results in pvalue < 2.2e-16. Therefore, not a normal distribution.

# Check the normality of the returns of each of the DJIA constituent stocks from 2008 to 2011
DJ_subc <- DJ_const["2008/2011", -27] # DC removed Visa and removed 2008-01-02 from data.
DJ_subc_lnrtn <- diff(x = DJ_subc, lag = 1, log = T)[-1, ]
DJ_subc_skew <- skewness(x = DJ_subc_lnrtn) 
# Result is a vector for each column (each constituent equity) and most are different from 0.
DJ_subc_kurt <- kurtosis(x = DJ_subc_lnrtn) # Most are much greater than 3.

# Plot the kurtosis against skewness:
plot(x = DJ_subc_skew, y = DJ_subc_kurt, type = "n", main = "DJIA Constituents, Skewness vs Kurtosis, 2008-11",
     xlab = "Skewness", ylab = "Kurtosis", ylim = c(0, 20)) # type = "n" means that nothing is plotted
text(x = DJ_subc_skew, y = DJ_subc_kurt, labels = names(DJ_subc_skew), cex = .6)

# Use the Jarque-Bera test. Note that we HAVE TO USE the apply() function because we're dealing with xts object.
# We get an error with lapply() and map() applies the function to every element within the xts object. 
apply(X = DJ_subc, MARGIN = 2, FUN = jarque.test)

## Recall that normality of a distribution is key for using the Black Scholes equation. We further stated that
## log returns is preferred because when summed (or aggregated to weeks and or months) these values will be
## normally distributed by virtue of the CLT. As stated in DC: "As returns are added together over longer time
## periods, a central limit effect takes place and returns tend to become more normal."

# Create a new DJ_subc from 2000 to 2015 and without Visa:
DJ_subc <- DJ_const["2000/2015", -27]
DJ_subc_lnrtn <- diff(x = DJ_subc, lag = 1, log = T)[-1, ]
class(DJ_subc_lnrtn) #xts, zoo; therefore, use apply()

# Create weekly and monthly log returns for all 29 DJIA constituent stocks:
DJ_subc_wk_lnrtn <- apply.weekly(x = DJ_subc_lnrtn, FUN = colSums)
DJ_subc_mnt_lnrtn <- apply.monthly(x = DJ_subc_lnrtn, FUN = colSums)

# Run a Jarque-Bera test of normality on each of the constituent stocks and their respective log-returns and
# pull out the p-values only.
apply(X = DJ_subc_lnrtn, MARGIN = 2, FUN = function(v){jarque.test(x = v)$p.value}) # Use str() to pull out list elements of 
# jarque.test() call. All p-values are 0, which means that none of the equities have normally distributed log returns.
apply(X = DJ_subc_wk_lnrtn, MARGIN = 2, FUN = function(v){jarque.test(x = v)$p.value}) # Again all values 0.
apply(X = DJ_subc_mnt_lnrtn, MARGIN = 2, FUN = function(v){jarque.test(x = v)$p.value})
# Chevron (CVX), 3M (MMM) and Pfizer (PFE) have normally distributed monthly log returns

# Using these period methods, we lose a lot of data. We could use rolling aggregation, however, this leads to
# greater correlation between the values and we need the iid assumption.

# We'll use the DJ_lnrtn_0811 dataset: Create rolling 21 day (monthly) and 63 day (quarterly) log returns.
library(magrittr)
DJ_lnrtn_0811_21dy <- rollapply(data = DJ_lnrtn_0811, width = 21, FUN = sum) %>%
   na.omit()
DJ_lnrtn_0811_63dy <- rollapply(data = DJ_lnrtn_0811, width = 63, FUN = sum) %>%
   na.omit()

# Analysis:
DJ_lnrtn_0811_dmq <- merge(DJ_lnrtn_0811, DJ_lnrtn_0811_21dy, DJ_lnrtn_0811_63dy, all = F)
plot.zoo(DJ_lnrtn_0811_dmq, plot.type = "multiple", main = "DJIA Log Returns: Daily, Monthly and Quarterly")
apply(X = DJ_lnrtn_0811_dmq, MARGIN = 2, FUN = skewness) # Aside for daily, all are skewed to the left.
apply(X = DJ_lnrtn_0811_dmq, MARGIN = 2, FUN = kurtosis) # Monthly has the lest kurtosis
apply(X = DJ_lnrtn_0811_dmq, MARGIN = 2, FUN = function(v){jarque.test(v)$p.value}) # pvalues are all 0. 

## Since log returns do not appear to be normally distributed, what distribution should we use? We use the
## Student t distribution: centering parameter mu, scaling parameter sigma, and df parameter nu. 

# Fit a Student t distribution to the daily DJ_lnrtn_0811 data and estimate the parameters:
# Again since we're dealing with the distribution of the values, the time index is not necessary.
DJ_lnrtn_0811_ordrd <- as.numeric(DJ_lnrtn_0811)[order(as.numeric(DJ_lnrtn_0811))]

# We need to fit a distribution to the data. There are a multitude of packages that do this. DC uses the QRM
# package and the fit.st() function. I will use the MASS package's fitdistr() function:
library(MASS)
DJ_lnrtn_0811_tfit <- fitdistr(x = DJ_lnrtn_0811_ordrd, densfun = "t")
# 21 warnings are produced but the parameter estimates approximate the estimates in DC using fit.st(). 

# To help the optimization process (note that fitdistr() use the optim() call), we can add starting points for the 
# location, scale, and df parameters. We can set these starting values by looking at the results of the estimates 
# above. Further, we can use a histogram of the data to set the lower bounds of the estimates.
hist(x = DJ_lnrtn_0811_ordrd, 
     probability = T,
     breaks = seq(from = min(range(DJ_lnrtn_0811_ordrd)),
                  to = max(range(DJ_lnrtn_0811_ordrd)),
                  length.out = 50), 
     main = "Histogram of DJIA Daily Log Returns, 2008-11", xlab = "log returns")
lines(x = DJ_lnrtn_0811_ordrd, 
      y = dnorm(x = DJ_lnrtn_0811_ordrd, mean = mean(DJ_lnrtn_0811_ordrd), sd = sd(DJ_lnrtn_0811_ordrd)), 
      col = "red")

DJ_lnrtn_0811_tfit <- fitdistr(x = DJ_lnrtn_0811_ordrd, 
                               densfun = "t", 
                               start = list(m = .0005,
                                            s = .009,
                                            df = 2.45),
                               lower = c(-.05, .001, 1))
str(DJ_lnrtn_0811_tfit)
DJ_lnrtn_0811_mu <- DJ_lnrtn_0811_tfit$estimate["m"]
DJ_lnrtn_0811_sigma <- DJ_lnrtn_0811_tfit$estimate["s"]
DJ_lnrtn_0811_df <- DJ_lnrtn_0811_tfit$estimate["df"]

# Now overlay the newly created t distribution to the histogram and normal plot overlay:
yval <- dt(x = (DJ_lnrtn_0811_ordrd - DJ_lnrtn_0811_mu)/DJ_lnrtn_0811_sigma, 
           df = DJ_lnrtn_0811_df) / DJ_lnrtn_0811_sigma # You need to scale each of the values and also scale 
# the resulting density. Uncertain as to why the second scaling occurs.
lines(x = DJ_lnrtn_0811_ordrd, y = yval, col = "blue")

## Apply the tests of normality to exchange rate and interest rate log returns.

# Create the DC dataset:
data("JPY_USD", package = "qrmdata")
fx <- merge(GBP_USD, EUR_USD, JPY_USD, all = F)["2000-12-31/2015"]
fx_lnrtn <- diff(x = fx, lag = 1, log = T)[-1, ]
fx_lnrtn_mnth <- apply.monthly(x = fx_lnrtn, FUN = colSums)

# Test the daily log returns:
plot.zoo(fx_lnrtn, plot.type = "multiple", main = "Foreign Exchange Log Returns, 2001-2015", xlab = "Date")
apply(X = fx_lnrtn, MARGIN = 2, FUN = jarque.test) # All three fx have pvalues close to zero.

# Test the monthly log returns:
plot.zoo(fx_lnrtn_mnth, plot.type = "multiple", main = "Foreign Exchange Monthly Log Returns, 2001-2015", 
         xlab = "Date")
apply(X = fx_lnrtn_mnth, MARGIN = 2, FUN = jarque.test) # JPY_USD appears to be normal. 

# Fit a Student t distribution to each of the exchange rates' daily log returns:
apply(X = fx_lnrtn, MARGIN = 2, FUN = function(v){
   fit <- fitdistr(x = v, 
                   densfun = "t", 
                   start = list(m = 0, s = .005, df = 2))
   fit$estimate
})
# 50 warnings

# Fit a Student t distribution to each of the exchange rates' monthly log returns:
apply(X = fx_lnrtn_mnth, MARGIN = 2, FUN = function(v){
   fit <- fitdistr(x = v, 
                   densfun = "t", 
                   start = list(m = 0, s = .005, df = 2))
   fit$estimate
})
# 30 warnings

## Create the DC zero coupon bond monthly log returns and simple returns:
zcbsub_mnth_lnrtn <- apply.monthly(x = zcb_dy_lnrtn["2006/2015", c("1.00y", "5.00y", "10.00y")], 
                                   FUN = colSums)
zcbsub_mnth_rtn <- apply.monthly(x = zcb_dy_rtn["2006/2015", c("1.00y", "5.00y", "10.00y")], 
                                 FUN = colSums) * 100
plot.zoo(zcbsub_mnth_lnrtn, plot.type = "multiple", main = "Zero Coupon Bond Yield Log Returns, 2006-15", 
         xlab = "Date", type = "h") # type = "h" could make things clearer.
plot.zoo(zcbsub_mnth_rtn, plot.type = "multiple", main = "Zero Coupon Bond Yield Simple Returns, 2006-15", 
         xlab = "Date", type = "h")

# Calculate each bond's skewness, kurtosis and Jarque-Bera test statistic:
apply(X = zcbsub_mnth_lnrtn, MARGIN = 2, FUN = skewness) # All values are different from 0.
apply(X = zcbsub_mnth_rtn, MARGIN = 2, FUN = skewness) # All values are different from 0 but 5 and 10 year simple
# returns are less than abs(1).

apply(X = zcbsub_mnth_lnrtn, MARGIN = 2, FUN = kurtosis) # All values are different from 3.
apply(X = zcbsub_mnth_rtn, MARGIN = 2, FUN = kurtosis) # 5 and 10 year have kurtosis close to 3. 

apply(X = zcbsub_mnth_lnrtn, MARGIN = 2, FUN = function(v) {jarque.test(x = v)$p.value}) # All pvalues are 0.
apply(X = zcbsub_mnth_rtn, MARGIN = 2, FUN = function(v) {jarque.test(x = v)$p.value})
# 5 year and 10 year zero coupon bonds appear to be normal. 

# QQ-plots for the 5 year and 10 year zero coupon bond simple returns:
qqnorm(y = zcbsub_mnth_rtn[, "5.00y"], main = "QQ-Plot for 5 Year Zero Coupon Bond Simple Returns")
qqline(y = zcbsub_mnth_rtn[, "5.00y"], col = "red")

qqnorm(y = zcbsub_mnth_rtn[, "10.00y"], main = "QQ-Plot for10 Year Zero Coupon Bond Simple Returns")
qqline(y = zcbsub_mnth_rtn[, "10.00y"], col = "red")

# Both appear to be normal, although the tails are a bit thicker. Plot the qq plots of the 5 and 10 year log returns:
qqnorm(y = zcbsub_mnth_lnrtn[, "5.00y"], main = "QQ-Plot for 5 Year Zero Coupon Bond Log Returns")
qqline(y = zcbsub_mnth_lnrtn[, "5.00y"], col = "red")

qqnorm(y = zcbsub_mnth_lnrtn[, "10.00y"], main = "QQ-Plot for 10 Year Zero Coupon Bond Log Returns")
qqline(y = zcbsub_mnth_lnrtn[, "10.00y"], col = "red")
# The qqplots don't appear to be that different. 

## This chapter considers the second important assumption of returns data for Black Scholes: iid. In short, 
## the returns should be a random walk. 

# First, fit a normal and t distribution to the DJIA 2008-11 log returns data to get parameter estimates:
tpars <- DJ_lnrtn_0811_tfit$estimate # We fitted the t distribution previously.
npars <- fitdistr(x = DJ_lnrtn_0811_ordrd, densfun = "normal")$estimate # Very different from DC

# Second, randomly draw (IID) n = length(DJ_lnrtn_0811_ordrd) values from a normal and t distribution with the
# parameters above and create xts objects with the simulated data.
nsim <- rnorm(n = length(DJ_lnrtn_0811)) * npars["sd"] + npars["mean"]# Same result as rnorm(n, npars["mean"], npars["sd"])
nsim_xts <- xts(x = nsim, order.by = time(x = DJ_lnrtn_0811))

tsim <- rt(n = length(DJ_lnrtn_0811), df = tpars["df"]) * tpars["s"] + tpars["m"]
tsim_xts <- xts(x = tsim, order.by = time(DJ_lnrtn_0811))

# Merge the three data sets and plot the returns.
plot.zoo(merge(DJ_lnrtn_0811, nsim_xts, tsim_xts), 
         plot.type = "multiple", 
         main = "DJIA Log Returns and Simulated Data from Normal and Student T Distribution, 2008-11",
         type = "h", 
         ylim = range(merge(DJ_lnrtn_0811, nsim_xts, tsim_xts))) # To create consistent y axes.
# Note how the real returns look more volatile and autocorrelated than the iid returns. There was clearly a period of large
# movements around the 2008 financial crisis followed by a quieter period in 2010. Note that the real data shows a great
# deal of volatility clustering, which is evidence of autocorrelations within the returns.

## Autocorrelations and the ACF plot:
# Plot the acf for the DJIA log returns and the simulated data
par(mfrow = c(3, 1))
acf(x = DJ_lnrtn_0811, main = "DJIA Log Returns Autocorrelation")
acf(x = nsim_xts, main = "Simulated Normal Autocorelation")
acf(x = tsim_xts, main = "Simulated T Autocorrelation")
# Very little evidence of serial correlation is found in the ACF plots for the log returns and the simulations.

# Plot the acf for absolute values and squared values of the log returns and simulated data:
acf(x = abs(DJ_lnrtn_0811), main = "DJIA Absolute Log Returns Autocorrelation")
acf(x = abs(nsim_xts), main = "Simulated Absolute Normal Autocorelation")
acf(x = abs(tsim_xts), main = "Simulated Absolute T Autocorrelation")

acf(x = DJ_lnrtn_0811^2, main = "DJIA Squared Log Returns Autocorrelation")
acf(x = nsim_xts^2, main = "Simulated Squared Normal Autocorelation")
acf(x = tsim_xts^2, main = "Simulated Squared T Autocorrelation")

par(mfrow = c(1, 1))
# The log returns acf changes dramatically when we look at absolute or squared return data. The real returns 
# in the DJIA log returns series behave very differently to the simulated data. The serial correlation in 
# absolute or squared returns is a consequence of volatility, which causes large returns to be followed by 
# further large returns, although not necessarily of the same sign. THE TAKEAWAY IS THAT RETURNS DATA AND THE 
# CLUSTERED VOLATILITY CAN TELL US MUCH ABOUT MAGNITUDE OF FUTURE RETURNS BUT NOT THE SIGN.

## The Ljung-Box Test quantifies the autocorrelations and test the iid assumption. It is calculated by summing
## the squared autocorrelations and assuming that the test statistic is a Chi square distribution with df
## equal to the number of lags.
# Create DC dataset from DJ_const:
DJ_const_0715_lnrtn <- diff(x = DJ_const["2006/2015", -27], lag = 1, log = T)[-1, ]

# Run the box test on DJ_lnrtn_0811 and DJ_const_0715_lnrtn and their respective absolute values:
Box.test(x = DJ_lnrtn_0811, lag = 10, type = "Ljung-Box") # X-squared = 34.552, df = 10, p-value = 0.0001489
Box.test(x = abs(DJ_lnrtn_0811), lag = 10, type = "Ljung-Box") # X-squared = 1082.9, df = 10, p-value < 2.2e-16

apply(X = DJ_const_0715_lnrtn, MARGIN = 2, FUN = Box.test, lag = 10, type = "Ljung-Box")
# Apple (AAPL), Boeing (BA), Caterpillar (CAT), Cisco (CSCO) and Home Depot (HD) are iid. 
apply(X = abs(DJ_const_0715_lnrtn), MARGIN = 2, FUN = Box.test, lag = 10, type = "Ljung-Box")
# All constiuents' absolute values are not iid and all values are close to 0. 

# You should notice that while the hypothesis of no serial correlation is rejected for many of the raw return
# series, it is rejected overwhelmingly for all of the absolute value series.

# Now aggregate the log returns monthly returns. Does this increase or decrease the degree of serial correlation? 
DJ_lnrtn_0811_mnth <- apply.monthly(x = DJ_lnrtn_0811, FUN = sum)
Box.test(x = DJ_lnrtn_0811_mnth, lag = 10, type = "Ljung-Box") # X-squared = 22.057, df = 10, p-value = 0.01481
Box.test(x = abs(DJ_lnrtn_0811_mnth), lag = 10, type = "Ljung-Box") # X-squared = 12.89, df = 10, p-value = 0.2299
# With absolute log returns we can't reject the hypothesis of iid. 

DJ_const_0715_lnrtn_mnth <- apply.monthly(x = DJ_const_0715_lnrtn, FUN = colSums)
apply(X = DJ_const_0715_lnrtn_mnth, MARGIN = 2, FUN = Box.test, lag = 10, type = "Ljung-Box")
# AXP, CAT, DIS, KO, and UTX are NOT iid. The remainder are all iid. 
apply(X = abs(DJ_const_0715_lnrtn_mnth), MARGIN = 2, FUN = Box.test, lag = 10, type = "Ljung-Box")
# CSCO, CVX (Chevron), HD, IBM, INTC, JNJ, KO, MCD, MMM, MSFT, NKE, PFE, PG, TRV, VZ, WMT, XOM are iid. 

# Autocorrelation decreases as you aggregate log returns. However, for absolute log returns, autocorrelation increases.

## When you take a long series of iid data, such as several thousand observations, and select a small subset
## of the most extreme observations, like less than 100, then these extremes appear at random and the spaces
## or gaps between the extremes follow a distribution that is very close to exponential. 
# Use the full DJIA index values and calculate returns. Choose 100 of the lowest returns and plot their absolute values.
# Note that 1987-10-19, or Black Monday, is an extreme outlier and so I'll delete from results.
DJ_lnrtn <- diff(x = DJ, lag = 1, log = T)
DJ_lnrtn_extrm <- DJ_lnrtn[order(DJ_lnrtn)[1:101], ]
DJ_lnrtn_extrm <- DJ_lnrtn_extrm[-DJ_lnrtn_extrm["1987-10-19", which.i = T], ]
plot(abs(DJ_lnrtn_extrm), type = "h", main = "DJIA Log Return, 100 Most Extreme Negative Returns")

# Compute the time difference between the large negative returns and create a histogram:
DJ_extrm_intrvl <- diff(time(DJ_lnrtn_extrm))
hist(as.numeric(DJ_extrm_intrvl), main = "Histogram of Intervals between Extreme Negative Returns")

# Compare the time difference between the large negative returns and that of a theoretical exponential distribution:
exp_quantiles <- qexp(p = seq(from = 0.001, to = .999, length.out = 100))
qqplot(x = exp_quantiles, y = DJ_extrm_intrvl, 
       main = "QQ Plot of Exponential Distribution and Intervals of Extreme Negative DJIA Log Returns")

# Run the same analysis but for iid returns:
iid_xts <- xts(x = rnorm(n = length(DJ_lnrtn)), order.by = time(DJ_lnrtn))
iid_extrm <- iid_xts[order(iid_xts)[1:100], ]
plot(abs(iid_extrm), type = "h", main = "IID, 100 Most Extreme Negative Values")
iid_extrm_intrvl <- diff(time(iid_extrm))
hist(as.numeric(iid_extrm_intrvl), main = "Histogram of INtervals between IID Extreme Negative Values")
qqplot(x = exp_quantiles, y = iid_extrm_intrvl, 
       main = "QQ Plot of Exponential Distribution and Intervals of Extreme IID Negative Values")
## For a volatile financial log-return series, the extremes appear in clusters during periods of high volatility. 
## This is another feature of real log-return data that we need to take account of when building models.

## There are certain stylized facts about returns data:
## 1) returns series are heavier tailed than normal: leptokurtic
## 2) volatility varies over time
## 3) returns series show very little autocorrelation
## 4) absolute vales of returns series show large autocorrelation
## 5) extreme returns show in clusters
## 6) returns aggregated over longer periods of time are more normal and less autocorrelated

# Check that the above stylized facts hold for fx and interest rate yield data:
# FX
data("CHF_USD", package = "qrmdata")
fx <- merge(GBP_USD, EUR_USD, JPY_USD, CHF_USD)
fx_lnrtn <- diff(x = fx, lag = 1, log = T)["2011/2015", ]
fx_lnrtn_wk <- apply.weekly(x = fx_lnrtn, FUN = colSums)
plot.zoo(fx_lnrtn, type = "h", main = "GBP, EUR, JPY, CHF to USD Daily Log Returns, 2011-15")
plot.zoo(fx_lnrtn_wk, type = "h", main = "GBP, EUR, JPY, CHF to USD Weekly Log Returns, 2011-15")

# Plot the acf for both daily returns and weekly returns and their respective absolute values. 
acf(fx_lnrtn, main = "Daily Log Return Autocorrelations")
acf(abs(fx_lnrtn), main = "Absolute Daily Log Return Autocorrelations")

acf(fx_lnrtn_wk, main = "Weekly Log Return Autocorrelations")
acf(abs(fx_lnrtn_wk), main = "Absolue Weekly Log Return Autocorrelations")

# Apply the Ljung-Box test to each of the FX components:
apply(X = fx_lnrtn, MARGIN = 2, FUN = Box.test, lag = 10, type = "Ljung-Box")
apply(X = abs(fx_lnrtn), MARGIN = 2, FUN = Box.test, lag = 10, type = "Ljung-Box")

apply(X = fx_lnrtn_wk, MARGIN = 2, FUN = Box.test, lag = 10, type = "Ljung-Box")
apply(X = abs(fx_lnrtn_wk), MARGIN = 2, FUN = Box.test, lag = 10, type = "Ljung-Box")
# Again note that for the aggregate returns, we have less autocorrelation. 

# Now check the iid assumption for the zero coupon bonds:
zcbsub_dy_lnrtn <- zcb_dy_lnrtn["2006/2015", c("1.00y", "5.00y", "10.00y")]

acf(zcbsub_dy_lnrtn, main = "Daily Log Return Autocorreations")
acf(abs(zcbsub_dy_lnrtn), main = "Daily Absolute Log Return Autocorreations")

apply(X = zcbsub_dy_lnrtn, MARGIN = 2, FUN = Box.test, lag = 10, type = "Ljung-Box")
apply(X = abs(zcbsub_dy_lnrtn), MARGIN = 2, FUN = Box.test, lag = 10, type = "Ljung-Box")

acf(zcbsub_mnth_lnrtn, main = "Monthly Log Return Autocorreations")
acf(abs(zcbsub_mnth_lnrtn), main = "Daily Absolute Log Return Autocorreations")

apply(X = zcbsub_mnth_lnrtn, MARGIN = 2, FUN = Box.test, lag = 10, type = "Ljung-Box")
apply(X = abs(zcbsub_mnth_lnrtn), MARGIN = 2, FUN = Box.test, lag = 10, type = "Ljung-Box")

## Now we're going to quantify risk using Value at Risk (VaR). Use the DJ_lnrtn_0809 data and first assume
## that the returns have a normal distribution with mean equal to the historical data sample mean and sd equal
## to sample sd. There is a question of whether this is a valid assumption, however, assume that returns are iid normal.
mean(DJ_lnrtn_0809) # -0.000444099
sd(DJ_lnrtn_0809) # 0.02001809

# Plot the density function so that we can visually determine and understand the VaR and the ES (expected shortfall):
xval <- seq(from = -4 * sd(DJ_lnrtn_0809), to = 4 * sd(DJ_lnrtn_0809), length.out = 100)
ydensity <- dnorm(x = xval, mean = mean(DJ_lnrtn_0809), sd = sd(DJ_lnrtn_0809))
DJ_lnrtn_0809_VaR99 <- qnorm(p = .99, mean = mean(DJ_lnrtn_0809), sd = sd(DJ_lnrtn_0809))

# DC used ESnorm() to find the ES of 99% for DJ_lnrtn_0809. I use a different approach:
rndm_sim <- rnorm(n = 2000, mean = mean(DJ_lnrtn_0809), sd = sd(DJ_lnrtn_0809))
DJ_lnrtn_0809_ES99 <- mean(rndm_sim[rndm_sim > DJ_lnrtn_0809_VaR99]) # Very close to DC's ESnorm()

plot(x = xval, y = ydensity, type = "l", main = "Distribution of DJIA Log Returns and VaR and ES")
abline(v = DJ_lnrtn_0809_VaR99, col = "red")
abline(v = DJ_lnrtn_0809_ES99, col = "green")
text(x = DJ_lnrtn_0809_ES99 + .005, y = 17.5, labels = "ES99", cex = .6)
text(x = DJ_lnrtn_0809_VaR99 - .005, y = 17.5, labels = "VaR99", cex = .6)

## Consider a portfolio with assets in three indices and based in the UK. Then the portfolio is exposed to five risk 
## factors. Check that these risk factors are heavier tailed than normal; highly volatile and subject to serial 
## autocorrelation.

data("SP500", package = "qrmdata")
data("SMI", package = "qrmdata")
data("FTSE", package = "qrmdata")
data("CHF_GBP", package = "qrmdata")
data("USD_GBP", package = "qrmdata")
riskfactors <- merge(FTSE, SP500, SMI, USD_GBP, CHF_GBP, all = F)["2000/2012"]
plot.zoo(riskfactors, plot.type = "multiple", main = "")

# Calculate the log returns and use the Jarque-Bera test for skewness and kurtosis. 
rf_lnrtns <- diff(x = riskfactors, lag = 1, log = T)[-1, ]
apply(X = rf_lnrtns, MARGIN = 2, FUN = jarque.test) # None of the risk factor returns are normal. 

# QQ plot with theoretical normal distribution. 
par(mfrow = c(3, 2))
apply(X = rf_lnrtns, MARGIN = 2, FUN = function(v){
   qqnorm(v)
   qqline(v, col = "red")
})
par(mfrow = c(1, 1))

# Check the autocorrelation of the five series:
acf(rf_lnrtns)
acf(abs(rf_lnrtns))
# The plots show that the absolute returns show strong autocorrelation and cross correlation.
apply(X = rf_lnrtns, MARGIN = 2, FUN = Box.test, lag = 10, type = "Ljung-Box")
# We see evidence of strong autocorrelation. 

## Historical simulation for a portfolio with 30% in FTSE, 40% in SP500 and 30% in SMI. Create a loss operator
## function to create historical gains and losses for the .3-.4-.3 portfolio of three national stock market indices 
## and their relevant FX. 
lossop <- function(series, wts = c(.3, .4, .3)) {
   if (is.xts(series)) {
      x <- coredata(series)
   } else if (is.matrix(series)) {
      x <- series
   } else {
      x <- matrix(data = series, nrow = 1)
   }
   loss <- apply(X = x, 
                 MARGIN = 1, 
                 FUN = function(x, wts){
                    1 - (wts[1] * exp(x[1]) + wts[2] * exp(x[2] + x[4]) + wts[3] * exp(x[3] + x[5])) # Losses are positive.
                 }, wts = wts)
   if (is.xts(series)) {
      loss <- xts(x = loss, order.by = time(series))
   }
   loss
}

# Check to see that the function works:
lossop(series = rep(x = -.1, times = 5))

# Create historical losses object: hstloss using the lossop() function. Then plot the losses and plot the losses 
# against a normal in a qqplot. Further plot the acf for losses and for absolute losses. Note that losses in this function
# are positive and gains are negative. See the function programming above.  
hstloss <- lossop(series = rf_lnrtns)
plot.zoo(hstloss, main = "Historical Losses of Hypothetical Portfolio")

qqnorm(y = hstloss, main = "QQ Plot of Historical Losses of Hypothetical Portfolio")
qqline(y = hstloss, col = "red")
# Does not seem normal.
jarque.test(x = as.numeric(hstloss)) # Significantly different and reject null of normality.

acf(x = hstloss)
acf(x = abs(hstloss))
# We see the same stylized facts as we found in other returns data.
Box.test(x = hstloss, lag = 10, type = "Ljung")
# Appears as though we can reject the claim of independent distribution. 

## Now let's estimate VaR and ES based on the simulated historical losses. First, we use a non parametric method
## using the sample quantile and then we assume that the historical losses are normally distributed. 
(hstloss_VaR99 <- quantile(x = hstloss, probs = .99))
(hstloss_ES99 <- mean(hstloss[hstloss >= hstloss_VaR99]))

# Plot the results using a non parametric kernel density estimate.
plot(density(x = hstloss), col = "blue",
     main = "Historical Loss Simulation Kernel Density Estimates, 99% VaR and 99% ES")
abline(v = hstloss_VaR99, col = "red")
text(x = hstloss_VaR99 - .01, y = 45, labels = "99% VaR", cex = .6)
abline(v = hstloss_ES99, col = "green")
text(x = hstloss_ES99 + .01, y = 45, labels = "99% ES", cex = .6)

# Now assume that the simulated historical returns are normally distributed with mu and sigma equal to sample 
# mean and sd:
(hstloss_nrmlVaR99 <- qnorm(p = .99, mean = mean(hstloss), sd = sd(hstloss)))
rndm_sim <- rnorm(n = 2000, mean = mean(hstloss), sd = sd(hstloss))
(hstloss_nrmlES99 <- mean(rndm_sim[rndm_sim >= hstloss_nrmlVaR99]))
# Note that both values are off from the non parametric values from the historical simulations. 

# Plot:
xval <- seq(from = -4 * sd(hstloss), to = 4 * sd(hstloss), length.out = 500)
ydensity <- dnorm(x = xval, mean = mean(hstloss), sd = sd(hstloss))
plot(x = xval, y = ydensity, type = "l", col = "blue",
     main = "Historical Loss Simulation Normal Density Estimates, 99% VaR and 99% ES")
abline(v = hstloss_nrmlVaR99, col = "red")
text(x = hstloss_nrmlVaR99 - .008, y = 18, labels = "99% VaR", cex = .6)
abline(v = hstloss_nrmlES99, col = "green")
text(x = hstloss_nrmlES99 + .008, y = 18, labels = "99% ES", cex = .6)

## Now we finally look at the Black Scholes formula: See https://en.wikipedia.org/wiki/Black%E2%80%93Scholes_model.

## 1) Note that the value of the call option at time t: C(S_t, t) is equal to N(d_1)S_t - N(d_2)PV(K) where N(.) is 
## a cumulative distribution term. Therefore, C(S_t, t) is an expected value with the value of the underlying stock
## and the present value of the strike price (the price that the holder of the option will pay)

## 2) The d_1 is increasing in sigma and d_2 is decreasing in sigma. Therefore, since N() is monotonic in d_1 and d_2,
## an increase in sigma will result in the value of the call option, C(S_t, t), is increasing in sigma. 

# The qrmtools package has a Black_Scholes() function. However, we can write our own code.
blackscholesformula <- function(S, t, K, T_mat, sigma, r, type = c("call", "put")) {
   d_1 <- (log(S/K) + (r + (sigma^2/2))*(T_mat - t))/(sigma*sqrt(T_mat - t))
   d_2 <- d_1 - (sigma*sqrt(T_mat - t))
   type <- match.arg(type)
   switch(EXPR = type, 
          "call" = {S*pnorm(d_1) - K*exp(-r*(T_mat - t))*pnorm(d_2)},
          "put" = {K*exp(-r*(T_mat - t))*pnorm(-d_2) - S*pnorm(-d_1)},
          stop("Wrong type. Use 'call' or 'put' for type argument"))
}
# Note that instead of using switch(), we could have used if else () formulation

# Assume the following options have a strike price K = 100, risk free rate r = .01, sigma = .2, and maturity at T = 1.
# Option 1: European call option with stock price S = 80
blackscholesformula(S = 80, t = 0, K = 100, T_mat = 1, sigma = .2, r = .01, type = "call") # 1.302245

# Option 2: European call option with stock price S = 120
blackscholesformula(S = 120, t = 0, K = 100, T_mat = 1, sigma = .2, r = .01, type = "call") # 22.94188

# Option 3: European put option with stock price S = 80
blackscholesformula(S = 80, t = 0, K = 100, T_mat = 1, sigma = .2, r = .01, type = "put") # 20.30723

# Option 4: European put option with stock price S = 120
blackscholesformula(S = 120, t = 0, K = 100, T_mat = 1, sigma = .2, r = .01, type = "put") # 1.94686

## Given a portfolio of options, one has to consider three risk factors: stock price, volatility, and interest rates.
## For this exercise, assume that interest rates are stable in the short term; stock price is the SP500 and the volatility
## measure is the Chicago Board Options Exchange's VIX which calculates the implied volatility using the a wide 
## range of SP500 index options.

# In this exercise, you will be able to verify whether the log-returns of volatility behave like other return
# data you have encountered, and to see how they vary with the log-returns of the S&P 500 index.
data("VIX", package = "qrmdata")
rfoptions <- merge(SP500["1990/2010"], VIX["1990/2010"])
rfoptions_lnrtn <- diff(x = rfoptions, lag = 1, log = T)[-1, ]
plot.zoo(rfoptions, plot.type = "multiple", main = "S&P 500 and VIX, 1990-2010", xlab = "Date")
plot.zoo(rfoptions_lnrtn, plot.type = "multiple", main = "S&P 500 and VIX Log Returns, 1990-2010", xlab = "Date")

# Create a scatterplot of the two returns:
pairs(as.zoo(rfoptions_lnrtn)) # Appear to be negatively correlated. We can also use the following:
plot(as.matrix(rfoptions_lnrtn), main = "Scatterplot of VIX Log Returns against S&P 500 Log Returns",
     xlab = "S&P 500 log returns", ylab = "VIX log returns")

# Check the normality assumption of the log returns for both the S&P 500 and VIX:
apply(X = rfoptions_lnrtn, MARGIN = 2, FUN = jarque.test) # We can reject the null of normality for both.
apply(X = rfoptions_lnrtn, MARGIN = 2, FUN = function(v){
   qqnorm(y = v, main = "QQ Plot of Log Returns")
   qqline(y = v, col = "red")
}) # Appears to be confirmed by the qqplots.

# Check the iid assumption of the log returns:
acf(rfoptions_lnrtn) # Not much autocorrelation or cross correlation in returns.
acf(abs(rfoptions_lnrtn)) # Strong autocorrelation for absolute values, especially for SP500 and SP500 and VIX,
# which we can confirm by:
cor(rfoptions_lnrtn)
# In short, the VIX log returns show the same stylized facts as other log returns.

## Simulate historical returns of a portfolio containing a single European call option on an equity index, the S&P500. 
## Assume further that we are interested in the one day change in value of such an option . To value the portfolio, 
## we will again have to create a loss operator which takes S and sigma as arguments (we again assume that r is 
## fixed over the time horizon), where S is the S&P500 index and sigma is VIX.

# Create the loss operator which we will call loss_f
loss_f <- function(series, r = 0.01, K = 100, T_mat = 1, sigma = 0.2, S = 100) {
   if (is.xts(series)) {
      x <- coredata(series)
   } else if (is.matrix(series)) {
      x <- series
   } else {
      x <- matrix(data = series, nrow = 1)
   }
   loss <- apply(X = x, 
                 MARGIN = 1, 
                 FUN = function(x, S, K, T_mat, sigma, r){
                    delta_t <- 1/250
                    V_0 <- blackscholesformula(S = S, t = 0, K = K, T_mat = T_mat, 
                                               sigma = sigma, r = r, type = "call")
                    V_1 <- blackscholesformula(S = S * exp(x[1]), t = delta_t, K = K, T_mat = T_mat,
                                               sigma = sigma * exp(x[2]), r = r, type = "call")
                    - (V_1 - V_0)/V_0
                 }, 
                 S = S, K = K, T_mat = T_mat, sigma = sigma, r = r)
   if (is.xts(series)) {
      loss <- xts(x = loss, order.by = time(series))
   }
   loss
}

# Confirm that the loss function works:
loss_f(series = c(-.1, -.1), S = 80, sigma = .2) # Loss of .8030928
loss_f(series = c(-.1, .1), S = 100, sigma = .2) # Loss of .4380754

# Create the object histloss by applying loss_f() to rfoptions_lnrtn, assuming S = 100 and sigma = 0.2, and then plot
# histloss.
histloss <- loss_f(series = rfoptions_lnrtn, S = 100, sigma = .2) # Assume K = 100, r = .01, T_mat = 1. 
class(histloss) # xts, zoo
head(histloss, 20)
plot.zoo(histloss, main = "Simulated Historical Losses of Options Portfolio")

# Check the normality and iid assumptions:
qqnorm(y = histloss, main = "QQ Plot of Simulated Historical Losses of Options Portfolio")
qqline(y = histloss, col = "red")
jarque.test(x = as.numeric(histloss)) # pvalue < 2.2e-16. Therefore, reject normality hypothesis.

acf(x = histloss)
acf(x = abs(histloss))
Box.test(x = histloss, lag = 20, type = "Ljung-Box") # IID assumption can be rejected. 

## Now using the histloss simulated data, we can calculate the non parametric density and the 99.5% VaR and ES. 
## Do the same assuming normal distribution.
# 1) Non parametric approach. 
(histloss_VaR995 <- quantile(x = histloss, probs = .995))
(histloss_ES995 <- mean(histloss[histloss >= histloss_VaR995]))

plot(density(histloss), col = "blue", xlab = "Loss", ylab = "Density",
     main = "Non Parametric Density Plot of Simulated Historical Losses of Options Porfolio")
abline(v = histloss_VaR995, col = "red")
text(x = histloss_VaR995 - .02, y = 8.2, labels = "99.5% VaR", col = "red", cex = .6)
abline(v = histloss_ES995, col = "green")
text(x = histloss_ES995 + .015, y = 8.2, labels = "99.5% ES", col = "green", cex = .6)

# 2) Parametric approach. 
mu <- mean(histloss); sigma <- sd(histloss)
(histloss_nrmlVaR995 <- qnorm(p = .995, mean = mu, sd = sigma))
rndm_sim <- rnorm(n = 2000, mean = mu, sd = sigma)
(histloss_nrmlES995 <- mean(rndm_sim[rndm_sim >= histloss_nrmlVaR995])) 
# As expected, slightly off from ESnorm() result in DC.

plot(x = seq(from = -4*sigma, to = 4*sigma, length.out = 200),
     y = dnorm(x = seq(from = -4*sigma, to = 4*sigma, length.out = 200), 
               mean = mu, 
               sd = sigma), 
     type = "l", 
     main = "Normal Distribution of Historical Losses of Options Portfolio", 
     xlab = "Loss", ylab = "Density", col = "blue")
abline(v = histloss_nrmlVaR995, col = "red")
text(x = .12, y = 5.5, labels = "99.5% VaR", col = "red", cex = .6)
abline(v = histloss_nrmlES995, col = "green")
text(x = .165, y = 5.5, labels = "99.5% ES", col = "green", cex = .6)

## Now repeat and calculate the empirical 99%  VaR using weekly log returns data. Further, loss_f() will have to 
## be modified to loss_f_wk()

# Calculate the weekly log returns series:
rfoptions_wk_lnrtn <- apply.weekly(x = rfoptions_lnrtn, FUN = colSums)

# Edit the loss_f() function. Only change is to delta_t
loss_wk_f <- function(series, r = 0.01, K = 100, T_mat = 1, sigma = 0.2, S = 100) {
   if (is.xts(series)) {
      x <- coredata(series)
   } else if (is.matrix(series)) {
      x <- series
   } else {
      x <- matrix(data = series, nrow = 1)
   }
   loss <- apply(X = x, 
                 MARGIN = 1, 
                 FUN = function(x, S, K, T_mat, sigma, r){
                    delta_t <- 5/250 
                    V_0 <- blackscholesformula(S = S, t = 0, K = K, T_mat = T_mat, 
                                               sigma = sigma, r = r, type = "call")
                    V_1 <- blackscholesformula(S = S * exp(x[1]), t = delta_t, K = K, T_mat = T_mat,
                                               sigma = sigma * exp(x[2]), r = r, type = "call")
                    - (V_1 - V_0)/V_0
                 }, 
                 S = S, K = K, T_mat = T_mat, sigma = sigma, r = r)
   if (is.xts(series)) {
      loss <- xts(x = loss, order.by = time(series))
   }
   loss
}

historical_loss_sim <- loss_wk_f(series = rfoptions_wk_lnrtn, S = 120, sigma = .25)
(VaR99 <- quantile(x = historical_loss_sim, probs = .99))
(ES99 <- mean(historical_loss_sim[historical_loss_sim >= VaR99]))

plot(density(historical_loss_sim), col = "blue", xlab = "Loss", ylab = "Density",
     main = "Non Parametric Density Plot of Simulated Weekly Historical Losses of Options Porfolio")
abline(v = VaR99, col = "red")
text(x = VaR99 - .02, y = 5.9, labels = "99% VaR", col = "red", cex = .6)
abline(v = ES99, col = "green")
text(x = ES99 + .015, y = 5.9, labels = "99% ES", col = "green", cex = .6)
