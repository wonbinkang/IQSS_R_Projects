library(tidyverse)
library(magrittr)

## Financial Analytics 
# Covers the very basics of NPV and methods to figure out the value of a project / corporation based on cash
# flows. Will include only code that is new or interesting.

# First chapter covers accounting concepts. Note that the terms vary. See slides for overview of the
# accounting terms.

# Gross profit = Revenue - direct expense; Operating income = Gross profit - operating expense (overhead);
# operating expense = SGA (sales, general, administrative) + depreciation + amortization.

# Depreciation: Consider the purchase of a $100,000 machine which you plan to use for 60 months and then sell
# for $10,000.
purchase_prce <- 100000
salvage_prce <- 10000
life <- 60

# 1) Straightline:
(deprec_per_mnth_strlin <- (purchase_prce - salvage_prce) / life) # 1500 per month

# 2) Units produced:
prod <- data_frame(Month = 1:60, 
                   Units = c(60, 50, 40, 30, 60, 50, 40, 30, 60, 50, 40, 30, 60, 50, 40, 30, 60, 50, 40, 30, 
                              60, 50, 40, 30, 60, 50, 40, 30, 60, 50, 40, 30, 60, 50, 40, 30, 60, 50, 40, 30, 
                              60, 50, 40, 30, 60, 50, 40, 30, 60, 50, 40, 30, 60, 50, 40, 30, 60, 50, 40, 30))
prod$deprec_per_mnth_untprd <- (purchase_prce - salvage_prce) * prod$Units / sum(prod$Units)
prod$deprec_per_mnth_strlin <- deprec_per_mnth_strlin

ggplot(prod, aes(x = Month)) + 
   geom_line(aes(y = deprec_per_mnth_strlin), col = "blue") + 
   geom_line(aes(y = deprec_per_mnth_untprd), col = "red")
# We can explain the patterns in the unit production depreciation as seasonal production trends. 

# Net income or net profit = operating income - tax - interest; where operating income = gross profit -
# operating expense (as noted above).

# Net income is not the end. We need to transform net income into cash in hand (recall Warren Buffet's quote
# from the slides). Income is calculated on an accrual basis, while free cashflow is on a cash basis. 

# Given a future $100 payment, interest rate of .08, plot the present value of $100 when t \in {1, ..., 10}. 
pv_100 <- data_frame(t = 1:10) %>%
   mutate(PV = 100 / (1 + .08)^t)
ggplot(data = pv_100, aes(x = t, y = PV)) + 
   geom_line(col = "blue") + 
   geom_label(aes(label = paste("$", round(PV), sep = ""))) + 
   labs(title = "Discounted Present Value of $100 by Year", 
        xlab = "Year", ylab = "Present Value") + 
   ylim(0, 100)

# Let's plot the PVs for different interest rates / discount factors over a 10 year time frame:
pv_100 <- expand.grid(t = 0:10, r = c(0, seq(from = .05, to = .12, by = .01)))
pv_100 %<>%
   mutate(PV = 100 / (1 + r)^t)
ggplot(data = pv_100, aes(x = t, y = PV, color = factor(r))) + 
   geom_line() + 
   ylim(0, 100) +
   labs(title = "Discounted Value of $100 by Year Received",
        x = "Number of Years in the Future",
        y = "Present Value ($)",
        col = "Discount Rate")

# Real vs nominal discount rates: Recall that we want to discount real (no inflation) cashflows with the real
# discount rate or nominal (with inflation) cashflows with the nominal discount rate. Generally, we get to
# work with real cashflows, but we might not always be so lucky. When we need to convert between them,
# remember to use this formula:
# Nominal Rate = (1 + Real Rate) * (1 + Inflation Rate) - 1
# Note that when dealing with time series data, there is a tension in presenting the data in a table. The tidy
# data framework would require that "time" be a single column, while we normally view data with time encoded
# across multiple columns. See the exercise below:
# Two options: new and old; contract for 10 years; choose option which has the lower present cost:
cf_old <- rep(500, 11)
cf_new <- c(2200, rep(300, 10))
r <- .12
options <- data_frame(t = rep(0:10, 2), 
                      cf = c(cf_old, cf_new), 
                      option = c(rep("old", 11), rep("new", 11)), 
                      PV = cf / (1 + r)^t)
options %>% 
   group_by(option) %>%
   summarise(naive_sum = sum(cf), soph_sum = sum(PV))

## The following section considers the profitability measures of projects:

# 1) Payback period: no discounting (time value of money is not considered); profits not considered.
# Fairly simple, but remember the code for identifying the first instance of a condition:
cashflow <- c(-50000, 1000, 5000, 5000, 5000, 10000, 10000, 10000, 10000, 10000, 10000)
f_pybck <- function(cf) {
   cum_cf <- cumsum(cf)
   pybck_t <- min(which(cum_cf >= 0))
   pybck_t
}
f_pybck(cf = cashflow)
# Note that we could could easily change the function to get a discounted payback period metric:
f_pv_pybck <- function(cf, r) {
   t <- 0:(length(cf) - 1)
   pv <- cf / (1 + r)^t
   cum_pv <- cumsum(pv)
   pybck_t <- min(which(cum_pv >= 0))
   pybck_t
}
f_pv_pybck(cf = cashflow, r = .06)

# 2) NPV

# 3) IRR
# Calculation of IRR (internal rate of return): the IRR is the rate of return such that the NPV of a stream of
# cash flows is equal to 0. In short, the rate is the hurdle rate, or the break even rate. If this rate is
# greater than the discount rate, then we might consider investing.

# DC uses the bisection method, but I can speed up using Newton-Raphson. The following functions are from Bond
# Valuation and Credit Risk project:

ytm_bisection <- function(price, p, r, Tau) {
   t <- 0:Tau
   cf <- c(-price, rep(p * r, Tau - 1), p * (1 + r))
   func <- function(y) {
      sum(cf / (1 + y)^t)
   }
   res <- uniroot(f = func, interval = c(0, 1)) # uniroot() uses the bisection method to arrive at a root.
   print(res$root)
}

# Test ytm() on DC exercise:
ytm_bisection(price = 95.79, p = 100, r = .05, Tau = 5)

# Note that the previous ytm() function uses the uniroot() call which in turn uses the expensive and slow
# bisection method. Newton-Raphson method is a quicker and less expensive method.
fprime <- function(x, f) {
   Delta <- 1e-10
   (f(x + Delta) - f(x)) / Delta
}

newton_raphson <- function(fun, x0, n) {
   vec_iter <- vector("numeric")
   for (i in 1:n) {
      x <- x0 - fun(x0) / fprime(x = x0, f = fun)
      vec_iter[i] <- x
      if (abs(x - x0) < 1e-5) {
         res <- list("iterations" = tail(vec_iter, n = 10), 
                     "root_approx" = tail(vec_iter, n = 1))
         return(res)
      }
      x0 <- x
   }
}

ytm_Newton <- function(price, p, r, Tau, n, x = .5) {
   t <- 0:Tau
   cf <- c(-price, rep(p * r, Tau - 1), p * (1 + r))
   func <- function(y) {
      sum(cf / (1 + y)^t)
   }
   x0 <- x
   res <- newton_raphson(fun = func, x0 = x0, n = n)
   print(res$root_approx)
}
ytm_Newton(price = 95.79, p = 100, r = .05, Tau = 5, n = 100, x = .5)

# Create function for calculating pv and npv:
f_pv <- function(cf, r, t) {
   pv <- cf / (1 + r)^t
   pv
}

f_npv <- function(cf, r) {
   t  <- 0:(length(cf) - 1)
   npv <- sum(f_pv(cf, r, t))
   npv
}

# Try the f_npv() call:
cashflow <- c(0, 20, 20, 20, 20, 20, 20, 10, 5); r_ex <- .06
f_npv(cf = cashflow, r = r_ex)

# Use the bisection method to calculate IRR: 
f_irr_bisec <- function(cf) {
   res <- uniroot(f = f_npv, interval = c(0, 1), cf = cf)
   res$root
}

cashflows <- c(-100, 20, 20, 20, 20, 20, 20, 10, 5)
f_irr_bisec(cf = cashflows)

# Use the Newton-Raphson method:
f_irr_newraph <- function(cf) {
   t <- 0:(length(cf) - 1)
   npv <- function(r) {
      sum(cf / (1 + r)^t)
   }
   res <- newton_raphson(fun = npv, x0 = .5, n = 100)
   res$root_approx
}
f_irr_newraph(cf = cashflows)

# 4) Profitability Index: NPV / Initial investment
f_prof_indx <- function(init_invst, cf, r) {
   npv <- f_npv(cf = cf, r = r)
   prof_index <- npv / abs(init_invst)
   prof_index
}
cashflows <- c(0, 20, 20, 20, 20, 20, 20, 10, 5)
f_prof_indx(init_invst = 100, cf = cashflows, r = .08)

# 5) Terminal Value (TV): Assume our forecasting period is until period t. Then future cashflows from t + 1
# onwards can be calculated as a single lump sum that is then discounted to PV. The reason we do this is
# because a longer forecasting horizon means that there is less accuracy in our forecasts.
cashflow <- c(-1000, 100, 100, 200, 300, 400, 500, 600, 600, 600, 600, 500, 500)
# Assume that the length(cashflow) - 1 is the final payment period and that last(cashflow) is the start of a
# stream of cashflows representing TV.
cf_tv <- last(cashflow)
final_t <- length(cashflow) - 1

# Assume a discount rate of .15 and calculate TV for cash growth rate of .1, .01, -.05. Note that we use a
# perpetuity calculation.
r <- .15; g1 <- .1; g2 <- .01; g3 <- -.05
(tv10 <- cf_tv / ((r - g1) * (1 + r)^final_t))
(tv01 <- cf_tv / ((r - g2) * (1 + r)^final_t))
(tv_minus05 <- cf_tv / ((r - g3) * (1 + r)^final_t))

# See slides for the benefits and drawbacks of each of the profitability measures discussed above. The gist is
# that NPV and IRR will lead to the same conclusion on the question of whether a project is profitable (recall
# that IRR calculation sets NPV = 0). However, between profitable projects, the two measures may lead to
# different results.
cashflow1 <- c(-50000, 100, 2000, 2000, 5000, 10000, 10000, 10000, 10000, 10000, 10000)
cashflow2 <- c(-1e+05, 2e+04, 2e+04, 2e+04, 2e+04, 2e+04)
cashflow3 <- c(-8000, 6000, 5000, 4000, 3000, 2000, 1000, 0)

# Calcualte the IRRs:
r1 <- f_irr_newraph(cf = cashflow1)
r2 <- f_irr_newraph(cf = cashflow2)
r3 <- f_irr_newraph(cf = cashflow3)

# If we use the IRRs from above as the discount rate, the NPVs should equal 0
f_npv(cf = cashflow1, r1) 
f_npv(cf = cashflow2, r2)
f_npv(cf = cashflow3, r3)

# Note that IRR has problems because it provides a rate and doesn't give context to the rate; namely the size
# of the investment. Furthermore, IRR calculations may lead to multiple IRRs or none. 
cf1 <- c(-5000, 450, 450, 450, 450, 450, 450, 450, 450, 450, 450)
cf2 <- c(-5000, 2000, 2000, 2000, 2000, 2000, 2000, -2000, -2000, -2000, -2000)
f_irr_newraph(cf = cf1) # -0.01871167. Rate should be between 0 and 1
f_irr_newraph(cf = cf2) # 0.1805686, but there is a second rate. 

df <- data_frame(rates = seq(0, .25, .005)) %>%
   group_by(rates) %>%
   mutate(npv1 = f_npv(cf = cf1, r = rates), 
          npv2 = f_npv(cf = cf2, r = rates))

ggplot(data = df, aes(x = rates, y = npv1)) + 
   geom_line(color = "blue") + 
   geom_line(aes(y = npv2), color = "red") + 
   labs(title = "NPV by Discount Rate", subtitle = "A Tale of Two Troubling Cashflows", 
        y = "NPV ($)",x = "Discount Rate (%)") + 
   geom_hline(yintercept = 0, col = "black", lwd = 1.2) + 
   annotate(geom = "text", x = .2, y = -500, label = "Two rates at which NPV = 0") + 
   annotate(geom = "text", x = .2, y = -2500, label = "No rates at which NPV = 0") 

# IRR and NPV should lead to the same decision on whether a project is profitable. In certain instances, this
# doesn't hold because IRR is in terms of rates and NPV is in absolute dollar values.
df <- data_frame(option = rep(x = 1:4, each = 11), time = rep(0:10, times = 4), 
                 cashflow = c(-10, rep(4, times = 10), -1000, rep(300, times = 10), 
                              -1e+5, rep(2e+4, times = 10), -10, rep(1, times = 10)))
# Note the difference in magnitude of the cashflows. 

# Create a new df:
df2 <- df %>%
   group_by(option) %>%
   summarise(npv = f_npv(cf = cashflow, r = .1),
             irr = f_irr_bisec(cf = cashflow)) # f_irr_newraph: returns netative IRR for option 4. 
ggplot(data = df2, aes(x = npv, y = irr, color = factor(option))) + 
   geom_point(size = 5) + 
   geom_hline(yintercept = .1) + 
   scale_y_continuous(labels = scales::percent) + 
   scale_x_continuous(labels = scales::dollar) + 
   labs(title = "NPV versus IRR for Project Alternatives", 
        subtitle = "NPV calculation assumes 10% discount rate",
        caption = "Line shows actual discount rate to asses IRR break-even",
        x = "NPV ($)", y = "IRR (%)", col = "Option")
# Based on this plot, project 4 is never profitable, but any of projects 1, 2, and 3 would appear to be
# profitable. However, which is the most profitable depends on which metric we use. Project 1 has the highest
# IRR, but project 3 undoubtedly produces the largest NPV. However, this is because it requires the highest
# initial investment.

## Case study: 
assumptions <- read_csv(file = "assumptions_cafe.csv", col_names = T)
colnames(assumptions) <- c("year", "sls_pr_dy", "capex", "pct_cannb", "mnt_cst", "dprc_cst", 
                           "prft_mrgn_pr_nitro", "prft_mrgn_pr_reg", "lbr_cst_pr_hr", "dys_pr_yr")
tax_rate <- .36
cf_df <- assumptions %>%
   mutate(sls_pr_yr = sls_pr_dy * dys_pr_yr,
          revenue = sls_pr_yr * prft_mrgn_pr_nitro,
          lbr_cst = dys_pr_yr * 0.5 * lbr_cst_pr_hr, 
          cnnb_cst = sls_pr_yr * pct_cannb * prft_mrgn_pr_reg) %>% # Business model
   mutate(dir_expns = lbr_cst + cnnb_cst + mnt_cst, 
          gross_profit = revenue - dir_expns,
          op_income = gross_profit - dprc_cst,
          net_income = op_income * (1 - tax_rate),
          cashflow = net_income + dprc_cst - capex) # Financial metrics.
 
# Scenario analysis via purrr package: nest - map workflow. The following exercises extensively use purrr's
# map() function.
# First, create a model calculation function:
f_busi_mod <- function(assumptions) {
   assumptions %>%
      mutate(sls_pr_yr = sls_pr_dy * dys_pr_yr,
             revenue = sls_pr_yr * prft_mrgn_pr_nitro,
             lbr_cst = dys_pr_yr * 0.5 * lbr_cst_pr_hr, 
             cnnb_cst = sls_pr_yr * pct_cannb * prft_mrgn_pr_reg) %>% # Business model
      mutate(dir_expns = lbr_cst + cnnb_cst + mnt_cst, 
             gross_profit = revenue - dir_expns,
             op_income = gross_profit - dprc_cst,
             net_income = op_income * (1 - tax_rate),
             cashflow = net_income + dprc_cst - capex)
}

# Second, change the assumptions df to an optimist and pessimist df:
optimist <- assumptions %>%
   mutate(sls_pr_dy = sls_pr_dy * 1.2, pct_cannb = 0.1)
pessimist <- assumptions %>%
   mutate(sls_pr_dy = sls_pr_dy * .8, prft_mrgn_pr_nitro = 1)

# Combine cf_df, pessimist and optimist into one df:
scenarios <- bind_rows(
   mutate(assumptions, scenario = "realist"),
   mutate(pessimist, scenario = "pessimist"),
   mutate(optimist, scenario = "optimist")
)

scenario_analysis <- scenarios %>%
   nest(-scenario) %>%
   mutate(new_cf = map(.x = data, .f = f_busi_mod)) %>%
   mutate(npv = map_dbl(.x = new_cf, .f = function(new_cf, r) {
      cfline <- new_cf$cashflow
      sum(f_pv(cf = cfline, r = r, t = 0:(length(cfline) - 1)))
   }, r = .2)) # Note that you can pass through the discount rate .2

scenario_analysis %>% 
   select(scenario, npv)

ggplot(data = scenario_analysis, mapping = aes(x = scenario, y = npv, fill = scenario)) + 
   geom_bar(stat = "identity") + 
   scale_y_continuous(labels = scales::dollar) + 
   labs(title = "NPV Scenario Analysis of Nitro Coffee Expansion") + 
   guides(fill = F) # To remove the legend

## Note that scenario analysis does not take into account the possibility of error. All of the above led to
## point estimates. Sensitivity analysis is a non-statistical yet helpful method of assessing uncertainty. 

# See how NPV changes when each of prft_mrgn_pr_nitro, lbr_cst_pr_hr, pct_cnnb, and sls_pr_dy are allowed to
# range from 50 to 150 percent of our assumptions estimates. Note sensitivity analysis is a form of
# comparative statics and we assume that all other variables are kept unchanged.

# Using the assumptions dataset, create the new_assmptns dataset, which includes different weights on chosen
# assumption variables. Using the new_assmptns dataset, calculate the NPVs of cashflows for each weight and
# store as sensitivity.
f_wgt_assmp <- function(data, weight, metric) {
   data[metric] <- data[metric] * weight
   data 
}

new_assmptns <- expand.grid(weight = seq(from = .5, to = 1.5, by = .1), 
                            metric = c("prft_mrgn_pr_nitro", "lbr_cst_pr_hr", "pct_cannb", "sls_pr_dy"), 
                            stringsAsFactors = F) %>%
   as_data_frame() %>% # This line and the previous line is not included in DC. DC code leads to incorrect result.
   mutate(scenario = map2(weight, metric, ~ f_wgt_assmp(data = assumptions, .x, .y)))
# Creates an array where scenario is a list of 44 (each permutation of weight and metric) and each element
# within the list is a  dataframe with data akin to the dataframe assumptions.

# Using similar code to the scenario analysis above:
sensitivity <- new_assmptns %>%
   mutate(new_cf_dfs = map(.x = scenario, .f = f_busi_mod)) %>%
   mutate(npv = map_dbl(.x = new_cf_dfs, .f = function(df, r) {
      cfline <- df$cashflow
      sum(f_pv(cf = cfline, r = r, t = 0:(length(cfline) - 1)))
   }, 
   r = .2))
names(sensitivity)
str(sensitivity$npv)

sensitivity %>%
   select(weight, metric, npv)

# Plot how the npv changes as you change the weights of each metric:
sensitivity %>%
   ggplot(aes(x = weight, y = npv, color = metric)) + 
      geom_line() + 
      scale_x_continuous(labels = scales::percent) + 
      scale_y_continuous(labels = scales::dollar) + 
      labs(title = "Sensitivity Analysis", 
           x = "Factor on Original Assumption",
           y = "Projected NPV",
           col = "Metric")

# Gathering and spreading data; Creating a waterfall plot using geom_rect()
cashflow <- data_frame(c("Metric", "Received", "Spent"), c(1, 100, 150), c(2, 200, 175), c(3, 300, 200), 
                       c(4, 400, 225), c(5, 500, 250), c(6, 500, 250))
colnames(cashflow) <- cashflow[1, ]
cashflow <- cashflow[-1, ]

# Practice gathering and spreading:
long_cash_flow <- cashflow %>%
   gather(key = Month, value = Value, -Metric)
tidy_cash_flow <- long_cash_flow %>%
   spread(key = Metric, value = Value)
untidy_cash_flow <- long_cash_flow %>%
   spread(key = Month, value = Value)

# Wrangle data for waterfall plot:
gross_profit_summary <- data_frame(metric = c("Sales Revenue", "Keg Cost", "Cannibalization Cost", 
                                              "Labor Cost", "Maintenance Cost"), 
                                   value = c(187200, -78240, -31200, -10000, -2500))
waterfall <- gross_profit_summary %>%
   mutate(end = cumsum(value), 
          start = lag(x = end, n = 1, default = 0))
gross_profit <- data_frame(metric = "Gross Profit", value = NA, 
                           end = sum(gross_profit_summary$value), start = 0)
waterfall_data <- bind_rows(waterfall, gross_profit) %>%
   mutate(row_num = row_number())

# Waterfall plot:
ggplot(waterfall_data, aes(fill = (end > start))) + # Note that if TRUE, then one fill, if FALSE, another fill.
   geom_rect(aes(xmin = row_num - 0.25, xmax = row_num + 0.25, 
                 ymin = start, ymax = end)) +
   geom_hline(yintercept = 0) +
   scale_x_continuous(breaks = waterfall_data$row_num, labels = waterfall_data$metric) +
   scale_y_continuous(labels = scales::dollar) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         axis.title.x = element_blank()) +
   guides(fill = FALSE) +
   labs(
      title = "Gross Profit for Proposed Nitro Coffee Expansion",
      subtitle = "Based on pro forma 10-year forecast")

## FINANCIAL TRADING USING QUANTSTRAT PACKAGE:
## The quantstrat package is currently only available at github, therefore, I'll use the script below to
## practice elements that can be run only on this machine.
library(quantmod) # loads xts and zoo
getSymbols(Symbols = "SPY", src = "yahoo", auto.assign = T, 
           from = "2000-01-01", to = "2016-06-30",
           adjust = T) # Note that adjusting for dividends and splits reduces prices markedly.
head(SPY); str(SPY); class(SPY)

plot.zoo(x = Cl(SPY), plot.type = "single", main = "SPY from Jan 1, 2000 to June 30, 2016", col = "blue",
         ylab = "SPY Closing Price")

## The basics of trading involve: data; indicator = func1(data); signal = func2(indicator); rule =
## func3(signal). 
getSymbols(Symbols = "LQD", src = "yahoo", auto.assign = T,
           from = "2000-01-01", to = "2016-06-30",
           adjust = T) 
head(LQD) # LQD data available only from 2002-07-30

# Calculate the 200 day simple moving average (SMA):
LQD_cl <- Cl(LQD)
sma_LQD <- rollapply(data = LQD_cl, width = 200, FUN = mean, na.rm = T)
sma_LQD <- sma_LQD[!is.na(sma_LQD)]
head(sma_LQD); class(sma_LQD)

plot(x = LQD_cl, main = "LQD from July 30, 2002 to June 30, 2016 and 200 day SMA", 
     col = "blue", ylab = "LQD Closing Price")
lines(x = sma_LQD, col = "red") 
# Note that we're back to an xts plot because plot.zoo is incompatible with lines.xts and lines.zoo() does not
# exist. Do not use plot.zoo with lines(). There are a number of problems with calling plots on xts and zoo objects. 

SPY_cl <- Cl(SPY)
sma_SPY <- rollapply(data = SPY_cl, width = 200, FUN = mean, na.rm = T)
sma_SPY <- sma_SPY[!is.na(sma_SPY)]
plot(x = SPY_cl, main = "SPY from July 30, 2002 to June 30, 2016 and 200 day SMA", 
     col = "blue", ylab = "SPY Closing Price")
lines(x = sma_SPY, col = "red")

## Review slides on quantstrat initialization, which includes the creation of accounts, portfolios, strategies
## and orders.

# Use the functions in the TTR (Technical Trading Rules) package to create indicators (transformation of raw
# market data)
library(TTR)
sma_SPY_TTR <- SMA(x = SPY_cl, n = 200)
rsi_SPY <- RSI(price = SPY_cl, n = 3) 
# RSI with 3 day look back period: RSI = 100 - [100 / {1 + mean(in period gain) / |mean(in period loss)|}].
# Usually the 14 day lookback period is norm. RSI above 70 is overpriced shares and under 30 is oversold
# shares (rule of thumb).

plot(x = SPY_cl, main = "SPY Closing Prices and 200 day SMA", col = "blue", ylab = "Price (USD)")
lines(x = sma_SPY, col = "red")

plot(x = SPY_cl, main = "SPY Closing Prices and 14 day RSI", col = "blue", ylab = "Price (USD)", 
     xlim = c(0, 250))
lines(x = RSI(price = SPY_cl, n = 14), col = "red")
lines(x = RSI(price = SPY_cl, n = 2), col = "orange", lwd = .5)
# Note that placing all three stats on a single plot is not a good idea.

## Now we need to add indicators to a strategy. Use quantstrat's add.indicator() which is akin in its syntax
## structure to the apply() family of R functions. SKIP, see DC slides.
# Here are a few indicator functions:
# Avg RSI of two RSIs
RSI_avg <- function(price, n1, n2) {
   rsi1 <- RSI(price = price, n = n1)
   rsi2 <- RSI(price = price, n = n2)
   rsi_avg <- (rsi1 + rsi2)/2
   colnames(rsi_avg) <- "RSI_AVG"
   rsi_avg
}

# David Varadi Oscillator
DVO <- function(HLC, navg = 2, percentlookback = 126) {
   ratio <- Cl(HLC) / (Hi(HLC) + Lo(HLC)) / 2
   avgratio <- SMA(x = ratio, n = navg)
   output <- runPercentRank(x = avgratio, n = percentlookback, exact.multiplier = 1) * 100
   colnames(output) <- "DVO"
   return(output)
}
plot(DVO(HLC(SPY)), col = "blue")




























