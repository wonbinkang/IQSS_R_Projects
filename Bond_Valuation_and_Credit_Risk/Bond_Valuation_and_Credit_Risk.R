library(Quandl)
library(quantmod)
library(tidyverse)

## FIXED INCOME AND BOND VALUATION
## First write a function calculating the value of a straight bond with principal, fixed coupon, NO embedded 
## options, fixed maturity and yield / discount rate:
fxd_bond_val <- function(p, r, Tau, y) {
   bond <- data.frame(cash_flow = c(rep(x = p * r, times = Tau - 1), p * (1 + r)))
   bond$t <- 1:Tau
   bond$discount_factor <- 1 / (1 + y)^bond$t
   bond$PV <- bond$cash_flow * bond$discount_factor
   print(sum(bond$PV))
}
fxd_bond_val(10^8, .04, 60, .06)

# Use the Quandl package to get the bond yield for Moodys BAA rated bonds:
baa <- Quandl(code = "FED/RIMLPBAAR_N_M", type = "xts")
periodicity(baa) # Monthly periodicity from Jan 1919 to Sep 2016
baa_yield <- baa["201609", drop = T] / 100 # drop = T extracts a scalar and not a subset of an xts object, 
# which is an xts object. 

# Now calculate the value of a bond with principal 100, coupon .05, maturity 5 years, and yield equal to baa_yield:
fxd_bond_val(p = 100, r = .05, Tau = 5, y = baa_yield)

# Note an important fact about bonds: price / value is inversely correlated with yield:
# Assume a bond with principal = 100, coupon = .1, and Tau = 20.
prc_yld <- data.frame(yld = seq(from = .01, to = .4, by = .01)) 
prc_yld %>%
   mutate(PV = map_dbl(.x = prc_yld$yld, 
                       .f = function(yld) {
                              fxd_bond_val(p = 100, r = .1, Tau = 20, yld)
                       }))
ggplot(prc_yld, aes(x = yld, y = PV)) + 
   geom_line(color = "blue") + xlab("Yield") + ylab("Present Value") +
   ggtitle("Price/Yield-to-Maturity (YTM)")
   

## Decomposing the yield:
## Yield = risk-free yield + spread
   # risk free yield = US treasury bond issued at similar type of bond and with similar maturity
   # spread quantifies the various risks associated with holding the bond:
      # credit risk: risk of issuer default
      # call risk: risk of issuer buyback at time disadvantageous to the holder
      # inflation risk: risk of inflation devaluing future cash flows
      # liquidity risk: most bonds are not traded publicly
# The risk premium for holding non risk free bonds can be approximated by the investment grade bond spread (in
# Moody's scale the difference between the Aaa bond yield and Baa bond yield).

# Load 10 year US Treasury yield data and plot
getSymbols(Symbols = "DGS10", src = "FRED", auto.assign = T)
class(DGS10) # xts zoo
periodicity(DGS10) # Daily periodicity from 1962-01-02 to 2018-11-19
plot.zoo(DGS10["200601/201811"], col = "blue", main = "10 Year US Treasury Yields", xlab = "Date", 
         ylab = "Yield (%)")

# Load the Aaa and Baa bond yields; calculate the difference and plot the difference
bond_yld <- Quandl(code = c("FED/RIMLPBAAR_N_M", "FED/RIMLPAAAR_N_M"), type = "xts")
bond_yld <- bond_yld["200601/201609"]
bond_yld$spread <- (bond_yld[, 1] - bond_yld[, 2]) * 100 # To create basis points (bps)
plot.zoo(x = bond_yld$spread, plot.type = "single", xlab = "Date", ylab = "Spread (bps)", 
         main = "Baa - Aaa Spread", col = "blue", lwd = 2)

# Write a function that will calculate the yield to maturity (YTM) on a bond given price, principal, coupon,
# and maturity:
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

## The next chapter discusses how bond prices change as the coupon and time to maturity changes. All of these
## changes are determined for a 1 basis point or .01% change in the yield.
# Think of these changes in terms of second order derivatives. The greater the size of the second order
# derivative, the greater the volatility:
# Fixed income instrument with p = 100, r = .05, Tau = 20, y = .06. Compare:
(fxd_bond_val(p = 100, r = .05, Tau = 20, y = .06) / 
       fxd_bond_val(p = 100, r = .05, Tau = 20, y = .0601) - 1) # .001191

(fxd_bond_val(p = 100, r = .02, Tau = 20, y = .06) / 
      fxd_bond_val(p = 100, r = .02, Tau = 20, y = .0601) - 1) # .0014319
# Lower coupon has greater volatility

(fxd_bond_val(p = 100, r = .05, Tau = 20, y = .06) / 
      fxd_bond_val(p = 100, r = .05, Tau = 20, y = .0601) - 1) # .0011901

(fxd_bond_val(p = 100, r = .05, Tau = 5, y = .06) / 
      fxd_bond_val(p = 100, r = .05, Tau = 5, y = .0601) - 1) # .000428
# Longer maturity has greater volatility

# Definition: Price value of a basis point:
(fxd_bond_val(p = 100, r = .05, Tau = 5, y = .06) - 
      fxd_bond_val(p = 100, r = .05, Tau = 5, y = .0601)) # $0.041 change in the price of bond.

## Macauly Duration estimates how much a bond’s price is likely to rise or fall if interest rates change 
## (the bond’s price sensitivity), and it can be thought of as a measurement of interest rate risk.
# Create a function for calculating Macaulay Duration:
macaulay_dur <- function(price, p, r, Tau, y) {
   t <- 1:Tau
   wgt <- t / (1 + y)^t
   cash_flow <- c(rep(p * r, Tau - 1), p * (1 + r))
   PV_wgtd <- wgt * cash_flow
   res <- sum(PV_wgtd) / price
   print(res)
}

# Review the definition of duration and notice the following:
   # 1) it is a measure of interest rate risk;
   # 2) measures how long it takes, in coupon payment periods, for an investor to be repaid the bond’s price
   #    by the bond’s total cash flows. 
      # a) zero coupon bond will have a duration equal to maturity (Tau)
      # b) plain vanilla bond will have a duration less than maturity 

## An approximation formula is:
   # (price(y + \delta y) - price(y - \delta y)) / 2 * price * \delta y = D
   # => \delta price / price = - D * \delta y => \delta price = -D * \delta y * price
   # where \delta y is the expected change in the yield y
   # The approximation implies that D * price is equal to the slope of the tangent line of the Price - yield
   # curve.

# Plain vanilla bond: price = fxd_bond_val(), p = 100, r = .1, Tau = 20, y = .1
# Use maccaully_dur:
price <- fxd_bond_val(p = 100, r = .1, Tau = 20, y = .1)
mac_dur <- macaulay_dur(price = price, p = 100, r = .1, Tau = 20, y = .1) # 9.36492

# Use approximation with expected \delta y = .01:
price_up <- fxd_bond_val(p = 100, r = .1, Tau = 20, y = .11)
price_down <- fxd_bond_val(p = 100, r = .1, Tau = 20, y = .09)
(apprx_dur <- (price_down - price_up) / (2 * price * .01)) # 8.545937
# Approximation is not very good

# Using the approximation, calculate the effect of duration on price when yield decreases by -0.01:
apprx_dur_pct_price <- -apprx_dur * (-0.01) # Don't forget the negative sign
apprx_dur_price <- -apprx_dur * (-0.01) * price

## The next topic is convexity, which is obvious given that duration is a linear approximation that gives poor
## approximations for larger values of \delta y. Using convexity, estimates should be improved.
## Convexity is the derivative of duration wrt y (or second derivative of fxd_bond_val function wrt y).

# Write a convexity function:
convexity <- function(price, p, r, Tau, y) {
   t <- 1:Tau
   wgt <- (t^2 + t) / ((1 + y)^t)
   cash_flow <- c(rep(p * r, Tau - 1), p * (1 + r))
   PV_wgtd <- wgt * cash_flow
   res <- sum(PV_wgtd) * (price * (1 + y)^2)^(-1)
   print(res)
}
# When working with Maccauly and convexity be careful of the annual coupon payments and the annual yield. 

## An approximation formula for convexity is:
   # convexity = ((price(y + \delta y) + price(y - \delta y)) - 2 * p) / (price * \delta y^2)
   # => \delta price / price = .5 * convexity * \delta y^2 => 
   # \delta price = price * .5 * convexity * \delta y^2
# where \delta y is the expected change in the yield y

# Compare the convexity calculations using convexity() and the DC approximation for a plain vanilla bond:
price <- fxd_bond_val(p = 100, r = .1, Tau = 20, y = .1) # Since the coupon and yield are equal, bond price 
# should be equal to face value.
actual_cvx <- convexity(price = 100, p = 100, r = .1, Tau = 20, y = .1) # 116.219

apprx <- (fxd_bond_val(p = 100, r = .1, Tau = 20, y = .11) + 
             fxd_bond_val(p = 100, r = .1, Tau = 20, y = .09) - 2 * price) / (price * (.01)^2) # 116.522
# Approximation is fairly close to convexity.

# Using the approximation, calculate the effect of convexity on price:
(apprx_cnvx_pct_price <- .5 * apprx * .01^2) # .005826
(apprx_cnvx_price <- .5 * apprx * .01^2 * price) # $0.5826
apprx_dur_price <- -apprx_dur * (-0.01) * price

# Using duration and convexity, estimate the price change of a bond for given change in yield:
price_change <- apprx_dur_price + apprx_cnvx_price
# The estimate new price for a -0.01 change in yield for this bond is:
price + price_change # 109.1285

# Now let's try this with the actual macaulay duration and convexity:
price - mac_dur * -.01 * price + (.5 * actual_cvx * .01^2 * price) # 109.946

# In short, the approximations of the first and second order derivatives are valid. 

# In conclusion, the duration is the first order derivative of the Price-yield curve and convexity is the
# second order derivtive. If we want to approximate the change in bond prices as yields change, we can use a
# Taylor approximation. The duration + convexity is a second order Taylor approximation.

## Exercise: 
#  Value a bond with a $100 par value, 3% coupon rate, and 8 years to maturity. This bond was rated Aaa by
#  Moody's and it was issued on September 30, 2016.
aaa <- Quandl(code = "FED/RIMLPAAAR_N_M", type = "xts") # type = "raw" returns a dataframe
aaa_yield <- as.numeric(aaa["201609"])/100 

# Calculate the value of the plain vanilla bond:
price <- fxd_bond_val(p = 100, r = .03, Tau = 8, y = aaa_yield) # 97.1711

# Now calculate the expect price of the bond assuming that the yield increases by 1 % (+0.01):
# Using functions:
new_price <- price - macaulay_dur(price = price, p = 100, r = .03, Tau = 8, y = aaa_yield) * .01  * price + 
             convexity(price = price, p = 100, r = .03, Tau = 8, y = aaa_yield) * .5 * (.01)^2 * price
new_price # 90.44304

# Using approximations:
# Calculate duration
apprx_dur <- (fxd_bond_val(p = 100, r = .03, Tau = 8, y = aaa_yield - .01) - 
                 fxd_bond_val(p = 100, r = .03, Tau = 8, y = aaa_yield + .01)) / (2 * price * .01) 
# 6.988271 which is close to macaulay_dur = 7.216969

apprx_dur_dp <- -apprx_dur * .01 * price
   
apprx_cnvx <- (fxd_bond_val(p = 100, r = .03, Tau = 8, y = aaa_yield - .01) + 
                  fxd_bond_val(p = 100, r = .03, Tau = 8, y = aaa_yield + .01) - 2 * price) / 
                  (price * .01^2)
# 58.66418 which is close to convexity() = 58.61533

apprx_cnvx_dp <- .5 * apprx_cnvx * (.01^2) * price

# Therefore, the new price is:
new_price <- price + apprx_dur_dp + apprx_cnvx_dp # 90.38342 ($0.06 away from the actual value)

# Note that I've been very loose with the positive and negative values of the terms. Be careful. 

## CREDIT RISK
## Re-read the decomposition of the yield above into credit risk, liquidity risk, call risk, and inflation
## risk. In the next set of exercises we'll be dealing with credit risk.
# Given credit risk, the expected loss (EL) is equal to the product of the probability of default, exposure at
# default, and loss given default => EL = PD * EAD * LGD, where LGD is percentage multiplied by the exposure. 
loans <- readRDS(file = "loan_data_ch1.rds") # Dim = 29092 x 8

# Create contingency tables for the discrete variables grade and loan_status:
table(loans$loan_status)
prop.table(table(loans$loan_status))

table(loans$grade, loans$loan_status)
prop.table(table(loans$grade, loans$loan_status)) * 100 # Percentages are of total

# Calculate percentages based on row and column:
prop.table(x = table(loans$grade, loans$loan_status), margin = 1) * 100 
# Greater likelihood of default the worse the grade.
prop.table(x = table(loans$grade, loans$loan_status), margin = 2) * 100 
# Of the total defaults, grade B has the highest proportion, which is again obvious since higher grades
# receive more loans.

## The contingency table analogue for continuous variables is the histogram. 
# Review the data for outliers and missing values:
hist(x = loans$int_rate, main = "Histogram of Interest Rates", xlab = "Interest rate (%)")
hist(x = loans$annual_inc, main = "Histogram of Annual Income", xlab = "Dollars($)") # Non informative plot:

# When dealing with a histogram that is not informative, first look at the breaks:
hist(x = loans$annual_inc, main = "Histogram of Annual Income", xlab = "Dollars($)")$breaks

# Since the original histogram had too few breaks, change the number of breaks using rule of thumb:
hist(x = loans$annual_inc, breaks = sqrt(nrow(loans)),
     main = "Histogram of Annual Income", xlab = "Dollars($)")

# This changed the histogram but we still do not have much information on the distribution of annual incomes. 
# Therefore, we need to identify a threshold for identifying outliers (here, only positive outliers will exist).
# 1) Expert judgment: assume expert states that incomes greater than 3 million are outliers. Remove the following
# rows:
hist(x = loans[-which(loans$annual_inc > 3e+6), ]$annual_inc, breaks = sqrt(nrow(loans)),
     main = "Histogram of Annual Incomes", xlab = "Dollars ($)")

# 2) Another rule of thumb: values greater than Q3 + 1.5*IQR (or Q1 - 1.5*IQR). Remove the following rows:
hist(x = loans[-which(loans$annual_inc > 
                         quantile(x = loans$annual_inc, probs = .75, na.rm = T) + 
                         (1.5*IQR(x = loans$annual_inc, na.rm = T))), ]$annual_inc, 
     breaks = sqrt(nrow(loans)), main = "Histogram of Annual Incomes", xlab = "Dollars ($)")
# Note that there over 1300 rows that are removed as outliers (too much??). 
# Furthermore, that there are strange peaks within the distribution. The reason for the peaks is that debtors
# report income in round numbers.

hist(x = loans$loan_amnt, breaks = 200, main = "Histogram of Loan Amounts", xlab = "Loan amount ($)")
# We can see the same phenomenon wh plotting the histogram of loan amounts. 

# Yet another uninformative histogram is for "age" variable:
hist(loans$age, xlab = "Age", main = "Histogram of Age")
plot(x = loans$age, ylab = "Age") # Two possible outliers / mistakes exist: age over 140 and age close to 100.
# Let's take a look at the data for individuals over 80:
loans[which(loans$age > 80), ]
# We can see that the individual aged 144 is also the individual with 6e+5 income. Throw away.
loans <- loans[-which(loans$age > 100), ]

# The other two rows appear to be consistent. Plot a new histogram:
hist(x = loans$age, breaks = 30, xlab = "Age", main = "Histogram of Age")


## What do we do with NA and missing values?
# First, identify the columns/variables where the NAs exist:
summary(loans) # 2776 NAs in int_rate and 809 in emp_length (both continuous vars). Note that similar 
# principles to the ones used below apply to categorical variables. 

# Identify the rows where NAs exist for a column:
na_index <- which(is.na(x = loans$int_rate))

# Next, determine what we want to do:
# 1) Delete rows with NA. Deleting a column is much less likely to happen. 
loans_na_delete <- loans[-na_index, ]

# 2) Replace / Impute with the median. If categorical variable, then use most frequent category.
ir_med <- median(x = loans$int_rate, na.rm = T)
loans_na_impute <- loans
loans_na_impute$int_rate[na_index] <- ir_med
summary(loans_na_impute$int_rate) # No NAs. 

# 3) Keep: For continuous variables, transform them into categorical variables with an NA category. We call
# this coarse classification. Use the dplyr functions:
library(tidyverse)
library(magrittr)
loans %<>%
   mutate(ir_cat = case_when(int_rate <= 8 ~ "0-8", 
                             int_rate > 8 & int_rate <= 11 ~ "8-11",
                             int_rate > 11 & int_rate <= 13.5 ~ "11-13.5",
                             int_rate > 13.5 ~ "13.5+",
                             is.na(int_rate) ~ "Missing")) %>%
   mutate(ir_cat = as.factor(ir_cat))
plot(loans$ir_cat)

# Coarse classification for emp_length:
loans %<>%
   mutate(emp_cat = case_when(emp_length <= 15 ~ "0-15",
                              emp_length > 15 & emp_length <= 30 ~ "15-30",
                              emp_length > 30 & emp_length <= 45 ~ "30-45",
                              emp_length > 45 ~ "45+",
                              is.na(emp_length) ~ "Missing")) %>%
   mutate(emp_cat = as.factor(emp_cat))

# Remove the coarse classified variables:
loans$int_rate <- NULL
loans$emp_length <- NULL

# The newly created variables have unintuitive factors. Reorder these factors:
table(loans$ir_cat)
table(loans$home_ownership)
loans %<>%
   mutate(ir_cat = fct_relevel(.f = ir_cat, "0-8", "8-11", "11-13.5", "13.5+", "Missing"),
          home_ownership = fct_relevel(.f = home_ownership, "RENT", "MORTGAGE", "OWN", "OTHER"))
levels(loans$ir_cat)

## Create training and testing dataset. Use the trained model to predict and create confusion table, which is
## a contingency table of correct and incorrect classifications. Note that by convention, the actual states
## are on the rows and the model predictions are on the columns.
set.seed(567)
train_index <- sample(x = 1:nrow(loans), size = round((2/3)*nrow(loans)))
train_loans <- loans[train_index, ]
test_loans <- loans[-train_index, ]

# Confusion matrix is omitted here. 

## Logistic Regression:
# Back the basics:
mod1_log <- glm(formula = loan_status ~ age + ir_cat + grade + loan_amnt + annual_inc, family = "binomial", 
                data = train_loans)
# Use summary for inference:
summary(mod1_log)
# Reference category is "0-8". What does .38 coefficient on ir_cat "8-11" mean? Compared to the reference category,
# odds of default multiplied by:
exp(.38) # = 1.462285 => increase in the odds of a default. 

# Predict on test set:
pred1 <- predict(object = mod1_log, newdata = test_loans, type = "response")
summary(pred1)
# Note that probability of default does not go over .3716557. Default probabilities are very low and so the 
# range provided by predict will also be low. 

# Include all variables in loans and check the range of predictions using summary:
mod2_logfull <- glm(formula = loan_status ~ ., data = train_loans, family = "binomial")
summary(mod2_logfull)
pred2 <- predict(object = mod2_logfull, newdata = test_loans, type = "response")
summary(pred2) # Range from .0000052 to .531422

# dplyr method of creating default predictions using a cutoff:
# First, figure out what cutoff to use:
prop.table(table(loans$loan_status)) # 11% default
quantile(x = pred1, probs = .89) # .1840819; set as cutoff for mod1_log
quantile(x = pred2, probs = .89) # .1867814; set as cutoff for mod2_logfull
test_loans %<>%
   mutate(pred1_prob = pred1,
          pred1_dummy = ifelse(test = pred1_prob > .184, yes = 1, no = 0),
          pred2_prob = pred2,
          pred2_dummy = ifelse(test = pred2_prob > .186, yes = 1, no = 0))
# Create a confusion matrix:
tbl_pred1 <- table(test_loans$loan_status, test_loans$pred1_dummy)
tbl_pred2 <- table(test_loans$loan_status, test_loans$pred2_dummy)
# Accuracy:
(pred1_acc <- sum(diag(tbl_pred1)) / sum(tbl_pred1))
(pred2_acc <- sum(diag(tbl_pred2)) / sum(tbl_pred2))
# Sensitivity: both models have very low sensitivity.
(pred1_snstvty <- tbl_pred1[2, 2] / sum(tbl_pred1[2, ]))
(pred2_snstvty <- tbl_pred2[2, 2] / sum(tbl_pred2[2, ]))

# Using a refined logistic model (mod3_log) and the test data, create a plot of the changes in Accuracy, Sensitivity and
# Specificity as we change the cutoff values:
mod3_log <- glm(formula = loan_status ~ grade + annual_inc + ir_cat, data = train_loans,
                family = binomial(link = logit))
summary(mod3_log)
test_loans %<>% 
   mutate(pred3_prob = predict(object = mod3_log, newdata = test_loans, type = "response"))

# Create a function that calculates accuracy, sensitivity and specificity for a given cutoff:
f_accss <- function(cff) {
   pred_dummy <- ifelse(test = test_loans$pred3_prob > cff, yes = 1, no = 0)
   tbl <- table(test_loans$loan_status, pred_dummy) # Note that the problem is that if there are no values of (0, 1), 
   # then the table() call removes that row or column. However, for our purposes, we need those columns. 
   if (ncol(tbl) == 1) {
      if (colnames(tbl) == "0") {
         tbl <- cbind(tbl, "1" = c(0, 0))
      } else {
         tbl <- cbind("0" = c(0, 0), tbl)
      } 
   } else {
      tbl
   }
   
   vec <- c(Cutoff = cff, 
            Accuracy = sum(diag(tbl)) / sum(tbl), 
            Sensitivity = tbl[2, 2] / sum(tbl[2, ]),
            Specificity = tbl[1, 1] / sum(tbl[1, ]))
   vec
}

accss <- as.data.frame(do.call(what = rbind, 
                               args = map(.x = seq(from = 0, to = 1, length.out = 200), 
                                          .f = f_accss)))
class(accss); head(accss, 20)

# Use the tidyverse to create a ggplot:
accss %<>%
   gather(key = Measure, value = Value, -Cutoff)
accss %>%
   ggplot(aes(x = Cutoff, y = Value, color = Measure)) + 
   geom_line() +
   theme_minimal()

# Link functions: within the binomial family there are a number of functions. Here, consider the complementary
# log-log function which is useful when the probabily of a binary event is very small or very large. In short,
# unlike the logit and probit, the clog-log is asymmetric.
# See: http://www.stat.ualberta.ca/~kcarrier/STAT562/comp_log_log

## Run a logit, probit and cloglog model with same independent variables and the same cutoff. Use purrr:
# Create a list of model names:
lst_mod_names <- list(logit = "logit", probit = "probit", cloglog = "cloglog")

# Call the models using lst_mod_names:
lst_mod_out <- map(.x = lst_mod_names, ~glm(loan_status ~ age + emp_cat + ir_cat + loan_amnt,
                                            family = binomial(link = .x), data = train_loans))

# Create lists of model predicted probs and dummies:
lst_mod_preds_prob <- map(.x = lst_mod_out, ~predict(.x, newdata = test_loans, type = "response"))
str(lst_mod_preds_prob)
lst_mod_preds_dummy <- map(.x = lst_mod_preds_prob, ~ifelse(test = .x > .14, yes = 1, no = 0))
str(lst_mod_preds_dummy)

# Create confusion table and calculate Accuracy:
map(.x = lst_mod_preds_dummy, ~table(test_loans$loan_status, .x))
map(.x = lst_mod_preds_dummy, ~table(test_loans$loan_status, .x)) %>%
   map(~ sum(diag(.x)) / nrow(test_loans))

## DECISION TREES: Now let's use decision trees for binary classification. 
## The key to understanding decision trees is to understand how classification threshold or categories are
## determined at each node. The gist is that at each node, the threshold is chosen to maximize the decrease in
## the node's complexity index. A popular complexity index is the Gini measure which is calculated as: 2 *
## prop(default) * prop(non_default).
# Assume at the root node, Gini = 2 * prop(default) * prop(non_default); leaf nodes are created by dividing
# sample by whether continuous variable x > x*. The Gini indices at the two new leaf nodes are created by: 2 *
# prop(default_left_leaf) * prop(non_default_left_leaf) and 2 * prop(default_right_leaf) *
# prop(non_default_right_leaf). The Gini_leaf at this level is prop(left) * Gini_left + prop(right) *
# Gini_right.
# The complexity is decreased by gain = Gini_root - Gini_leaf. We choose variable x and x* s.t. |Gini_root - Gini_leaf|
# is maximized. Note that rpart uses improve measure, where improvie = n * gain. 
# Using the train_loans data, create a decision tree using all variables:
library(rpart)
mod4_tree_default <- rpart(formula = loan_status ~ ., data = train_loans, method = "class")
plot(mod4_tree_default)
# Error in plot.rpart(mod4_tree_default) : fit is not a tree, just a root
# This is exactly like setting a very low or very high cutoff in the logistic regression. Gain is maximized by
# predicting that all cases are non-defaults. This will often happen when the data is highly unbalanced as in
# the case of default data.

## There are four methods to overcome unbalanced datasets. APPLY ONLY TO TRAINING SET AND NOT TEST SET!
## Note that these changes can be used for other reasons.

## First, undersampling or oversampling. For default data, we'd undersample nondefaults or oversample defaults
## in our training set.
# Undersampling method is not clear. DC creates an undersampled sample where 2/3 of 6570 sample are non default. 
# One question is whether to undersample with or without replacement. Some undersampling sets the default and nondefault
# numbers to be the same. I'll use the DC method here:
undersamp_nondef <- train_loans %>%
   filter(loan_status == 0) %>%
   sample_n(size = 2 * sum(train_loans$loan_status), replace = F)
str(undersamp_nondef)

undersample <- train_loans %>%
   filter(loan_status == 1) %>%
   bind_rows(undersamp_nondef)

# Fit a decision tree model: 
mod5_tree_undersample <- rpart(formula = loan_status ~ ., data = undersample, method = "class", 
                               control = rpart.control(cp = .001))
# cp = complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not
# attempted. The main role of this parameter is to save computing time by pruning off splits that are
# obviously not worthwhile.

# Plot the decision tree:
plot(mod5_tree_undersample, uniform = T)
text(mod5_tree_undersample, use.n = T) # use.n = T adds the nondefault/default numbers for each node. 

## Second, changing the prior probabilities. rpart() uses the proportion of default vs nondefault as priors for 
## the probability of default. We can change these priors. Note that under and over sampling was basically changing
## the prior probabilities. 
mod6_tree_prior <- rpart(formula = loan_status ~ ., data = train_loans, method = "class", 
                         control = rpart.control(cp = .001), 
                         parms = list(prior = c(.7, .3))) # priors where Pr(Y = 0) = .7; Pr(Y = 1) = .3 where Y \in {0, 1}
plot(mod6_tree_prior, uniform = T)
text(mod6_tree_prior, use.n = T)

## Third, including a loss matrix s.t. the cost of a false negative (classifying a default as a nondefault) is
## much greater than the cost of a false positive (classifying a nondefault as a default).
# The default loss matrix has zeros on its diagonals (no cost for correct true positive and true negative) and
# 1s off diagonal (cost for false positive and false negative).
loss_matrix7 <- matrix(data = c(0, 1, 10, 0), nrow = 2, byrow = T)
mod7_tree_loss <- rpart(formula = loan_status ~ ., data = train_loans, method = "class", 
                        control = rpart.control(cp = .001), 
                        parms = list(loss = loss_matrix7))
plot(mod7_tree_loss, uniform = T)
text(mod7_tree_loss, use.n = T)

## Fourth, we can add a weight vector for each obs in the dataset. In this model, we'll also tinker with the
## rpart.control() function.
# Create obs_weights: 3 for default and 1 for nondefault
obs_weights <- ifelse(test = train_loans$loan_status == 1, yes = 3, no = 1)
mod8_tree_wgt_contrl <- rpart(formula = loan_status ~ ., data = train_loans, method = "class", 
                              control = rpart.control(minsplit = 5, minbucket = 2, cp = .001), 
                              # minsplit = min n of obs in node to attempt split;
                              # minbucket = min n of obs in terminal node (default is 1/3 of minsplit)
                              weights = obs_weights)
plot(mod8_tree_wgt_contrl, uniform = T)
text(mod8_tree_wgt_contrl, use.n = T)

## Each of the four models (5-8) were overly complex and their plots were not informative and cluttered. Too much
## complexity could mean overfitting and poor performance on the test set. Therefore, the next step in using a
## decision tree is to prune the tree. We will focus on pruning using the complexity parameter.
# Review the model:
str(mod5_tree_undersample)

# To print mod5_tree_undersample$cptable, use:
printcp(x = mod5_tree_undersample) 

# To plot mod5_tree_undersample$cptable, use:
plotcp(x = mod5_tree_undersample)

# Focus on the in-training-set cross validation error (xerror).
# The min xerror is at level 5 with CP of .0027972. Extract from the mod5:
min_cp_mod5 <- mod5_tree_undersample$cptable[which.min(mod5_tree_undersample$cptable[, "xerror"]), "CP"]

# Prune the tree and plot:
mod5_pruntree_undersample <- prune(tree = mod5_tree_undersample, cp = min_cp_mod5)
plot(mod5_pruntree_undersample, uniform = T)
text(mod5_pruntree_undersample, use.n = T)
# A much more simplistic tree. However, this tree has a lot to be desired. For example, what does
# "home_ownership = ac" mean? What about "emp_cat = ad"?

library(rpart.plot)
prp(x = mod5_pruntree_undersample, extra = 1) # A much cleaner plot. Note that prp() has tons of options. 

## For models 5 through 8, prune the trees (we're repeating model 5) using purrr:
lst_tree_mods <- list(model_5 = mod5_tree_undersample, 
                      model_6 = mod6_tree_prior,
                      model_7 = mod7_tree_loss,
                      model_8 = mod8_tree_wgt_contrl)
# Create the cptables and cp plots:
walk(.x = lst_tree_mods, .f = printcp); walk(.x = lst_tree_mods, .f = plotcp)

# Extract the complexity parameter that minimizes cross validation error:
min_cp_vec <- map_dbl(.x = lst_tree_mods, .f = function(x) {
   i <- which.min(x[["cptable"]][, "xerror"])
   x[["cptable"]][i, "CP"]
})

# Create a list of decision tree models pruned by the min_cp_vec and plot the pruned decision tree:
lst_pruntree_mods <- map2(.x = lst_tree_mods, .y = min_cp_vec, ~prune(tree = .x, cp = .y))
walk(.x = lst_pruntree_mods, .f = prp, extra = 1)

## Use purrr package to make predictions and the confusion table for models 5 through 8; calculate accuracy:
lst_pruntree_preds_probs <- map(.x = lst_pruntree_mods, ~predict(object = .x, newdata = test_loans))
str(lst_tree_preds_probs) # Probability of nondefault and default are presented. 
# We can use the predicted probabilities to set custom cutoffs. Determining the optimal cutoff will be covered below.
lst_pruntree_preds_dummy <- map(.x = lst_pruntree_mods, .f = predict, newdata = test_loans, type = "class")

map(.x = lst_pruntree_preds_dummy, .f = function(x) {
   tbl <- table(test_loans$loan_status, x)
   acc <- sum(diag(tbl)) / nrow(test_loans)
   list("Confusion Table" = tbl, "Accuracy" = acc)
})
# Notice that the loss matrix model has a very low accuracy and weights model has the highest accuracy. 

## How do we determine the optimal cutoff for binary classification problems? 
## Use mod3_log (refined logistic model) and the pruned mod8_tree_wgt_contrl to make predictions. To determine the
## cutoff, we first choose an acceptance rate which will determine the cutoff and the bad_loan rate. We can
## then determine the ratio of bad loans we can include in our portfolio.
pred3 <- predict(object = mod3_log, newdata = test_loans, type = "response")
pred8 <- lst_pruntree_preds_probs[["model_8"]][, 2]
summary(pred3); summary(pred8) # The range of probabilities are very different and will require different cutoffs.

# Calculate the bad loan rate for a given acceptance rate: 80%, using the logistic mod3.
cutoff80_mod3 <- quantile(x = pred3, probs = .8)
loan_yn_mod3_cff80 <- ifelse(test = pred3 > cutoff80_mod3, yes = "no_loan", no = "loan")

# Of the individuals that received a loan, what is the ratio of default? Filter test_loans by only those who
# received a loan; then calculate the number of bad loans in this subset.
x <- test_loans$loan_status[loan_yn_mod3_cff80 == "loan"]
sum(x) / length(x) # Bad loan rate of .09552584


# Calculate the bad loan rate for a given acceptance rate: 80%, using the pruned mod8.
cutoff80_mod8 <- quantile(x = pred8, probs = .8)
loan_yn_mod8_cff80 <- ifelse(test = pred8 > cutoff80_mod8, yes = "no_loan", no = "loan")

# Of the individuals that received a loan, what is the ratio of default? Filter test_loans by only those who
# received a loan; then calculate the number of bad loans in this subset.
x <- test_loans$loan_status[loan_yn_mod8_cff80 == "loan"]
sum(x) / length(x) # Bad loan rate of .1029613

## Create the STRATEGY CURVE. This is the bad loan rate as a function of the acceptance rate. Use the cloglog
## model and the pruned loss matrix mod.
accpt_rate <- seq(from = 0, to = 1, length.out = 500)

head(lst_mod_preds_prob[["cloglog"]])
predclog <- lst_mod_preds_prob[["cloglog"]]

head(lst_pruntree_preds_probs[["model_7"]])
pred7 <- lst_pruntree_preds_probs[["model_7"]][, 2]

# Calculate the bad loan rate for cloglog:
badloan_cloglog <- map_dbl(.x = accpt_rate, .f = function(accpt) {
   cff <- quantile(x = predclog, probs = accpt)
   yes_no <- ifelse(test = predclog > cff, yes = "no_loan", no = "loan")
   x <- test_loans$loan_status[yes_no == "loan"]
   sum(x) / length(x)
})
class(badloan_cloglog); length(badloan_cloglog)

# Calculate the bad loan rate for cloglog:
badloan_mod7 <- map_dbl(.x = accpt_rate, .f = function(accpt) {
   cff <- quantile(x = pred7, probs = accpt)
   yes_no <- ifelse(test = pred7 > cff, yes = "no_loan", no = "loan")
   x <- test_loans$loan_status[yes_no == "loan"]
   sum(x) / length(x)
})
class(badloan_mod7); length(badloan_mod7)

# We could've created a nested map function to get the required bad loan rates. 

# Create a dataframe and plot using ggplot:
data_frame(accpt_rate, "C_loglog" = badloan_cloglog, "Pruned_lss_mtrx" = badloan_mod7) %>%
   gather(key = model, value = value, -accpt_rate) %>%
   ggplot(aes(x = accpt_rate, y = value, color = model)) + 
   geom_line() + 
   theme_minimal() + 
   labs(title = "Strategy Curve for C-loglog and Pruned Loss Matrix Tree") + 
   scale_x_continuous(name = "Acceptance rate", labels = scales::percent) + 
   scale_y_continuous(name = "Bad loan rate", labels = scales::percent)

# Based on the plot, a bank with a 45% acceptance rate will choose the pruned tree model to reduce bad loan
# rates while a bank with a 85% acceptance rate will choose the cloglog model.

## The exercise above helped us understand how a bank might choose a model to reduce the rate of bad loans.
## However, a bank might want to know what is the overall best model. How do we answer this question?

# First, we can use the ROC (Receiver Operating Characteristic) curve which plots the sensitivity against 1 -
# specificity => Sensitivity = f(1 - Specificity). Note that we care more about Sensitivity than Specificity.
# Furthermore, we don't use Accuracy because Accuracy is maximized by setting a high cutoff. See the graph
# plotting Accuracy, Sensitivity and Specificity as functions of cutoffs.

# The ROC curve starts at the lower left where Sensitivity = 0 and Specificity = 1 <=> cutoff = 1; to the
# upper right where Sensitivity = 1 and Specificity = 0 <=> cutoff = 0. Therefore, a curve that approaches the 
# upper left has higher Sensitivity and higher Specificity which is the desired result. The 45 degree line would 
# be random assignment of default and nondefault. 

# Let's create some ROC curves for previous models using the pROC package:
library(pROC)

# First create the objects containing information to plot the ROC curve. We'll use the logit, probit, and
# cloglog models in lst_mod_preds_prob and add the full variable logit (mod2_logfull)'s predictions (pred2) to the list:
str(lst_mod_preds_prob)
lst_mod_preds_prob$full_logit <- pred2
str(lst_mod_preds_prob)

lst_ROC_logit <- map(.x = lst_mod_preds_prob, ~roc(test_loans$loan_status, .x))

# Second, plot the four ROCs on the same graph. Note the x-axis
plot(lst_ROC_logit[[1]])
lines(lst_ROC_logit[[2]], col = "blue")
lines(lst_ROC_logit[[3]], col = "red")
lines(lst_ROC_logit[[4]], col = "green")
legend(x = "bottomright", legend = c("logit", "probit", "cloglog", "full_logit"), lty = 1, 
       col = c("black", "blue", "red", "green"))
# Logit, probit, and cloglog have very similar ROCs while the full logit has a better ROC. In short, for this
# dataset, more information is better. 

# Third, calculate the AUC for each model:
map_dbl(.x = lst_ROC_logit, .f = auc) # Again the full_logit has the highest AUC. 

# Repeat the process for pruned decision tree models using lst_pruntree_preds_probs. 
# Change the names of the list elements to be more informative:
names(lst_pruntree_preds_probs) <- c("ptree_undrsmpl", "ptree_prior", "ptree_loss", "ptree_wgt")
lst_ROC_ptree <- map(.x = lst_pruntree_preds_probs, ~roc(response = test_loans$loan_status, 
                                                         predictor = .x[, 2]))
plot(lst_ROC_ptree[[1]])
lines(lst_ROC_ptree[[2]], col = "blue")
lines(lst_ROC_ptree[[3]], col = "red")
lines(lst_ROC_ptree[[4]], col = "green")
legend(x = "bottomright", legend = c("Undersampled", "Prior probability", "Loss matrix", "Weight"), 
       lty = 1, col = c("black", "blue", "red", "green"))
# Loss matrix appears to have the best ROC. However, recall that accuracy was very low.
map_dbl(.x = lst_ROC_ptree, .f = auc) # As expected, ptree_loss has the greatest AUC. 

## The AUC can also help us determine the inputs that we should include in a logistic model. Note that using
## the P-values is also a valid method, but since we have a test set and our purpose is prediction, we should
## make use of AUC based pruning.
# Start with the full logistic model and calculate the AUC:
auc(response = test_loans$loan_status, predictor = pred2) # .6511

# Now systematically delete one function at a time and check to see if there is an improvement in the AUC:
rm_loan_amnt <- glm(formula = loan_status ~ grade + home_ownership + annual_inc + age + ir_cat + emp_cat, 
                    family = binomial(link = "logit"), data = train_loans)
pred_rm_loan_amnt <- predict(object = rm_loan_amnt, newdata = test_loans, type = "response")
auc(response = test_loans$loan_status, predictor = pred_rm_loan_amnt)

rm_home_ownership <- glm(formula = loan_status ~ loan_amnt + grade + annual_inc + age + ir_cat + emp_cat, 
                         family = binomial(link = "logit"), data = train_loans)
pred_rm_home_ownership <- predict(object = rm_home_ownership, newdata = test_loans, type = "response")
auc(response = test_loans$loan_status, predictor = pred_rm_home_ownership)
# ...
# The process is repeated until the model with the highest AUC is found. Then the process is repeated by
# removing 1 variable from the highest AUC model and checking to see whether the AUC increases.