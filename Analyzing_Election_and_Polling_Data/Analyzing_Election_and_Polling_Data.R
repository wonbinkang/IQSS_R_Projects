library(tidyverse)
library(lubridate)
library(skimr)
library(xts)

# Gallup Presidential approval poll
approval <- read_csv(file = "gallup_approval_polls.csv", 
                     col_names = T, 
                     col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                      Inaug = col_date(format = "%m/%d/%y")))
# Best and worst approval ratings:
approval %>% 
   group_by(President) %>%
   summarise(Average_Approval = mean(Approve)) %>%
   arrange(desc(Average_Approval))

# Practice using the pull() call by pulling disapproval for Obama:
Obama <- approval %>%
   select(President, Date, Approve, Disapprove) %>%
   filter(President == "Obama") %>%
   pull(Disapprove)
mean(Obama) # 45.70599

# Calculate Obama's approval rating by month:
approval %>%
   select(President, Date, Approve) %>%
   filter(President == "Obama") %>%
   mutate(Poll_month = month(Date)) %>% # Use lubridate's month() function to extract the month from Date object. 
   group_by(Poll_month) %>%
   summarise(MeanApp_month = mean(Approve))

# Approval ratings by month is not very interesting or very informative. A more informative statistic is the
# rolling average of approval ratings. Calculate rolling averages for Obama:
Obama_RollAveAppr <- approval %>%
   select(President, Date, Approve) %>%
   filter(President == "Obama") %>%
   arrange(Date) %>%
   mutate(RollAveAppr = rollapply(data = Approve, 
                                  width = 10, 
                                  FUN = mean, 
                                  align = "right", # Index the 10 approval ratings by the latest date ("right)
                                  fill = NA)) # Drop items with NA. 
# Calculate rolling average for Trump:
Trump_RollAveAppr <- approval %>%
   select(President, Date, Approve) %>%
   filter(President == "Trump") %>%
   arrange(Date) %>%
   mutate(RollAveAppr = rollapply(data = Approve, width = 10, FUN = mean, align = "right", fill = NA))

# Visualize Obama and Trump together:
ggplot(data = rbind(Obama_RollAveAppr, Trump_RollAveAppr), 
       aes(x = Date, y = RollAveAppr, color = President)) + 
   geom_line()

# Visualize all of the presidents in dataset using days from inauguration:
approval %>%
   group_by(President) %>%
   arrange(Days) %>%
   mutate(RollAveAppr = rollapply(data = Approve, width = 10, FUN = mean, align = "right", fill = NA)) %>%
   ggplot(aes(x = Days, y = RollAveAppr, color = President)) + 
      geom_line()

## Using a new dataset of generic polls for House and Senate elections. The polls are "generic" because they
## ask the respondents' support for or approval of a major political party and not specific candidate.
generic_polls <- read_csv(file = "generic_ballot.csv", col_names = T, 
                           col_types = cols(Date = col_date(format = "%m/%d/%Y"),
                                            ElecDay = col_date(format = "%m/%d/%Y")))
# Review the polling data and calculate margin of Democrat support:
generic_polls <- generic_polls %>%
   mutate(Dem_poll_margin = Democrats - Republicans,
          Dem_vote_margin = DemVote - RepVote)
generic_polls_2016 <- generic_polls %>%
   filter(ElecYear == 2016) 

# Check to see how the polls have changed over time. Note that instead of using each poll, we average the
# polls taken in each calendar year;
generic_polls %>%
   group_by(year(x = Date)) %>%
   summarise(AvgDemPollMarg = mean(Dem_poll_margin) / 100) %>%
   ggplot(aes(x = `year(x = Date)`, y = AvgDemPollMarg)) + 
      geom_line() + 
      geom_hline(yintercept = 0, color = "red") + 
      scale_y_continuous(labels = scales::percent) +
      xlab("Year")

# or we can average all polls taken in the same year and month:
generic_polls %>%
   group_by(year(x = Date), month(x = Date)) %>%
   summarise(AvgDemPollMarg = mean(Dem_poll_margin) / 100) %>%
   unite(col = Year_month, `year(x = Date)`, `month(x = Date)`, sep = "-") %>%
   mutate(Year_month = as.yearmon(x = Year_month, format = "%Y-%m")) %>% # Creates S3: yearmon class
   ggplot(aes(x = Year_month, y = AvgDemPollMarg)) + 
      geom_line() + 
      geom_hline(yintercept = 0, color = "red") + 
      scale_y_continuous(labels = scales::percent) +
      xlab("Year") 
# Note that ggplot does not "know" how to pick a scale for yearmon class. It plots as continuous, which is
# correct. However, keep this in mind for the future.

## The key purpose of polls is to predict election outcomes. Are generic ballots able to predict election
## outcomes? To smooth the data, we use ElecYear to aggregate the polls starting from the 1990 election (polls
## covering previous elections may be less reliable).
generic_polls_1990 <- generic_polls %>%
   filter(ElecYear >= 1990)
generic_polls_1990 %>%
   group_by(ElecYear) %>%
   summarise(AvgDemPollMargin = mean(Dem_poll_margin), 
             AvgDemVoteMargin = mean(Dem_vote_margin)) %>%
   mutate(error = AvgDemPollMargin - AvgDemVoteMargin) %>%
   ggplot(aes(x = ElecYear, y = error)) + 
      geom_point() + 
      geom_hline(yintercept = 0, color = "red")
# Note that the errors are mostly positive. Democrats polled better than the election results. 

# If the generic ballots are the statistic we are using to predict election outcomes, then we can quantify the
# error of generic ballots like any other statistic we use to estimate a parameter or obs. 
# Calculate the RMSE of generic ballots: 
rmse <- generic_polls_1990 %>%
   group_by(ElecYear) %>%
   summarise(AvgDemPollMargin = mean(Dem_poll_margin), 
             AvgDemVoteMargin = mean(Dem_vote_margin)) %>%
   mutate(error = AvgDemPollMargin - AvgDemVoteMargin) %>%
   summarise(rmse = sqrt(mean(error^2))) %>%
   pull(rmse)
CI <- 1.96 * rmse

# Using RMSE and CI, add the upper and lower CIs to AvgDemPollMargin
polls_votes_err_CI <- generic_polls_1990 %>%
   group_by(ElecYear) %>%
   summarise(AvgDemPollMargin = mean(Dem_poll_margin), 
             AvgDemVoteMargin = mean(Dem_vote_margin)) %>%
   mutate(error = AvgDemPollMargin - AvgDemVoteMargin, 
          upper = AvgDemPollMargin + CI, 
          lower = AvgDemPollMargin - CI)

# Using the df above, create an error bar plot:
ggplot(data = polls_votes_err_CI, aes(x = ElecYear)) + 
   geom_point(aes(y = AvgDemPollMargin), color = "blue", size = 1.5) + 
   geom_point(aes(y = AvgDemVoteMargin), color = "red", size = 1.5) + 
   geom_errorbar(aes(ymin = lower, ymax = upper), color = "green", width = .5) +
   theme_minimal()
# Note that election results are all within the margin of error, however, the margin of error or CI, is very
# wide.

# We can aggregate the data in a linear model and make predictions:
model <- lm(formula = AvgDemVoteMargin ~ AvgDemPollMargin, data = polls_votes_err_CI)
summary(model)
predict(object = model, newdata = data.frame(AvgDemPollMargin = 5)) # 1.398335

## Did white counties vote more predominantly for Trump? Use choroplethr package to map and linear regression
## to statistically analyze this question.
pres_county <- read_csv(file = "us_pres_2016_by_county.csv", col_names = T)

# Remove national level data:
pres_county_wrangled <- pres_county %>%
   select(-c("national.party.percent", "national.count", "is.national.winner")) %>%
   spread(key = party, value = vote.count) %>%
   mutate(Dem_pct_vote = D / county.total.count) %>%
   rename("FIPS" = county.fips, "County_total_votes" = county.total.count, "County" = county.name, 
          "State" = state.name, "Dem_votes" = D, "Other_votes" = O, "Rep_votes" = R)

# Using the choroplethr package, load(df_county_demographics) and left_join with pres_county_wrangled. 
# Map the county votes and demographics data to a choropleth. Omitted. 

# Use simple linear regression to analyze the merged dataset. Omitted. 

## Analyze the polling data prior to the Brexit vote and the Brexit referendum results
# Brexit poll data:
brexit_polls <- read_csv(file = "brexit_polls.csv", 
                         col_names = T, 
                         col_types = cols(Date = col_date(format = "%m/%d/%y")))
skim(brexit_polls)

# Plot the Remain camp's margin of lead and add a loess curve:
ggplot(data = brexit_polls, aes(x = Date, y = Remain - Leave)) + 
   geom_point() + 
   geom_smooth(method = "loess", se = F, span = .8)

# Average the polls from the final week before the referendum:
brexit_polls %>%
   filter(Date >= as.Date("2016-06-16")) %>%
   mutate(RemainMargin = Remain - Leave) %>%
   summarise(mean(RemainMargin)) %>%
   pull() # 2.53333

# Brexit results data:
brexit_results <- read_csv(file = "brexit_results.csv", col_names = T, na = c("", "NA", "N/A"))
skim(brexit_results) # 59 NA in degree variable (percentage of voters w. college degrees). 

# Is voting Labor in the 2015 general election correlated with voting Leave? 
ggplot(brexit_results, aes(x = lab_2015, y = leave_share)) + 
   geom_point() # There doesn't seem to be a clear relationship; Consider other constituency characteristics.
ggplot(brexit_results, aes(x = age_18to24, y = leave_share)) + geom_point() # Negative? 
ggplot(brexit_results, aes(x = con_2015, y = leave_share)) + geom_point() # Positive? 
ggplot(brexit_results, aes(x = ukip_2015, y = leave_share)) + 
   geom_point() + # Very strong positive
   geom_smooth(method = "lm", se = F)
ggplot(brexit_results, aes(x = unemployed, y = leave_share)) + geom_point() # Negative? 
ggplot(brexit_results, aes(x = degree, y = leave_share)) + geom_point() # Very strong negative

brexit_model <- lm(formula = leave_share ~ ukip_2015 + degree, data = brexit_results)
summary(brexit_model)

## For the 2018 House of Rep race, data is from https://elections.huffingtonpost.com/pollster/2018-national-house-race
## Data is in tsv format. 
poll_2018 <- read_tsv(file = "18-US-House-poll-responses-clean.tsv", col_names = T)
# Note that by using read_tsv, the df is in much better shape than the DC data. 

# Restrict attention to more recent polls:
poll_2018 <- poll_2018 %>%
   filter(end_date >= as.Date("2018-08-01")) %>%
   mutate(Dem_margin = Democrat - Republican)
poll_2018 %>%
   pull(Dem_margin) %>%
   mean(na.rm = T) # The polled Democratic margin is 7.52459

# Use historical data to train a model: 
skim(generic_polls)
# Use polls conducted in Aug - Nov, and polls beginning in 1980:
train1 <- generic_polls %>%
   filter(year(Date) == ElecYear & month(Date) %in% c(8, 9, 10, 11) & ElecYear >= 1980) %>%
   select(Date, ElecYear, Dem_poll_margin, Dem_vote_margin)
ggplot(train1, aes(x = Dem_poll_margin, y = Dem_vote_margin)) + 
   geom_point() +
   geom_smooth(method = "lm", se = F)

# We can use each poll to train our linear model, or:
train2 <- generic_polls %>%
   filter(year(Date) == ElecYear & month(Date) %in% c(8, 9, 10, 11) & ElecYear >= 1980) %>%
   group_by(ElecYear) %>%
   summarise(Dem_poll_margin = mean(Dem_poll_margin), 
             Dem_vote_margin = mean(Dem_vote_margin))
ggplot(train2, aes(x = Dem_poll_margin, y = Dem_vote_margin)) + 
   geom_point() + 
   geom_smooth(method = "lm", se = F) + 
   geom_text(aes(label = ElecYear), vjust = 1.2)
# DC argues that based on the labels, Democrats underperform in elections relative to polls when they are in
# power, while overperform when out of power. To capture this, we add a dummy variable, Dem_pres, which is
# equal to 1 if a Democrat is in the White House and 0 otherwise to both training sets.
train1 <- train1 %>%
   mutate(Dem_pres = case_when(ElecYear == 1980 ~ 1,
                               ElecYear %in% c(1994, 1996, 1998, 2000) ~ 1,
                               ElecYear %in% c(2010, 2012, 2014, 2016) ~ 1, 
                               TRUE ~ 0))

train2 <- train2 %>%
   mutate(Dem_pres = case_when(ElecYear == 1980 ~ 1,
                               ElecYear %in% c(1994, 1996, 1998, 2000) ~ 1,
                               ElecYear %in% c(2010, 2012, 2014, 2016) ~ 1, 
                               TRUE ~ 0))

# Train two linear models: lm1 and lm2
lm1 <- lm(formula = Dem_vote_margin ~ Dem_poll_margin + Dem_pres, data = train1)
summary(lm1)
lm2 <- lm(formula = Dem_vote_margin ~ Dem_poll_margin + Dem_pres, data = train2)
summary(lm2)
# Coefficient on Dem_pres for both models is approx -5.1 and significant. 

## Now that we've trained the data, we can make predictions. Assume that a Republican is in power and the
## polls show the Democrats ahead by 7.5 percentage points. Calculate the CI.
pred_1 <- predict(object = lm1, newdata = data.frame(Dem_poll_margin = 7.5, Dem_pres = 0)) 
se_1 <- 1.96 * sqrt(mean((lm1$fitted.values - train1$Dem_vote_margin)^2))
CI_1 <- c(upper = pred_1 + se_1, lower = pred_1 - se_1)

pred_2 <- predict(object = lm2, newdata = data.frame(Dem_poll_margin = 7.5, Dem_pres = 0)) 
se_2 <- 1.96 * sqrt(mean((lm2$fitted.values - train2$Dem_vote_margin)^2))
CI_2 <- c(upper = pred_2 + se_2, lower = pred_2 - se_2)

## Moving on to the presidential election, we use Alan Abramowitz model. The data below contains the approval
## rating of the sitting president and the vote share of the president's party, whether the president seeking
## reelection or another candidate from his party.
pres_elecs <- read_csv(file = "pres_elecs.csv", col_names = T) %>%
   arrange(Year)
ggplot(pres_elecs, aes(x = pres_approve, y = vote_share)) + 
   geom_text(aes(label = Year)) + 
   geom_smooth(method = "lm", se = F)
pres_lm <- lm(formula = vote_share ~ pres_approve + q2_gdp + two_plus_terms, data = pres_elecs) 
# two_plus_terms is whether the president's party has been in office for two or more terms. 
summary(pres_lm)
pres_elecs$fitted <- pres_lm$fitted.values
ggplot(pres_elecs, aes(x = vote_share, y = fitted)) + 
   geom_point() +
   geom_text(aes(label = Year), vjust = 1.2, color = "blue") +
   geom_abline(color = "red") # 45 degree line where fitted = vote_share 
# Calculate RMSE:
sqrt(mean((pres_elecs$fitted - pres_elecs$vote_share)^2)) # 1.67005