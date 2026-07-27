---
title: "Flight Project"
author: "Jessica Kotlarek"
date: "2025-10-23"
output: pdf_document
---

```{r}
#install.packages("anyflights")
#library(anyflights)
library(nycflights13)
library(dplyr)
library(MASS)
library(tidyverse)
options(scipen = 999)
library(bestglm)
library(car)
library(glmnet)
library(olsrr)
library(lmtest)
library(glmtoolbox)
library(pROC)
```

```{r EDA}
flights
weather
planes
airlines

# I was originally calculating the response variable, delayed, based on if the 
# flight departed late or not, but have since switched it to if the plane landed
# delayed or not, since that would be more important to passengers

all_data <- flights %>%
    # calculating response variable (1 if delayed, 0 if not)
    mutate(delayed = ifelse(arr_delay > 15, 1, 0),
           cancelled = ifelse(is.na(dep_time), 1, 0),
    # breaking up parts of day
    # right = FALSE --> [0,6), [6,12), [12,18), [18,24) 
    dep_period = cut(hour, breaks = c(0,6,12,18,24),
                     labels = c("Night","Morning","Afternoon","Evening"),
                     right = FALSE),
    # weekday or weekend
    weekend = factor(ifelse(weekdays(time_hour) %in% c("Saturday","Sunday"),
                            "Weekend","Weekday"))) %>%
    # joining weather
    left_join(weather, by = c("year","month","day","hour","origin")) %>%
    # season, if NA, make NA
    mutate(season = case_when(
        month %in% c(12, 1, 2)  ~ "Winter",
        month %in% c(3, 4, 5)   ~ "Spring",
        month %in% c(6, 7, 8)   ~ "Summer",
        month %in% c(9, 10, 11) ~ "Fall",
        TRUE ~ NA_character_)) %>%    
    # raining or not
    mutate(rain_flag = factor(ifelse(precip > 0, "Rain", "No Rain"))) %>% 
    # join airlines (for carrier name)
    left_join(airlines %>% rename(carrier_name = name), by = "carrier") %>% 
    # making categorical vars factors
    mutate(
    carrier = factor(carrier),
    origin = factor(origin),
    dep_period = factor(dep_period),
    weekend = factor(weekend),
    season = factor(season),
    rain_flag = factor(rain_flag),
    carrier_name = factor(carrier_name)) %>% 
    mutate(carrier_name = fct_relevel(carrier_name, "United Air Lines Inc."),
           season = fct_relevel(season, "Spring"),
           origin = fct_relevel(origin, "JFK"),
           dep_period = fct_relevel(dep_period, "Morning")) %>% 
    mutate(distance_thousand_miles = distance / 1000)


# cancelled flights
cancelled <- sum(all_data$cancelled)
cancelled
# 8255 cancelled flights - not doing analysis on this but good to know 

# analytic dataset 
analytic <- all_data %>% 
    # for some reason select just isn't working even though I loaded in dplyr
    # so an LLM told me to do dplyr::select and that seems to work
     dplyr::select(carrier_name, origin, distance_thousand_miles, dep_period, weekend,
           wind_speed, visib, season, rain_flag, delayed)

# I originally included the table "planes" and calculated how old a plane was
# to account for potential maintenance issues, but there were 58,000 missing values
# so I had to remove it

# where are the NAs?
summary(analytic)
# the most NAs show up in the delayed variable (9,430 for delayed 
# compared to 1-2k for other vars), and since delayed is the response,
# I wouldn't want to try to impute here, and since I only lose ~ 4% of the data
# when I drop the NAs, its okay


# only lose 3.3% of the total data so this is okay
# the NA
analytic_no_na <- analytic %>% 
    drop_na()
```

```{r variable graphs}
# AIRLINE
# count of flights per airline:
analytic_no_na %>%
    count(carrier_name) %>%
    ggplot(aes(x = reorder(carrier_name, n), y = n)) +
    geom_col() +
    # so we can read labels
    coord_flip() +
    labs(
    title = "Number of Flights per Airline",
    x = "Airline",
    y = "Count of Flights") +
    theme_minimal()

# count of delays per airline:
analytic_no_na %>%
    filter(delayed == 1) %>% # keep only delayed flights
    count(carrier_name, name = "n_delays") %>%
    ggplot(aes(x = reorder(carrier_name, n_delays), y = n_delays)) +
    geom_col() +
    # so we can read labels
    coord_flip() +
    labs(
    title = "Number of Delays per Airline",
    x = "Airline",
    y = "Count of Delays") +
    theme_minimal()

# percent of delayed flights per airline
analytic_no_na %>%
  group_by(carrier_name) %>%
  summarize(pct_delay = mean(delayed)) %>%
  ggplot(aes(x = pct_delay, y = reorder(carrier_name, pct_delay))) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  labs(
    title = "Percent of Delayed Flights per Airline",
    x = "Percent Delayed",
    y = "Airline"
  ) +
  theme_minimal()
# deciding to make United baseline because it has many flights, and is in the
# middle of the pack for percent delayed (so comparisons are meaningful and 
# not extreme)


# AIRPORT
# number of flights per airport
analytic_no_na %>%
    count(origin) %>%
    ggplot(aes(x = reorder(origin, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(title = "Number of Flights per Airport",
         x = "Origin Airport",
         y = "Count of Flights") +
    theme_minimal()

# number of delayed flights per airport
analytic_no_na %>%
    filter(delayed == 1) %>%
    count(origin, name = "n_delays") %>%
    ggplot(aes(x = reorder(origin, n_delays), y = n_delays)) +
    geom_col() +
    coord_flip() +
    labs(title = "Number of Delayed Flights per Airport",
         x = "Origin Airport",
         y = "Count of Delayed Flights") +
    theme_minimal()

# percent of delayed flights per airport
analytic_no_na %>%
    group_by(origin) %>%
    summarize(pct_delay = mean(delayed)) %>%
    ggplot(aes(x = pct_delay, y = reorder(origin, pct_delay))) +
    geom_col() +
    scale_x_continuous(labels = scales::percent) +
    labs(title = "Percent of Delayed Flights per Airport",
         x = "Percent Delayed",
         y = "Origin Airport") +
    theme_minimal()
    # deciding to make JFK baseline


# SEASON
# number of flights per season:
analytic_no_na %>% 
    count(season) %>% 
    ggplot(aes(x = reorder(season, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(title = "Number of Flights per Season",
         x = "Season",
         y = "Count of Flights") +
    theme_minimal()

# number of delayed flights per season
analytic_no_na %>%
    filter(delayed == 1) %>%
    count(season, name = "n_delays") %>%
    ggplot(aes(x = reorder(season, n_delays), y = n_delays)) +
    geom_col() +
    coord_flip() +
    labs(title = "Number of Delayed Flights per Season",
         x = "Season",
         y = "Count of Delayed Flights") +
    theme_minimal()

# percent of delayed flights per seaosn
analytic_no_na %>%
    group_by(season) %>%
    summarize(pct_delay = mean(delayed)) %>%
    ggplot(aes(x = pct_delay, y = reorder(season, pct_delay))) +
    geom_col() +
    scale_x_continuous(labels = scales::percent) +
    labs(title = "Percent of Delayed Flights per Season",
         x = "Percent Delayed",
         y = "Season") +
    theme_minimal()
# deciding to make spring baseline since its in the middle ground




# TIME OF DAY
# number of flights per time of day:
analytic_no_na %>% 
    count(dep_period) %>% 
    ggplot(aes(x = reorder(dep_period, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(title = "Number of Flights per Time of Day",
         x = "Time of Day",
         y = "Count of Flights") +
    theme_minimal()

# number of delayed flights per time of day
analytic_no_na %>%
    filter(delayed == 1) %>%
    count(dep_period, name = "n_delays") %>%
    ggplot(aes(x = reorder(dep_period, n_delays), y = n_delays)) +
    geom_col() +
    coord_flip() +
    labs(title = "Number of Delayed Flights per Time of Day",
         x = "Time of Day",
         y = "Count of Delayed Flights") +
    theme_minimal()

# percent of delayed flights per time of day
analytic_no_na %>%
    group_by(dep_period) %>%
    summarize(pct_delay = mean(delayed)) %>%
    ggplot(aes(x = pct_delay, y = reorder(dep_period, pct_delay))) +
    geom_col() +
    scale_x_continuous(labels = scales::percent) +
    labs(title = "Percent of Delayed Flights per Time of Day",
         x = "Percent Delayed",
         y = "Time of Day") +
    theme_minimal()
# deciding to make morning baseline since its in the middle ground

```



```{r full model}
# Fit full model
full_model <- glm(delayed ~ carrier_name + origin + dep_period + weekend + rain_flag 
                  + distance_thousand_miles + wind_speed + visib + season, 
                  data = analytic_no_na, family = binomial)

summary(full_model)

# reference variables:

# season: spring
# rain: no rain
# weekend: weekday
# departure period: morning
# origin: JFK
# carrier: United
```

```{r stepwise selection}
# stepwise selection
step_model <- stepAIC(full_model, 
                      direction = "both")

step_model$anova
summary(step_model)
```

```{r forward selection}
# null model
null_model <- glm(delayed ~ 1, data = analytic_no_na, family = binomial)

# all preds
full_scope <- ~ carrier_name + origin + dep_period + weekend + rain_flag + distance_thousand_miles + wind_speed + visib + season

# forward selection
forward_model <- stepAIC(null_model,
                         scope = full_scope,
                         direction = "forward",
                         trace = TRUE)
summary(forward_model)
forward_model$anova
```

```{r backwards selection}
# backwards selection
backwards_model <- stepAIC(full_model,
                           direction = "backward",
                           trace = TRUE)
summary(backwards_model)
backwards_model$anova
```


For all 3 model selection methods, stepwise, forward, and backwards, the same model is produced, which is also the full model. So I will be using the full model going forward.

```{r model}
summary(full_model)

# reference variables:

# season: spring
# rain: no rain
# weekend: weekday
# departure period: morning
# origin: JFK
# carrier: United

```


```{r checks}
# LINEARITY ASSUMPTION

# can't have zero values for box tidwell
data <- analytic_no_na %>% 
    mutate(wind_speed_no_zero = ifelse(wind_speed == 0, 0.01, wind_speed),
           visib_no_zero = ifelse(visib == 0, 0.01, visib)) %>% 
    dplyr::select(- wind_speed, -visib)

# prediction plots didn't work because of the ~350,000 values
boxTidwell(delayed ~ + wind_speed_no_zero + visib_no_zero + distance_thousand_miles, data = data)


# boxtidwell transformations
data$wind_bt  <- data$wind_speed_no_zero^0.81625 # ^lambda
data$visib_bt <- data$visib_no_zero^1.62422 # ^lambda
data$dist_bt  <- data$distance_thousand_miles^1.51908 # ^lambda

fit_bt <- glm(delayed ~ wind_bt + visib_bt + dist_bt,
              data = data,
              family = binomial)
summary(fit_bt)

boxTidwell(delayed ~ + wind_bt + visib_bt + dist_bt, data = data)



# NEW MODEL

# full model with box tidwell transformations
analytic_no_na <- analytic_no_na %>% 
    mutate(wind_speed_no_zero = ifelse(wind_speed == 0, 0.01, wind_speed),
           visib_no_zero = ifelse(visib == 0, 0.01, visib)) %>% 
    dplyr::select(-wind_speed, -visib)

analytic_no_na$wind_bt  <- analytic_no_na$wind_speed_no_zero^0.81625 # ^lambda
analytic_no_na$visib_bt <- analytic_no_na$visib_no_zero^1.62422 # ^lambda
analytic_no_na$dist_bt  <- analytic_no_na$distance_thousand_miles^1.51908 # ^lambda


new_full_model <- glm(delayed ~ carrier_name + origin + dep_period + weekend + rain_flag 
                  + dist_bt + wind_bt + visib_bt + season, 
                  data = analytic_no_na, family = binomial)
summary(new_full_model)



# MODEL FIT

# hosmer-lemeshow GOF test
hltest(new_full_model)
# p-val < 0.000000000000000222 
# model lacks fit, but this is common with so much data



# test overall model significance
# does the full model fit significantly better than an intercept-only model?
null_model <- glm(delayed ~ 1, data = analytic_no_na, family = binomial)
lrtest(new_full_model, null_model)
# there is a lot of evidence that the predictors collectively explain variation in flight delays


# NO MULTICOLLINEARITY

# for vifs, use raw VIF for:
# weekend (4)
# rain_flag (5)
# distance_thousand_miles (6)
# wind_speed (7)
# visib (8)
# BECAUSE BINARY/NUMERIC

# use GVIF^(1/(2·Df)) for:
# carrier_name (1)
# origin (2)
# dep_period (3)
# season (9)
# BECAUSE MANY LEVELS 
vif(new_full_model)
# all vifs good
# GVIF^(1/(2·Df)) rescales it so values are comparable to ordinary VIF thresholds



# INFLUENTIAL OBSERVATIONS

# df betas
# 2/sqrt(n) is typical cutoff for large n
dfb <- dfbeta(new_full_model)
cut <- 2 / sqrt(nrow(dfb))
sum(abs(dfb) > cut)



# cook's distance
cooks <- cooks.distance(new_full_model)
summary(cooks)
cutoff <- 4 / nrow(analytic_no_na)
sum(cooks > cutoff)

# 1,340 observations with large DFBETAs
# 10,372 observations with Cook’s distance > 4/n
# but with 325,000 observations, these counts are expected with large n



# PREDICTIVE PERFORMANCE
# auc
roc.object <- roc(new_full_model$y~fitted(new_full_model))
plot(roc)
auc(roc.object)
# 69.71%

```


```{r testing interactions}
int_model_1 <- glm(delayed ~ carrier_name + origin + dep_period + weekend + rain_flag 
                 + dist_bt + wind_bt + visib_bt + season + origin:dep_period,
                 data = analytic_no_na, family = binomial)
int_model_2 <- glm(delayed ~ carrier_name + origin + dep_period + weekend + rain_flag 
                 + dist_bt + wind_bt + visib_bt + season + origin:dep_period + season:rain_flag,
                 data = analytic_no_na, family = binomial)

# adding one interaction
lrtest(new_full_model, int_model)
# adding a second interactions
lrtest(new_full_model, int_model_2)

AIC(int_model_1, int_model_2, new_full_model)
# model 2 is better

vif(int_model_2)
# not bad

summary(int_model_2)

# while the interactions improve the model, it is only a slight improvement
# and therefore not worth including in the model, it would complicate it and 
# make interpretations difficult, make some vars insignificant too
```

```{r final model}
summary(new_full_model)
```

