title: "Lab 5 - Model Building Competition"
subtitle: "Your First Data Analysis"
author: "Sujan Kakumanu"
date: "2025-10-06"
format: 
  html:
  code-fold: false
toc: true
toc-location: left
theme: cosmo

  

options(scipen = 999)
library(tidyverse)
library(tidycensus)
library(broom)
library(scales)

census_api_key("a55638f442ab081818dbf0ae29a32549253a2ab0")

challenge_data <- get_acs(
  geography = "county",
  state = "PA",
  variables = c(
    home_value = "B25077_001",      # YOUR TARGET
    total_pop = "B01003_001",       # Total population
    median_income = "B19013_001",   # Median household income
    median_age = "B01002_001",      # Median age
    percent_college = "B15003_022", # Bachelor's degree or higher
    median_rent = "B25058_001",     # Median rent
    poverty_rate = "B17001_002"     # Population in poverty
  ),
  year = 2022,
  output = "wide"
)

plot(challenge_data$median_incomeE, challenge_data$home_valueE)
plot(challenge_data$total_popE, challenge_data$home_valueE)
plot(challenge_data$median_ageE, challenge_data$home_valueE)
plot(challenge_data$percent_collegeE, challenge_data$home_valueE)
plot(challenge_data$median_rentE, challenge_data$home_valueE)
plot(challenge_data$poverty_rateE, challenge_data$home_valueE)

plot(log(challenge_data$percent_collegeE), challenge_data$home_valueE)

install.packages("recipes")
install.packages("ggplot2")
install.packages("lattice")
install.packages("e1071")
install.packages("prodlim")
install.packages("recipes")
install.packages("caret", dependencies = TRUE)

library(caret)

# 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

cv_model <- train(home_valueE ~ median_incomeE,
                  data = challenge_data,
                  method = "lm",
                  trControl = train_control)

cv_model$results


cv_model <- train(home_valueE ~ median_rentE,
                  data = challenge_data,
                  method = "lm",
                  trControl = train_control)

cv_model$results

cv_model <- train(home_valueE ~ log(percent_collegeE),
                  data = challenge_data,
                  method = "lm",
                  trControl = train_control)

cv_model$results

cv_model <- train(home_valueE ~ median_incomeE + median_rentE + log(percent_collegeE),
                  data = challenge_data,
                  method = "lm",
                  trControl = train_control)
cv_model$results

```