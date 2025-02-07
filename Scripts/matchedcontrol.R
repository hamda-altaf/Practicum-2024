library(dplyr)
library(MatchIt)

# Keep only necessary variables
matching_data <- filtered_data %>%
  select(RHQ360, DEPR_LVL, RIDAGEYR) %>%
  mutate(RHQ360 = factor(RHQ360, levels = c(1, 2), labels = c("Yes", "No")))  # Convert endometriosis to factor

# Perform exact matching on age
matched_data <- matchit(RHQ360 ~ RIDAGEYR, data = matching_data, method = "exact")

# Check balance
summary(matched_data)

# Get the matched dataset
matched_df <- match.data(matched_data)

#recoding depression into a binary variable
matched_df <- matched_df %>%
  mutate(DEPR_BINARY = ifelse(DEPR_LVL == "No depression", 0, 1))  # 0 = No depression, 1 = Moderate/Severe depression


library(survival)

# Fit conditional logistic regression model
clogit_model <- clogit(DEPR_BINARY ~ RHQ360 + strata(RIDAGEYR), data = matched_df)

# Display results
summary(clogit_model)

#Odds Ratios & Confidence Intervals (OR > 1, endometriosis increases depression risk; if OR < 1, it decreases risk)
exp(cbind(OR = coef(clogit_model), confint(clogit_model)))

##multinomial log regression with all three depression levels

library(nnet)
multinom_model <- multinom(DEPR_LVL ~ RHQ360 + strata(RIDAGEYR), data = matched_df)
summary(multinom_model)
exp(coef(multinom_model))  # Get odds ratios
``
