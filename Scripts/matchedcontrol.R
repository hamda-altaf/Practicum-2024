library(dplyr)
library(MatchIt)

# Keep only necessary variables
matching_data <- filtered_data %>%
  select(RHQ360, DEPR_LVL, RIDAGEYR) %>%
  mutate(RHQ360 = factor(RHQ360, levels = c(2,1), labels = c("No", "Yes")))  # Convert endometriosis to factor with No as the control group

# Perform exact matching on age
matched_data <- matchit(RHQ360 ~ RIDAGEYR, data = matching_data, method = "exact")

# Check balance
summary(matched_data)
plot(matched_data)

#1:1 nearest neighbour matching
matched_data_nn <- matchit(RHQ360 ~ RIDAGEYR, data = matching_data, method = "nearest", ratio = 1)

summary(matched_data_nn)

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

#clogit with nn (1:1)

matched_df_nn <- match.data(matched_data_nn)
matched_df_nn <- matched_df_nn %>%
       mutate(DEPR_BINARY = ifelse(DEPR_LVL == "No depression", 0, 1))  # 0 = No depression, 1 = Moderate/Severe depression
clogit_model_nn <- clogit(DEPR_BINARY ~ RHQ360 + strata(RIDAGEYR), data = matched_df_nn)
summary(clogit_model_nn)
exp(cbind(OR = coef(clogit_model_nn), confint(clogit_model_nn)))



##multinomial log regression with all three depression levels

library(nnet)
multinom_model <- multinom(DEPR_LVL ~ RHQ360 + strata(RIDAGEYR), data = matched_df)
summary(multinom_model)
exp(coef(multinom_model))  # Get odds ratios
``
