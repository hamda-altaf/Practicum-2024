# Load necessary library
library(ggplot2)

# Recode RHQ360 into binary: Yes = 1, No = 0
final_data$RHQ360 <- ifelse(final_data$RHQ360 == 1, 1, 0)

table(final_data$RHQ360)



# Fit logistic regression model
model <- glm(RHQ360 ~ CIDDSCOR + CIDGSCOR + RIDAGEYR, 
             family = binomial(), data = final_data)

# Calculate the odds ratios (exponentiate the coefficients)
odds_ratios <- exp(coef(model))

# Calculate confidence intervals for the odds ratios
conf_intervals <- exp(confint(model))

# Create a data frame for plotting
odds_ratio_df <- data.frame(
  predictor = names(odds_ratios),
  odds_ratio = odds_ratios,
  lower_ci = conf_intervals[, 1],
  upper_ci = conf_intervals[, 2]
)

# Plot the odds ratios with confidence intervals
ggplot(odds_ratio_df, aes(x = predictor, y = odds_ratio)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Odds Ratio") +
  xlab("Predictor") +
  theme_minimal()

##ORs for GAD VS ENDOMETRIOSIS STATUS##

# Load necessary library
library(ggplot2)

# Recode RHQ360 into binary: Yes = 1, No = 0
final_data$RHQ360 <- ifelse(final_data$RHQ360 == 1, 1, 0)

# Recode CIDGSCOR: 1 = Positive Diagnosis, 5 = Negative Diagnosis
final_data$CIDGSCOR <- ifelse(final_data$CIDGSCOR == 1, 1, 0)

# Fit logistic regression model (GAD vs Endometriosis)
model <- glm(RHQ360 ~ CIDGSCOR, 
             family = binomial(), data = final_data)

# Calculate the odds ratios (exponentiate the coefficients)
odds_ratios <- exp(coef(model))

# Calculate confidence intervals for the odds ratios
conf_intervals <- exp(confint(model))

# Create a data frame for plotting
odds_ratio_df <- data.frame(
  predictor = names(odds_ratios),
  odds_ratio = odds_ratios,
  lower_ci = conf_intervals[, 1],
  upper_ci = conf_intervals[, 2]
)

# Plot the odds ratios with confidence intervals
ggplot(odds_ratio_df, aes(x = predictor, y = odds_ratio)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Odds Ratio") +
  xlab("Predictor") +
  theme_minimal()
