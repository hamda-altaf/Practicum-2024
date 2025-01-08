# Load required libraries
library(tidyverse)
library(haven)

# Subset the data for women of reproductive age (20-39 years)
final_data_age <- final_data %>%
  filter(RIDAGEYR >= 20 & RIDAGEYR <= 39, RIAGENDR == 2)  # Women (RIAGENDR == 2)

# Check missing data
missing_data <- final_data_age %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  gather(key = "Variable", value = "MissingCount") %>%
  arrange(desc(MissingCount))

print(missing_data)

# Check summary statistics for key variables
summary_stats <- final_data_age %>%
  summarise(
    Endometriosis = sum(RHQ360 == 1, na.rm = TRUE),  # Assuming RHQ360 is coded as 1 for endometriosis
    GAD = sum(CIDGSCOR == 1, na.rm = TRUE),  # Assuming CIDGSCOR is coded as 1 for GAD
    Depression = sum(CIDDSCOR == 1, na.rm = TRUE)  # Assuming CIDDSCOR is coded as 1 for depression
  )

print(summary_stats)

# Calculate the proportion of women with GAD, depression, and endometriosis
proportion_data <- final_data_age %>%
  mutate(
    GAD = ifelse(CIDGSCOR == 1, "Yes", "No"),
    Depression = ifelse(CIDDSCOR == 1, "Yes", "No"),
    Endometriosis = ifelse(RHQ360 == 1, "Yes", "No")
  ) %>%
  group_by(GAD, Depression, Endometriosis) %>%
  summarise(Proportion = n() / nrow(final_data_age) * 100) %>%
  ungroup()

# Print the proportions
print(proportion_data)

# Visualize the distribution of Endometriosis, GAD, and Depression
ggplot(final_data_age, aes(x = Endometriosis, fill = GAD)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Endometriosis by GAD Status", y = "Proportion", fill = "GAD")

ggplot(final_data_age, aes(x = Endometriosis, fill = Depression)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Endometriosis by Depression Status", y = "Proportion", fill = "Depression")

# Create a contingency table to examine the relationship between GAD, Depression, and Endometriosis
contingency_table <- table(final_data_age$GAD, final_data_age$Depression, final_data_age$Endometriosis)
print(contingency_table)

# Perform a chi-square test to assess independence between GAD, Depression, and Endometriosis
chi_test <- chisq.test(contingency_table)
print(chi_test)
