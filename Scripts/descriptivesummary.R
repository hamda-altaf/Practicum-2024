summary(final_data)

library(dplyr)

# Create age groups (customize as needed)
final_data <- final_data %>%
  mutate(age_group = case_when(
    RIDAGEYR < 20 ~ "<20",
    RIDAGEYR >= 20 & RIDAGEYR <= 39 ~ "20-39",
    RIDAGEYR >= 40 & RIDAGEYR <= 54 ~ "40-54",
    RIDAGEYR > 54 ~ ">54"
  ))

# Three-way summary (example for CIDDSCOR stratified by gender and age group)
summary_table <- final_data %>%
  group_by(RIAGENDR, age_group) %>%
  summarize(
    count = n(),
    mean_CIDDSCOR = mean(CIDDSCOR, na.rm = TRUE),
    sd_CIDDSCOR = sd(CIDDSCOR, na.rm = TRUE),
    .groups = "drop"
  )

# View the table
print(summary_table)

# Count and percentage of CIDDSCOR responses
CIDDSCOR_summary <- final_data %>%
  group_by(CIDDSCOR) %>%
  summarize(
    count = n(),
    percent = (n() / nrow(final_data)) * 100
  )

# View the summary
print(CIDDSCOR_summary)

# Install summarytools if not already installed
# install.packages("summarytools")

library(summarytools)

# Descriptive summary for specific variables (e.g., RIDAGEYR, CIDDSCOR, CIDGSCOR)
dfSummary(final_data[, c("RIDAGEYR", "CIDDSCOR", "CIDGSCOR")])

