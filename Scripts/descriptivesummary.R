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

# Cross-tabulation
table(final_data$RIAGENDR, final_data$age_group, final_data$CIDDSCOR)

# Cross-tabulation of GAD and endometriosis
table(final_data$CIDGSCOR, final_data$RHQ360) 

#proportions of yes, no, don't know by +ve, -ve diagnosis
prop.table(table(final_data$CIDGSCOR, final_data$RHQ360)) * 100


#######

# Load necessary libraries
library(tidyverse)

# Recode categorical variables for better readability
final_data <- final_data %>%
  mutate(
    GAD_status = case_when(
      CIDGSCOR == 1 ~ "Positive Diagnosis",
      CIDGSCOR == 5 ~ "Negative Diagnosis",
      TRUE ~ NA_character_
    ),
    DD_status = case_when(
      CIDDSCOR == 1 ~ "Positive Diagnosis",
      CIDDSCOR == 5 ~ "Negative Diagnosis",
      TRUE ~ NA_character_
    ),
    Endometriosis_status = case_when(
      RHQ360 == 1 ~ "Yes",
      RHQ360 == 2 ~ "No",
      RHQ360 == 7 ~ "Refused",
      RHQ360 == 9 ~ "Don't know",
      TRUE ~ NA_character_
    ),
    Gender = case_when(
      RIAGENDR == 1 ~ "Male",
      RIAGENDR == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  )

# Filter the data for females only
female_data <- final_data %>%
  filter(Gender == "Female", RIDAGEYR >= 20 & RIDAGEYR <= 39)

# Frequency counts and percentages for categorical variables
categorical_summary <- female_data %>%
  summarise(
    Total_Females = n(),
    GAD_Positive_Count = sum(GAD_status == "Positive Diagnosis", na.rm = TRUE),
    GAD_Negative_Count = sum(GAD_status == "Negative Diagnosis", na.rm = TRUE),
    DD_Positive_Count = sum(DD_status == "Positive Diagnosis", na.rm = TRUE),
    DD_Negative_Count = sum(DD_status == "Negative Diagnosis", na.rm = TRUE),
    Endometriosis_Yes_Count = sum(Endometriosis_status == "Yes", na.rm = TRUE),
    Endometriosis_No_Count = sum(Endometriosis_status == "No", na.rm = TRUE),
    Endometriosis_Refused_Count = sum(Endometriosis_status == "Refused", na.rm = TRUE),
    Endometriosis_Dont_Know_Count = sum(Endometriosis_status == "Don't know", na.rm = TRUE)
  ) %>%
  mutate(
    GAD_Positive_Percent = GAD_Positive_Count / Total_Females * 100,
    GAD_Negative_Percent = GAD_Negative_Count / Total_Females * 100,
    DD_Positive_Percent = DD_Positive_Count / Total_Females * 100,
    DD_Negative_Percent = DD_Negative_Count / Total_Females * 100,
    Endometriosis_Yes_Percent = Endometriosis_Yes_Count / Total_Females * 100,
    Endometriosis_No_Percent = Endometriosis_No_Count / Total_Females * 100,
    Endometriosis_Refused_Percent = Endometriosis_Refused_Count / Total_Females * 100,
    Endometriosis_Dont_Know_Percent = Endometriosis_Dont_Know_Count / Total_Females * 100
  )

# Display categorical variable summary
print("Categorical Summary:")
print(categorical_summary)

# Means and standard deviations for continuous variables (age)
continuous_summary <- female_data %>%
  summarise(
    Mean_Age = mean(RIDAGEYR, na.rm = TRUE),
    SD_Age = sd(RIDAGEYR, na.rm = TRUE)
  )

# Display continuous variable summary
print("Continuous Variable Summary (Age):")
print(continuous_summary)

#######

# Create bar plots for GAD, DD, and endometriosis prevalence
# Adjust each plot's theme for better spacing and readability
# Adjust each plot's theme for even smaller titles and fix overlapping x-axis labels
gad_plot <- female_data %>%
  count(GAD_status) %>%
  ggplot(aes(x = GAD_status, y = n, fill = GAD_status)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Prevalence of Generalized Anxiety Disorder (GAD)",
    x = "GAD Status",
    y = "Count"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 8), # Even smaller title
    axis.text.x = element_text(angle = 30, hjust = 1) # Rotate x-axis labels
  ) +
  scale_fill_brewer(palette = "Set2")

dd_plot <- female_data %>%
  count(DD_status) %>%
  ggplot(aes(x = DD_status, y = n, fill = DD_status)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Prevalence of Depressive Disorder (DD)",
    x = "DD Status",
    y = "Count"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 8), # Even smaller title
    axis.text.x = element_text(angle = 30, hjust = 1) # Rotate x-axis labels
  ) +
  scale_fill_brewer(palette = "Set2")

endo_plot <- female_data %>%
  count(Endometriosis_status) %>%
  filter(Endometriosis_status %in% c("Yes", "No")) %>%
  ggplot(aes(x = Endometriosis_status, y = n, fill = Endometriosis_status)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Prevalence of Self-Reported Endometriosis",
    x = "Endometriosis Status",
    y = "Count"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 8), # Even smaller title
    axis.text.x = element_text(angle = 30, hjust = 1) # Rotate x-axis labels
  ) +
  scale_fill_brewer(palette = "Set2")

age_plot <- female_data %>%
  ggplot(aes(x = RIDAGEYR)) +
  geom_histogram(binwidth = 2, fill = "#69b3a2", color = "black") +
  labs(
    title = "Age Distribution (Females Aged 20-39)",
    x = "Age (years)",
    y = "Count"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 8) # Even smaller title
  )

# Combine the plots using patchwork
library(patchwork)

combined_plot <- (
  gad_plot + dd_plot +
    plot_layout(ncol = 2, widths = c(1, 1))
) /
  (
    endo_plot + age_plot +
      plot_layout(ncol = 2, widths = c(1, 1))
  )

# Add an overarching title to the combined plot
combined_plot <- combined_plot +
  plot_annotation(
    title = "Overview of GAD, DD, Endometriosis, and Age Distribution",
    theme = theme(
      plot.title = element_text(size = 9, face = "bold", hjust = 0.5) # Even smaller overarching title
    )
  )

# Display the combined plot
print(combined_plot)

###

#prevalence of endometriosis based on GAD or DD status
final_data %>%
  group_by(CIDGSCOR) %>%
  summarise(
    Endometriosis_Yes = sum(RHQ360 == 1, na.rm = TRUE),
    Endometriosis_No = sum(RHQ360 == 2, na.rm = TRUE),
    Total = n(),
    Prevalence = Endometriosis_Yes / Total * 100
  )

###

#Create and summarize combined variables, e.g., co-occurrence of GAD and DD
final_data <- final_data %>%
  mutate(GAD_DD_Combined = case_when(
    CIDGSCOR == 1 & CIDDSCOR == 1 ~ "Both GAD & DD",
    CIDGSCOR == 1 & CIDDSCOR == 5 ~ "GAD Only",
    CIDGSCOR == 5 & CIDDSCOR == 1 ~ "DD Only",
    CIDGSCOR == 5 & CIDDSCOR == 5 ~ "Neither"
  ))

table(final_data$GAD_DD_Combined)

#Show the distribution of women with both GAD and DD, only GAD, only DD, or neither:
ggplot(final_data, aes(x = GAD_DD_Combined, fill = GAD_DD_Combined)) +
  geom_bar() +
  labs(title = "Combined GAD and DD Status", x = "Status", y = "Count") +
  theme_minimal()

#how the prevalence of endometriosis differs by GAD status
ggplot(final_data, aes(x = as.factor(CIDGSCOR), fill = as.factor(RHQ360))) +
  geom_bar(position = "fill") +
  labs(title = "Endometriosis Prevalence by GAD Status", x = "GAD Status", y = "Proportion") +
  scale_fill_discrete(name = "Endometriosis") +
  theme_minimal()

#how the prevalence of endometriosis differs by DD status
ggplot(final_data, aes(x = as.factor(CIDDSCOR), fill = as.factor(RHQ360))) +
  geom_bar(position = "fill") +
  labs(
    title = "Endometriosis Prevalence by DD Status",
    x = "DD Status",
    y = "Proportion"
  ) +
  scale_fill_discrete(
    name = "Endometriosis",
    labels = c("Yes", "No", "Refused", "Don't know")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )


###

#Chi-square test for independence
chisq.test(table(final_data$CIDGSCOR, final_data$RHQ360))

###

#T-test to compare age across groups (e.g., GAD positive vs. negative)
t.test(RIDAGEYR ~ CIDGSCOR, data = final_data)

###
#Compare the odds of self-reported endometriosis in women with GAD or DD versus those without
library(epitools)
gad_vs_endo <- table(final_data$GAD_status, final_data$Endometriosis_status)
oddsratio(gad_vs_endo)

# View the contingency table
print(gad_vs_endo)

# Create contingency table for DD and Endometriosis status
dd_vs_endo <- table(final_data$DD_status, final_data$Endometriosis_status)

# View the contingency table
print(dd_vs_endo)

# Calculate the odds ratio for GAD vs Endometriosis status
library(epitools)
odds_gad <- oddsratio(gad_vs_endo) 
print(odds_gad)

#For women with a positive GAD diagnosis (CIDGSCOR = 1), the odds ratio is 1.000, which indicates no difference in the odds of having endometriosis compared to those with a negative GAD diagnosis (CIDGSCOR = 5).
#For women with a negative GAD diagnosis (CIDGSCOR = 5), the odds ratio is 1.382792, suggesting that the odds of having endometriosis are about 38% higher for women with negative GAD compared to women with positive GAD.
#p-value for the chi-square test is 0.9297617, which is well above the typical threshold of 0.05, indicating that the difference in odds between the two groups is not statistically significant.
#The Fisher's exact test gives a p-value of 0.6773172, which further supports that there is no statistically significant association between GAD status and endometriosis in this sample.

###

# Calculate the odds ratio for DD vs Endometriosis status
odds_dd <- oddsratio(dd_vs_endo)
print(odds_dd) #did not calculate properly due to low counts for don't know

#manually calculate odds ratio for DD vs Endometriosis status
# Define the counts from the table
a <- 1  # DD Negative, Endometriosis: Don't know
b <- 1083  # DD Negative, Endometriosis: No
c <- 86  # DD Positive, Endometriosis: No
d <- 7  # DD Positive, Endometriosis: Yes

# Calculate the odds ratio
odds_ratio <- (a * d) / (b * c)

# Print the odds ratio
print(odds_ratio) #calculated odds ratio is very small, which suggests that the odds of having endometriosis are extremely low in the group of women with a positive diagnosis of depressive disorder (DD) compared to those without DD
#fisher's exact test? 

#If the odds ratio is greater than 1, it suggests that the presence of endometriosis is associated with a higher odds of having GAD or DD.
#If the odds ratio is less than 1, it suggests that the presence of endometriosis is associated with a lower odds of having GAD or DD.
#If the odds ratio is 1, there is no difference in the odds between the group


###
#Summarize the data by multiple groupings (e.g., by age group and GAD status)
final_data %>%
  group_by(CIDGSCOR, age_group = cut(RIDAGEYR, breaks = c(20, 30, 40), labels = c("20-29", "30-39"))) %>%
  summarise(
    Endometriosis_Yes = sum(RHQ360 == 1, na.rm = TRUE),
    Total = n(),
    Prevalence = Endometriosis_Yes / Total * 100
  )



