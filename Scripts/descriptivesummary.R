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

# Calculate proportions
prop_data <- prop.table(table(final_data$CIDGSCOR, final_data$RHQ360)) * 100

# Convert the table to a data frame for ggplot
prop_data_df <- as.data.frame(prop_data)
colnames(prop_data_df) <- c("CIDGSCOR", "RHQ360", "Proportion")

# Map values for clarity
prop_data_df$CIDGSCOR <- factor(prop_data_df$CIDGSCOR, 
                                levels = c(1, 5), 
                                labels = c("Yes", "Don't Know"))
prop_data_df$RHQ360 <- factor(prop_data_df$RHQ360, 
                              levels = c(1, 2), 
                              labels = c("Positive Diagnosis", "Negative Diagnosis"))

# Create the stacked bar plot
ggplot(prop_data_df, aes(x = RHQ360, y = Proportion, fill = CIDGSCOR)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Proportions of GAD Responses by Diagnosis",
    x = "Diagnosis (RHQ360)",
    y = "Proportion (%)",
    fill = "GAD Responses"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Yes" = "#1f78b4", "Don't Know" = "#33a02c"))  # Adjust colors if needed

# Optional: Create a grouped bar plot instead
ggplot(prop_data_df, aes(x = RHQ360, y = Proportion, fill = CIDGSCOR)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    title = "Proportions of GAD Responses by Diagnosis",
    x = "Diagnosis (RHQ360)",
    y = "Proportion (%)",
    fill = "GAD Responses"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Yes" = "#1f78b4", "Don't Know" = "#33a02c"))  # Adjust colors if needed


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

# Frequency counts and percentages for categorical variables
categorical_summary <- female_data %>%
  summarise(
    Total_Females = n(),
    GAD_Positive_Count = sum(GAD_status == "Positive Diagnosis", na.rm = TRUE),
    GAD_Negative_Count = sum(GAD_status == "Negative Diagnosis", na.rm = TRUE),
    DD_Positive_Count = sum(DD_status == "Positive Diagnosis", na.rm = TRUE),
    DD_Negative_Count = sum(DD_status == "Negative Diagnosis", na.rm = TRUE), # Added DD Negative count
    Endometriosis_Yes_Count = sum(Endometriosis_status == "Yes", na.rm = TRUE),
    Endometriosis_No_Count = sum(Endometriosis_status == "No", na.rm = TRUE),
    Endometriosis_Refused_Count = sum(Endometriosis_status == "Refused", na.rm = TRUE),
    Endometriosis_Dont_Know_Count = sum(Endometriosis_status == "Don't know", na.rm = TRUE)
  ) %>%
  mutate(
    GAD_Positive_Percent = GAD_Positive_Count / Total_Females * 100,
    GAD_Negative_Percent = GAD_Negative_Count / Total_Females * 100,
    DD_Positive_Percent = DD_Positive_Count / Total_Females * 100,
    DD_Negative_Percent = DD_Negative_Count / Total_Females * 100, # Added DD Negative percentage
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
gad_plot <- female_data %>%
  count(GAD_status) %>%
  ggplot(aes(x = GAD_status, y = n, fill = GAD_status)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Prevalence of GAD",
    x = "GAD Status",
    y = "Count"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15), # Even smaller title
    axis.text.x = element_text(angle = 30, hjust = 1) # Rotate x-axis labels
  ) +
  scale_fill_brewer(palette = "Set2")

dd_plot <- female_data %>%
  count(DD_status) %>%
  ggplot(aes(x = DD_status, y = n, fill = DD_status)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Prevalence of DD",
    x = "DD Status",
    y = "Count"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15), # Even smaller title
    axis.text.x = element_text(angle = 30, hjust = 1) # Rotate x-axis labels
  ) +
  scale_fill_brewer(palette = "Set2")

endo_plot <- female_data %>%
  count(Endometriosis_status) %>%
  filter(Endometriosis_status %in% c("Yes", "No")) %>%
  ggplot(aes(x = Endometriosis_status, y = n, fill = Endometriosis_status)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Prevalence of Endometriosis",
    x = "Endometriosis Status",
    y = "Count"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14), # Even smaller title
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
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14) # Even smaller title
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

# New plot: Prevalence of GAD and DD by Endometriosis Status
gad_dd_by_endo_plot <- female_data %>%
  count(Endometriosis_status, GAD_status, DD_status) %>%
  filter(Endometriosis_status %in% c("Yes", "No")) %>%
  ggplot(aes(x = Endometriosis_status, y = n, fill = interaction(GAD_status, DD_status))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Prevalence of GAD & DD by Endometriosis Status",
    x = "Endometriosis Status",
    y = "Count",
    fill = "GAD x DD"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 9), # Smaller plot title
    axis.text.x = element_text(size = 12),                           # X-axis text size
    axis.title.x = element_text(size = 14),                          # X-axis label size
    axis.title.y = element_text(size = 14),                          # Y-axis label size
    legend.position = "right",                                       # Move legend to the side
    legend.text = element_text(size = 8),                            # Smaller legend text
    legend.title = element_text(size = 9),                           # Smaller legend title
    legend.key.size = unit(0.4, "cm"),                               # Smaller legend keys
    legend.spacing.x = unit(0.2, "cm"),                              # Reduce spacing between legend items
    legend.margin = margin(t = 0, b = 0, unit = "cm")                # Minimize legend margins
  ) +
  scale_fill_brewer(palette = "Set2")



###

#prevalence of endometriosis based on GAD status
final_data %>%
  group_by(CIDGSCOR) %>%
  summarise(
    Endometriosis_Yes = sum(RHQ360 == 1, na.rm = TRUE),
    Endometriosis_No = sum(RHQ360 == 2, na.rm = TRUE),
    Total = n(),
    Prevalence = Endometriosis_Yes / Total * 100
  )

#prevalence of endometriosis based on DD status
final_data %>%
  group_by(CIDDSCOR) %>%
  summarise(
    Endometriosis_Yes = sum(RHQ360 == 1, na.rm = TRUE),
    Endometriosis_No = sum(RHQ360 == 2, na.rm = TRUE),
    Total = n(),
    Prevalence = Endometriosis_Yes / Total * 100
  )
#prevalence (percentage of individuals with endometriosis out of the total number of 
#individuals in each DD status group) of endometriosis is slightly higher in individuals 
#with a positive depressive disorder diagnosis (7.53%) compared to those with a negative depressive disorder diagnosis (5.08%)

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

#Chi-square test for independence between GAD and Endometriosis 
chisq.test(table(final_data$CIDGSCOR, final_data$RHQ360))
#small chi-square ts, large p-value: there is no statistically significant relationship between GAD diagnosis and self-reported endometriosis status

###

#T-test to compare the age distribution between two groups based on Generalized Anxiety Disorder (GAD) status
t.test(RIDAGEYR ~ CIDDSCOR, data = final_data)
#indicates that there is a statistically significant difference in the mean age between the two groups, GAD positive vs. GAD negative

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
print(odds_dd) 

#If the odds ratio is greater than 1, it suggests that the presence of endometriosis is associated with a higher odds of having GAD or DD.
#If the odds ratio is less than 1, it suggests that the presence of endometriosis is associated with a lower odds of having GAD or DD.
#If the odds ratio is 1, there is no difference in the odds between the group


###

#summary table showing the prevalence of endometriosis within different age groups and GAD statuses
final_data %>%
  group_by(CIDGSCOR, age_group = cut(RIDAGEYR, breaks = c(20, 30, 40), labels = c("20-29", "30-39"))) %>%
  summarise(
    Endometriosis_Yes = sum(RHQ360 == 1, na.rm = TRUE),
    Total = n(),
    Prevalence = Endometriosis_Yes / Total * 100
  )
#Prevalence in age group 20-29: GAD-positive individuals (CIDGSCOR = 1) show a 0% prevalence of endometriosis (no cases).
#GAD-negative individuals (CIDGSCOR = 5) show a 3.4% prevalence of endometriosis.
#Prevalence in age group 30-39: GAD-positive individuals (CIDGSCOR = 1) have a 12.5% prevalence of endometriosis.
#GAD-negative individuals (CIDGSCOR = 5) have an 8.15% prevalence of endometriosis.

###table 1
# Install the Hmisc package if you haven't already
install.packages("Hmisc")

# Load the necessary libraries
library(tidyverse)
library(arsenal)
library(haven)
library(Hmisc)  # Required for label()

# Recode categorical variables for readability
final_data$GAD_status <- factor(final_data$GAD_status, levels = c("Positive Diagnosis", "Negative Diagnosis"), labels = c("Positive", "Negative"))
final_data$DD_status <- factor(final_data$DD_status, levels = c("Positive Diagnosis", "Negative Diagnosis"), labels = c("Positive", "Negative"))
final_data$Endometriosis_status <- factor(final_data$Endometriosis_status, levels = c("Yes", "No", "Refused", "Don't know"))
final_data$Gender <- factor(final_data$Gender, levels = c("Female", "Male"), labels = c("Female", "Male"))

# Label the variables
label(final_data$Gender) <- "Gender"
label(final_data$RIDAGEYR) <- "Age"
label(final_data$GAD_status) <- "GAD Status"
label(final_data$DD_status) <- "Depression Diagnosis"
label(final_data$Endometriosis_status) <- "Endometriosis Status"

# Units for continuous variables (e.g., age)
units(final_data$RIDAGEYR) <- "Years"

# Filter for female data, and create age groups
female_data <- final_data %>%
  filter(Gender == "Female", RIDAGEYR >= 20 & RIDAGEYR <= 39)

# Generate Table 1 with characteristics by GAD status
table1( ~ RIDAGEYR + Gender + GAD_status + DD_status + Endometriosis_status | GAD_status,
        data = female_data,
        caption = "Table 1: Baseline Characteristics by GAD Status",
        render.continuous = c("Mean [SD]", "Median [Min, Max]"),  # Use appropriate summary statistics
        overall = c(left = "Total"))
####table 1 second try###
# Install and load necessary libraries
install.packages("tableone")
library(tidyverse)
library(tableone)

# Recode categorical variables for readability
final_data$GAD_status <- factor(final_data$GAD_status, levels = c("Positive Diagnosis", "Negative Diagnosis"), labels = c("Positive", "Negative"))
final_data$DD_status <- factor(final_data$DD_status, levels = c("Positive Diagnosis", "Negative Diagnosis"), labels = c("Positive", "Negative"))
final_data$Endometriosis_status <- factor(final_data$Endometriosis_status, levels = c("Yes", "No", "Refused", "Don't know"))
final_data$Gender <- factor(final_data$Gender, levels = c("Female", "Male"), labels = c("Female", "Male"))

# Filter for female data
female_data <- final_data %>%
  filter(Gender == "Female", RIDAGEYR >= 20 & RIDAGEYR <= 39)

# Create a table with baseline characteristics by GAD status
table1 <- CreateTableOne(vars = c("RIDAGEYR", "Gender", "GAD_status", "DD_status", "Endometriosis_status"), 
                         strata = "GAD_status", 
                         data = female_data, 
                         factorVars = c("Gender", "GAD_status", "DD_status", "Endometriosis_status"))

# Print the table
print(table1)

###table 1 try 3###
# Remove rows where GAD_status has missing values
female_data <- female_data %>%
  filter(!is.na(GAD_status))  # Remove rows with NA in GAD_status

# Alternatively, remove rows with any missing values (if needed for other variables too)
# female_data <- female_data %>%
#   filter(complete.cases(female_data))  # Remove rows with any missing values

# Generate Table 1 with characteristics by GAD status
library(table1)
table1_result <- table1( ~ RIDAGEYR + Gender + GAD_status + DD_status + Endometriosis_status | GAD_status,
                         data = female_data,
                         caption = "Table 1: Baseline Characteristics by GAD Status",
                         render.continuous = c("Mean [SD]", "Median [Min, Max]"),
                         overall = c(left = "Total"))

# Print the table
print(table1_result)

#####try 4
# Define the create_summary_table function with a sample size check for the Shapiro test
create_summary_table <- function(data, variables, var_names = NULL, level_names = NULL) {
  # Recode factor levels based on the level_names argument
  if (!is.null(level_names)) {
    for (var in names(level_names)) {
      if (var %in% colnames(data)) {
        data[[var]] <- factor(data[[var]], levels = names(level_names[[var]]), labels = level_names[[var]])
      }
    }
  }
  
  # Initialize a list to store rows for the table
  table_rows <- list()
  
  # Loop through variables to calculate values for the table
  for (var in variables) {
    var_name <- ifelse(!is.null(var_names) && var %in% names(var_names), var_names[[var]], var)
    
    # Handle categorical variables
    if (is.factor(data[[var]])) {
      counts <- table(data[[var]])
      proportions <- round(prop.table(counts) * 100, 1)
      
      # Add a row for the categorical variable name
      table_rows[[length(table_rows) + 1]] <- data.frame(
        Variable = var_name,
        Level = NA,
        Summary = NA
      )
      
      # Add rows for each level of the categorical variable
      for (level_name in names(counts)) {
        table_rows[[length(table_rows) + 1]] <- data.frame(
          Variable = NA,
          Level = level_name,
          Summary = paste0(counts[[level_name]], " (", proportions[[level_name]], "%)")
        )
      }
    }
    
    # Handle continuous variables
    else if (is.numeric(data[[var]])) {
      # Check sample size before applying the Shapiro test
      sample_size <- sum(!is.na(data[[var]]))
      if (sample_size >= 3 && sample_size <= 5000) {
        # Check for normality using Shapiro-Wilk test
        normal_test <- shapiro.test(data[[var]])$p.value
        if (normal_test > 0.05) {
          # Normally distributed: Use mean and SD
          mean_val <- round(mean(data[[var]], na.rm = TRUE), 1)
          sd_val <- round(sd(data[[var]], na.rm = TRUE), 1)
          summary_stat <- paste0(mean_val, " (SD ", sd_val, ")")
        } else {
          # Non-normally distributed: Use median and IQR
          median_val <- round(median(data[[var]], na.rm = TRUE), 1)
          iqr_val <- round(IQR(data[[var]], na.rm = TRUE), 1)
          summary_stat <- paste0(median_val, " [IQR: ", iqr_val, "]")
        }
      } else {
        # If sample size is too small, skip the normality test and just show median and IQR
        median_val <- round(median(data[[var]], na.rm = TRUE), 1)
        iqr_val <- round(IQR(data[[var]], na.rm = TRUE), 1)
        summary_stat <- paste0(median_val, " [IQR: ", iqr_val, "]")
      }
      
      # Add row for continuous variable
      table_rows[[length(table_rows) + 1]] <- data.frame(
        Variable = var_name,
        Level = NA,
        Summary = summary_stat
      )
    }
  }
  
  # Combine rows into a data frame
  summary_table <- bind_rows(table_rows) %>%
    select(Variable, Level, Summary)
  
  return(summary_table)
}

# Generate the summary table with the updated function
summary_table <- create_summary_table(
  data = female_data,
  variables = c("RIDAGEYR", "Gender", "GAD_status", "DD_status", "Endometriosis_status"),
  var_names = var_names,
  level_names = level_names
)

# Formatting the table using flextable
formatted_summary_table <- summary_table %>%
  flextable() %>%
  set_header_labels(
    Variable = "Covariate",
    Level = "",
    Summary = "Summary Statistics"
  ) %>%
  theme_vanilla() %>%
  autofit() %>%
  bold(part = "header") %>%
  bold(j = "Variable", part = "all") %>%  
  italic(j = "Level", part = "all") %>%
  align(j = c("Variable", "Level"), align = "left", part = "all") %>%
  align(j = "Summary", align = "center", part = "all")

# Display the formatted table
formatted_summary_table





