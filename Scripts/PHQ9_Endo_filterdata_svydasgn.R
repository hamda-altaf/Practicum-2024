# Load required libraries
library(tidyverse)
library(haven)
library(survey)


# Define the directory containing the .xpt files
data_dir <- "/Users/hamdaaltaf/Desktop/Version Control/Practicum_2024/Datasets"

# Load datasets for 2005-2006 cycle
demo <- read_xpt(file.path(data_dir, "DEMO_D.xpt"))
rhq <- read_xpt(file.path(data_dir, "RHQ_D.xpt"))
dpq <- read_xpt(file.path(data_dir, "DPQ_D.xpt"))
bmx <- read_xpt(file.path(data_dir, "BMX_D.xpt"))

# Select relevant variables
selected_vars <- c("SEQN", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDMARTL", "DMDEDUC2", 
                   "INDFMPIR", "WTMEC2YR", "SDMVPSU", "SDMVSTRA")
demo <- demo %>% select(all_of(selected_vars))

rhq <- rhq %>% select(SEQN, RHQ010, RHQ131, RHQ360)
dpq <- dpq %>% select(SEQN, DPQ010:DPQ090)
bmx <- bmx %>% select(SEQN, BMXBMI)

# Merge datasets by SEQN
raw_data <- reduce(list(demo, rhq, dpq, bmx), full_join, by = "SEQN")
cat("Sample size (raw data):", nrow(raw_data), "\n")

# Exclusions
filtered_data <- raw_data %>%
  # Exclude males
  filter(RIAGENDR == 2) %>%
  {cat("Excluded men:", nrow(raw_data) - nrow(.), "\n"); .} %>%
  
  # Keep only ages 12-150 (exclude ages <12)
  filter(RIDAGEYR >= 12 & RIDAGEYR <= 150) %>%
  {cat("Excluded ineligible ages:", nrow(raw_data) - nrow(.), "\n"); .} %>%
  
  # Exclude incomplete endometriosis data (7 = Refused, 9 = Don't know, . = Missing)
  filter(RHQ360 %in% c(1, 2)) %>%
  {cat("Excluded incomplete endometriosis:", nrow(raw_data) - nrow(.), "\n"); .} %>%
  
  # Exclude incomplete PHQ-9 data (7 = Refused, 9 = Don't know, . = Missing)
  filter(!if_any(starts_with("DPQ"), ~ . %in% c(7, 9, NA))) %>%
  {cat("Excluded incomplete PHQ-9:", nrow(raw_data) - nrow(.), "\n"); .} %>%
  
  # Exclude refused (777), don't know (999), and missing (.) for age at menarche (RHQ010)
  filter(!RHQ010 %in% c(777, 999)) %>%
  filter(!is.na(RHQ010)) %>%
  {cat("Excluded incomplete age at menarche:", nrow(raw_data) - nrow(.), "\n"); .} %>%
  
  # Exclude missing demographic variables
  filter(!if_any(c("RIDRETH1", "DMDMARTL", "DMDEDUC2", "INDFMPIR"), is.na)) %>%
  {cat("Excluded missing demographic variables:", nrow(raw_data) - nrow(.), "\n"); .} %>%
  
  # Exclude missing BMI
  filter(!is.na(BMXBMI)) %>%
  {cat("Excluded missing BMI:", nrow(raw_data) - nrow(.), "\n"); .}

cat("Final sample size:", nrow(filtered_data), "\n")

# Compute PHQ-9 total score and categorize depression levels
filtered_data <- filtered_data %>%
  rowwise() %>%
  mutate(DEPR_TOT = sum(c_across(DPQ010:DPQ090), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(DEPR_LVL = case_when(
    DEPR_TOT < 10 ~ "No depression",
    DEPR_TOT >= 10 & DEPR_TOT <= 14 ~ "Moderate depression",
    DEPR_TOT >= 15 ~ "Severe depression"
  ))

# Save datasets
write_csv(raw_data, file.path(data_dir, "NHANES_2005_2006_raw.csv"))
write_csv(filtered_data, file.path(data_dir, "NHANES_2005_2006_cleaned.csv"))

# Create survey object
survey_design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  data = filtered_data,
  nest = TRUE
)

# Check survey design
print(survey_design)


# Create the 2x3 table
endometriosis_depression_table <- table(filtered_data$RHQ360, filtered_data$DEPR_LVL)

# Rename rows for clarity (Endometriosis: 1 = Yes, 2 = No)
rownames(endometriosis_depression_table) <- c("Yes Endometriosis", "No Endometriosis")

# Print the table
print(endometriosis_depression_table)

