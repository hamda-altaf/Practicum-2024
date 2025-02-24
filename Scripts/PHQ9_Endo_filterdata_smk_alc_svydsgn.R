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
smq <- read_xpt(file.path(data_dir, "SMQ_D.xpt"))  # Smoking data
alq <- read_xpt(file.path(data_dir, "ALQ_D.xpt"))  # Alcohol use data

# Select relevant variables
selected_vars <- c("SEQN", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "DMDMARTL", "DMDEDUC2", 
                   "INDFMPIR", "WTMEC2YR", "SDMVPSU", "SDMVSTRA")
demo <- demo %>% select(all_of(selected_vars))

rhq <- rhq %>% select(SEQN, RHQ010, RHQ131, RHQ360)
dpq <- dpq %>% select(SEQN, DPQ010:DPQ090)
bmx <- bmx %>% select(SEQN, BMXBMI)
smq <- smq %>% select(SEQN, SMQ020, SMQ040)  # Smoking variables
alq <- alq %>% select(SEQN, ALQ110, ALQ120Q, ALQ120U)  # Alcohol variables

# Define smoking status
smq <- smq %>%
  mutate(SMOKING_STATUS = case_when(
    SMQ020 == 2 ~ "Never",
    SMQ020 == 1 & SMQ040 == 3 ~ "Former smoker",
    SMQ020 == 1 & SMQ040 %in% c(1, 2) ~ "Current smoker",
    TRUE ~ NA_character_
  ))

# Define alcohol use
alq <- alq %>%
  # Convert ALQ120Q to a yearly frequency
  mutate(ALQ120Q_YEARLY = case_when(
    ALQ120U == 1 ~ ALQ120Q * 52,  # Weekly → Yearly
    ALQ120U == 2 ~ ALQ120Q * 12,  # Monthly → Yearly
    ALQ120U == 3 ~ ALQ120Q,       # Already yearly
    TRUE ~ NA_real_
  )) %>%
  # Categorize alcohol consumption
  mutate(ALCOHOL_USE = case_when(
    ALQ110 == 2 ~ "Never in lifetime",
    ALQ110 == 1 & !is.na(ALQ120Q_YEARLY) & ALQ120Q_YEARLY <= 11 ~ "Infrequent",
    ALQ110 == 1 & !is.na(ALQ120Q_YEARLY) & ALQ120Q_YEARLY >= 12 & ALQ120Q_YEARLY <= 104 ~ "Light",
    ALQ110 == 1 & !is.na(ALQ120Q_YEARLY) & ALQ120Q_YEARLY > 104 ~ "Moderate/Heavy",
    TRUE ~ NA_character_
  ))

# Merge datasets by SEQN
raw_data <- reduce(list(demo, rhq, dpq, bmx, smq, alq), full_join, by = "SEQN")
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
  {cat("Excluded missing BMI:", nrow(raw_data) - nrow(.), "\n"); .} %>%
  
  # Keep alcohol data but don't drop "Refused" or "Don't Know" responses
  filter(
    !(ALQ110 == 1 & is.na(ALQ120Q))  # Only exclude if they drank before and missing frequency data
  ) %>%
  {cat("Excluded truly missing alcohol use data:", nrow(raw_data) - nrow(.), "\n"); .}

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

filtered_data <- filtered_data %>%
  mutate(
    Depression_Binary = case_when(
      DEPR_LVL == "No depression" ~ 0,
      DEPR_LVL %in% c("Moderate depression", "Severe depression") ~ 1,
      TRUE ~ NA_real_  # Keep missing values as NA
    )
  )

filtered_data <- filtered_data %>%
  mutate(
    Endometriosis = case_when(
      RHQ360 == 1 ~ "Yes",  # 1 corresponds to Yes (Endometriosis present)
      RHQ360 == 2 ~ "No",   # 2 corresponds to No (Endometriosis not present)
      TRUE ~ NA_character_  # Keep NA for missing values
    )
  )

filtered_data <- filtered_data %>%
  mutate(
    AgeGroup = case_when(
      RIDAGEYR >= 20 & RIDAGEYR <= 29 ~ "20-29",
      RIDAGEYR >= 30 & RIDAGEYR <= 40 ~ "30-40",
      RIDAGEYR >= 41 & RIDAGEYR <= 54 ~ "41-54"
    ),
    Race = factor(RIDRETH1, labels = c("Mexican American", "Other Hispanic", "Non-Hispanic White",
                                       "Non-Hispanic Black", "Other Race")),
    PIR_Category = case_when(
      INDFMPIR < 1.35 ~ "Low-income",
      INDFMPIR >= 1.35 & INDFMPIR < 3.0 ~ "Medium-income",
      INDFMPIR >= 3.0 ~ "High-income"
    ),
    BMI_Category = case_when(
      BMXBMI < 18.5 ~ "Underweight",
      BMXBMI >= 18.5 & BMXBMI < 25 ~ "Normal weight",
      BMXBMI >= 25 & BMXBMI < 30 ~ "Overweight",
      BMXBMI >= 30 ~ "Obese"
    )
  )

filtered_data <- filtered_data %>%
  left_join(select(rhq, SEQN, RHQ131), by = "SEQN") %>%
  mutate(
    # Rename the correct RHQ131 column
    RHQ131 = RHQ131.y,
    
    # Age groups
    AgeGroup = case_when(
      RIDAGEYR >= 20 & RIDAGEYR <= 29 ~ "20-29",
      RIDAGEYR >= 30 & RIDAGEYR <= 40 ~ "30-40",
      RIDAGEYR >= 41 & RIDAGEYR <= 54 ~ "41-54"
    ),
    
    # Categorical BMI
    BMI_Category = case_when(
      BMXBMI < 18.5 ~ "Underweight",
      BMXBMI >= 18.5 & BMXBMI < 25 ~ "Normal weight",
      BMXBMI >= 25 & BMXBMI < 30 ~ "Overweight",
      BMXBMI >= 30 ~ "Obese"
    ),
    
    # Marital status categories
    Marital_Status = case_when(
      DMDMARTL %in% c(1, 2) ~ "Married",
      DMDMARTL == 3 ~ "Divorced",
      DMDMARTL == 4 ~ "Separated",
      DMDMARTL == 5 ~ "Spinster"
    ),
    
    # Education level categories
    Education_Level = case_when(
      DMDEDUC2 %in% c(1, 2) ~ "Less than High School",
      DMDEDUC2 == 3 ~ "High School",
      DMDEDUC2 %in% c(4, 5) ~ "College and Above"
    ),
    
    # Recode Race (combine Other Hispanic and Other Race)
    Race = case_when(
      RIDRETH1 == 1 ~ "Mexican American",
      RIDRETH1 == 3 ~ "Non-Hispanic White",
      RIDRETH1 == 4 ~ "Non-Hispanic Black",
      RIDRETH1 %in% c(2, 5) ~ "Other"
    ),
    
    # History of pregnancy (convert to binary: 1 = Yes, 0 = No)
    History_Pregnancy = as.numeric(RHQ131 == 1)
  ) %>%
  select(-RHQ131.x, -RHQ131.y)  # Drop duplicate columns

filtered_data <- filtered_data %>%
  mutate(
    AgeGroup = factor(AgeGroup),
    BMI_Category = factor(BMI_Category),
    Marital_Status = factor(Marital_Status),
    Education_Level = factor(Education_Level),
    Race = factor(Race),
    ALCOHOL_USE = factor(ALCOHOL_USE),
    SMOKING_STATUS = factor(SMOKING_STATUS)
  )

# Save datasets
write_csv(raw_data, file.path(data_dir, "NHANES_2005_2006_raw.csv"))
write_csv(filtered_data, file.path(data_dir, "NHANES_2005_2006_cleaned_smk_alc.csv"))

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
