# Load the necessary libraries
library(dplyr)

# Assuming the demo_data_combined is the combined demo data from all cycles
# Assuming RHQ_data, Depression_data, and GAD_data are already merged for the relevant cycles

# Step 1: Total number of people in the merged demo datasets (0-150 years old)
total_demo_people <- nrow(demo_data_combined)
cat("Total people in merged demo datasets (0-150 years old):", total_demo_people, "\n")

# Step 2: Number of females 20-54 years
females_20_54 <- demo_data_combined %>% 
  filter(RIAGENDR == 2, RIDAGEYR >= 20, RIDAGEYR <= 54)
cat("Females 20-54 years:", nrow(females_20_54), "\n")

# Step 3: Number of females < 20 and > 54 years
females_less_20 <- demo_data_combined %>% 
  filter(RIAGENDR == 2, RIDAGEYR < 20)
females_more_54 <- demo_data_combined %>% 
  filter(RIAGENDR == 2, RIDAGEYR > 54)
cat("Females <20 years:", nrow(females_less_20), "\n")
cat("Females >54 years:", nrow(females_more_54), "\n")

# Step 4: Number of males 0-150 years
males_0_150 <- demo_data_combined %>% 
  filter(RIAGENDR == 1, RIDAGEYR >= 0, RIDAGEYR <= 150)
cat("Males 0-150 years:", nrow(males_0_150), "\n")

# Step 5: Of females 20-54 years, how many are in the RHQ merged dataset
females_20_54_in_RHQ <- females_20_54 %>% 
  filter(SEQN %in% RHQ_data$SEQN)
cat("Females 20-54 years in RHQ:", nrow(females_20_54_in_RHQ), "\n")
cat("Females 20-54 years NOT in RHQ:", nrow(females_20_54) - nrow(females_20_54_in_RHQ), "\n")

# Step 6: Of females 20-54 in RHQ, how many are <20 and how many are between 39-54 years
females_20_54_in_RHQ_less_20 <- females_20_54_in_RHQ %>% 
  filter(RIDAGEYR < 20)
females_20_54_in_RHQ_39_54 <- females_20_54_in_RHQ %>% 
  filter(RIDAGEYR >= 39, RIDAGEYR <= 54)
cat("Females <20 in RHQ:", nrow(females_20_54_in_RHQ_less_20), "\n")
cat("Females 39-54 in RHQ:", nrow(females_20_54_in_RHQ_39_54), "\n")

# Step 7: Of the females 20-54 in RHQ, how many are 20-39 years old
females_20_39_in_RHQ <- females_20_54_in_RHQ %>% 
  filter(RIDAGEYR >= 20, RIDAGEYR <= 39)
cat("Females 20-39 years in RHQ:", nrow(females_20_39_in_RHQ), "\n")

# Step 8: Of females 20-39 in RHQ, how many are in the depression dataset and how many are in the GAD dataset
females_20_39_in_depression <- females_20_39_in_RHQ %>% 
  filter(SEQN %in% Depression_data$SEQN)
females_20_39_in_GAD <- females_20_39_in_RHQ %>% 
  filter(SEQN %in% GAD_data$SEQN)
cat("Females 20-39 years in Depression dataset:", nrow(females_20_39_in_depression), "\n")
cat("Females 20-39 years in GAD dataset:", nrow(females_20_39_in_GAD), "\n")

# Step 9: Missing females 20-39 from depression and GAD datasets
females_20_39_missing_depression <- nrow(females_20_39_in_RHQ) - nrow(females_20_39_in_depression)
females_20_39_missing_GAD <- nrow(females_20_39_in_RHQ) - nrow(females_20_39_in_GAD)
cat("Missing females 20-39 years in Depression dataset:", females_20_39_missing_depression, "\n")
cat("Missing females 20-39 years in GAD dataset:", females_20_39_missing_GAD, "\n")

# Merge the females 20-39 from RHQ data with Depression data
females_20_39_in_RHQ_depression <- females_20_39_in_RHQ %>%
  left_join(Depression_data, by = "SEQN")

# Merge the females 20-39 from RHQ data with GAD data
females_20_39_in_RHQ_GAD <- females_20_39_in_RHQ %>%
  left_join(GAD_data, by = "SEQN")

# Step 10: Of females 20-39 in RHQ and Depression datasets, how many answered the CIDDSCOR question
females_20_39_in_depression_answered <- females_20_39_in_RHQ_depression %>%
  filter(!is.na(CIDDSCOR))
cat("Females 20-39 in Depression dataset who answered CIDDSCOR:", nrow(females_20_39_in_depression_answered), "\n")

# Step 11: Of females 20-39 in RHQ and GAD datasets, how many answered the CIDGSCOR question
females_20_39_in_GAD_answered <- females_20_39_in_RHQ_GAD %>% 
  filter(!is.na(CIDGSCOR))
cat("Females 20-39 in GAD dataset who answered CIDGSCOR:", nrow(females_20_39_in_GAD_answered), "\n")

# Step 12: Positive, Negative, and Missing diagnoses in Depression (CIDDSCOR)
depression_positive <- sum(females_20_39_in_depression_answered$"CIDDSCOR" == 1, na.rm = TRUE)
depression_negative <- sum(females_20_39_in_depression_answered$"CIDDSCOR" == 5, na.rm = TRUE)
depression_missing <- sum(is.na(females_20_39_in_depression_answered$"CIDDSCOR"))
cat("Positive diagnoses in Depression:", depression_positive, "\n")
cat("Negative diagnoses in Depression:", depression_negative, "\n")
cat("Missing diagnoses in Depression:", depression_missing, "\n")

# Step 13: Positive, Negative, and Missing diagnoses in GAD (CIDGSCOR)
GAD_positive <- sum(females_20_39_in_GAD_answered$CIDGSCOR == 1, na.rm = TRUE)
GAD_negative <- sum(females_20_39_in_GAD_answered$CIDGSCOR == 5, na.rm = TRUE)
GAD_missing <- sum(is.na(females_20_39_in_GAD_answered$CIDGSCOR))
cat("Positive diagnoses in GAD:", GAD_positive, "\n")
cat("Negative diagnoses in GAD:", GAD_negative, "\n")
cat("Missing diagnoses in GAD:", GAD_missing, "\n")

##ignore this if the script above running smoothly################
##############check 1 because was experiencing error in step 10#########
# Assuming 'females_20_39_in_RHQ' and 'Depression_data' have been merged already

# Merge the females 20-39 from RHQ data with Depression data
females_20_39_in_RHQ_depression <- females_20_39_in_RHQ %>%
  left_join(Depression_data, by = "SEQN")

# Check if the 'CIDDSCOR' variable exists in the merged dataset
if ("CIDDSCOR" %in% colnames(females_20_39_in_RHQ_depression)) {
  # Step 10: Of females 20-39 in RHQ and Depression datasets, how many answered the CIDDSCOR question
  females_20_39_in_depression_answered <- females_20_39_in_RHQ_depression %>%
    filter(!is.na(CIDDSCOR))
  
  cat("Females 20-39 in Depression dataset who answered CIDDSCOR:", nrow(females_20_39_in_depression_answered), "\n")
} else {
  cat("The 'CIDDSCOR' variable is missing from the Depression data\n")
}

#########check 2 because was experiencing error in step 10#####################

# Step 10: Merge the females 20-39 from RHQ data with Depression data
females_20_39_in_RHQ_depression <- females_20_39_in_RHQ %>%
  left_join(Depression_data, by = "SEQN")

# Check if the 'CIDDSCOR' variable exists in the merged dataset
if ("CIDDSCOR" %in% colnames(females_20_39_in_RHQ_depression)) {
  # Step 10: Of females 20-39 in RHQ and Depression datasets, how many answered the CIDDSCOR question
  females_20_39_in_depression_answered <- females_20_39_in_RHQ_depression %>% 
    filter(!is.na(CIDDSCOR))
  cat("Females 20-39 in Depression dataset who answered CIDDSCOR:", nrow(females_20_39_in_depression_answered), "\n")
} else {
  cat("The 'CIDDSCOR' variable is missing from the Depression data\n")
}

# Step 11: Of females 20-39 in RHQ and GAD datasets, how many answered the CIDGSCOR question
if ("CIDGSCOR" %in% colnames(females_20_39_in_GAD)) {
  females_20_39_in_GAD_answered <- females_20_39_in_GAD %>% 
    filter(!is.na(CIDGSCOR))
  cat("Females 20-39 in GAD dataset who answered CIDGSCOR:", nrow(females_20_39_in_GAD_answered), "\n")
} else {
  cat("The 'CIDGSCOR' variable is missing from the GAD data\n")
}

# Step 12: Positive, Negative, and Missing diagnoses in Depression (CIDDSCOR)
if (exists("females_20_39_in_depression_answered")) {
  depression_positive <- sum(females_20_39_in_depression_answered$CIDDSCOR == 1, na.rm = TRUE)
  depression_negative <- sum(females_20_39_in_depression_answered$CIDDSCOR == 5, na.rm = TRUE)
  depression_missing <- sum(is.na(females_20_39_in_depression_answered$CIDDSCOR))
  cat("Positive diagnoses in Depression:", depression_positive, "\n")
  cat("Negative diagnoses in Depression:", depression_negative, "\n")
  cat("Missing diagnoses in Depression:", depression_missing, "\n")
}

# Step 13: Positive, Negative, and Missing diagnoses in GAD (CIDGSCOR)
if (exists("females_20_39_in_GAD_answered")) {
  GAD_positive <- sum(females_20_39_in_GAD_answered$CIDGSCOR == 1, na.rm = TRUE)
  GAD_negative <- sum(females_20_39_in_GAD_answered$CIDGSCOR == 5, na.rm = TRUE)
  GAD_missing <- sum(is.na(females_20_39_in_GAD_answered$CIDGSCOR))
  cat("Positive diagnoses in GAD:", GAD_positive, "\n")
  cat("Negative diagnoses in GAD:", GAD_negative, "\n")
  cat("Missing diagnoses in GAD:", GAD_missing, "\n")
}

#######check for error in step 13###########
# Step 13: Positive, Negative, and Missing diagnoses in GAD (CIDGSCOR)
if (exists("females_20_39_in_GAD_answered")) {
  # Print the first few rows of the dataset to inspect
  print(head(females_20_39_in_GAD_answered))
  
  # Print the column names to confirm CIDGSCOR is present and check for any typos
  print(colnames(females_20_39_in_GAD_answered))
  
  # Check if the dataset is empty
  if (nrow(females_20_39_in_GAD_answered) == 0) {
    cat("No females 20-39 found in the GAD dataset.\n")
  } else {
    # If the dataset is not empty, calculate diagnoses
    GAD_positive <- sum(females_20_39_in_GAD_answered$CIDGSCOR == 1, na.rm = TRUE)
    GAD_negative <- sum(females_20_39_in_GAD_answered$CIDGSCOR == 5, na.rm = TRUE)
    GAD_missing <- sum(is.na(females_20_39_in_GAD_answered$CIDGSCOR))
    
    # Print results
    cat("Positive diagnoses in GAD:", GAD_positive, "\n")
    cat("Negative diagnoses in GAD:", GAD_negative, "\n")
    cat("Missing diagnoses in GAD:", GAD_missing, "\n")
  }
} else {
  cat("The 'females_20_39_in_GAD_answered' dataset does not exist.\n")
}

####check for GAD cont.#######

# Step 1: Filter the GAD dataset for females aged 20-39
females_20_39_in_GAD <- GAD_data %>%
  filter(RIAGENDR == 2, RIDAGEYR >= 20, RIDAGEYR <= 39)

# Step 2: Check if the CIDGSCOR variable exists in the GAD dataset
if ("CIDGSCOR" %in% colnames(females_20_39_in_GAD)) {
  # Step 3: Filter for those who answered the CIDGSCOR question
  females_20_39_in_GAD_answered <- females_20_39_in_GAD %>%
    filter(!is.na(CIDGSCOR))
  
  # Step 4: Output the result
  cat("Females 20-39 in GAD dataset who answered CIDGSCOR:", nrow(females_20_39_in_GAD_answered), "\n")
} else {
  cat("The 'CIDGSCOR' variable is missing from the GAD dataset\n")
}

###check to see how many in GAD######
# Step 1: Check how many people are in the GAD dataset
total_in_GAD <- nrow(GAD_data)
cat("Total people in GAD dataset:", total_in_GAD, "\n")

# Step 2: Filter the dataset to find how many answered the CIDGSCOR question
females_20_39_in_GAD_answered <- GAD_data %>%
  filter(!is.na(CIDGSCOR))

# Step 3: Check how many people answered the CIDGSCOR question
answered_GAD <- nrow(females_20_39_in_GAD_answered)
cat("Females 20-39 in GAD dataset who answered CIDGSCOR:", answered_GAD, "\n")
