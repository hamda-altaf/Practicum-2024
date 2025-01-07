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

########additional########
# Step 1: Filter for females aged 20-54 in the RHQ merged dataset
females_20_54_in_RHQ <- demo_data_combined %>%
  filter(RIAGENDR == 2, RIDAGEYR >= 20, RIDAGEYR <= 54) %>%
  inner_join(RHQ_data_combined, by = "SEQN")

# Step 2: Count responses for the RHQ360 variable
RHQ360_summary <- females_20_54_in_RHQ %>%
  mutate(RHQ360_category = case_when(
    RHQ360 == 1 ~ "Yes",
    RHQ360 == 2 ~ "No",
    RHQ360 == 7 ~ "Refused",
    RHQ360 == 9 ~ "Don't know",
    is.na(RHQ360) ~ "Missing"
  )) %>%
  group_by(RHQ360_category) %>%
  summarise(count = n(), .groups = "drop")

# Step 3: Print the summary
cat("Summary of responses to RHQ360 among females aged 20-54:\n")
print(RHQ360_summary)

# Step 1: Filter for females aged 20-54 in the RHQ merged dataset
females_20_54_in_RHQ <- demo_data_combined %>%
  filter(RIAGENDR == 2, RIDAGEYR >= 20, RIDAGEYR <= 54) %>%
  inner_join(RHQ_data_combined, by = "SEQN")

# Step 2: Count the number of females aged 20-54 in the RHQ dataset
total_females_20_54_in_RHQ <- nrow(females_20_54_in_RHQ)

# Step 3: Print the result
cat("Total number of females aged 20-54 in the RHQ merged dataset:", total_females_20_54_in_RHQ, "\n")

# Step 1: Total number of females aged 20-54 in demo_data_combined
females_20_54_in_demo <- demo_data_combined %>%
  filter(RIAGENDR == 2, RIDAGEYR >= 20, RIDAGEYR <= 54)

total_females_20_54_in_demo <- nrow(females_20_54_in_demo)
cat("Total number of females aged 20-54 in demo_data_combined:", total_females_20_54_in_demo, "\n")

# Step 2: Number of females aged 20-54 in demo_data_combined who are in RHQ_data_combined
females_20_54_in_demo_in_RHQ <- females_20_54_in_demo %>%
  filter(SEQN %in% RHQ_data_combined$SEQN)

total_females_20_54_in_RHQ <- nrow(females_20_54_in_demo_in_RHQ)
cat("Number of females aged 20-54 in demo_data_combined who are in RHQ merged dataset:", total_females_20_54_in_RHQ, "\n")

# Step 3: Number of females aged 20-54 in demo_data_combined who are NOT in RHQ_data_combined
females_20_54_in_demo_not_in_RHQ <- females_20_54_in_demo %>%
  filter(!(SEQN %in% RHQ_data_combined$SEQN))

total_females_20_54_not_in_RHQ <- nrow(females_20_54_in_demo_not_in_RHQ)
cat("Number of females aged 20-54 in demo_data_combined who are NOT in RHQ merged dataset:", total_females_20_54_not_in_RHQ, "\n")

###

# Step 1: Filter females aged 20-54 in RHQ merged dataset
females_20_54_in_RHQ <- demo_data_combined %>%
  filter(RIAGENDR == 2, RIDAGEYR >= 20, RIDAGEYR <= 54, SEQN %in% RHQ_data_combined$SEQN)

# Step 2: Merge demographic and RHQ data for females 20-54
females_20_54_with_RHQ360 <- females_20_54_in_RHQ %>%
  inner_join(RHQ_data_combined, by = "SEQN")

# Step 3: How many answered and did not answer RHQ360
answered_RHQ360 <- females_20_54_with_RHQ360 %>%
  filter(!is.na(RHQ360))

not_answered_RHQ360 <- females_20_54_with_RHQ360 %>%
  filter(is.na(RHQ360))

cat("Females aged 20-54 who answered RHQ360:", nrow(answered_RHQ360), "\n")
cat("Females aged 20-54 who did NOT answer RHQ360:", nrow(not_answered_RHQ360), "\n")

# Step 4: Of those who answered RHQ360, breakdown by response
response_breakdown <- answered_RHQ360 %>%
  group_by(RHQ360) %>%
  summarise(Count = n()) %>%
  mutate(Response = case_when(
    RHQ360 == 1 ~ "Yes",
    RHQ360 == 2 ~ "No",
    RHQ360 == 7 ~ "Refused",
    RHQ360 == 9 ~ "Don't know",
    is.na(RHQ360) ~ "Missing"
  ))

cat("\nBreakdown of responses to RHQ360:\n")
print(response_breakdown)

# Step 5: Add summary of missing responses
missing_count <- nrow(not_answered_RHQ360)
cat("\nMissing responses to RHQ360:", missing_count, "\n")

###

# Step 1: Filter females aged 20-54 in RHQ merged dataset who answered RHQ360
females_20_54_answered_RHQ360 <- females_20_54_with_RHQ360 %>%
  filter(!is.na(RHQ360))

# Step 2: Split into age groups (20-39 and >39 to 54)
females_20_39 <- females_20_54_answered_RHQ360 %>%
  filter(RIDAGEYR >= 20, RIDAGEYR <= 39)

females_40_54 <- females_20_54_answered_RHQ360 %>%
  filter(RIDAGEYR > 39, RIDAGEYR <= 54)

# Step 3: Count the number of females in each age group
cat("Females aged 20-39 who answered RHQ360:", nrow(females_20_39), "\n")
cat("Females aged 40-54 who answered RHQ360:", nrow(females_40_54), "\n")

#######

# Step 1: Filter females 20-39 in RHQ merged dataset who answered RHQ360
females_20_39_answered_RHQ360 <- females_20_54_with_RHQ360 %>%
  filter(RIDAGEYR >= 20, RIDAGEYR <= 39, !is.na(RHQ360))

# Step 2: Check how many are in the depression dataset
females_20_39_in_depression <- females_20_39_answered_RHQ360 %>%
  filter(SEQN %in% Depression_data_combined$SEQN)

# Step 3: Calculate how many are missing from the depression dataset
females_20_39_missing_depression <- nrow(females_20_39_answered_RHQ360) - nrow(females_20_39_in_depression)

# Step 4: Output the results
cat("Females aged 20-39 in RHQ merged dataset who answered RHQ360 and are in the Depression dataset:", nrow(females_20_39_in_depression), "\n")
cat("Females aged 20-39 in RHQ merged dataset who answered RHQ360 and are missing from the Depression dataset:", females_20_39_missing_depression, "\n")


##########

# Step 1: Filter females 20-39 in the RHQ merged dataset who answered RHQ360
females_20_39_answered_RHQ360 <- females_20_54_with_RHQ360 %>%
  filter(RIDAGEYR >= 20, RIDAGEYR <= 39, !is.na(RHQ360))

# Step 2: Check how many are in the GAD dataset
females_20_39_in_GAD <- females_20_39_answered_RHQ360 %>%
  filter(SEQN %in% GAD_data_combined$SEQN)

# Step 3: Calculate how many are missing from the GAD dataset
females_20_39_missing_GAD <- nrow(females_20_39_answered_RHQ360) - nrow(females_20_39_in_GAD)

# Step 4: Output the results
cat("Females aged 20-39 in RHQ merged dataset who answered RHQ360 and are in the GAD dataset:", nrow(females_20_39_in_GAD), "\n")
cat("Females aged 20-39 in RHQ merged dataset who answered RHQ360 and are missing from the GAD dataset:", females_20_39_missing_GAD, "\n")

######

# Step 1: Filter females 20-39 in the RHQ merged dataset who answered RHQ360
females_20_39_answered_RHQ360 <- females_20_54_with_RHQ360 %>%
  filter(RIDAGEYR >= 20, RIDAGEYR <= 39, !is.na(RHQ360))

# Step 2: Check how many are in both the GAD and Depression datasets
females_20_39_in_both <- females_20_39_answered_RHQ360 %>%
  filter(SEQN %in% GAD_data_combined$SEQN & SEQN %in% Depression_data_combined$SEQN)

# Step 3: Calculate how many are missing from both datasets
females_20_39_missing_both <- females_20_39_answered_RHQ360 %>%
  filter(!(SEQN %in% GAD_data_combined$SEQN) & !(SEQN %in% Depression_data_combined$SEQN))

# Step 4: Output the results
cat("Females aged 20-39 in RHQ merged dataset who answered RHQ360 and are in BOTH GAD and Depression datasets:", nrow(females_20_39_in_both), "\n")
cat("Females aged 20-39 in RHQ merged dataset who answered RHQ360 and are MISSING from BOTH GAD and Depression datasets:", nrow(females_20_39_missing_both), "\n")

#####

# Step 1: Merge the GAD and Depression data with the females_20_39_in_both_GAD_and_Depression dataset
females_20_39_in_both_GAD_and_Depression_merged <- females_20_39_in_both_GAD_and_Depression %>%
  left_join(GAD_data_combined %>% select(SEQN, CIDGSCOR), by = "SEQN") %>%
  left_join(Depression_data_combined %>% select(SEQN, CIDDSCOR), by = "SEQN")

# Step 2: Filter for those who answered both CIDGSCOR and CIDDSCOR
females_answered_both <- females_20_39_in_both_GAD_and_Depression_merged %>%
  filter(!is.na(CIDGSCOR) & !is.na(CIDDSCOR))

# Step 3: Filter for those who are missing both CIDGSCOR and CIDDSCOR
females_missing_both <- females_20_39_in_both_GAD_and_Depression_merged %>%
  filter(is.na(CIDGSCOR) & is.na(CIDDSCOR))

# Step 4: Output the results
cat("Females aged 20-39 in RHQ merged dataset who answered RHQ360 and are in BOTH GAD and Depression datasets AND answered both CIDGSCOR and CIDDSCOR:", nrow(females_answered_both), "\n")
cat("Females aged 20-39 in RHQ merged dataset who answered RHQ360 and are in BOTH GAD and Depression datasets AND are MISSING both CIDGSCOR and CIDDSCOR:", nrow(females_missing_both), "\n")

####
# Step 1: Filter for females 20-39 who answered RHQ360, are in both GAD and Depression datasets,
# and have answered both CIDGSCOR and CIDDSCOR
females_answered_both <- females_20_39_in_both_GAD_and_Depression_merged %>%
  filter(!is.na(CIDGSCOR) & !is.na(CIDDSCOR))

# Step 2: Identify positive and negative diagnoses for both CIDGSCOR and CIDDSCOR
# Positive diagnosis is coded as 1, and negative diagnosis is coded as 5 for both variables

# Count of positive diagnoses for both CIDGSCOR and CIDDSCOR (both CIDGSCOR == 1 and CIDDSCOR == 1)
positive_both <- females_answered_both %>%
  filter(CIDGSCOR == 1 & CIDDSCOR == 1)

# Count of negative diagnoses for both CIDGSCOR and CIDDSCOR (both CIDGSCOR == 5 and CIDDSCOR == 5)
negative_both <- females_answered_both %>%
  filter(CIDGSCOR == 5 & CIDDSCOR == 5)

# Output the results
cat("Females aged 20-39 who answered RHQ360, are present in both GAD and Depression datasets, have answered both CIDGSCOR and CIDDSCOR, and have a positive diagnosis for both:", nrow(positive_both), "\n")
cat("Females aged 20-39 who answered RHQ360, are present in both GAD and Depression datasets, have answered both CIDGSCOR and CIDDSCOR, and have a negative diagnosis for both:", nrow(negative_both), "\n")

#########

# Step 1: Filter for females 20-39 who answered RHQ360, are in both GAD and Depression datasets,
# and have answered both CIDGSCOR and CIDDSCOR
females_answered_both <- females_20_39_in_both_GAD_and_Depression_merged %>%
  filter(!is.na(CIDGSCOR) & !is.na(CIDDSCOR))

# Step 2: Identify females who have one positive diagnosis and one negative diagnosis
# Positive for CIDGSCOR and negative for CIDDSCOR
positive_CIDGSCOR_negative_CIDDSCOR <- females_answered_both %>%
  filter(CIDGSCOR == 1 & CIDDSCOR == 5)

# Negative for CIDGSCOR and positive for CIDDSCOR
negative_CIDGSCOR_positive_CIDDSCOR <- females_answered_both %>%
  filter(CIDGSCOR == 5 & CIDDSCOR == 1)

# Step 3: Count the number of females in each group
cat("Females aged 20-39 who answered RHQ360, are present in both GAD and Depression datasets, have answered both CIDGSCOR and CIDDSCOR, and have a positive diagnosis for CIDGSCOR and a negative diagnosis for CIDDSCOR:", nrow(positive_CIDGSCOR_negative_CIDDSCOR), "\n")
cat("Females aged 20-39 who answered RHQ360, are present in both GAD and Depression datasets, have answered both CIDGSCOR and CIDDSCOR, and have a negative diagnosis for CIDGSCOR and a positive diagnosis for CIDDSCOR:", nrow(negative_CIDGSCOR_positive_CIDDSCOR), "\n")

#####

# Step 1: Filter for females 20-39 who answered RHQ360 and are in both GAD and Depression datasets
females_answered_both <- females_20_39_in_both_GAD_and_Depression_merged %>%
  filter(!is.na(RHQ360))

# Step 2: Identify those who answered both CIDGSCOR and CIDDSCOR (both valid)
answered_both <- females_answered_both %>%
  filter(!is.na(CIDGSCOR) & !is.na(CIDDSCOR))

# Step 3: Identify those who are missing both CIDGSCOR and CIDDSCOR
missing_both <- females_answered_both %>%
  filter(is.na(CIDGSCOR) & is.na(CIDDSCOR))

# Step 4: Identify those who answered only one of the two
answered_one_only <- females_answered_both %>%
  filter((!is.na(CIDGSCOR) & is.na(CIDDSCOR)) | (is.na(CIDGSCOR) & !is.na(CIDDSCOR)))

# Step 5: Count the number of females in each group
cat("Females aged 20-39 who answered RHQ360 and are present in both GAD and Depression datasets who answered both CIDGSCOR and CIDDSCOR:", nrow(answered_both), "\n")
cat("Females aged 20-39 who answered RHQ360 and are present in both GAD and Depression datasets who are missing both CIDGSCOR and CIDDSCOR:", nrow(missing_both), "\n")
cat("Females aged 20-39 who answered RHQ360 and are present in both GAD and Depression datasets who answered only one of CIDGSCOR or CIDDSCOR:", nrow(answered_one_only), "\n")

####

# Step 1: Filter for females 20-39 who answered RHQ360 and are in both GAD and Depression datasets
females_answered_both <- females_20_39_in_both_GAD_and_Depression_merged %>%
  filter(!is.na(RHQ360))

# Step 2: Identify those who answered only CIDGSCOR (and not CIDDSCOR)
answered_only_CIDGSCOR <- females_answered_both %>%
  filter(!is.na(CIDGSCOR) & is.na(CIDDSCOR))

# Step 3: Identify those who answered only CIDDSCOR (and not CIDGSCOR)
answered_only_CIDDSCOR <- females_answered_both %>%
  filter(is.na(CIDGSCOR) & !is.na(CIDDSCOR))

# Step 4: Count the number of females in each group
cat("Females 20-39 who answered RHQ360 and are in both GAD and Depression datasets who answered only CIDGSCOR:", nrow(answered_only_CIDGSCOR), "\n")
cat("Females 20-39 who answered RHQ360 and are in both GAD and Depression datasets who answered only CIDDSCOR:", nrow(answered_only_CIDDSCOR), "\n")

####


# Step 1: Filter for females 20-39 who answered RHQ360 and are in the merged Depression dataset
females_answered_RHQ_and_in_Depression <- females_20_39_in_RHQ %>%
  filter(SEQN %in% Depression_data_combined$SEQN)

# Step 2: Count those who answered CIDDSCOR
answered_CIDDSCOR <- females_answered_RHQ_and_in_Depression %>%
  filter(!is.na(CIDDSCOR))

# Step 3: Count those who are missing CIDDSCOR
missing_CIDDSCOR <- females_answered_RHQ_and_in_Depression %>%
  filter(is.na(CIDDSCOR))

# Step 4: Display the counts
cat("Females 20-39 who answered RHQ360 and are in the merged Depression dataset who answered CIDDSCOR:", nrow(answered_CIDDSCOR), "\n")
cat("Females 20-39 who answered RHQ360 and are in the merged Depression dataset who are missing CIDDSCOR:", nrow(missing_CIDDSCOR), "\n")



