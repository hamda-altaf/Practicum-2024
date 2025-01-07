# Step 1: Filter females aged 20-39 years from the demo data
females_20_39 <- demo_data_combined %>% 
  filter(RIAGENDR == 2, RIDAGEYR >= 20, RIDAGEYR <= 39)

# Step 2: Find females aged 20-39 years who answered RHQ360 (Yes or No)
females_20_39_rhq <- RHQ_data_combined %>% 
  filter(SEQN %in% females_20_39$SEQN, !is.na(RHQ360))

cat("Females aged 20-39 years who answered RHQ360:", nrow(females_20_39_rhq), "\n")

# Step 3: Find how many of these females are present in the merged depression dataset
females_20_39_in_depression <- females_20_39_rhq %>% 
  filter(SEQN %in% Depression_data_combined$SEQN)

cat("Females aged 20-39 years who answered RHQ360 and are in the depression dataset:", nrow(females_20_39_in_depression), "\n")

# Step 4: Check how many of these answered CIDDSCOR and how many did not
females_20_39_ciddscor_answered <- Depression_data_combined %>% 
  filter(SEQN %in% females_20_39_in_depression$SEQN, !is.na(CIDDSCOR))

females_20_39_ciddscor_missing <- Depression_data_combined %>% 
  filter(SEQN %in% females_20_39_in_depression$SEQN, is.na(CIDDSCOR))

cat("Females aged 20-39 years who answered CIDDSCOR:", nrow(females_20_39_ciddscor_answered), "\n")
cat("Females aged 20-39 years who did not answer CIDDSCOR:", nrow(females_20_39_ciddscor_missing), "\n")

######

# Step 1: Filter females aged 20-39 years from the demo data
females_20_39 <- demo_data_combined %>% 
  filter(RIAGENDR == 2, RIDAGEYR >= 20, RIDAGEYR <= 39)

# Step 2: Find females aged 20-39 years who answered RHQ360 (Yes or No)
females_20_39_rhq <- RHQ_data_combined %>% 
  filter(SEQN %in% females_20_39$SEQN, !is.na(RHQ360))

cat("Females aged 20-39 years who answered RHQ360:", nrow(females_20_39_rhq), "\n")

# Step 3: Find how many of these females are present in the merged GAD dataset
females_20_39_in_gad <- females_20_39_rhq %>% 
  filter(SEQN %in% GAD_data_combined$SEQN)

cat("Females aged 20-39 years who answered RHQ360 and are in the GAD dataset:", nrow(females_20_39_in_gad), "\n")

# Step 4: Check how many of these answered CIDGSCOR and how many did not
females_20_39_cidgscor_answered <- GAD_data_combined %>% 
  filter(SEQN %in% females_20_39_in_gad$SEQN, !is.na(CIDGSCOR))

females_20_39_cidgscor_missing <- GAD_data_combined %>% 
  filter(SEQN %in% females_20_39_in_gad$SEQN, is.na(CIDGSCOR))

cat("Females aged 20-39 years who answered CIDGSCOR:", nrow(females_20_39_cidgscor_answered), "\n")
cat("Females aged 20-39 years who did not answer CIDGSCOR:", nrow(females_20_39_cidgscor_missing), "\n")

######

# Step 1: Filter females aged 20-39 from the demo data
females_20_39 <- demo_data_combined %>%
  filter(RIAGENDR == 2, RIDAGEYR >= 20, RIDAGEYR <= 39)

# Step 2: Find females aged 20-39 who answered RHQ360
females_20_39_rhq <- RHQ_data_combined %>%
  filter(SEQN %in% females_20_39$SEQN, !is.na(RHQ360))

cat("Females aged 20-39 who answered RHQ360:", nrow(females_20_39_rhq), "\n")

# Step 3: Find how many of these females are present in the merged GAD dataset
females_20_39_in_gad <- females_20_39_rhq %>%
  filter(SEQN %in% GAD_data_combined$SEQN)

cat("Females aged 20-39 who answered RHQ360 and are in the GAD dataset:", nrow(females_20_39_in_gad), "\n")

# Step 4: Check how many of these answered CIDGSCOR and group them by diagnosis
females_20_39_in_gad_with_cidgscor <- GAD_data_combined %>%
  filter(SEQN %in% females_20_39_in_gad$SEQN, !is.na(CIDGSCOR))

# Count the number of positive, negative, and missing diagnoses
positive_diagnosis <- females_20_39_in_gad_with_cidgscor %>%
  filter(CIDGSCOR == 1) %>%
  nrow()

negative_diagnosis <- females_20_39_in_gad_with_cidgscor %>%
  filter(CIDGSCOR == 5) %>%
  nrow()

missing_diagnosis <- females_20_39_in_gad %>%
  filter(!SEQN %in% females_20_39_in_gad_with_cidgscor$SEQN) %>%
  nrow()

cat("Females aged 20-39 with a positive diagnosis (CIDGSCOR = 1):", positive_diagnosis, "\n")
cat("Females aged 20-39 with a negative diagnosis (CIDGSCOR = 5):", negative_diagnosis, "\n")
cat("Females aged 20-39 with missing CIDGSCOR:", missing_diagnosis, "\n")


#########

# Step 1: Filter females aged 20-39 from the demo data
females_20_39 <- demo_data_combined %>%
  filter(RIAGENDR == 2, RIDAGEYR >= 20, RIDAGEYR <= 39)

# Step 2: Find females aged 20-39 who answered RHQ360
females_20_39_rhq <- RHQ_data_combined %>%
  filter(SEQN %in% females_20_39$SEQN, !is.na(RHQ360))

cat("Females aged 20-39 who answered RHQ360:", nrow(females_20_39_rhq), "\n")

# Step 3: Find how many of these females are present in the merged Depression dataset
females_20_39_in_depression <- females_20_39_rhq %>%
  filter(SEQN %in% Depression_data_combined$SEQN)

cat("Females aged 20-39 who answered RHQ360 and are in the Depression dataset:", nrow(females_20_39_in_depression), "\n")

# Step 4: Check how many of these answered CIDDSCOR and group them by diagnosis
females_20_39_in_depression_with_ciddscor <- Depression_data_combined %>%
  filter(SEQN %in% females_20_39_in_depression$SEQN, !is.na(CIDDSCOR))

# Count the number of positive, negative, and missing diagnoses
positive_diagnosis <- females_20_39_in_depression_with_ciddscor %>%
  filter(CIDDSCOR == 1) %>%
  nrow()

negative_diagnosis <- females_20_39_in_depression_with_ciddscor %>%
  filter(CIDDSCOR == 5) %>%
  nrow()

missing_diagnosis <- females_20_39_in_depression %>%
  filter(!SEQN %in% females_20_39_in_depression_with_ciddscor$SEQN) %>%
  nrow()

cat("Females aged 20-39 with a positive diagnosis (CIDDSCOR = 1):", positive_diagnosis, "\n")
cat("Females aged 20-39 with a negative diagnosis (CIDDSCOR = 5):", negative_diagnosis, "\n")
cat("Females aged 20-39 with missing CIDDSCOR:", missing_diagnosis, "\n")

#######
# Count the number of people with a positive diagnosis for CIDDSCOR
positive_diagnosis_count <- final_data %>%
  filter(CIDDSCOR == 1) %>%  # Filter rows where CIDDSCOR equals 1
  nrow()  # Count the number of rows

# Print the result
cat("Number of people with a positive diagnosis for CIDDSCOR:", positive_diagnosis_count, "\n")

# Count the number of people with a negative diagnosis for CIDDSCOR
negative_diagnosis_count <- final_data %>%
  filter(CIDDSCOR == 5) %>%  # Filter rows where CIDDSCOR equals 5
  nrow()  # Count the number of rows

# Print the result
cat("Number of people with a negative diagnosis for CIDDSCOR:", negative_diagnosis_count, "\n")

#####

# Count the number of people with a negative diagnosis for CIDGSCOR
negative_diagnosis_count_GAD <- final_data %>%
  filter(CIDGSCOR == 5) %>%  # Filter rows where CIDGSCOR equals 5
  nrow()  # Count the number of rows

# Print the result
cat("Number of people with a negative diagnosis for CIDGSCOR:", negative_diagnosis_count_GAD, "\n")

####
# Count the number of people with a positive diagnosis for CIDGSCOR
positive_diagnosis_count_GAD <- final_data %>%
  filter(CIDGSCOR == 1) %>%  # Filter rows where CIDGSCOR equals 1
  nrow()  # Count the number of rows

# Print the result
cat("Number of people with a positive diagnosis for CIDGSCOR:", positive_diagnosis_count_GAD, "\n")

#######

# Count the number of people (rows) in the final_data
total_people <- nrow(final_data)

# Print the result
cat("Total number of people in the final data:", total_people, "\n")

####

# Filter data for females aged 20-39 from all cycles
findata <- final_data %>%
  filter(RIAGENDR == 2,  # Female (assuming 2 represents females)
         RIDAGEYR >= 20 & RIDAGEYR <= 39) %>%  # Age between 20 and 39
  
  # Keep only those who answered RHQ360, CIDDSCOR, and CIDGSCOR in all three cycles
  filter(!is.na(RHQ360),   # answered RHQ360 (Yes or No)
         !is.na(CIDDSCOR), # answered CIDDSCOR
         !is.na(CIDGSCOR)) %>% # answered CIDGSCOR
  
  # Make sure they do not have missing values for CIDDSCOR and CIDGSCOR in all cycles
  filter(complete.cases(CIDDSCOR, CIDGSCOR)) 

# Count the number of people in the filtered dataset
num_people_findata <- nrow(findata)

# Print the result
cat("Number of people in findata:", num_people_findata, "\n")

