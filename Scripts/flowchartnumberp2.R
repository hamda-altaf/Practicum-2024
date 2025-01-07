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

###

# Count the number of female participants aged 20-54 who answered RHQ360
female_20_54_rhq360_count <- final_data %>%
  filter(RIAGENDR == 2,   # Female participants (assuming 2 represents females)
         RIDAGEYR >= 20 & RIDAGEYR <= 39,  # Age between 20 and 54
         !is.na(RHQ360)) %>%  # Ensure RHQ360 is answered (not missing)
  nrow()  # Count the number of rows

# Print the result
cat("Number of female participants aged 20-54 who answered RHQ360:", female_20_54_rhq360_count, "\n")

###
# Merge demo_data with RHQ data for the current cycle
merged_data_rhq <- merge(demo_data, RHQ_data, by = "SEQN", all = TRUE)  # Assuming RHQ_data contains RHQ360

# Filter for females aged 20-54 who answered RHQ360
female_20_54_rhq360_count <- merged_data_rhq %>%
  filter(RIAGENDR == 2,   # Female participants (assuming 2 represents females)
         RIDAGEYR >= 20 & RIDAGEYR <= 54,  # Age between 20 and 54
         !is.na(RHQ360)) %>%  # Ensure RHQ360 is answered (not missing)
  nrow()  # Count the number of rows

# Print the result
cat("Number of female participants aged 20-54 who answered RHQ360:", female_20_54_rhq360_count, "\n")

#####

# Load necessary libraries
library(tidyverse)
library(haven)

# Define the directory containing the .xpt files
data_dir <- "/Users/hamdaaltaf/Desktop/Version Control/Practicum_2024/Datasets"

# Define cycle years and prefixes
cycles <- c("1999-2000" = "DEMO",
            "2001-2002" = "DEMO_B",
            "2003-2004" = "DEMO_C")

# Define dataset prefixes
datasets <- list(
  RHQ = c("RHQ", "RHQ_B", "RHQ_C")
)

# Initialize lists to store combined data for each cycle
demo_data_list <- list()
RHQ_data_list <- list()

for (i in seq_along(cycles)) {
  cycle_name <- names(cycles)[i]
  
  # Load demographic file and include age and gender variables
  demo_file <- file.path(data_dir, paste0(cycles[i], ".xpt"))
  demo_data <- read_xpt(demo_file) %>% select(SEQN, RIAGENDR, RIDAGEYR)
  demo_data_list[[cycle_name]] <- demo_data
  
  # Load RHQ data for the current cycle
  RHQ_data <- read_xpt(file.path(data_dir, paste0(datasets$RHQ[i], ".xpt"))) %>% select(SEQN, RHQ360)
  RHQ_data_list[[cycle_name]] <- RHQ_data
}

# Combine the data across cycles
demo_data_combined <- bind_rows(demo_data_list)
RHQ_data_combined <- bind_rows(RHQ_data_list)

# Step 1: Filter females ages 20-54 years from the combined demo data
females_20_54 <- demo_data_combined %>% 
  filter(RIAGENDR == 2, RIDAGEYR >= 20, RIDAGEYR <= 54)

# Step 2: Find how many females from demo data are present in the RHQ dataset
females_in_RHQ <- females_20_54 %>%
  filter(SEQN %in% RHQ_data_combined$SEQN)

# Output the result
cat("Number of female participants ages 20-54 in the RHQ dataset:", nrow(females_in_RHQ), "\n")

#######

# Load necessary libraries
library(tidyverse)
library(haven)

# Define the directory containing the .xpt files
data_dir <- "/Users/hamdaaltaf/Desktop/Version Control/Practicum_2024/Datasets"

# Define cycle years and prefixes
cycles <- c("1999-2000" = "DEMO",
            "2001-2002" = "DEMO_B",
            "2003-2004" = "DEMO_C")

# Define dataset prefixes
datasets <- list(
  RHQ = c("RHQ", "RHQ_B", "RHQ_C")
)

# Initialize lists to store combined data for each cycle
demo_data_list <- list()
RHQ_data_list <- list()

for (i in seq_along(cycles)) {
  cycle_name <- names(cycles)[i]
  
  # Load demographic file and include age and gender variables
  demo_file <- file.path(data_dir, paste0(cycles[i], ".xpt"))
  demo_data <- read_xpt(demo_file) %>% select(SEQN, RIAGENDR, RIDAGEYR)
  demo_data_list[[cycle_name]] <- demo_data
  
  # Load RHQ data for the current cycle
  RHQ_data <- read_xpt(file.path(data_dir, paste0(datasets$RHQ[i], ".xpt"))) %>% select(SEQN, RHQ360)
  RHQ_data_list[[cycle_name]] <- RHQ_data
}

# Combine the data across cycles
demo_data_combined <- bind_rows(demo_data_list)
RHQ_data_combined <- bind_rows(RHQ_data_list)

# Step 1: Filter females ages 20-54 years from the combined demo data
females_20_54 <- demo_data_combined %>% 
  filter(RIAGENDR == 2, RIDAGEYR >= 20, RIDAGEYR <= 54)

# Step 2: Merge the filtered females data with RHQ data to find the ones who answered RHQ360
females_in_RHQ <- females_20_54 %>%
  filter(SEQN %in% RHQ_data_combined$SEQN) %>%
  left_join(RHQ_data_combined, by = "SEQN")

# Step 3: Count how many answered Yes, No, Refused, Don't know, and how many are missing
response_counts <- females_in_RHQ %>%
  filter(!is.na(RHQ360)) %>%  # Exclude missing responses
  mutate(
    Response = case_when(
      RHQ360 == 1 ~ "Yes",
      RHQ360 == 2 ~ "No",
      RHQ360 == 7 ~ "Refused",
      RHQ360 == 9 ~ "Don't know",
      TRUE ~ "Other"  # Catch all for unexpected values
    )
  ) %>%
  count(Response)  # This will count the responses for each category

# Step 4: Add a "Missing" category for missing answers
missing_count <- females_in_RHQ %>%
  filter(is.na(RHQ360)) %>%
  summarise(Missing = n()) %>%
  pull(Missing)

# Add the missing count to the result
response_counts <- bind_rows(response_counts, tibble(Response = "Missing", n = missing_count))

# Output the result
print(response_counts)

###

# Load necessary libraries
library(tidyverse)
library(haven)

# Define the directory containing the .xpt files
data_dir <- "/Users/hamdaaltaf/Desktop/Version Control/Practicum_2024/Datasets"

# Define cycle years and prefixes
cycles <- c("1999-2000" = "DEMO",
            "2001-2002" = "DEMO_B",
            "2003-2004" = "DEMO_C")

# Define dataset prefixes
datasets <- list(
  RHQ = c("RHQ", "RHQ_B", "RHQ_C")
)

# Initialize lists to store combined data for each cycle
demo_data_list <- list()
RHQ_data_list <- list()

for (i in seq_along(cycles)) {
  cycle_name <- names(cycles)[i]
  
  # Load demographic file and include age and gender variables
  demo_file <- file.path(data_dir, paste0(cycles[i], ".xpt"))
  demo_data <- read_xpt(demo_file) %>% select(SEQN, RIAGENDR, RIDAGEYR)
  demo_data_list[[cycle_name]] <- demo_data
  
  # Load RHQ data for the current cycle
  RHQ_data <- read_xpt(file.path(data_dir, paste0(datasets$RHQ[i], ".xpt"))) %>% select(SEQN, RHQ360)
  RHQ_data_list[[cycle_name]] <- RHQ_data
}

# Combine the data across cycles
demo_data_combined <- bind_rows(demo_data_list)
RHQ_data_combined <- bind_rows(RHQ_data_list)

# Step 1: Filter females ages 20-54 years from the combined demo data
females_20_54 <- demo_data_combined %>% 
  filter(RIAGENDR == 2, RIDAGEYR >= 20, RIDAGEYR <= 54)

# Step 2: Merge the filtered females data with RHQ data to find the ones who answered RHQ360
females_in_RHQ <- females_20_54 %>%
  filter(SEQN %in% RHQ_data_combined$SEQN) %>%
  left_join(RHQ_data_combined, by = "SEQN")

# Step 3: Count how many answered the RHQ360 question (i.e., non-missing values)
answered_RHQ <- females_in_RHQ %>%
  filter(!is.na(RHQ360)) %>%
  summarise(Answered = n()) %>%
  pull(Answered)

# Output the result
cat("Number of females 20-54 years who answered RHQ360:", answered_RHQ, "\n")

#####

females_in_RHQ %>%
  distinct(SEQN) %>%
  summarise(Unique_Participants = n())
females_in_RHQ %>%
  filter(is.na(RHQ360)) %>%
  summarise(Missing = n()) %>%
  pull(Missing)
females_in_RHQ %>%
  summarise(Total_Participants = n())
unique(females_in_RHQ$RHQ360)
response_counts <- females_in_RHQ %>%
  filter(!is.na(RHQ360)) %>%  # Exclude missing responses
  mutate(
    Response = case_when(
      RHQ360 == 1 ~ "Yes",
      RHQ360 == 2 ~ "No",
      RHQ360 == 7 ~ "Refused",
      RHQ360 == 9 ~ "Don't know",
      TRUE ~ "Other"  # For unexpected or invalid values
    )
  ) %>%
  count(Response)
response_counts

######

# Filter for "Yes" and "No" responses, excluding "Don't know"
response_counts_yes_no <- females_in_RHQ %>%
  filter(RHQ360 %in% c(1, 2)) %>%  # Only include Yes (1) and No (2)
  mutate(
    Response = case_when(
      RHQ360 == 1 ~ "Yes",
      RHQ360 == 2 ~ "No",
      TRUE ~ "Other"
    )
  ) %>%
  count(Response)

# Output the result
print(response_counts_yes_no)

###

# Filter for females aged 20-39 years who answered Yes or No to RHQ360
response_counts_20_39 <- females_in_RHQ %>%
  filter(RIAGENDR == 2, RIDAGEYR >= 20, RIDAGEYR <= 39, RHQ360 %in% c(1, 2)) %>%
  mutate(
    Response = case_when(
      RHQ360 == 1 ~ "Yes",
      RHQ360 == 2 ~ "No",
      TRUE ~ "Other"
    )
  ) %>%
  count(Response)

# Output the result
print(response_counts_20_39)

