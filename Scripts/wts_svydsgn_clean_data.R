# Load required libraries
library(tidyverse)
library(haven)
library(survey)

# Define the directory containing the .xpt files
data_dir <- "/Users/hamdaaltaf/Desktop/Version Control/Practicum_2024/Datasets"

# Define cycle years and prefixes
cycles <- c("1999-2000" = "DEMO",
            "2001-2002" = "DEMO_B",
            "2003-2004" = "DEMO_C")

# Define dataset prefixes
datasets <- list(
  RHQ = c("RHQ", "RHQ_B", "RHQ_C"),
  GAD = c("CIQGAD", "CIQGAD_B", "CIQGAD_C"),
  Depression = c("CIQMDEP", "CIQDEP_B", "CIQDEP_C")
)

# Define variables of interest
selected_vars <- c("SEQN", "SDDSRVYR", "RIAGENDR", "RIDAGEYR", "RIDRETH1", 
                   "DMDMARTL", "WTMEC2YR", "WTMEC4YR", "SDMVPSU", 
                   "SDMVSTRA", "RHQ360", "CIDDSCOR", "CIDGSCOR")

vars_interest <- list(
  RHQ = "RHQ360",
  GAD = "CIDGSCOR",
  Depression = "CIDDSCOR"
)

# Initialize list to store combined data for each cycle
combined_cycles <- list()

# Loop through cycles
for (i in seq_along(cycles)) {
  cycle_name <- names(cycles)[i]
  
  # Load demographic file
  demo_file <- file.path(data_dir, paste0(cycles[i], ".xpt"))
  demo_data <- read_xpt(demo_file)
  
  # Select only the columns that exist in the data
  demo_data <- demo_data %>%
    select(SEQN, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH1, DMDMARTL, 
           SDMVPSU, SDMVSTRA, everything()) %>%
    select(intersect(names(demo_data), selected_vars))  # Keep only selected variables that exist
  
  # Check if WTMEC4YR exists, and if so, include it
  if ("WTMEC4YR" %in% names(demo_data)) {
    demo_data <- demo_data %>% select(SEQN, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH1, DMDMARTL, 
                                      WTMEC2YR, WTMEC4YR, SDMVPSU, SDMVSTRA)
  } else {
    demo_data <- demo_data %>% select(SEQN, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDRETH1, DMDMARTL, 
                                      WTMEC2YR, SDMVPSU, SDMVSTRA)
  }
  
  # Initialize list to store datasets for this cycle
  data_list <- list(demo_data)
  
  # Loop through datasets (RHQ, GAD, Depression)
  for (dataset in names(datasets)) {
    data_file <- file.path(data_dir, paste0(datasets[[dataset]][i], ".xpt"))
    var_name <- vars_interest[[dataset]]
    
    # Load and filter dataset
    data <- read_xpt(data_file) %>% select(SEQN, all_of(var_name))
    colnames(data)[2] <- var_name  # Standardize column name
    
    # Append to data list
    data_list[[dataset]] <- data
  }
  
  # Merge all datasets for this cycle
  merged_cycle <- reduce(data_list, full_join, by = "SEQN") %>%
    mutate(Cycle = cycle_name)  # Add cycle identifier
  
  # Append to combined_cycles
  combined_cycles[[cycle_name]] <- merged_cycle
}


# Combine all cycles into a single dataset
raw_data <- bind_rows(combined_cycles)

# Calculate and print sample size for raw data
cat("Sample size (raw data):", nrow(raw_data), "\n")

# Calculate combined weights (MEC6YR)
raw_data <- raw_data %>%
  mutate(MEC6YR = case_when(
    SDDSRVYR %in% c(1, 2) ~ (2 / 3) * WTMEC4YR,  # For 1999-2002
    SDDSRVYR == 3 ~ (1 / 3) * WTMEC2YR,         # For 2003-2004
    TRUE ~ NA_real_                           # For other cycles or invalid cases
  ))

# Save the raw dataset with combined weights
write_csv(raw_data, file.path(data_dir, "NHANES_1999_2004_raw_with_weights.csv"))

# Filter to include only females (RIAGENDR == 2)
female_data <- raw_data %>% filter(RIAGENDR == 2)

# Save the dataset with females only
write_csv(female_data, file.path(data_dir, "NHANES_1999_2004_females.csv"))

# Calculate and print sample size for female data
cat("Sample size (females only):", nrow(female_data), "\n")

# Remove missing, refused, and "don't know" for main variables
cleaned_data <- female_data %>%
  filter(
    !is.na(CIDDSCOR) & CIDDSCOR %in% c(1, 5),        # Keep valid Depression scores
    !is.na(CIDGSCOR) & CIDGSCOR %in% c(1, 5),        # Keep valid GAD scores
    !is.na(RHQ360) & RHQ360 %in% c(1, 2)             # Keep valid endometriosis responses
  )

# Save the final cleaned dataset
write_csv(cleaned_data, file.path(data_dir, "NHANES_1999_2004_cleaned.csv"))

# Calculate and print sample size for cleaned data
cat("Sample size (cleaned data):", nrow(cleaned_data), "\n")

# Create a survey design object
survey_design <- svydesign(
  id = ~SDMVPSU,                 # Primary Sampling Unit
  strata = ~SDMVSTRA,            # Stratification
  weights = ~MEC6YR,             # Combined weights
  data = cleaned_data,           # The cleaned data
  nest = TRUE                    # Specify nested design (if applicable)
)

# Check the survey design object
survey_design
