# Load required libraries
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
  RHQ = c("RHQ", "RHQ_B", "RHQ_C"),
  GAD = c("CIQGAD", "CIQGAD_B", "CIQGAD_C"),
  Depression = c("CIQMDEP", "CIQDEP_B", "CIQDEP_C")
)

# Define variables of interest
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
  
  # Load demographic file and include age variable
  demo_file <- file.path(data_dir, paste0(cycles[i], ".xpt"))
  demo_data <- read_xpt(demo_file) %>% select(SEQN, RIAGENDR, RIDAGEYR)  # Include gender and age columns
  
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
final_data <- bind_rows(combined_cycles)

# Filter out "Don't Know" responses (RHQ360 == 9)
final_data <- final_data %>%
  filter(RHQ360 != 9)

# Filter complete cases
final_data <- final_data %>% filter(complete.cases(.))

# Preview the final dataset
head(final_data)

# Save the final dataset (optional)
write_csv(final_data, file.path(data_dir, "NHANES_1999_2004_combined.csv"))

### WITHOUT DON'T KNOW REMOVED FROM RHQ360####
# Load required libraries
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
  RHQ = c("RHQ", "RHQ_B", "RHQ_C"),
  GAD = c("CIQGAD", "CIQGAD_B", "CIQGAD_C"),
  Depression = c("CIQMDEP", "CIQDEP_B", "CIQDEP_C")
)

# Define variables of interest
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
  
  # Load demographic file and include age variable
  demo_file <- file.path(data_dir, paste0(cycles[i], ".xpt"))
  demo_data <- read_xpt(demo_file) %>% select(SEQN, RIAGENDR, RIDAGEYR)  # Include gender and age columns
  
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
final_data <- bind_rows(combined_cycles)

# Filter complete cases
final_data <- final_data %>% filter(complete.cases(.))

# Preview the final dataset
head(final_data)

# Save the final dataset (optional)
write_csv(final_data, file.path(data_dir, "NHANES_1999_2004_combined.csv"))


###########Some checks######################

# Check gender distribution in demo_data
table(demo_data$RIAGENDR)

# Check gender distribution in the other datasets
table(RHQ_data$RIAGENDR)
table(GAD_data$RIAGENDR)
table(Depression_data$RIAGENDR)

# Check gender distribution in final_data
table(final_data$RIAGENDR)

min_age <- min(final_data$RIDAGEYR, na.rm = TRUE)
max_age <- max(final_data$RIDAGEYR, na.rm = TRUE)

cat("Minimum age:", min_age, "\n")
cat("Maximum age:", max_age, "\n")
