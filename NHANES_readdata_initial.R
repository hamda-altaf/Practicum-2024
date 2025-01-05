library(tidyverse)
library(nhanesA)
library(dplyr)

# Function to load, merge, and filter NHANES data
merge_nhanes_data <- function(CIQGAD_code, RHQ_code, CIDDSCOR_code) {
  # Load datasets
  CIQGAD <- nhanes(CIQGAD_code)[, c("SEQN", "CIDGSCOR")]
  RHQ <- nhanes(RHQ_code)[, c("SEQN", "RHQ360")]
  CIDDSCOR <- nhanes(CIDDSCOR_code)[, c("SEQN", "CIDDSCOR")]
  
  # Align datasets on "SEQN" using full_join
  merged_data <- CIQGAD %>%
    full_join(RHQ, by = "SEQN") %>%
    full_join(CIDDSCOR, by = "SEQN")
  
  # Remove rows with missing values
  complete_data <- merged_data %>%
    filter(!is.na(CIDGSCOR) & !is.na(RHQ360) & !is.na(CIDDSCOR))
  
  return(complete_data)
}

# Load and clean data for each cycle
all_vars1999 <- merge_nhanes_data("CIQGAD", "RHQ", "CIQMDEP") %>%
  mutate(Cycle = "1999-2000")  # Add cycle identifier

all_vars2001 <- merge_nhanes_data("CIQGAD_B", "RHQ_B", "CIQDEP_B") %>%
  mutate(Cycle = "2001-2002")

all_vars2003 <- merge_nhanes_data("CIQGAD_C", "RHQ_C", "CIQDEP_C") %>%
  mutate(Cycle = "2003-2004")

# Combine all cycles into a single dataset
all_cycles <- bind_rows(all_vars1999, all_vars2001, all_vars2003)

# Preview the combined dataset
head(all_cycles)


###########################DATA CLEANING############################################
CIQGAD_vars<- c("CIQGAD", "CIQGAD_B", "DPQ030", "DPQ040", "DPQ050", 
              "DPQ060", "DPQ070", "DPQ080", "DPQ090")
# Filter out rows where any of the DPQ variables are 7 or 9 using package 'dplyr' (refused/don't know) 
df_fin <- dpq_J %>%
  dplyr::mutate(across(all_of(dpq_vars), ~na_if(., 7))) %>%
  dplyr::mutate(across(all_of(dpq_vars), ~na_if(., 9)))

###### variable transformation

df_fin <- df_fin %>% mutate(DEPR_TOT = DPQ010 + DPQ020 + DPQ030 + DPQ040 + DPQ050 + DPQ060 + DPQ070 + DPQ080 + DPQ090)
