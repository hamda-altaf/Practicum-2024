#Dec 2; tried to add dietry fibre (DRXTFIBE) for years 1999-2002 (unavailable after that) 
#but failed to merge without it in the 2003-2004 cycle


# Function to load, merge, and filter NHANES data
merge_nhanes_data <- function(CIQGAD_code, RHQ_code, CIQMDEP_code, DRXTOT_code) {
  # Load datasets
  CIQGAD <- nhanes(CIQGAD_code)[, c("SEQN", "CIDGSCOR")]
  RHQ <- nhanes(RHQ_code)[, c("SEQN", "RHQ360")]
  CIQMDEP <- nhanes(CIQMDEP_code)[, c("SEQN", "CIDDSCOR")]
  DRXTOT <- nhanes(DRXTOT_code)[, c("SEQN", "DRXTFIBE")]
  
  # Align datasets on "SEQN" using full_join
  merged_data <- CIQGAD %>%
    full_join(RHQ, by = "SEQN") %>%
    full_join(CIQMDEP, by = "SEQN") %>%
    full_join(DRXTOT, by = "SEQN")
  
  # Remove rows with missing values
  complete_data <- merged_data %>%
    filter(!is.na(CIDGSCOR) & !is.na(RHQ360) & !is.na(CIDDSCOR) & !is.na(DRXTFIBE))
  
  return(complete_data)
}

# Load and clean data for each cycle
all_vars1999 <- merge_nhanes_data("CIQGAD", "RHQ", "CIQMDEP", "DRXTOT") %>%
  mutate(Cycle = "1999-2000")  # Add cycle identifier

all_vars2001 <- merge_nhanes_data("CIQGAD_B", "RHQ_B", "CIQDEP_B", "DRXTOT_B") %>%
  mutate(Cycle = "2001-2002")

all_vars2003 <- merge_nhanes_data("CIQGAD_C", "RHQ_C", "CIQDEP_C") %>%
  mutate(Cycle = "2003-2004") 
