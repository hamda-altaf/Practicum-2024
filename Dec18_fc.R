#gave correct coding to GPT for the three variables

# Load necessary libraries
library(tidyverse)
library(nhanesA)

# Function to load, merge, and filter NHANES data
merge_nhanes_data <- function(CIQGAD_code, RHQ_code, CIQMDEP_code) {
  # Load datasets using nhanesA package
  CIQGAD <- nhanes(CIQGAD_code) %>%
    select(SEQN, CIDGSCOR)  # GAD score
  RHQ <- nhanes(RHQ_code) %>%
    select(SEQN, RHQ360)  # Endometriosis
  CIQMDEP <- nhanes(CIQMDEP_code) %>%
    select(SEQN, CIDDSCOR)  # Depression score
  
  # Align datasets on "SEQN" using full_join
  merged_data <- CIQGAD %>%
    full_join(RHQ, by = "SEQN") %>%
    full_join(CIQMDEP, by = "SEQN")
  
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


#####Code to create the flowchart#####
# Install DiagrammeR if not already installed
if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
  install.packages("DiagrammeR")
}

# Load DiagrammeR library
library(DiagrammeR)

# Create a flowchart
grViz("
digraph flowchart {
  graph [layout = dot, rankdir = TB]

  # Node definitions
  node [shape = ellipse, style = filled, color = lightblue]
  A [label = 'Start: NHANES Data (1999-2004)']

  node [shape = rectangle, style = filled, color = lightgrey]
  B [label = 'Step 1: Combine three cycles\n(1999-2000, 2001-2002, 2003-2004)']
  C [label = 'Step 2: Retain variables of interest\n(CIDGSCOR, RHQ360, CIDDSCOR)']
  D [label = 'Step 3: Remove rows with missing values']

  node [shape = ellipse, style = filled, color = lightgreen]
  E [label = 'Result: Final dataset for analysis']

  # Define edges
  A -> B
  B -> C
  C -> D
  D -> E
}
")

