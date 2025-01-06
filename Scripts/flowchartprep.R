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

# Loop through cycles to load the demo and other datasets
demo_data_list <- list()
RHQ_data_list <- list()
Depression_data_list <- list()
GAD_data_list <- list()

for (i in seq_along(cycles)) {
  cycle_name <- names(cycles)[i]
  
  # Load demographic file and include age and gender variables
  demo_file <- file.path(data_dir, paste0(cycles[i], ".xpt"))
  demo_data <- read_xpt(demo_file) %>% select(SEQN, RIAGENDR, RIDAGEYR)
  demo_data_list[[cycle_name]] <- demo_data
  
  # Load RHQ, Depression, and GAD data for the current cycle
  RHQ_data <- read_xpt(file.path(data_dir, paste0(datasets$RHQ[i], ".xpt"))) %>% select(SEQN, RHQ360)
  Depression_data <- read_xpt(file.path(data_dir, paste0(datasets$Depression[i], ".xpt"))) %>% select(SEQN, CIDDSCOR)
  GAD_data <- read_xpt(file.path(data_dir, paste0(datasets$GAD[i], ".xpt"))) %>% select(SEQN, CIDGSCOR)
  
  RHQ_data_list[[cycle_name]] <- RHQ_data
  Depression_data_list[[cycle_name]] <- Depression_data
  GAD_data_list[[cycle_name]] <- GAD_data
}

# Combine the data across cycles
demo_data_combined <- bind_rows(demo_data_list)
RHQ_data_combined <- bind_rows(RHQ_data_list)
Depression_data_combined <- bind_rows(Depression_data_list)
GAD_data_combined <- bind_rows(GAD_data_list)

# Now we can proceed to calculate the flow
flow_data <- tibble(
  Step = c("Demo Datasets", "After RHQ", "After Depression", "After GAD"),
  Included = c(nrow(demo_data_combined), 
               sum(demo_data_combined$SEQN %in% RHQ_data_combined$SEQN), 
               sum(demo_data_combined$SEQN %in% Depression_data_combined$SEQN),
               sum(demo_data_combined$SEQN %in% GAD_data_combined$SEQN)),
  Dropped = c(0, 
              nrow(demo_data_combined) - sum(demo_data_combined$SEQN %in% RHQ_data_combined$SEQN), 
              nrow(demo_data_combined) - sum(demo_data_combined$SEQN %in% Depression_data_combined$SEQN), 
              nrow(demo_data_combined) - sum(demo_data_combined$SEQN %in% GAD_data_combined$SEQN)),
  Diagnosed_Depression_Positive = c(NA, 
                                    sum(Depression_data_combined$CIDDSCOR == 1 & Depression_data_combined$SEQN %in% demo_data_combined$SEQN), 
                                    sum(Depression_data_combined$CIDDSCOR == 1 & Depression_data_combined$SEQN %in% demo_data_combined$SEQN),
                                    sum(Depression_data_combined$CIDDSCOR == 1 & Depression_data_combined$SEQN %in% demo_data_combined$SEQN)),
  Diagnosed_GAD_Positive = c(NA, 
                             sum(GAD_data_combined$CIDGSCOR == 1 & GAD_data_combined$SEQN %in% demo_data_combined$SEQN),
                             sum(GAD_data_combined$CIDGSCOR == 1 & GAD_data_combined$SEQN %in% demo_data_combined$SEQN),
                             sum(GAD_data_combined$CIDGSCOR == 1 & GAD_data_combined$SEQN %in% demo_data_combined$SEQN))
)

# Create the flowchart plot using ggplot2
ggplot(flow_data, aes(x = Step, y = Included, fill = Step)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste("Included:", Included, "\nDropped:", Dropped)), vjust = -0.5, size = 4) +
  labs(title = "Flowchart of Participants in Each Dataset",
       subtitle = "Includes Number of Participants Included and Dropped at Each Stage",
       x = "Step", y = "Number of Participants") +
  theme_minimal() +
  theme(legend.position = "none")
