# 3. MNAR (Missing Not at Random) - Missingness depends on the variable itself

# Set a seed for reproducibility
set.seed(123)

# For RHQ360: Missingness depends on the value of RHQ360 itself
# Higher values of RHQ360 are more likely to be missing
if (any(!is.na(mnar_data$RHQ360))) {  # Ensure there are non-missing values to apply the rule
  prob_rhq360 <- ifelse(!is.na(mnar_data$RHQ360), 
                        mnar_data$RHQ360 / max(mnar_data$RHQ360, na.rm = TRUE), 
                        0)  # Probability proportional to value
  eligible_indices_rhq360 <- which(!is.na(mnar_data$RHQ360))
  max_sample_size_rhq360 <- length(eligible_indices_rhq360)
  sample_size_rhq360 <- min(floor(nrow(mnar_data) * missing_rate_mar), max_sample_size_rhq360)
  
  # Sample the indices based on probabilities
  sample_indices_rhq360 <- sample(eligible_indices_rhq360, 
                                  size = sample_size_rhq360, 
                                  prob = prob_rhq360[eligible_indices_rhq360])
  
  # Set the selected indices to NA
  mnar_data$RHQ360[sample_indices_rhq360] <- NA
}

# For CIDDSCOR: Missingness depends on the value of CIDDSCOR itself
# Individuals with higher depression scores (CIDDSCOR) are more likely to have missing values
if (any(!is.na(mnar_data$CIDDSCOR))) {  # Ensure there are non-missing values to apply the rule
  prob_ciddscor <- ifelse(!is.na(mnar_data$CIDDSCOR), 
                          mnar_data$CIDDSCOR / max(mnar_data$CIDDSCOR, na.rm = TRUE), 
                          0)  # Probability proportional to value
  eligible_indices_ciddscor <- which(!is.na(mnar_data$CIDDSCOR))
  max_sample_size_ciddscor <- length(eligible_indices_ciddscor)
  sample_size_ciddscor <- min(floor(nrow(mnar_data) * missing_rate_mar), max_sample_size_ciddscor)
  
  # Sample the indices based on probabilities
  sample_indices_ciddscor <- sample(eligible_indices_ciddscor, 
                                    size = sample_size_ciddscor, 
                                    prob = prob_ciddscor[eligible_indices_ciddscor])
  
  # Set the selected indices to NA
  mnar_data$CIDDSCOR[sample_indices_ciddscor] <- NA
}

# For CIDGSCOR: Missingness depends on the value of CIDGSCOR itself
# Individuals with higher GAD scores (CIDGSCOR) are more likely to have missing values
if (any(!is.na(mnar_data$CIDGSCOR))) {  # Ensure there are non-missing values to apply the rule
  prob_cidgscor <- ifelse(!is.na(mnar_data$CIDGSCOR), 
                          mnar_data$CIDGSCOR / max(mnar_data$CIDGSCOR, na.rm = TRUE), 
                          0)  # Probability proportional to value
  eligible_indices_cidgscor <- which(!is.na(mnar_data$CIDGSCOR))
  max_sample_size_cidgscor <- length(eligible_indices_cidgscor)
  sample_size_cidgscor <- min(floor(nrow(mnar_data) * missing_rate_mar), max_sample_size_cidgscor)
  
  # Sample the indices based on probabilities
  sample_indices_cidgscor <- sample(eligible_indices_cidgscor, 
                                    size = sample_size_cidgscor, 
                                    prob = prob_cidgscor[eligible_indices_cidgscor])
  
  # Set the selected indices to NA
  mnar_data$CIDGSCOR[sample_indices_cidgscor] <- NA
}





# Combine the datasets into one list
missing_data_list <- list(
  MCAR = mcar_data,
  MAR = mar_data,
  MNAR = mnar_data  # Updated MNAR data with the new simulation
)

# Create a plot for each missingness mechanism
missing_plots <- lapply(names(missing_data_list), function(data_name) {
  missing_data <- missing_data_list[[data_name]]
  amelia_plot <- missmap(missing_data, main = paste("Missingness Mechanism:", data_name))
  return(amelia_plot)
})

# Each plot will show the missing values for the variables and how they are distributed across the data. 
# Display the plots
missing_plots[[1]]  # MCAR plot
missing_plots[[2]]  # MAR plot
missing_plots[[3]]  # MNAR plot


##### Visualisation using naniar and VIM #################

library(naniar)
library(VIM)
library(finalfit)

# Select only the relevant columns
mcar_data_selected <- mcar_data %>%
  select(SEQN, RIDAGEYR, RIDRETH1, DMDMARTL, RHQ360, CIDDSCOR, CIDGSCOR)
mar_data_selected <- mar_data %>%
  select(SEQN, RIDAGEYR, RIDRETH1, DMDMARTL, RHQ360, CIDDSCOR, CIDGSCOR)
mnar_data_selected <- mnar_data %>%
  select(SEQN, RIDAGEYR, RIDRETH1, DMDMARTL, RHQ360, CIDDSCOR, CIDGSCOR)

# Visualize missing data with naniar
naniar::vis_miss(mcar_data_selected)
naniar::vis_miss(mar_data_selected)
naniar::vis_miss(mnar_data_selected)

# Aggregate missing data with VIM
VIM::aggr(mcar_data_selected, numbers = TRUE, prop = c(TRUE, FALSE))
VIM::aggr(mar_data_selected, numbers = TRUE, prop = c(TRUE, FALSE))
VIM::aggr(mnar_data_selected, numbers = TRUE, prop = c(TRUE, FALSE))

#### To check missing data by category

# Visualize missing data by RHQ360 categories (Yes/No/Other)
naniar::vis_miss(mar_data_selected %>% filter(!is.na(RHQ360)))
naniar::vis_miss(mnar_data_selected %>% filter(!is.na(RHQ360)))

# Visualize missing data by CIDDSCOR categories (Positive Diagnosis/Negative Diagnosis)
naniar::vis_miss(mar_data_selected %>% filter(!is.na(CIDDSCOR)))
naniar::vis_miss(mnar_data_selected %>% filter(!is.na(CIDDSCOR)))

# Visualize missing data by CIDGSCOR categories (Positive Diagnosis/Negative Diagnosis)
naniar::vis_miss(mar_data_selected %>% filter(!is.na(CIDGSCOR)))
naniar::vis_miss(mnar_data_selected %>% filter(!is.na(CIDGSCOR)))

# Aggregate missing data with VIM by RHQ360 categories
VIM::aggr(mar_data_selected %>% filter(!is.na(RHQ360)), numbers = TRUE, prop = c(TRUE, FALSE))
VIM::aggr(mnar_data_selected %>% filter(!is.na(RHQ360)), numbers = TRUE, prop = c(TRUE, FALSE))

# Aggregate missing data with VIM by CIDDSCOR categories
VIM::aggr(mar_data_selected %>% filter(!is.na(CIDDSCOR)), numbers = TRUE, prop = c(TRUE, FALSE))
VIM::aggr(mnar_data_selected %>% filter(!is.na(CIDDSCOR)), numbers = TRUE, prop = c(TRUE, FALSE))

# Aggregate missing data with VIM by CIDGSCOR categories
VIM::aggr(mar_data_selected %>% filter(!is.na(CIDGSCOR)), numbers = TRUE, prop = c(TRUE, FALSE))
VIM::aggr(mnar_data_selected %>% filter(!is.na(CIDGSCOR)), numbers = TRUE, prop = c(TRUE, FALSE))

###

library(finalfit)
library(knitr)
library(kableExtra)

# How explanatory variables differ between rows where RHQ360 is missing vs. not missing for MAR and MNAR data
mar_data %>% 
  finalfit::missing_compare(dependent = "RHQ360",  # Outcome variable (RHQ360)
                            explanatory = c("RIDAGEYR", "RIDRETH1", "CIDGSCOR", "CIDDSCOR", "DMDMARTL")) %>% 
  knitr::kable(row.names = FALSE, align = c("l", "l", "r", "r", "r", "r"),
               caption = "Comparison by RHQ360 missing vs not missing using MAR data") %>% 
  kable_styling(full_width = F)

mnar_data %>% 
  finalfit::missing_compare(dependent = "RHQ360",  # Outcome variable (RHQ360)
                            explanatory = c("RIDAGEYR", "RIDRETH1", "CIDGSCOR", "CIDDSCOR", "DMDMARTL")) %>% 
  knitr::kable(row.names = FALSE, align = c("l", "l", "r", "r", "r", "r"),
               caption = "Comparison by RHQ360 missing vs not missing using MNAR data") %>% 
  kable_styling(full_width = F)
