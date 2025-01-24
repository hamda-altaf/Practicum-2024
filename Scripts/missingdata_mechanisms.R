# Load necessary libraries
library(ggplot2)
library(dplyr)
library(Amelia)  # For visualizing missingness

# Assuming `raw_data` is your original dataset
#simulating missing data under three common missingness mechanisms

# Set the missing rate
missing_rate_mcar <- 0.2  # 20% missing at random
missing_rate_mar <- 0.2   # 20% missing for MAR and MNAR

# Create a copy of the original data for each mechanism
mcar_data <- raw_data
mar_data <- raw_data
mnar_data <- raw_data

# 1. MCAR (Missing Completely at Random) - Remove random data points
set.seed(123)

# Ensure sample size is not larger than available non-missing values
sample_size_mcar_rhq <- floor(nrow(mcar_data) * missing_rate_mcar)
sample_size_mcar_rhq <- min(sum(!is.na(mcar_data$RHQ360)), sample_size_mcar_rhq)
mcar_data$RHQ360[sample(which(!is.na(mcar_data$RHQ360)), size = sample_size_mcar_rhq)] <- NA

sample_size_mcar_depression <- floor(nrow(mcar_data) * missing_rate_mcar)
sample_size_mcar_depression <- min(sum(!is.na(mcar_data$CIDDSCOR)), sample_size_mcar_depression)
mcar_data$CIDDSCOR[sample(which(!is.na(mcar_data$CIDDSCOR)), size = sample_size_mcar_depression)] <- NA

sample_size_mcar_gad <- floor(nrow(mcar_data) * missing_rate_mcar)
sample_size_mcar_gad <- min(sum(!is.na(mcar_data$CIDGSCOR)), sample_size_mcar_gad)
mcar_data$CIDGSCOR[sample(which(!is.na(mcar_data$CIDGSCOR)), size = sample_size_mcar_gad)] <- NA


# 2. MAR (Missing at Random) - Missingness depends on other variables
# Missingness in RHQ360 depends on depression score (CIDDSCOR)
set.seed(123)
condition_rhq <- which(!is.na(mar_data$CIDDSCOR) & mar_data$CIDDSCOR == 1)
sample_size_rhq <- floor(nrow(mar_data) * missing_rate_mar)
sample_size_rhq <- min(length(condition_rhq), sample_size_rhq)
mar_data$RHQ360[sample(condition_rhq, size = sample_size_rhq)] <- NA

# Missingness in CIDDSCOR depends on GAD score (CIDGSCOR)
set.seed(123)
condition_cid <- which(!is.na(mar_data$CIDGSCOR) & mar_data$CIDGSCOR == 1)
sample_size_cid <- floor(nrow(mar_data) * missing_rate_mar)
sample_size_cid <- min(length(condition_cid), sample_size_cid)
mar_data$CIDDSCOR[sample(condition_cid, size = sample_size_cid)] <- NA

# Missingness in CIDGSCOR depends on age (RIDAGEYR)
set.seed(123)
condition_gad <- which(!is.na(mar_data$RIDAGEYR) & mar_data$RIDAGEYR > 50)
sample_size_gad <- floor(nrow(mar_data) * missing_rate_mar)
sample_size_gad <- min(length(condition_gad), sample_size_gad)
mar_data$CIDGSCOR[sample(condition_gad, size = sample_size_gad)] <- NA


# 3. MNAR (Missing Not at Random) - Missingness depends on the variable itself
# Missingness in RHQ360 depends on depression score (CIDDSCOR) - Specifically for positive diagnosis
set.seed(123)
condition_rhq_mnar <- which(!is.na(mnar_data$CIDDSCOR) & mnar_data$CIDDSCOR == 1)
sample_size_rhq_mnar <- floor(nrow(mnar_data) * missing_rate_mar)
sample_size_rhq_mnar <- min(length(condition_rhq_mnar), sample_size_rhq_mnar)
mnar_data$RHQ360[sample(condition_rhq_mnar, size = sample_size_rhq_mnar)] <- NA

# Missingness in CIDDSCOR depends on GAD score (CIDGSCOR) - Specifically for positive diagnosis
set.seed(123)
condition_cid_mnar <- which(!is.na(mnar_data$CIDGSCOR) & mnar_data$CIDGSCOR == 1)
sample_size_cid_mnar <- floor(nrow(mnar_data) * missing_rate_mar)
sample_size_cid_mnar <- min(length(condition_cid_mnar), sample_size_cid_mnar)
mnar_data$CIDDSCOR[sample(condition_cid_mnar, size = sample_size_cid_mnar)] <- NA

# Missingness in CIDGSCOR depends on age (RIDAGEYR) - Specifically for older individuals
set.seed(123)
condition_gad_mnar <- which(!is.na(mnar_data$RIDAGEYR) & mnar_data$RIDAGEYR > 50)
sample_size_gad_mnar <- floor(nrow(mnar_data) * missing_rate_mar)
sample_size_gad_mnar <- min(length(condition_gad_mnar), sample_size_gad_mnar)
mnar_data$CIDGSCOR[sample(condition_gad_mnar, size = sample_size_gad_mnar)] <- NA


######### Visualization of missingness using Amelia###########

# Combine the datasets into one list
missing_data_list <- list(
  MCAR = mcar_data,
  MAR = mar_data,
  MNAR = mnar_data
)

# Create a plot for each missingness mechanism
missing_plots <- lapply(names(missing_data_list), function(data_name) {
  missing_data <- missing_data_list[[data_name]]
  amelia_plot <- missmap(missing_data, main = paste("Missingness Mechanism:", data_name))
  return(amelia_plot)
})

#Each plot will show the missing values for the variables and how they are distributed across the data. 
# Display the plots
missing_plots[[1]]  # MCAR plot
missing_plots[[2]]  # MAR plot
missing_plots[[3]]  # MNAR plot


##### Visualisation using naniar and VIM#################

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

####to check missing data by category

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

#how explanatory variables differ between rows where RHQ360 is missing vs. not missing for MAR and MNAR data
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

