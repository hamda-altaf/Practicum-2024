# Load necessary libraries
library(survey)
library(tidyverse)

# Create binary depression outcome and endometriosis exposure
filtered_data <- filtered_data %>%
  mutate(
    Depression = case_when(
      DEPR_LVL == "No depression" ~ 0,
      DEPR_LVL %in% c("Moderate depression", "Severe depression") ~ 1
    ),
    Endometriosis = as.numeric(RHQ360 == 1)  # 1 = Yes, 0 = No
  )

# Convert categorical covariates into factors
filtered_data <- filtered_data %>%
  mutate(
    AgeGroup = case_when(
      RIDAGEYR >= 20 & RIDAGEYR <= 29 ~ "20-29",
      RIDAGEYR >= 30 & RIDAGEYR <= 40 ~ "30-40",
      RIDAGEYR >= 41 & RIDAGEYR <= 54 ~ "41-54"
    ),
    Race = factor(RIDRETH1, labels = c("Mexican American", "Other Hispanic", "Non-Hispanic White",
                                       "Non-Hispanic Black", "Other Race")),
    PIR_Category = case_when(
      INDFMPIR < 1.35 ~ "Low-income",
      INDFMPIR >= 1.35 & INDFMPIR < 3.0 ~ "Medium-income",
      INDFMPIR >= 3.0 ~ "High-income"
    ),
    BMI_Category = case_when(
      BMXBMI < 18.5 ~ "Underweight",
      BMXBMI >= 18.5 & BMXBMI < 25 ~ "Normal weight",
      BMXBMI >= 25 & BMXBMI < 30 ~ "Overweight",
      BMXBMI >= 30 ~ "Obese"
    )
  )

# Now define the survey design *after* adding the new variables
survey_design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  data = filtered_data,
  nest = TRUE
)

# Model 1: Unadjusted
model1 <- svyglm(Depression ~ Endometriosis, design = survey_design, family = quasibinomial())

# Model 2: Adjusted for age, PIR, and race
model2 <- svyglm(Depression ~ Endometriosis + AgeGroup + PIR_Category + Race, design = survey_design, family = quasibinomial())

# Model 3: Fully adjusted (all covariates)
model3 <- svyglm(Depression ~ Endometriosis + AgeGroup + PIR_Category + Race + BMI_Category, 
                 design = survey_design, family = quasibinomial())

# Display results
summary(model1)
summary(model2)
summary(model3)

########interactions: this is problematic############
# Model 4: Interaction model for subgroup analyses
model_interaction <- svyglm(
  Depression_Binary ~ Endometriosis * AgeGroup + Endometriosis * PIR_Category, 
  design = survey_design, family = binomial()
)

# Display results
summary(model_interaction)

# Assess interaction terms significance (p-value for each interaction)



#########SUB GROUP Analayses

# Subgroup: Age 20-29
age_20_29 <- subset(filtered_data, AgeGroup == "20-29")
survey_design_age_20_29 <- svydesign(
  id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR, data = age_20_29, nest = TRUE
)
model_age_20_29 <- svyglm(Depression_Binary ~ Endometriosis, design = survey_design_age_20_29, family = binomial())
summary(model_age_20_29)

# Subgroup: Age 30-40
age_30_40 <- subset(filtered_data, AgeGroup == "30-40")
survey_design_age_30_40 <- svydesign(
  id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR, data = age_30_40, nest = TRUE
)
model_age_30_40 <- svyglm(Depression_Binary ~ Endometriosis, design = survey_design_age_30_40, family = quasibinomial())
summary(model_age_30_40)

# Subgroup: Age 41-54
age_41_54 <- subset(filtered_data, AgeGroup == "41-54")
survey_design_age_41_54 <- svydesign(
  id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR, data = age_41_54, nest = TRUE
)
model_age_41_54 <- svyglm(Depression_Binary ~ Endometriosis, design = survey_design_age_41_54, family = quasibinomial())
summary(model_age_41_54)


###########no survey weights##########

# Fit a logistic regression model without survey weights
model_no_weights_1 <- glm(Depression ~ Endometriosis + AgeGroup + PIR_Category + 
                            Race + BMI_Category, 
                          data = filtered_data, family = binomial())

# Display results
summary(model_no_weights_1)

# Subgroup: Age 30-40
age_30_40_no_weights <- subset(filtered_data, AgeGroup == "30-40")

# Fit the model without survey weights
model_no_weights_2 <- glm(Depression ~ Endometriosis, 
                          data = age_30_40_no_weights, family = binomial())

# Display results
summary(model_no_weights_2)

# Fit the interaction model without survey weights
model_no_weights_3 <- glm(Depression ~ Endometriosis * AgeGroup + Endometriosis * PIR_Category + 
                            Endometriosis * Race + Endometriosis * BMI_Category, 
                          data = filtered_data, family = binomial())

# Display results
summary(model_no_weights_3)

##################models she asked#######

# Model 1: No survey weights, no adjustment for all covariates
model_1 <- glm(Depression ~ Endometriosis, 
               data = filtered_data, 
               family = binomial(link = "logit"))
summary(model_1)

# Model 2: With survey weights, no adjustment for all covariates using binomial family
model_2_binomial <- svyglm(Depression ~ Endometriosis, 
                           design = survey_design, 
                           family = binomial(link = "logit"))

# Summarize the model
summary(model_2_binomial)

# Model 3: No survey weights, with adjustment for all covariates
model_3 <- glm(Depression ~ Endometriosis + AgeGroup + PIR_Category + Race + BMI_Category + marital_status + educational_level + smoking_status + alcohol_use, 
               data = filtered_data, 
               family = binomial(link = "logit"))
summary(model_3)

model_3 <- glm(Depression ~ Endometriosis + AgeGroup + PIR_Category + Race + BMI_Category + DMDMARTL + DMDEDUC2 + SMOKING_STATUS + ALCOHOL_USE, data = filtered_data, family = binomial(link = "logit"))

# Model 4: With survey weights, with adjustment for all covariates
model_4 <- svyglm(Depression ~ Endometriosis + AgeGroup + PIR_Category + Race + BMI_Category + DMDMARTL + DMDEDUC2 + SMOKING_STATUS + ALCOHOL_USE, 
                  design = survey_design, 
                  family = binomial(link = "logit"))
summary(model_4)

######## RIGHT#####

# Model 1: No survey weights, no adjustment for all covariates
model_1 <- glm(Depression_Binary ~ Endometriosis, 
               data = filtered_data, 
               family = binomial(link = "logit"))
summary(model_1)

# Model 2: With survey weights, no adjustment for all covariates using binomial family
model_2 <- svyglm(Depression_Binary ~ Endometriosis, 
                           design = survey_design, 
                           family = binomial(link = "logit"))

summary(model_2)


# Fit logistic regression model without survey weights
model_3 <- glm(
  Depression_Binary ~ Endometriosis + AgeGroup + RHQ010 + ALCOHOL_USE + BMI_Category + 
    Education_Level + Marital_Status + PIR_Category + Race + SMOKING_STATUS + 
    History_Pregnancy, 
  family = binomial(),  # Binomial family for logistic regression
  data = filtered_data
)

# Summarize model results
summary(model_3)

# Fit weighted logistic regression model with binomial family
model_4 <- svyglm(
  Depression_Binary ~ Endometriosis + AgeGroup + RHQ010 + ALCOHOL_USE + BMI_Category + 
    Education_Level + Marital_Status + PIR_Category + Race + SMOKING_STATUS + 
    History_Pregnancy, 
  design = survey_design, 
  family = binomial()  # Changed to binomial family
)

# Summarize model results
summary(model_4)

# Fit weighted logistic regression model with binomial family (removed race, alcohol, education, BMI)
model_4_reduced <- svyglm(
  Depression_Binary ~ Endometriosis + AgeGroup + RHQ010 + Marital_Status + PIR_Category + SMOKING_STATUS + 
    History_Pregnancy, 
  design = survey_design, 
  family = binomial()  # Changed to binomial family
)

# Summarize model results
summary(model_4_reduced)

#weighted log reg model 2 (from paper) with just age, race, PIR
model_2_paper <- svyglm(
  Depression_Binary ~ Endometriosis + AgeGroup + PIR_Category + Race, 
  design = survey_design, 
  family = binomial()  # Changed to binomial family
)

# Summarize model results
summary(model_2_paper)

# Model 5: Interaction model for subgroup analyses
model_interaction <- svyglm(
  Depression_Binary ~ Endometriosis * AgeGroup + Endometriosis * PIR_Category, 
  design = survey_design, family = binomial()
)

# Display results
summary(model_interaction)




#########SUB GROUP Analyses

# Subgroup: Age 20-29
age_20_29 <- subset(filtered_data, AgeGroup == "20-29")
survey_design_age_20_29 <- svydesign(
  id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR, data = age_20_29, nest = TRUE
)
model_age_20_29 <- svyglm(Depression_Binary ~ Endometriosis, design = survey_design_age_20_29, family = binomial())
summary(model_age_20_29)

# Subgroup: Age 30-40
age_30_40 <- subset(filtered_data, AgeGroup == "30-40")
survey_design_age_30_40 <- svydesign(
  id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR, data = age_30_40, nest = TRUE
)
model_age_30_40 <- svyglm(Depression_Binary ~ Endometriosis, design = survey_design_age_30_40, family = quasibinomial())
summary(model_age_30_40)

# Subgroup: Age 41-54
age_41_54 <- subset(filtered_data, AgeGroup == "41-54")
survey_design_age_41_54 <- svydesign(
  id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR, data = age_41_54, nest = TRUE
)
model_age_41_54 <- svyglm(Depression_Binary ~ Endometriosis, design = survey_design_age_41_54, family = quasibinomial())
summary(model_age_41_54)




