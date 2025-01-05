library(dplyr)
library(flowchart)

# Step 1: Total participants from NHANES 1999-2004 cycles
total_participants <- nrow(final_data)

# Step 2: Number of female participants and excluded males
females <- final_data %>% filter(RIAGENDR == 2) # Assuming RIAGENDR == 2 is female
num_females <- nrow(females)
excluded_males <- total_participants - num_females

# Step 3: Females who answered the endometriosis question (RHQ360)
answered_endometriosis <- females %>% filter(!is.na(RHQ360) & RHQ360 != 7 & RHQ360 != 9)
num_answered_endometriosis <- nrow(answered_endometriosis)
refused_endometriosis <- sum(females$RHQ360 == 7, na.rm = TRUE)
dont_know_endometriosis <- sum(females$RHQ360 == 9, na.rm = TRUE)
missing_endometriosis <- sum(is.na(females$RHQ360))

# Step 4: Females who completed depression and GAD questionnaires
completed_GAD <- answered_endometriosis %>% filter(!is.na(CIDGSCOR))
num_completed_GAD <- nrow(completed_GAD)
missing_GAD <- nrow(answered_endometriosis) - num_completed_GAD

completed_depression <- answered_endometriosis %>% filter(!is.na(CIDDSCOR))
num_completed_depression <- nrow(completed_depression)
missing_depression <- nrow(answered_endometriosis) - num_completed_depression

# Step 5: Females aged 20-39 who completed GAD and depression questionnaires
GAD_20_39 <- completed_GAD %>% filter(RIDAGEYR >= 20 & RIDAGEYR <= 39)
num_GAD_20_39 <- nrow(GAD_20_39)

depression_20_39 <- completed_depression %>% filter(RIDAGEYR >= 20 & RIDAGEYR <= 39)
num_depression_20_39 <- nrow(depression_20_39)

# Step 6: Diagnosis of GAD and depression
# Defining positive diagnosis threshold as 1
positive_diagnosis_value <- 1

# Diagnosing GAD
positively_diagnosed_GAD <- sum(GAD_20_39$CIDGSCOR == positive_diagnosis_value, na.rm = TRUE)
negatively_diagnosed_GAD <- num_GAD_20_39 - positively_diagnosed_GAD

# Diagnosing Depression
positively_diagnosed_depression <- sum(depression_20_39$CIDDSCOR == positive_diagnosis_value, na.rm = TRUE)
negatively_diagnosed_depression <- num_depression_20_39 - positively_diagnosed_depression

# Create the Flowchart
fc <- as_fc(label = "NHANES Participants 1999-2004 (3 Cycles)",
            N = total_participants) %>%
  fc_split(N = c(num_females, excluded_males),
           label = c("Female Participants", "Male Participants Excluded")) %>%
  fc_split(N = c(num_answered_endometriosis, refused_endometriosis + dont_know_endometriosis + missing_endometriosis),
           label = c("Answered Endometriosis Question", "Excluded (Refused, Don't Know, Missing)"),
           text_pattern_exc = "Refused: {refused_endometriosis}\nDon't Know: {dont_know_endometriosis}\nMissing: {missing_endometriosis}") %>%
  fc_split(N = c(num_completed_GAD, missing_GAD),
           label = c("Completed GAD Questionnaire", "Missing GAD Data")) %>%
  fc_split(N = c(num_completed_depression, missing_depression),
           label = c("Completed Depression Questionnaire", "Missing Depression Data")) %>%
  fc_split(N = c(num_GAD_20_39, num_depression_20_39),
           label = c("Aged 20-39 Answered GAD", "Aged 20-39 Answered Depression")) %>%
  fc_split(N = c(positively_diagnosed_GAD, negatively_diagnosed_GAD),
           label = c("Positively Diagnosed GAD", "Negatively Diagnosed GAD")) %>%
  fc_split(N = c(positively_diagnosed_depression, negatively_diagnosed_depression),
           label = c("Positively Diagnosed Depression", "Negatively Diagnosed Depression")) %>%
  fc_draw(title = "Flowchart of NHANES 1999-2004 Participants")

