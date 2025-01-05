# Step 1: Total participants from NHANES 1999-2004 cycles
total_participants <- nrow(final_data)
print(paste("Total participants:", total_participants))

# Step 2: Number of female participants and excluded males
females <- final_data %>% filter(RIAGENDR == 2)
num_females <- nrow(females)
excluded_males <- total_participants - num_females
print(paste("Number of females:", num_females))
print(paste("Excluded males:", excluded_males))

# Step 3: Females who answered the endometriosis question
answered_endometriosis <- females %>% filter(!is.na(RHQ360) & RHQ360 != 7 & RHQ360 != 9)
num_answered_endometriosis <- nrow(answered_endometriosis)
refused_endometriosis <- sum(females$RHQ360 == 7, na.rm = TRUE)
dont_know_endometriosis <- sum(females$RHQ360 == 9, na.rm = TRUE)
missing_endometriosis <- sum(is.na(females$RHQ360))
total_answered_endometriosis <- num_answered_endometriosis - (refused_endometriosis + dont_know_endometriosis + missing_endometriosis)
total_excluded_endometriosis <- refused_endometriosis + dont_know_endometriosis + missing_endometriosis
print(paste("Total answered endometriosis:", total_answered_endometriosis))
print(paste("Total excluded endometriosis:", total_excluded_endometriosis))

# Debugging: Check the total after the first two splits
print(paste("Sum of answered and excluded endometriosis:", total_answered_endometriosis + total_excluded_endometriosis))
print(paste("Total females:", num_females))

# Step 4: Females who completed depression and GAD questionnaires
completed_GAD <- answered_endometriosis %>% filter(!is.na(CIDGSCOR))
num_completed_GAD <- nrow(completed_GAD)
missing_GAD <- num_answered_endometriosis - num_completed_GAD
completed_depression <- answered_endometriosis %>% filter(!is.na(CIDDSCOR))
num_completed_depression <- nrow(completed_depression)
missing_depression <- num_answered_endometriosis - num_completed_depression
print(paste("Completed GAD:", num_completed_GAD))
print(paste("Missing GAD data:", missing_GAD))
print(paste("Completed depression:", num_completed_depression))
print(paste("Missing depression data:", missing_depression))

# Step 5: Females aged 20-39 who completed GAD and depression questionnaires
GAD_20_39 <- completed_GAD %>% filter(RIDAGEYR >= 20 & RIDAGEYR <= 39)
num_GAD_20_39 <- nrow(GAD_20_39)
depression_20_39 <- completed_depression %>% filter(RIDAGEYR >= 20 & RIDAGEYR <= 39)
num_depression_20_39 <- nrow(depression_20_39)
print(paste("Aged 20-39 who completed GAD:", num_GAD_20_39))
print(paste("Aged 20-39 who completed depression:", num_depression_20_39))

# Step 6: Diagnosis of GAD and depression
positively_diagnosed_GAD <- sum(GAD_20_39$CIDGSCOR == 1, na.rm = TRUE)
negatively_diagnosed_GAD <- num_GAD_20_39 - positively_diagnosed_GAD
positively_diagnosed_depression <- sum(depression_20_39$CIDDSCOR == 1, na.rm = TRUE)
negatively_diagnosed_depression <- num_depression_20_39 - positively_diagnosed_depression
print(paste("Positively diagnosed GAD:", positively_diagnosed_GAD))
print(paste("Positively diagnosed depression:", positively_diagnosed_depression))

# Now construct the flowchart ensuring each step matches the total number of records
fc <- as_fc(label = "NHANES Participants 1999-2004 (3 Cycles)", N = total_participants) %>%
  fc_split(N = c(num_females, excluded_males), label = "Female Participants") %>%
  fc_split(N = c(num_answered_endometriosis, total_participants - num_answered_endometriosis), 
           label = "Answered Endometriosis Question") %>%
  fc_split(N = c(total_answered_endometriosis, total_excluded_endometriosis),
           label = "Excluded (Refused, Don't Know, Missing)") %>%
  fc_split(N = c(num_completed_GAD, missing_GAD), label = "Completed GAD Questionnaire") %>%
  fc_split(N = c(num_completed_depression, missing_depression), label = "Completed Depression Questionnaire") %>%
  fc_split(N = c(num_GAD_20_39, num_depression_20_39), label = "Aged 20-39 Answered GAD & Depression") %>%
  fc_split(N = c(positively_diagnosed_GAD, negatively_diagnosed_GAD), label = "Positively Diagnosed GAD") %>%
  fc_split(N = c(positively_diagnosed_depression, negatively_diagnosed_depression), label = "Positively Diagnosed Depression") %>%
  fc_draw(title = "Flowchart of NHANES 1999-2004 Participants")




