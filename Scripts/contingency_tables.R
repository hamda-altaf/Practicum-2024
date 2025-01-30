# Load necessary libraries
library(tidyverse)

# Create contingency tables
table1 <- table(cleaned_data$RHQ360, cleaned_data$CIDGSCOR, dnn = c("Endometriosis", "GAD Diagnosis"))
table2 <- table(cleaned_data$RHQ360, cleaned_data$CIDDSCOR, dnn = c("Endometriosis", "Depression Diagnosis"))
table3 <- table(paste(cleaned_data$RHQ360, cleaned_data$CIDGSCOR, cleaned_data$CIDDSCOR, sep = "_"))

# Print tables
cat("Contingency Table 1: Endometriosis vs. GAD Diagnosis\n")
print(table1)

cat("\nContingency Table 2: Endometriosis vs. Depression Diagnosis\n")
print(table2)

cat("\nContingency Table 3: Endometriosis vs. Both GAD and Depression Diagnoses\n")
print(table3)
