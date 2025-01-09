library(dplyr)
library(flextable)

# Shapiro-Wilk test for normality for age
shapiro.test(final_data$RIDAGEYR)

library(dplyr)
library(flextable)

# Function to create a summary table
create_summary_table <- function(data, variables, var_names = NULL, level_names = NULL) {
  # Recode factor levels based on the level_names argument
  if (!is.null(level_names)) {
    for (var in names(level_names)) {
      if (var %in% colnames(data)) {
        data[[var]] <- factor(data[[var]], levels = names(level_names[[var]]), labels = level_names[[var]])
      }
    }
  }
  
  # Initialize a list to store rows for the table
  table_rows <- list()
  
  # Loop through variables to calculate values for the table
  for (var in variables) {
    var_name <- ifelse(!is.null(var_names) && var %in% names(var_names), var_names[[var]], var)
    
    # Handle categorical variables
    if (is.factor(data[[var]]) || is.character(data[[var]])) {
      counts <- table(data[[var]])
      proportions <- round(prop.table(counts) * 100, 1)
      
      # Add a row for the categorical variable name
      table_rows[[length(table_rows) + 1]] <- data.frame(
        Variable = var_name,
        Level = NA,
        Summary = NA
      )
      
      # Add rows for each level of the categorical variable
      for (level_name in names(counts)) {
        table_rows[[length(table_rows) + 1]] <- data.frame(
          Variable = NA,
          Level = level_name,
          Summary = paste0(counts[[level_name]], " (", proportions[[level_name]], "%)")
        )
      }
    }
    
    # Handle continuous variables
    else if (is.numeric(data[[var]])) {
      # Check for normality
      normal_test <- shapiro.test(data[[var]])$p.value
      if (normal_test > 0.05) {
        # Normally distributed: Use mean and SD
        mean_val <- round(mean(data[[var]], na.rm = TRUE), 1)
        sd_val <- round(sd(data[[var]], na.rm = TRUE), 1)
        summary_stat <- paste0(mean_val, " (SD ", sd_val, ")")
      } else {
        # Non-normally distributed: Use median and IQR
        median_val <- round(median(data[[var]], na.rm = TRUE), 1)
        iqr_val <- round(IQR(data[[var]], na.rm = TRUE), 1)
        summary_stat <- paste0(median_val, " [IQR: ", iqr_val, "]")
      }
      
      # Add row for continuous variable
      table_rows[[length(table_rows) + 1]] <- data.frame(
        Variable = var_name,
        Level = NA,
        Summary = summary_stat
      )
    }
  }
  
  # Combine rows into a data frame
  summary_table <- bind_rows(table_rows) %>%
    select(Variable, Level, Summary)
  
  return(summary_table)
}

# Define level names for categorical variables
level_names <- list(
  RIAGENDR = c("1" = "Male", "2" = "Female"),
  CIDGSCOR = c("1" = "Positive Diagnosis", "5" = "Negative Diagnosis"),
  CIDDSCOR = c("1" = "Positive Diagnosis", "5" = "Negative Diagnosis"),
  RHQ360 = c("1" = "Yes", "2" = "No")
)

# Create the summary table
summary_table <- create_summary_table(
  data = final_data,
  variables = c("RIDAGEYR", "RIAGENDR", "RHQ360", "CIDGSCOR", "CIDDSCOR"),
  var_names = c(
    RIDAGEYR = "Age (years)",
    RIAGENDR = "Sex",
    RHQ360 = "Endometriosis Diagnosis",
    CIDGSCOR = "GAD Status",
    CIDDSCOR = "DD Status"
  ),
  level_names = level_names
)

# Format the table using flextable
formatted_summary_table <- summary_table %>%
  flextable() %>%
  set_header_labels(
    Variable = "Covariate",
    Level = "",
    Summary = "Summary Statistics"
  ) %>%
  theme_vanilla() %>%
  add_header_lines("Table of Baseline Characteristics") %>%
  add_footer_lines(c(
    "Continuous variables are presented as median [IQR] or mean (SD), depending on normality.",
    "Categorical variables are presented as counts (percentages)."
  )) %>%
  autofit() %>%
  bold(part = "header") %>%
  bold(j = "Variable", part = "all") %>%  
  italic(j = "Level", part = "all") %>%
  align(j = c("Variable", "Level"), align = "left", part = "all") %>%
  align(j = "Summary", align = "center", part = "all")

# Display the formatted table
formatted_summary_table

# Export the table as a Word document or image if needed
# save_as_docx(formatted_summary_table, path = "summary_table.docx")
# save_as_image(formatted_summary_table, path = "summary_table.png")

###made the footnotes smaller#####
library(dplyr)
library(flextable)

# Format the table using flextable
formatted_summary_table <- summary_table %>%
  flextable() %>%
  set_header_labels(
    Variable = "Covariate",
    Level = "",
    Summary = "Summary Statistics"
  ) %>%
  theme_vanilla() %>%
  add_header_lines("Table of Baseline Characteristics") %>%
  add_footer_lines(c(
    "Continuous variables are presented as median [IQR] or mean (SD), depending on normality.",
    "Categorical variables are presented as counts (percentages)."
  )) %>%
  fontsize(part = "footer", size = 8) %>%  # Reduce font size for footnotes
  autofit() %>%
  bold(part = "header") %>%
  bold(j = "Variable", part = "all") %>%  
  italic(j = "Level", part = "all") %>%
  align(j = c("Variable", "Level"), align = "left", part = "all") %>%
  align(j = "Summary", align = "center", part = "all")

# Display the formatted table
formatted_summary_table

# Export the table as a Word document or image if needed
# save_as_docx(formatted_summary_table, path = "summary_table.docx")
# save_as_image(formatted_summary_table, path = "summary_table.png")
