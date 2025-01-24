# Load necessary libraries
library(dplyr)
library(ggplot2)
library(Amelia)
library(psych)  # For descriptive statistics

# Assuming `raw_data` is the dataset with the variables:
# RHQ360, CIDDSCOR (depression score), CIDGSCOR (GAD score), RIDAGEYR (age), and RIDRETH1 (race)

# 1. View the first few rows of the dataset
print("First few rows of the dataset:")
head(raw_data)

# 2. Check the structure of the dataset (data types, column names, etc.)
print("Structure of the dataset:")
str(raw_data)

# 3. Summary statistics for each variable
print("Summary statistics:")
summary(raw_data)

# 4. Descriptive statistics (mean, median, standard deviation, etc.)
print("Descriptive statistics:")
describe(raw_data[, c("RHQ360", "CIDDSCOR", "CIDGSCOR", "RIDAGEYR", "RIDRETH1")])

# 5. Check for missing values
print("Count of missing values per column:")
colSums(is.na(raw_data))

# 6. Visualize the missing data pattern (if any) with percentages
plot_missing <- function(data) {
  missing_data <- colSums(is.na(data))
  total_rows <- nrow(data)
  missing_data <- missing_data[missing_data > 0]
  
  if (length(missing_data) > 0) {
    percentages <- round((missing_data / total_rows) * 100, 1)
    
    # Adjust margins to accommodate angled x-axis labels
    par(mar = c(12, 4, 4, 2))  # Bottom, Left, Top, Right margins
    
    # Set ylim to ensure space above the tallest bar
    ylim_max <- max(missing_data) * 1.2
    bar_positions <- barplot(
      missing_data, 
      main = "Missing Data Pattern", 
      ylab = "Count of Missing Values", 
      xlab = "",  # Remove x-axis label
      col = "skyblue", 
      ylim = c(0, ylim_max), # Extend y-axis range
      names.arg = rep("", length(missing_data)) # Hide default labels
    )
    
    # Add angled x-axis labels
    text(
      x = bar_positions, 
      y = par("usr")[3] - 0.02 * diff(par("usr")[3:4]), # Position slightly below the axis
      labels = names(missing_data), 
      srt = 45,  # Rotate text by 45 degrees
      adj = 1,   # Right-align text
      xpd = TRUE, # Allow drawing outside plot region
      cex = 0.8  # Smaller text size
    )
    
    # Overlay text showing the percentage of missing data
    text(
      x = bar_positions, 
      y = missing_data, 
      labels = paste0(percentages, "%"), 
      pos = 3, 
      cex = 0.8, 
      col = "red"
    )
  } else {
    print("No missing values found.")
  }
}

# Example usage of missing data plot
plot_missing(raw_data)

# 7. Visualize the distribution of the numeric variable (Age)
numeric_cols <- raw_data %>% select(RIDAGEYR)  # Only select Age (numeric variable)
if (ncol(numeric_cols) > 0) {
  print("Histogram of Age:")
  print(ggplot(raw_data, aes(x = RIDAGEYR)) +
          geom_histogram(bins = 30, fill = "skyblue", color = "black") +
          ggtitle("Distribution of Age"))
} else {
  print("No numeric columns to plot.")
}

# 8. Box plot to detect outliers in the numeric variable (Age)
print("Box plot for Age to detect outliers:")
print(ggplot(raw_data, aes(x = "1", y = RIDAGEYR)) +
        geom_boxplot(fill = "lightgreen", color = "black") +
        ggtitle("Box plot of Age") +
        xlab("") +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()))

# 9. Distribution of categorical variables (Race, RHQ360, CIDDSCOR, CIDGSCOR)
categorical_cols <- raw_data %>% select(RIDRETH1, RHQ360, CIDDSCOR, CIDGSCOR)  # Select categorical variables
if (ncol(categorical_cols) > 0) {
  print("Bar plots of categorical variables:")
  
  # Loop through each categorical variable and plot its distribution
  for (col in colnames(categorical_cols)) {
    print(ggplot(raw_data, aes_string(col)) +
            geom_bar(fill = "orange", color = "black") +
            ggtitle(paste("Distribution of", col)))
  }
} else {
  print("No categorical columns to plot.")
}


