# Load required libraries
library(MASS)
library(caret)
library(moments)
library(gtsummary)

# 1. Read the cleaned data for the model (excluding Height, Weight, BMI)
data_model <- read.csv("preprocessed_model.csv")

# Ensure Obesity_Level is an Ordered Factor
data_model$Obesity_Level <- factor(data_model$Obesity_Level, 
                                   levels = c("Insufficient Weight", "Normal Weight", 
                                              "Overweight Level I", "Overweight Level II", 
                                              "Obesity Type I", "Obesity Type II", "Obesity Type III"),
                                   ordered = TRUE)

# 2. Split dataset into Train/Test (80/20)
set.seed(123) # Ensure reproducibility of results
trainIndex <- createDataPartition(data_model$Obesity_Level, p = 0.8, list = FALSE)
train_data <- data_model[trainIndex, ]
test_data  <- data_model[-trainIndex, ]

# 3. Build the Ordinal Logistic Regression model
# Predict Obesity_Level based on Diet_score, Age, Gender, and key habits
model_ordinal <- polr(Obesity_Level ~ Diet_score + Age + Gender + 
                        Family_history_with_overweight + Physical_activity + 
                        food_between_meals + Alcohol_consumption, 
                      data = train_data, Hess = TRUE)

# 4. Summarize model results
print(summary(model_ordinal))

# 5. Evaluate accuracy on the Test set
predictions <- predict(model_ordinal, test_data)
conf_matrix <- confusionMatrix(predictions, test_data$Obesity_Level)

print(conf_matrix)

# Read the Exploratory Data Analysis (EDA) dataset (includes BMI)
data_eda <- read.csv("preprocessed_eda.csv")

data_eda$Obesity_Level <- factor(data_eda$Obesity_Level, 
                                 levels = c("Insufficient Weight", "Normal Weight", 
                                            "Overweight Level I", "Overweight Level II", 
                                            "Obesity Type I", "Obesity Type II", "Obesity Type III"),
                                 ordered = TRUE)

# 6. Skewness
# Note: Using data_eda because data_model no longer has the BMI column
cat("Skewness of Age:", skewness(data_eda$Age), "\n")
cat("Skewness of BMI:", skewness(data_eda$BMI), "\n")
cat("Skewness of Diet_score:", skewness(data_eda$Diet_score), "\n")

# 7. Chi-square Test
table_family <- table(data_eda$Family_history_with_overweight, data_eda$Obesity_Level)
chi_test_family <- chisq.test(table_family)
print(chi_test_family)

df_eda <- as.data.frame(data_eda)

# 8. Baseline Characteristics
table_1 <- df_eda[, c("Age", "Gender", "BMI", "Diet_score", "Physical_activity", 
                      "Family_history_with_overweight", "Obesity_Level")] %>%
  tbl_summary(
    by = Obesity_Level, # Column to group by
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})", # Use Median because data is skewed
      all_categorical() ~ "{n} ({p}%)"             # Use Frequency and %
    ),
    missing_text = "Missing data"
  ) %>%
  add_p() %>% # Automatically run statistical tests (Kruskal-Wallis / Chi-square) to compare groups
  modify_header(label = "**Characteristic/Variable**")

# Display Table 1
table_1


# Load plotting libraries
# install.packages(c("ggplot2", "corrplot", "dplyr")) # Uncomment this line if not installed
library(ggplot2)
library(corrplot)
library(dplyr)

# ---------------------------------------------------------
# CHART 1: Lifestyle Score Density (Violin + Boxplot)
# Purpose: Highlight the "Epidemiological Paradox" of Diet_score
# ---------------------------------------------------------
plot_1 <- ggplot(data_eda, aes(x = Obesity_Level, y = Diet_score, fill = Obesity_Level)) +
  geom_violin(alpha = 0.5, color = NA) + # Draw the distribution shape (Violin)
  geom_boxplot(width = 0.2, fill = "white", color = "black", alpha = 0.8) + # Add Boxplot in the middle
  labs(title = "Chart 1: Distribution of Diet Score by Obesity Level",
       x = "Obesity Level",
       y = "Healthy Lifestyle Score (0 - 9)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1), # Tilt X-axis text for readability
        legend.position = "none")

dev.new()
print(plot_1)

# ---------------------------------------------------------
# CHART 2: Family History Proportion (100% Stacked Bar Chart)
# Purpose: Demonstrate the absolute power of Genetics
# ---------------------------------------------------------
plot_2 <- ggplot(data_eda, aes(x = Obesity_Level, fill = Family_history_with_overweight)) +
  geom_bar(position = "fill", color = "white") + # position="fill" to scale to 100% proportions
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Chart 2: Proportion of Family History of Overweight by Obesity Level",
       x = "Obesity Level",
       y = "Percentage (%)",
       fill = "Family History?") +
  scale_fill_manual(values = c("No" = "#56B4E9", "Yes" = "#E69F00")) + # Choose prominent blue/orange colors
  theme_light() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

dev.new()
print(plot_2)

# ---------------------------------------------------------
# CHART 3: Lifestyle Habits Correlation Matrix (Heatmap)
# Purpose: Check for multicollinearity & relationships between behaviors
# ---------------------------------------------------------
# Filter out numeric columns to calculate correlation
num_cols <- data_eda %>% select(where(is.numeric))
cor_matrix <- cor(num_cols, use = "complete.obs")

# Open a slightly larger graphics window if needed, then plot
cat("\nDrawing Chart 3: Correlation Heatmap...\n")
corrplot(cor_matrix, 
         method = "color",       # Color the squares
         type = "upper",         # Draw only the upper half to reduce clutter
         order = "hclust",       # Cluster similar habits
         addCoef.col = "black",  # Show correlation coefficients
         number.cex = 0.7,       # Adjust number size
         tl.col = "black",       # Black axis text
         tl.srt = 45,            # Tilt text 45 degrees
         title = "Chart 3: Correlation Heatmap",
         mar = c(0,0,2,0))       # Adjust margins so text isn't cut off