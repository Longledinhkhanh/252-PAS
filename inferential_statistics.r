# Load additional libraries for inferential plotting
library(ggplot2)
library(dplyr)
library(broom)      # To extract Odds Ratios from the model
library(ggridges)   # To draw the Ridgeline Plot

# =========================================================================
# CHART 1: FOREST PLOT - POWER OF PREDICTORS (ODDS RATIO)
# =========================================================================
cat("\nProcessing data and drawing Forest Plot...\n")

# Get model coefficients, calculate Odds Ratios (exponentiate = TRUE) and Confidence Intervals
tidy_model <- tidy(model_ordinal, conf.int = TRUE, exponentiate = TRUE) %>%
  # Remove boundary markers (cutpoints/intercepts containing "|")
  filter(!grepl("\\|", term))

# Draw Forest Plot
plot_forest <- ggplot(tidy_model, aes(x = estimate, y = reorder(term, estimate))) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), 
                  color = "#0072B2", size = 0.8, linewidth = 1.2) +
  # Null line at x = 1
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 1) +
  # Convert X-axis to logarithmic scale for better visibility if OR is too large
  scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10, 15)) + 
  labs(title = "Chart 1: Forest Plot - Impact of Habits on Obesity",
       subtitle = "Odds Ratio (OR) and 95% Confidence Interval (Log Scale)",
       x = "Odds Ratio (OR > 1: Increases obesity risk | OR < 1: Decreases risk)",
       y = "Independent Variables") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

dev.new()
print(plot_forest)

# =========================================================================
# CHART 2: CONFUSION MATRIX HEATMAP
# =========================================================================

cat("\nDrawing Confusion Matrix Heatmap...\n")

# Convert Confusion Matrix table to Data Frame
cm_data <- as.data.frame(conf_matrix$table)

# Draw Heatmap
plot_cm <- ggplot(cm_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  # Create color gradient from white to dark red
  scale_fill_gradient(low = "white", high = "#D55E00") +
  # Print numbers on each tile
  geom_text(aes(label = Freq), color = "black", size = 4, fontface = "bold") +
  labs(title = "Chart 2: Confusion Matrix Heatmap",
       subtitle = "Visualizing the blurry boundaries between Overweight and Obesity",
       x = "Actual (Reference)",
       y = "Model Prediction",
       fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 14))

dev.new()
print(plot_cm)

# =========================================================================
# CHART 3: RIDGELINE PLOT - AGE AND OBESITY
# =========================================================================
cat("\nDrawing Ridgeline Plot for Age...\n")

plot_ridge <- ggplot(data_model, aes(x = Age, y = Obesity_Level, fill = Obesity_Level)) +
  geom_density_ridges(alpha = 0.7, scale = 1.5, color = "white") +
  # Choose a nice color palette
  scale_fill_viridis_d(option = "plasma") + 
  labs(title = "Chart 3: Ridgeline Plot - Age Shift Across Obesity Levels",
       subtitle = "Obesity severity increases proportionally with age",
       x = "Age",
       y = "Obesity Level") +
  theme_ridges() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        axis.title.x = element_text(hjust = 0.5))

dev.new()
print(plot_ridge)

# Install brant library if not available: install.packages("brant")
library(brant)

# Run Brant test for the model
cat("Brant Test for Proportional Odds assumption:\n")
brant(model_ordinal)


# Install car library if not available: install.packages("car")
library(car)

# Run VIF check
# Since polr doesn't directly support vif(), we temporarily run a standard linear model to get the indices
model_vif_check <- lm(as.numeric(Obesity_Level) ~ Diet_score + Age + Gender + 
                        Family_history_with_overweight + Physical_activity + 
                        food_between_meals + Alcohol_consumption, data = train_data)

cat("Multicollinearity Indices (VIF):\n")
vif(model_vif_check)

# Create null model (Intercept only)
model_null <- polr(Obesity_Level ~ 1, data = train_data, Hess = TRUE)

# Compare the 2 models using Likelihood Ratio Test (ANOVA)
cat("Overall Goodness-of-Fit Test (Likelihood Ratio Test):\n")
anova(model_null, model_ordinal)

# Calculate Top-2 Accuracy from Confusion Matrix (Maximum deviation of 1 level)
cm_table <- conf_matrix$table

# Get values on the diagonal (100% correct predictions)
exact_matches <- sum(diag(cm_table))

# Get values one cell away from the diagonal (Off by 1 level)
off_by_one <- sum(diag(cm_table[-1, -ncol(cm_table)])) + sum(diag(cm_table[-nrow(cm_table), -1]))

total_obs <- sum(cm_table)
top_2_accuracy <- (exact_matches + off_by_one) / total_obs

cat(sprintf("Extended Accuracy (Top-2 Accuracy - Max deviation of 1 level): %.2f%%\n", top_2_accuracy * 100))