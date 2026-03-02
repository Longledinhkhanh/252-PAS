data2 <- read.csv(file = "preprocessed.csv", header = TRUE, sep = ",") # Read the preprocessed data


#Helper function to extract summary statistics
get_summary_stats <- function(data, column_name) {
  column_data <- data[[column_name]]
  stats <- list(
    mean = mean(column_data, na.rm = TRUE),
    median = median(column_data, na.rm = TRUE),
    sd = sd(column_data, na.rm = TRUE),
    var = var(column_data, na.rm = TRUE),
    min = min(column_data, na.rm = TRUE),
    max = max(column_data, na.rm = TRUE),
    quantiles = quantile(column_data, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  )
  return(stats)
}

#Example usage: Get summary statistics for Age
age_stats <- get_summary_stats(data2, "Age")
print(age_stats)

#histogram for Age using Gaussian density curve
hist(data2$Age, breaks=15, probability=TRUE, main="Histogram of Age with Gaussian Density Curve", xlab="Age", col="lightblue", xlim=c(0,60), ylim=c(0,0.1))
curve(dnorm(x, mean=mean(data2$Age, na.rm=TRUE), sd=sd(data2$Age, na.rm=TRUE)), add=TRUE, col="red", lwd=2)
dev.new()

#histogram for vegetable_eat_daily using Gaussian density curve
hist(data2$vegetable_eat_daily, breaks= 7, probability=TRUE, main="Histogram of Vegetable Eat Daily with Gaussian Density Curve", xlab="Vegetable Eat Daily", col="lightgreen", xlim=c(0,5), ylim=c(0,1))
curve(dnorm(x, mean=mean(data2$vegetable_eat_daily, na.rm=TRUE), sd=sd(data2$vegetable_eat_daily, na.rm=TRUE)), add=TRUE, col="blue", lwd=2)
dev.new()
#graph Obesity Level counts
barplot(table(data2$Obesity_Level), main="Obesity Level Counts", xlab="Obesity Level", ylab="Counts", col="lightcoral")
#add number on top of bars
obesity_counts <- table(data2$Obesity_Level)
text(x=barplot(table(data2$Obesity_Level)), y=obesity_counts, labels=obesity_counts, pos=3, cex=0.8)
dev.new()
# Display frequency table for vegetable_eat_daily variable
hist(data2$vegetable_eat_daily, main="Frequency of Vegetable Eat Daily", xlab="Vegetable Eat Daily", col="lightgreen")
dev.new()
#Comparison of Physical activity values and TUE values using boxplots
boxplot(data2$Physical_activity, data2$TUE, names=c("Physical Activity", "TUE"), main="Boxplot of Physical Activity and TUE", col=c("lightblue", "lightgreen"))
dev.new()
#Comparison of good diet score vs bad diet score using boxplots
boxplot(data2$Diet_score ~ data2$Obesity_Level, main="Boxplot of Diet Score by Obesity Level", xlab="Obesity Level", ylab="Diet Score", col="lightpink")
dev.new()
#Pie chart for Method_of_Transport variable
mtrans_counts <- table(data2$Method_of_Transport)
pie(mtrans_counts, labels=names(mtrans_counts), main="Pie Chart of Method of Transport", col=rainbow(length(mtrans_counts)))
dev.new()   
#Age group and BMI relationship using bar plot
age_bmi_table <- table(data2$Age_group, data2$Obesity_Level)
barplot(age_bmi_table, beside=TRUE, main="Age Group vs Obesity Level", xlab="Age Group", ylab="Counts", col=rainbow(nrow(age_bmi_table)), legend=rownames(age_bmi_table))
dev.new()

#Scatter plot of Age vs Weight with regression line
  ggplot(data2, aes(x=Age, y=Weight)) +
    geom_point(color='blue', alpha=0.5) +
    geom_smooth(method='lm', color='red') +
    labs(title='Scatter Plot of Age vs Weight with Regression Line', x='Age', y='Weight') +
    theme_minimal()
dev.new()

#Boxplot of BMI by Obesity Category
ggplot(data2, aes(x = Obesity_Level, y = BMI)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(title = "BMI by Obesity Category", x = "Obesity Level", y = "BMI") +
  theme_minimal()
dev.new()



#Obesity type I, II and III vs Gender using stacked bar plot
data_obese <- subset(data2, Obesity_Level %in% c("Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))
ggplot(data_obese, aes(x = Gender, fill = Obesity_Level)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Obesity Level vs Gender", x = "Gender", y = "Count") +
  theme_minimal()
dev.new()

#Normal weight vs Gender using stacked bar plot
data_normal <- subset(data2, Obesity_Level == "Normal_Weight")
ggplot(data_normal, aes(x = Gender, fill = Obesity_Level)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Normal Weight vs Gender", x = "Gender", y = "Count") +
  theme_minimal()
dev.new()

#Insufficient weight vs Gender using stacked bar plot
data_insufficient <- subset(data2, Obesity_Level == "Insufficient_Weight")
ggplot(data_insufficient, aes(x = Gender, fill = Obesity_Level)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Insufficient Weight vs Gender", x = "Gender", y = "Count") +
  theme_minimal() 
dev.new()

#Overweight Level I and II vs Gender using stacked bar plot
data_overweight <- subset(data2, Obesity_Level %in% c("Overweight_Level_I", "Overweight_Level_II"))
ggplot(data_overweight, aes(x = Gender, fill = Obesity_Level)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Overweight Level vs Gender", x = "Gender", y = "Count") +
  theme_minimal()
dev.new()

#Alcohol consumption vs Obesity Type I, II and III using stacked bar plot
data_obese <- subset(data2, Obesity_Level %in% c("Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))
ggplot(data_obese, aes(x = Alcohol_consumption, fill = Obesity_Level)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Alcohol Consumption vs Obesity Level", x = "Alcohol Consumption", y = "Count") +
  theme_minimal()
dev.new()

#Alcohol consumption vs Normal weight using stacked bar plot
data_normal <- subset(data2, Obesity_Level == "Normal_Weight") 
ggplot(data_normal, aes(x = Alcohol_consumption, fill = Obesity_Level)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Alcohol Consumption vs Normal Weight", x = "Alcohol Consumption", y = "Count") +
  theme_minimal()
dev.new()

#Alcohol consumption vs Insufficient weight using stacked bar plot 
data_insufficient <- subset(data2, Obesity_Level == "Insufficient_Weight")
ggplot(data_insufficient, aes(x = Alcohol_consumption, fill = Obesity_Level)) +
  geom_bar (position = "stack", color = "black", fill = "lightgreen") +
  labs(title= "Alcohol Consumption vs Insufficient Weight", x = "Alcohol Consumption", y = "Count") +
  theme_minimal()
dev.new()


#Obesity type I, II and III Vs Age group using chart, with age group goes from younger to older
data_obese <- subset(data2, Obesity_Level %in% c("Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))
ggplot(data_obese, aes(x = Age_group, fill = Obesity_Level)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Obesity Level vs Age Group", x = "Age Group", y = "Count") +
  theme_minimal() +
  scale_x_discrete(limits = c("Teen", "Young adult", "Adult", "Middle age", "Older")) +
  ylim(c(0, 600))
dev.new()

#Overweight Level I and II Vs Age group using chart, with age group goes from younger to older
data_overweight <- subset(data2, Obesity_Level %in% c("Overweight_Level_I", "Overweight_Level_II"))
ggplot(data_overweight, aes(x = Age_group, fill = Obesity_Level)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Overweight Level vs Age Group", x = "Age Group", y = "Count") +
  theme_minimal() +
  scale_x_discrete(limits = c("Teen", "Young adult", "Adult", "Middle age", "Older")) +
  ylim(c(0, 400))
dev.new()

#Normal weight Vs Age group using chart, with age group goes from younger to older
data_normal <- subset(data2, Obesity_Level == "Normal_Weight")
ggplot(data_normal, aes(x = Age_group, fill = Obesity_Level)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Normal Weight vs Age Group", x = "Age Group", y = "Count") +
  theme_minimal() +
  scale_x_discrete(limits = c("Teen", "Young adult", "Adult", "Middle age", "Older")) +
  ylim(c(0, 200))
dev.new()

# Insufficient weight Vs Age group using chart, with age group goes from younger to older
data_insufficient <- subset(data2, Obesity_Level == "Insufficient_Weight")
ggplot(data_insufficient, aes(x = Age_group, fill = Obesity_Level)) +
  geom_bar(position = "stack", color = "black") +
  labs(title = "Insufficient Weight vs Age Group", x = "Age Group", y = "Count") +
  theme_minimal() +
  scale_x_discrete(limits = c("Teen", "Young adult", "Adult", "Middle age", "Older")) +
  ylim(c(0, 10))


#Obesity type I, II and III vs Weight using density plot
data_obese <- subset(data2, Obesity_Level %in% c("Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))
ggplot(data_obese, aes(x = Weight, fill = Obesity_Level)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Weight by Obesity Level", x = "Weight", y = "Density") +
  theme_minimal() +
  xlim(c(60,150))

#ObesityLevel vs Physical activity using density plot
ggplot(data2, aes(x = Physical_activity, fill = Obesity_Level)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Physical Activity by Obesity Level", x = "Physical Activity", y = "Density") +
  theme_minimal() +
  xlim(c(-1,4 ))

#based on that, you can separate obesity level vs Physical activity into 4 density plot, with each obesity level has its own density plot

#use all data to create heat map of correlation between continuous variables, with color goes from blue to red, and add correlation values on top of each cell
library(ggplot2)
library(reshape2)
continuous_vars <- data2[, sapply(data2, is.numeric)]
cor_matrix <- cor(continuous_vars, use = "complete.obs")
cor_matrix_melt <- melt(cor_matrix)
ggplot(cor_matrix_melt, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1), space="Lab", name="Correlation") +
  geom_text(aes(label=round(value, 2)), color="black", size=4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  labs(title="Correlation Heatmap of Continuous Variables", x="", y="")


#KDE plot for height and weight
ggplot(data2, aes(x=Height, y=Weight)) +
  geom_point(color='blue', alpha=0.5) +
  geom_density2d(color='red') +
  labs(title='KDE Plot of Height and Weight', x='Height', y='Weight') +
  theme_minimal()

  #KDE plot for Obesity typeI, II and III (combined) with height and weight
data_obese <- subset(data2, Obesity_Level %in% c("Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))
ggplot(data_obese, aes(x=Weight, y=Height)) +
  geom_point(color='blue', alpha=0.5) +
  geom_density2d(color='red') +
  labs(title='KDE Plot of Height and Weight for Obesity Type I, II and III', x='Weight', y='Height') +
  xlim(c(60,150)) +
  ylim(c(1.4,2)) +
  theme_minimal()

#KDE plot for Normal weight with height and weight
data_normal <- subset(data2, Obesity_Level == "Normal_Weight")
ggplot(data_normal, aes(x=Weight, y=Height)) +
  geom_point(color='blue', alpha=0.5) +
  geom_density2d(color='red') +
  labs(title='KDE Plot of Height and Weight for Normal Weight', x='Weight', y='Height') +
  xlim(c(25,100)) +
  ylim(c(1.4,2)) +
  theme_minimal()

#KDE plot for Insufficient weight with height and weight
data_insufficient <- subset(data2, Obesity_Level == "Insufficient_Weight")
ggplot(data_insufficient, aes(x=Weight, y=Height)) +
  geom_point(color='blue', alpha=0.5) +
  geom_density2d(color='red') +
  labs(title='KDE Plot of Height and Weight for Insufficient Weight', x='Weight', y='Height') +
  xlim(c(25,80)) +
  ylim(c(1.4,2)) +
  theme_minimal()

#KDE plot for Overweight Level I and II (combined) with height and weight
data_overweight <- subset(data2, Obesity_Level %in% c("Overweight_Level_I", "Overweight_Level_II"))
ggplot(data_overweight, aes(x=Weight, y=Height)) +
  geom_point(color='blue', alpha=0.5) +
  geom_density2d(color='red') +
  labs(title='KDE Plot of Height and Weight for Overweight Level I and II', x='Weight', y='Height') +
  xlim(c(40, 120)) +
  ylim(c(1.4,2)) +
  theme_minimal()

  #distribution of BMI by Obesity level using bar plot
ggplot(data2, aes(x = Obesity_Level, y = BMI)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightblue", color = "black") +
  labs(title = "Average BMI by Obesity Level", x = "Obesity Level", y = "Average BMI") +
  theme_minimal()

#distribution of Age by Obesity level using bar plot
library(ggplot2)
ggplot(data2, aes(x = Obesity_Level, y = Age)) +
  geom_bar(stat = "summary", fun = "mean", fill = "lightgreen", color = "black") +
  labs(title = "Average Age by Obesity Level", x = "Obesity Level", y = "Average Age") +
  theme_minimal()

#Bar chart for Transport method by Obesity level
ggplot(data2, aes(x = Method_of_Transport, fill = Obesity_Level)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Transport Method by Obesity Level", x = "Transport Method", y = "Count") +
  ylim(c(0, 350)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

#Bar chart for Alcohol consumption by Obesity level
ggplot(data2, aes(x = Alcohol_consumption, fill = Obesity_Level)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Alcohol Consumption by Obesity Level", x = "Alcohol Consumption", y = "Count") +
  ylim(c(0, 350)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

#Density chart for Physical activity vs Electronic usage daily by Obesity Type I, II and III combined
data_obese <- subset(data2, Obesity_Level %in% c("Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))
ggplot(data_obese, aes(x = Physical_activity, fill = Electronic_usage_daily)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Physical Activity vs Electronic Usage Daily for Obesity Type I, II and III", x = "Physical Activity", y = "Density") +
  theme_minimal() +
  xlim(c(-1,4 )) +
  scale_fill_gradient(low = "lightblue", high = "red")


#Diet score distribution by Obesity level using box plot
library(ggplot2)
ggplot(data2, aes(x = Obesity_Level, y = Diet_score)) +
  geom_boxplot(fill = "lightpink", color = "black") +
  labs(title = "Diet Score Distribution by Obesity Level", x = "Obesity Level", y = "Diet Score") +
  theme_minimal()

#Diet score distribution by Obesity I, II and III using density plot
data_obese <- subset(data2, Obesity_Level %in% c("Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))
ggplot(data_obese, aes(x = Diet_score, fill = Obesity_Level)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Diet Score by Obesity Level (I, II and III)", x = "Diet Score", y = "Density") +
  theme_minimal()

#Diet score distribution by Normal weight using density plot
data_normal <- subset(data2, Obesity_Level == "Normal_Weight")
ggplot(data_normal, aes(x = Diet_score, fill = Obesity_Level)) +
  geom_density(alpha = 0.5, fill = "lightgreen") +
  labs(title = "Density Plot of Diet Score for Normal Weight", x = "Diet Score", y = "Density") +
  theme_minimal()