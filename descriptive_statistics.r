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


#histogram for FCVC using Gaussian density curve
hist(data2$FCVC, breaks= 7, probability=TRUE, main="Histogram of FCVC with Gaussian Density Curve", xlab="FCVC", col="lightgreen", xlim=c(0,5), ylim=c(0,1))
curve(dnorm(x, mean=mean(data2$FCVC, na.rm=TRUE), sd=sd(data2$FCVC, na.rm=TRUE)), add=TRUE, col="blue", lwd=2)

#graph Obesity Level counts
barplot(table(data2$NObeyesdad), main="Obesity Level Counts", xlab="Obesity Level", ylab="Counts", col="lightcoral")

# Display frequency table for FCVC variable
hist(data2$FCVC, main="Frequency of FCVC", xlab="FCVC", col="lightgreen")

#Comparison of FAF values and TUE values using boxplots
boxplot(data2$FAF, data2$TUE, names=c("FAF", "TUE"), main="Boxplot of FAF and TUE", col=c("lightblue", "lightgreen"))

#Comparison of good diet score vs bad diet score using boxplots
boxplot(data2$Diet_score ~ data2$NObeyesdad, main="Boxplot of Diet Score by Obesity Level", xlab="Obesity Level", ylab="Diet Score", col="lightpink")

#Pie chart for MTRANS variable
mtrans_counts <- table(data2$MTRANS)
pie(mtrans_counts, labels=names(mtrans_counts), main="Pie Chart of MTRANS", col=rainbow(length(mtrans_counts)))

#Risk score distribution based on BMI, FAVC, FCVC, FAF, TUE, and family history
hist(data2$Diet_score, breaks=10, main="Histogram of Diet Score", xlab="Diet Score", col="lightyellow")

#Age group and BMI relationship using bar plot
age_bmi_table <- table(data2$Age_group, data2$NObeyesdad)
barplot(age_bmi_table, beside=TRUE, main="Age Group vs Obesity Level", xlab="Age Group", ylab="Counts", col=rainbow(nrow(age_bmi_table)), legend=rownames(age_bmi_table))

library(ggplot2)


#Scatter plot of Age vs Weight with regression line
ggplot(data2, aes(x=Age, y=Weight)) +
  geom_point(color='blue', alpha=0.5) +
  geom_smooth(method='lm', color='red') +
  labs(title='Scatter Plot of Age vs Weight with Regression Line', x='Age', y='Weight') +
  theme_minimal()

#Boxplot of BMI by Obesity Category
ggplot(data2, aes(x = NObeyesdad, y = BMI)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(title = "BMI by Obesity Category", x = "Obesity Level", y = "BMI")
  theme_minimal()


