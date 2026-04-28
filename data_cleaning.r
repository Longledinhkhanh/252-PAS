data <- read.csv(file = "ObesityDataSet_raw_and_data_sinthetic.csv", header = TRUE, sep = "," ,stringsAsFactors = FALSE)

#Rename columns for easier reference
colnames(data) <- c("Gender", "Age", "Height", "Weight", "Family_history_with_overweight", "high_calorie_food_eat", "vegetable_eat_daily", "main_meals_daily",
 "food_between_meals", "SMOKE", "Water_daily", "Calories_monitoring",
  "Physical_activity", "Electronic_usage_daily", "Alcohol_consumption", "Method_of_Transport", "Obesity_Level")


data$Gender <- factor(data$Gender, levels =c("Female", "Male"), labels = c("Female", "Male"))


binary_cols <- c("Family_history_with_overweight", "high_calorie_food_eat", "SMOKE", "Calories_monitoring")

for (col in binary_cols) {
  data[[col]] <- factor(data[[col]],
  levels = c("no", "yes"),
  labels = c("No", "Yes"))
}


data$food_between_meals_num <- as.numeric(factor(data$food_between_meals,
  levels = c("no", "Sometimes", "Frequently", "Always" ),
  ordered = TRUE)) -1

data$food_between_meals <- factor(data$food_between_meals,
  levels = c("no", "Sometimes", "Frequently", "Always" ),
  labels = c("No", "Sometimes", "Frequently", "Always" ),
  ordered = TRUE)

# In Alcohol Comsumption, because only 1 person always drink, which makes data inconsistent in large number, we change it into Frequently.
data$Alcohol_consumption[data$Alcohol_consumption =="Always"] <- "Frequently"

data$Alcohol_consumption_num <- as.numeric(factor(data$Alcohol_consumption,
  levels = c("no", "Sometimes", "Frequently" ),
  ordered = TRUE)) -1

data$Alcohol_consumption <- factor(data$Alcohol_consumption,
  levels = c("no", "Sometimes", "Frequently" ),
  labels = c("No", "Sometimes", "Frequently" ),
  ordered = TRUE)


data$Obesity_Level_num <- as.numeric(factor(data$Obesity_Level,
  levels = c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I", "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"),
  ordered = TRUE)) -1

data$Obesity_Level <- factor(data$Obesity_Level,
  levels = c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I", "Overweight_Level_II", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"),
  labels = c("Insufficient Weight", "Normal Weight", "Overweight Level I", "Overweight Level II", "Obesity Type I", "Obesity Type II", "Obesity Type III"),
  ordered = TRUE)

data$Method_of_Transport_num <- as.numeric(factor(data$Method_of_Transport,
  levels = c("Automobile", "Motorbike", "Public_Transportation", "Walking", "Bike"),
  ordered = TRUE)) -1

data$Method_of_Transport <- factor(data$Method_of_Transport,
  levels = c("Automobile", "Motorbike", "Public_Transportation", "Walking", "Bike"),
  labels = c("Automobile", "Motorbike", "Public Transportation", "Walking", "Bike"),
  ordered = TRUE)





# Round continuous values to 2 decimal places, Age to 0 decimal places
cols_to_round <- c("Height", "Weight", "Water_daily", "Physical_activity",  "vegetable_eat_daily", "Electronic_usage_daily")
data[ , cols_to_round] <- lapply(data[ , cols_to_round], round, 2)
data$Age <- floor(data$Age)
data$main_meals_daily <- ceiling(data$main_meals_daily)
data$Electronic_usage_daily <- round(data$Electronic_usage_daily, 1)

#BMI Calculation  ; data$BMI = weight (kg) / (height (m))^2
data$BMI <- round(data$Weight / ((data$Height) ^ 2), 2)


#Create Age_group feature
data$Age_group <- cut(
  data$Age,
  breaks = c(0, 18, 25, 35, 50, Inf),
  labels = c("Teen", "Young adult", "Adult", "Middle age", "Older"),
  ordered_result = TRUE)



#Diet score calculation based on high_calorie_food_eat, vegetable_eat_daily, Physical_activity, Alcohol_consumption, water_daily, method_of_transport, food_between_meals, SMOKE, Calories_monitoring, main_meals_daily by assigning 1 point for each healthy behavior and 0 points for unhealthy behavior, then summing the points to get a total diet score for each individual. The higher the score, the healthier the diet.
data$Diet_score <- rowSums(data.frame(
  ifelse(data$high_calorie_food_eat == "No", 1, 0),
  ifelse(data$vegetable_eat_daily >= 3, 1, 0),
  ifelse(data$Physical_activity >= 3, 1, 0),
  ifelse(data$Alcohol_consumption == "No", 1, 0),
  ifelse(data$Water_daily >= 2, 1, 0),
  ifelse(data$Method_of_Transport_num %in% c(3, 4), 1, 0),
  ifelse(data$food_between_meals == "No", 1, 0),
  ifelse(data$SMOKE == "No", 1, 0),
  ifelse(data$Calories_monitoring == "Yes", 1, 0),
  ifelse(data$main_meals_daily >= 3, 1, 0)
))

print(table(data$Diet_score))

# FILE FOR GRAPHS
write.csv(data, "preprocessed_eda.csv", row.names = FALSE)

#FILE FOR MODELS (REMOVE LEAKAGE DATA)
data_model <-subset(data, select = -c(Height, Weight, BMI))
write.csv(data_model, "preprocessed_model.csv", row.names = FALSE)


print(summary(data))
summary(data)


