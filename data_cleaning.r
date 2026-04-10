data <- read.csv(file = "ObesityDataSet_raw_and_data_sinthetic.csv", header = TRUE, sep = "," ,stringsAsFactors = FALSE)

#Rename columns for easier reference
colnames(data) <- c("Gender", "Age", "Height", "Weight", "Family_history_with_overweight", "high_calorie_food_eat", "vegetable_eat_daily", "main_meals_daily",
 "food_between_meals", "SMOKE", "Water_daily", "Calories_monitoring",
  "Physical_activity", "Electronic_usage_daily", "Alcohol_consumption", "Method_of_Transport", "Obesity_Level")

#Check each value 
  table(data$Alcohol_consumption)

#BMI Calculation  ; data$BMI = weight (kg) / (height (m))^2
data$BMI <- data$Weight / ((data$Height) ^ 2)

#round BMI to 2 decimal places
data$BMI <- round(data$BMI, 2)

# Round continuous values to 2 decimal places, Age to 0 decimal places
cols_to_round <- c("Height", "Weight", "Water_daily", "Physical_activity",  "vegetable_eat_daily", "Electronic_usage_daily")
data[ , cols_to_round] <- lapply(data[ , cols_to_round], round, 2)
data$Age <- floor(data$Age)
data$main_meals_daily <- ceiling(data$main_meals_daily)
table(data$Weight)

#Create Age_group feature
data$Age_group <- cut(
  data$Age,
  breaks = c(0, 18, 25, 35, 50, Inf),
  labels = c("Teen", "Young adult", "Adult", "Middle age", "Older")
)
table(data$Age_group)
# Export preprocessed data
write.csv(data, "preprocessed.csv", row.names = FALSE)

#Diet score calculation based on high_calorie_food_eat, vegetable_eat_daily, Physical_activity, Alcohol_consumption, water_daily, method_of_transport, food_between_meals, SMOKE, Calories_monitoring, main_meals_daily by assigning 1 point for each healthy behavior and 0 points for unhealthy behavior, then summing the points to get a total diet score for each individual. The higher the score, the healthier the diet.
data$Diet_score <- 0
data$Diet_score <- data$Diet_score + ifelse(data$high_calorie_food_eat == 0, 1, 0) # 1 point for not eating high calorie food frequently
data$Diet_score <- data$Diet_score + ifelse(data$vegetable_eat_daily >= 3, 1, 0) # 1 point for eating vegetables daily 3 or more times
data$Diet_score <- data$Diet_score + ifelse(data$Physical_activity >= 3, 1, 0) # 1 point for having physical activity 3 or more times a week
data$Diet_score <- data$Diet_score + ifelse(data$Alcohol_consumption == "no", 1, 0) # 1 point for not drinking alcohol
data$Diet_score <- data$Diet_score + ifelse(data$Water_daily >= 2, 1, 0) # 1 point for drinking water daily 2 or more liters
data$Diet_score <- data$Diet_score + ifelse(data$Method_of_Transport %in% c("Walking", "Bike"), 1, 0) # 1 point for using active transportation
data$Diet_score <- data$Diet_score + ifelse(data$food_between_meals == "no", 1, 0) # 1 point for not eating food between meals
data$Diet_score <- data$Diet_score + ifelse(data$SMOKE == "no", 1, 0) # 1 point for not smoking
data$Diet_score <- data$Diet_score + ifelse(data$Calories_monitoring == "yes", 1, 0) # 1 point for monitoring calories daily
data$Diet_score <- data$Diet_score + ifelse(data$main_meals_daily >= 3, 1, 0) # 1 point for having 3 or more main meals daily
table(data$Diet_score)
#Add Diet_score into preprocessed data
write.csv(data, "preprocessed.csv", row.names = FALSE)


#round numbers of Electronic_usage_daily to 1 decimal places
data$Electronic_usage_daily <- round(data$Electronic_usage_daily, 1)
table(data$Electronic_usage_daily)

#Name,Role,Type,Units,Description,Missing Values
#Gender,feature,categorical,,,No
#Age,feature,continuous,,,No
#Height,feature,continuous,,,No
#Weight,feature,continuous,,,No
#family_history_with_overweight,feature,binary,,Has a family member suffered or suffers from overweight?,No
#FAVC,feature,binary,,Do you eat high caloric food frequently?,No
#FCVC,feature,integer,,Do you usually eat vegetables in your meals?,No
#NCP,feature,continuous,,How many main meals do you have daily?,No
#CAEC,feature,categorical,,Do you eat any food between meals?,No
#SMOKE,feature,binary,,Do you smoke?,No
#CH2O,feature,continuous,,How much water do you drink daily?,No
#SCC,feature,binary,,Do you monitor the calories you eat daily?,No
#FAF,feature,continuous,,How often do you have physical activity?,No
#TUE,feature,integer,,How much time do you use technological devices such as cell phone, videogames, television, computer and others?,No
#CALC,feature,categorical,,How often do you drink alcohol?,No
#MTRANS,feature,categorical,,Which transportation do you usually use?,No
#NObeyesdad,target,categorical,,Obesity level,No
summary(data)
head(data)

#I can change almost all information into numeric values, and soon I will do that to make graphs and charts easier to understand, but it will be saved secretly in the background, and I will keep the original data for reference.

