data <- read.csv(file = "ObesityDataSet_raw_and_data_sinthetic.csv", header = TRUE, sep = "," ,stringsAsFactors = FALSE)

#Convert Gender to binary value (0 = MALE, 1 = Female) (uses regex pattern matching)
data$Gender <- ifelse(data$Gender == "Female", 1, 0)
table(data$Gender)

#Convert family_history_with_overweight to binary value (0 = NO, 1 = YES ) (uses regex pattern matching)
data$family_history_with_overweight <- ifelse(data$family_history_with_overweight == "yes", 1, 0)
table(data$family_history_with_overweight)

#Convert FAVC to binary value (0 = no, 1 = yes) (uses regex pattern matching)
data$FAVC <- ifelse(data$FAVC == "yes", 1, 0)
table(data$FAVC)

#Convert SMOKE to binary value (0 = no, 1 = yes) (uses regex pattern matching)
data$SMOKE <- ifelse(data$SMOKE == "yes", 1, 0)
table(data$SMOKE)

#Convert SCC to binary value (0 = no, 1 = yes) (uses regex pattern matching)
data$SCC <- ifelse(data$SCC == "yes", 1, 0)
table(data$SCC)


#Cinvert CAEC to ordinal values ( include blank as 0, no/No as 1, sometimes/Sometimes as 2, frequently/Frequently as 3, always/Always as 4)
data$CAEC <- tolower(trimws(data$CAEC))
data$CAEC <- ifelse(data$CAEC == "", 0,
                    ifelse(data$CAEC == "no", 1,
                           ifelse(data$CAEC == "sometimes", 2,
                                  ifelse(data$CAEC == "frequently", 3,
                                         ifelse(data$CAEC == "always", 4, NA)))))
table(data$CAEC)

#CALC to ordinal values ( include blank as 0, no/No as 1, sometimes/Sometimes as 2, frequently/Frequently as 3, always/Always as 4)
data$CALC <- tolower(trimws(data$CALC))
data$CALC <- ifelse(data$CALC == "", 0,
                    ifelse(data$CALC == "no", 1,
                           ifelse(data$CALC == "sometimes", 2,
                                  ifelse(data$CALC == "frequently", 3,
                                         ifelse(data$CALC == "always", 4, NA)))))
table(data$CALC)

#MTRANS to ordinal values (include blank as 0, Walking as 1, Bike as 2, Motorbike as 3, Public_Transportation as 4, Car as 5)
data$MTRANS <- tolower(trimws(data$MTRANS))
data$MTRANS <- ifelse(data$MTRANS == "", 0,
                      ifelse(data$MTRANS == "walking", 1,
                             ifelse(data$MTRANS == "bike", 2,
                                    ifelse(data$MTRANS == "motorbike", 3,
                                           ifelse(data$MTRANS == "public_transportation", 4,
                                                  ifelse(data$MTRANS == "automobile", 5, NA))))))
table(data$MTRANS)

#NObeyesdad to ordinal values (blank as 0, Insufficient Wight as 1, Normal Weight as 2, Overweight Level I as 3, Overweight Level II as 4, Obesity Type I as 5, Obesity Type II as 6, Obesity Type III as 7)
data$NObeyesdad <- tolower(trimws(data$NObeyesdad))
data$NObeyesdad <- ifelse(data$NObeyesdad == "", 0,
                          ifelse(data$NObeyesdad == "insufficient_weight", 1,
                                 ifelse(data$NObeyesdad == "normal_weight", 2,
                                        ifelse(data$NObeyesdad == "overweight_level_i", 3,
                                               ifelse(data$NObeyesdad == "overweight_level_ii", 4,
                                                      ifelse(data$NObeyesdad == "obesity_type_i", 5,
                                                             ifelse(data$NObeyesdad == "obesity_type_ii", 6,
                                                                    ifelse(data$NObeyesdad == "obesity_type_iii", 7, NA))))))))
table(data$NObeyesdad)

# Convert all values to numeric
data <- as.data.frame(sapply(data, as.numeric))

# Round continuous values to 2 decimal places, Age to 0 decimal places
cols_to_round <- c("Height", "Weight", "CH2O", "FAF",  "FCVC", "TUE")
data[ , cols_to_round] <- lapply(data[ , cols_to_round], round, 2)
data$Age <- floor(data$Age)
data$NCP <- ceiling(data$NCP)

#Create new feature Diet_score as a risk score distribution based on FCVC, FAVC, CH2O, SCC, SMOKE, and FAF
data$Diet_score <- data$FCVC + (4 - data$FAVC) + data$CH2O + (4 - data$SCC) + (4 - data$SMOKE) + data$FAF 
table(data$Diet_score)

#Create Age_group feature
data$Age_group <- cut(
  data$Age,
  breaks = c(0, 18, 25, 35, 50, Inf),
  labels = c("Teen", "Young adult", "Adult", "Middle age", "Older")
)

#BMI Calculation  ; data$BMI = weight (kg) / (height (m))^2
data$BMI <- data$Weight / ((data$Height) ^ 2)

#round BMI to 2 decimal places
data$BMI <- round(data$BMI, 2)

# Export preprocessed data
write.csv(data, "preprocessed.csv", row.names = FALSE)



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
