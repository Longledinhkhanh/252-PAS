library(caTools)
library(pscl)
library(caret)
library(PRROC)
library(pROC)

#Make this reproducible
set.seed(123)

data <- read.csv("preprocessed.csv")
split <- sample.split(data$NObeyesdad, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

#fitting models
model1 <- glm(NObeyesdad ~ Age + Gender + Height + Weight + BMI + FAVC + FCVC + NCP + CAEC + SMOKE + CH2O + SCC + FAF + TUE + CALC + MTRANS + family_history_with_overweight + Diet_score + Age_group, data = train_data, family = binomial)
family = binomial(link = "logit")
summary(model1)

model2 <- glm(BMI ~ FCVC + FAF + TUE + Diet_score + Age_group, data = train_data, family = gaussian)
family = gaussian(link = "identity")
summary(model2)

model3 <- glm()