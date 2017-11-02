

library(tidyverse)
library(janitor)
library(glmnet)

# Import training data set
data <- read.csv("train.csv", header = T, stringsAsFactors = F) %>%
  select(-Name, -Ticket, -Cabin) %>%
  clean_names()
data[data$sex=="male", "sex"] <- 0
data[data$sex=="female", "sex"] <- 1
data$sex <- as.numeric(data$sex)
# Dummy code the nominal variable 'Embarked' as multiple binary columns
data <- data %>%
  mutate(embarked_s = ifelse(embarked=="S", 1, 0), 
         embarked_c = ifelse(embarked=="C", 1, 0), 
         embarked_q = ifelse(embarked=="Q", 1, 0)) %>%
  select(-embarked)

# Replace NA values in the age column with the median age
data[is.na(data$age)==T, "age"] <- 28
data <- as.matrix(data)

# Randomly subset the data into a train and test set
train_index <- sample(1:nrow(data), .66*nrow(data))
train.x <- data[train_index, c(3:11)]
train.y <- as.factor(data[train_index, 2])
test.x <- data[-train_index, c(3:11)]
test.y <- as.factor(data[-train_index, 2])

# Train each type of regularized regression model using cross validation
mod.ridge <- cv.glmnet(train.x, train.y, type.measure="mse", alpha=0, family="binomial")
mod.lasso <- cv.glmnet(train.x, train.y, type.measure="mse", alpha=1, family="binomial")
mod.elasticnet <- cv.glmnet(train.x, train.y, type.measure="mse", alpha=.5, family="binomial")

# Generate predicted values for the binary 'survived' outcome variable
pred.ridge <- predict(object=mod.ridge, s=mod.ridge$lambda.1se, newx=test.x, type='response')
pred.lasso <- predict(object=mod.lasso, s=mod.lasso$lambda.1se, newx=test.x, type='response')
pred.elasticnet <- predict(object=mod.elasticnet, s=mod.elasticnet$lambda.1se, newx=test.x, type='response')

# Calculate the mean squared error for each model
mse.ridge <- mean((pred.ridge - as.numeric(as.character(test.y)))^2)
mse.lasso <- mean((pred.lasso - as.numeric(as.character(test.y)))^2)
mse.elasticnet <- mean((pred.elasticnet - as.numeric(as.character(test.y)))^2)

# Results
mse.ridge # 0.1617886
mse.lasso # 0.1663584
mse.elasticnet # 0.1659667
