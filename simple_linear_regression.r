######################## problem1 ####################################################
# Load the data
calories_consumed <- read.csv("C:\\Users\\DELL\\Downloads\\calories_consumed.csv", header = T)
View(calories_consumed)
colnames(calories_consumed)
# Exploratory data analysis
summary(calories_consumed)

install.packages("Hmisc")
library(Hmisc)
describe(calories_consumed)

install.packages("lattice")
library("lattice") # dotplot is part of lattice package

# Graphical exploration
dotplot(calories_consumed$Weight.gained..grams., main = "Dot Plot of Weight.gained..grams.")
dotplot(calories_consumed$Calories.Consumed, main = "Dot Plot of Calories.Consumed")


boxplot(calories_consumed$Weight.gained..grams., col = "dodgerblue4")
boxplot(calories_consumed$Calories.Consumed, col = "red", horizontal = T)

hist(calories_consumed$Weight.gained..grams.)
hist(calories_consumed$Calories.Consumed)

# Normal QQ plot
qqnorm(calories_consumed$Weight.gained..grams.)
qqline(calories_consumed$Weight.gained..grams.)

qqnorm(calories_consumed$Calories.Consumed)
qqline(calories_consumed$Calories.Consumed)

hist(calories_consumed$Weight.gained..grams., prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(calories_consumed$Weight.gained..grams.))             # add a density estimate with defaults
lines(density(calories_consumed$Weight.gained..grams., adjust = 2), lty = "dotted")   # add another "smoother" density

hist(calories_consumed$Calories.Consumed, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(calories_consumed$Calories.Consumed))             # add a density estimate with defaults
lines(density(calories_consumed$Calories.Consumed, adjust = 2), lty = "dotted")   # add another "smoother" density

# Bivariate analysis
# Scatter plot
plot(calories_consumed$Weight.gained..grams., calories_consumed$Calories.Consumed, main = "Scatter Plot", col = "Dodgerblue4", 
     col.main = "Dodgerblue4", col.lab = "Dodgerblue4", xlab = "Waist Ciscumference", 
     ylab = "Adipose Tissue area", pch = 20)  # plot(x,y)


## alternate simple command
plot(calories_consumed$Weight.gained..grams., calories_consumed$Calories.Consumed)

attach(calories_consumed)

# Correlation Coefficient
cor(Weight.gained..grams., Calories.Consumed)

# Covariance
cov(Weight.gained..grams., Calories.Consumed)

# Linear Regression model
reg <- lm(Calories.Consumed ~ Weight.gained..grams., data = calories_consumed) # Y ~ X
summary(reg)

confint(reg, level = 0.95)

pred <- predict(reg, interval = "predict")
pred <- as.data.frame(pred)

View(pred)

# ggplot for adding Regression line for data
library(ggplot2)

ggplot(data = calories_consumed, aes(Weight.gained..grams., Calories.Consumed) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ x)

# Alternate way
ggplot(data = calories_consumed, aes(x = Weight.gained..grams., y = Calories.Consumed)) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = c, aes(x = Weight.gained..grams., y = pred$fit))

# Evaluation the model for fitness 
cor(pred$fit, calories_consumed$Calories.Consumed)

reg$residuals
rmse1 <- sqrt(mean(reg$residuals^2))
rmse1

# Transformation Techniques

# input = log(x); output = y

plot(log(Weight.gained..grams.), Calories.Consumed)
cor(log(Weight.gained..grams.), Calories.Consumed)

reg_log <- lm(Calories.Consumed ~ log(Weight.gained..grams.), data = calories_consumed)
summary(reg_log)

confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval = "predict")

pred <- as.data.frame(pred)
cor(pred$fit, calories_consumed$Calories.Consumed)

rmse2 <- sqrt(mean(reg_log$residuals^2))
rmse2

# Regression line for data
ggplot(data = calories_consumed, aes(log(Weight.gained..grams.), Calories.Consumed) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ log(x))


# Log transformation applied on 'y'
# input = x; output = log(y)

plot(Weight.gained..grams., log(Calories.Consumed))
cor(Weight.gained..grams., log(Calories.Consumed))

reg_log1 <- lm(log(Calories.Consumed) ~ Weight.gained..grams., data = calories_consumed)
summary(reg_log1)

predlog <- predict(reg_log1, interval = "predict")
predlog <- as.data.frame(predlog)
reg_log1$residuals
sqrt(mean(reg_log1$residuals^2))

pred <- exp(predlog)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, calories_consumed$Calories.Consumed)

res_log1 = Calories.Consumed - pred$fit
rmse3 <- sqrt(mean(res_log1^2))
rmse3

# Regression line for data
ggplot(data = calories_consumed, aes(Weight.gained..grams., log(Calories.Consumed)) ) +
  geom_point() + stat_smooth(method = lm, formula = log(y) ~ x)

# Non-linear models = Polynomial models
# input = x & x^2 (2-degree) and output = log(y)

reg2 <- lm(log(Calories.Consumed) ~ Weight.gained..grams. + I(Weight.gained..grams.*Weight.gained..grams.), data = calories_consumed)
summary(reg2)

predlog <- predict(reg2, interval = "predict")
pred <- exp(predlog)

pred <- as.data.frame(pred)
cor(pred$fit, calories_consumed$Calories.Consumed)

res2 = Calories.Consumed - pred$fit
rmse4 <- sqrt(mean(res2^2))
rmse4

# Regression line for data
ggplot(data = calories_consumed, aes(Weight.gained..grams., log(Calories.Consumed)) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))

#choose the best model using RMSE
#Printing all model RMSE
table_rmse <- data.frame("Model" =  c("SLR", "Log model", "Exp model", "Poly model"),"RMSE" = c(rmse1, rmse2, rmse3, rmse4))
table_rmse


# Data Partition
# Random Sampling
n <- nrow(calories_consumed)
n1 <- n * 0.8
n2 <- n - n1

train_ind <- sample(1:n, n1)
train <- calories_consumed[train_ind, ]
test <-  calories_consumed[-train_ind, ]

plot(train$Weight.gained..grams., log(train$Calories.Consumed))
plot(test$Weight.gained..grams., log(test$Calories.Consumed))

model <- lm(Calories.Consumed ~ Weight.gained..grams., data = train) # Y ~ X
summary(model)

confint(model,level=0.95)

log_res <- predict(model,interval = "confidence", newdata = test)

#predict_original <- exp(log_res) # converting log values to original values
predict_original <- as.data.frame(log_res)
test_error <- test$Calories.Consumed - predict_original$fit # calculate error/residual
test_error

test_rmse <- sqrt(mean(test_error^2))
test_rmse

log_res_train <- predict(model, interval = "confidence", newdata = train)

#predict_original_train <- exp(log_res_train) # converting log values to original values
predict_original_train <- as.data.frame(log_res_train)
train_error <- train$Calories.Consumed - predict_original_train$fit # calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))
train_rmse

############################## problem2 ###################################################
# Load the data
delivery_data <- read.csv("C:\\Users\\DELL\\Downloads\\delivery_time.csv", header = T)
View(delivery_data)

# Exploratory data analysis
summary(delivery_data)
colnames(delivery_data)

install.packages("Hmisc")
library(Hmisc)
describe(delivery_data)


install.packages("lattice")
library("lattice") # dotplot is part of lattice package

# Graphical exploration
dotplot(delivery_data$Sorting.Time, main = "Dot Plot of Sorting.Time")
dotplot(delivery_data$Delivery.Time, main = "Dot Plot of Delivery.Time")

boxplot(delivery_data$Sorting.Time, col = "dodgerblue4")
boxplot(delivery_data$Delivery.Time, col = "red", horizontal = T)

hist(delivery_data$Sorting.Time)
hist(delivery_data$Delivery.Time)

# Normal QQ plot
qqnorm(delivery_data$Sorting.Time)
qqline(delivery_data$Sorting.Time)

qqnorm(delivery_data$Delivery.Time)
qqline(delivery_data$Delivery.Time)

hist(delivery_data$Sorting.Time, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(delivery_data$Sorting.Time))             # add a density estimate with defaults
lines(density(delivery_data$Sorting.Time, adjust = 2), lty = "dotted")   # add another "smoother" density

hist(delivery_data$Delivery.Time, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(delivery_data$Delivery.Time))             # add a density estimate with defaults
lines(density(delivery_data$Delivery.Time, adjust = 2), lty = "dotted")   # add another "smoother" density

# Bivariate analysis
# Scatter plot
plot(delivery_data$Sorting.Time, delivery_data$Delivery.Time, main = "Scatter Plot", col = "Dodgerblue4", 
     col.main = "Dodgerblue4", col.lab = "Dodgerblue4", xlab = "Waist Ciscumference", 
     ylab = "Adipose Tissue area", pch = 20)  # plot(x,y)


## alternate simple command
plot(delivery_data$Sorting.Time, delivery_data$Delivery.Time)

attach(delivery_data)

# Correlation Coefficient
cor(Sorting.Time, Delivery.Time)

# Covariance
cov(Sorting.Time, Delivery.Time)

# Linear Regression model
reg <- lm(Delivery.Time ~ Sorting.Time, data = delivery_data) # Y ~ X
summary(reg)

confint(reg, level = 0.95)


pred <- predict(reg, interval = "predict")
pred <- as.data.frame(pred)

View(pred)

# ggplot for adding Regression line for data
library(ggplot2)

ggplot(data = delivery_data, aes(Sorting.Time, Delivery.Time) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ x)

# Evaluation the model for fitness 
cor(pred$fit, delivery_data$Delivery.Time)

reg$residuals
rmse1 <- sqrt(mean(reg$residuals^2))
rmse1


# Transformation Techniques

# input = log(x); output = y

plot(log(Sorting.Time), Delivery.Time)
cor(log(Sorting.Time), Delivery.Time)

reg_log <- lm(Delivery.Time ~ log(Sorting.Time), data = delivery_data)
summary(reg_log)

confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval = "predict")

pred <- as.data.frame(pred)
cor(pred$fit, delivery_data$Delivery.Time)

rmse2 <- sqrt(mean(reg_log$residuals^2))
rmse2

# Regression line for data
ggplot(data = delivery_data, aes(log(Sorting.Time), Delivery.Time) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ log(x))


# Log transformation applied on 'y'
# input = x; output = log(y)

plot(Sorting.Time, log(Delivery.Time))
cor(Sorting.Time, log(Delivery.Time))

reg_log1 <- lm(log(Delivery.Time) ~ Sorting.Time, data = delivery_data)
summary(reg_log1)

predlog <- predict(reg_log1, interval = "predict")
predlog <- as.data.frame(predlog)
reg_log1$residuals
sqrt(mean(reg_log1$residuals^2))

pred <- exp(predlog)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, delivery_data$Delivery.Time)

res_log1 = Delivery.Time - pred$fit
rmse3 <- sqrt(mean(res_log1^2))
rmse3

# Regression line for data
ggplot(data = delivery_data, aes(Sorting.Time, log(Delivery.Time)) ) +
  geom_point() + stat_smooth(method = lm, formula = log(y) ~ x)


# Non-linear models = Polynomial models
# input = x & x^2 (2-degree) and output = log(y)

reg2 <- lm(log(Delivery.Time) ~ Sorting.Time + I(Sorting.Time*Sorting.Time), data = delivery_data)
summary(reg2)

predlog <- predict(reg2, interval = "predict")
pred <- exp(predlog)

pred <- as.data.frame(pred)
cor(pred$fit, delivery_data$Delivery.Time)

res2 = Delivery.Time - pred$fit
rmse4 <- sqrt(mean(res2^2))
rmse4

# Regression line for data
ggplot(data = delivery_data, aes(Sorting.Time, log(Delivery.Time)) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))

#choosing best model using RMSE
table_rmse <- data.frame("Model" =  c("SLR", "Log model", "Exp model", "Poly model"),"RMSE" = c(rmse1, rmse2, rmse3, rmse4))
table_rmse

# Data Partition
# Random Sampling
n <- nrow(delivery_data)
n1 <- n * 0.8
n2 <- n - n1

train_ind <- sample(1:n, n1)
train <- delivery_data[train_ind, ]
test <-  delivery_data[-train_ind, ]

plot(train$Sorting.Time, log(train$Delivery.Time))
plot(test$Sorting.Time, log(test$Delivery.Time))

model <- lm(Delivery.Time ~ log(Sorting.Time), data = train)
summary(model)


confint(model,level=0.95)

log_res <- predict(model,interval = "confidence", newdata = test)


predict_original <- as.data.frame(log_res)
test_error <- test$Delivery.Time - predict_original$fit # calculate error/residual
test_error

test_rmse <- sqrt(mean(test_error^2))
test_rmse

log_res_train <- predict(model, interval = "confidence", newdata = train)

predict_original_train <- as.data.frame(log_res_train)
train_error <- train$Delivery.Time - predict_original_train$fit # calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))
train_rmse



##################################Problem 3#######################################
library(readr)
#Loading dataset
emp_data <- read.csv("C:\\Users\\DELL\\Downloads\\emp_data.csv" , header = T)

#Performing EDA
summary(emp_data)

install.packages("Hmisc")
library(Hmisc)
describe(emp_data)

# Graphical exploration
install.packages("lattice")
library("lattice")
dotplot(emp_data$Salary_hike, main = "Dot Plot of salary hike")
dotplot(emp_data$Churn_out_rate, main = "Dot Plot of Churn out rate")

boxplot(emp_data$Salary_hike, col = "dodgerblue4")
boxplot(emp_data$Churn_out_rate, col = "red", horizontal = T)

hist(emp_data$Salary_hike)
hist(emp_data$Churn_out_rate)

# Normal QQ plot
qqnorm(emp_data$Salary_hike)
qqline(emp_data$Salary_hike)

qqnorm(emp_data$Churn_out_rate)
qqline(emp_data$Churn_out_rate)

hist(emp_data$Salary_hike, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(emp_data$Salary_hike))             # add a density estimate with defaults
lines(density(emp_data$Salary_hike, adjust = 2), lty = "dotted")   # add another "smoother" density

hist(emp_data$Churn_out_rate, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(emp_data$Churn_out_rate))             # add a density estimate with defaults
lines(density(emp_data$Churn_out_rate, adjust = 2), lty = "dotted")   # add another "smoother" density

# Bivariate analysis
# Scatter plot
plot(emp_data$Salary_hike, emp_data$Churn_out_rate, main = "Scatter Plot", col = "Dodgerblue4", 
     col.main = "Dodgerblue4", col.lab = "Dodgerblue4", xlab = "Weight gained in grams", 
     ylab = "Calories Consumed", pch = 20)  # plot(x,y)

## alternate simple command
plot(emp_data$Salary_hike, emp_data$Churn_out_rate)

attach(emp_data)

# Correlation Coefficient
cor(Salary_hike, Churn_out_rate)

# Covariance
cov(Salary_hike, Churn_out_rate)

# Linear Regression model
reg <- lm(Churn_out_rate ~ ., data = emp_data) # Y ~ X
summary(reg)

#confidence interval for model with level 95%
confint(reg, level = 0.95)

pred <- predict(reg, interval = "predict")
pred <- as.data.frame(pred)

# ggplot for adding Regression line for data
library(ggplot2)

ggplot(data = emp_data, aes(Salary_hike, Churn_out_rate) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ x)

# Alternate way
ggplot(data = emp_data, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = emp_data, aes(x = Salary_hike, y = pred$fit))

# Evaluation the model for fitness 
cor(pred$fit, emp_data$Churn_out_rate)

reg$residuals
rmse1 <- sqrt(mean(reg$residuals^2))
rmse1

# Transformation Techniques
# input = log(x); output = y
#Log Transformation
plot(log(Salary_hike), Churn_out_rate)
cor(log(Salary_hike), Churn_out_rate)

#model with log transformation
reg_log <- lm(Churn_out_rate ~ log(Salary_hike), data = emp_data)
summary(reg_log)

#confidence interval with level 95%
confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval = "predict")

pred <- as.data.frame(pred)
cor(pred$fit, emp_data$Churn_out_rate)

rmse2 <- sqrt(mean(reg_log$residuals^2))
rmse2

# Regression line for data
ggplot(data = emp_data, aes(log(Salary_hike), Churn_out_rate) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ log(x))

# Alternate way
ggplot(data = emp_data, aes(x = log(Salary_hike), y = Churn_out_rate)) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = emp_data, aes(x = log(Salary_hike), y = pred$fit))

# Log transformation applied on 'y'
# input = x; output = log(y)
plot(Salary_hike, log(Churn_out_rate))
cor(Salary_hike, log(Churn_out_rate))

reg_log1 <- lm(log(Churn_out_rate) ~ Salary_hike, data = emp_data)
summary(reg_log1)

predlog <- predict(reg_log1, interval = "predict")
predlog <- as.data.frame(predlog)
reg_log1$residuals
sqrt(mean(reg_log1$residuals^2))

pred <- exp(predlog)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, emp_data$Churn_out_rate)

res_log1 = Churn_out_rate - pred$fit
rmse3 <- sqrt(mean(res_log1^2))
rmse3

# Regression line for data
ggplot(data = emp_data, aes(Salary_hike, log(Churn_out_rate)) ) +
  geom_point() + stat_smooth(method = lm, formula = log(y) ~ x)

# Alternate way
ggplot(data = emp_data, aes(x = Salary_hike, y = log(Churn_out_rate))) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = emp_data, aes(x = Salary_hike, y = predlog$fit))


# Non-linear models = Polynomial models
# input = x & x^2 (2-degree) and output = log(y)
reg2 <- lm(log(Churn_out_rate) ~ Salary_hike + I(Salary_hike*Salary_hike), data = emp_data)
summary(reg2)

predlog <- predict(reg2, interval = "predict")
pred <- exp(predlog)

pred <- as.data.frame(pred)
cor(pred$fit, emp_data$Churn_out_rate)

res2 = Churn_out_rate - pred$fit
rmse4 <- sqrt(mean(res2^2))
rmse4

# Regression line for data
ggplot(data = emp_data, aes(Salary_hike, log(Churn_out_rate)) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))

# Alternate way
ggplot(data = emp_data, aes(x = Salary_hike + I(Salary_hike*Salary_hike), y = log(Churn_out_rate))) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = emp_data, aes(x = Salary_hike + I(Salary_hike^2), y = pred$fit))

#Printing all model RMSE
table_rmse <- data.frame("Model" =  c("SLR", "Log model", "Exp model", "Poly model"),"RMSE" = c(rmse1, rmse2, rmse3, rmse4))
table_rmse

# Data Partition
# Random Sampling
n <- nrow(emp_data)
n1 <- n * 0.8
n2 <- n - n1

train_ind <- sample(1:n, n1)
train <- emp_data[train_ind, ]
test <-  emp_data[-train_ind, ]

plot(train$Salary_hike, log(train$Churn_out_rate))
plot(test$Salary_hike, log(test$Churn_out_rate))

model <- lm(log(Churn_out_rate) ~ Salary_hike + I(Salary_hike * Salary_hike), data = train)
summary(model)

confint(model,level=0.95)

log_res <- predict(model,interval = "confidence", newdata = test)

predict_original <- exp(log_res) # converting log values to original values
predict_original <- as.data.frame(predict_original)
test_error <- test$Churn_out_rate - predict_original$fit # calculate error/residual
test_error

test_rmse <- sqrt(mean(test_error^2))
test_rmse

log_res_train <- predict(model, interval = "confidence", newdata = train)

predict_original_train <- exp(log_res_train) # converting log values to original values
predict_original_train <- as.data.frame(predict_original_train)
train_error <- train$Churn_out_rate - predict_original_train$fit # calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))
train_rmse

#######################################Problem 4########################################
library(readr)
#Loading dataset
salary_data <- read.csv("C:\\Users\\DELL\\Downloads\\Salary_Data.csv" , header = T)

#Performing EDA
summary(salary_data)

install.packages("Hmisc")
library(Hmisc)
describe(salary_data)

# Graphical exploration
install.packages("lattice")
library("lattice")
dotplot(salary_data$YearsExperience, main = "Dot Plot of Years of Experiance")
dotplot(salary_data$Salary, main = "Dot Plot of Salary")

boxplot(salary_data$YearsExperience, col = "dodgerblue4")
boxplot(salary_data$Salary, col = "red", horizontal = T)

hist(salary_data$YearsExperience)
hist(salary_data$Salary)

# Normal QQ plot
qqnorm(salary_data$YearsExperience)
qqline(salary_data$YearsExperience)

qqnorm(salary_data$Salary)
qqline(salary_data$Salary)

hist(salary_data$YearsExperience, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(salary_data$YearsExperience))             # add a density estimate with defaults
lines(density(salary_data$YearsExperience, adjust = 2), lty = "dotted")   # add another "smoother" density

hist(salary_data$Salary, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(salary_data$Salary))             # add a density estimate with defaults
lines(density(salary_data$Salary, adjust = 2), lty = "dotted")   # add another "smoother" density

# Bivariate analysis
# Scatter plot
plot(salary_data$YearsExperience, salary_data$Salary, main = "Scatter Plot", col = "Dodgerblue4", 
     col.main = "Dodgerblue4", col.lab = "Dodgerblue4", xlab = "Weight gained in grams", 
     ylab = "Calories Consumed", pch = 20)  # plot(x,y)

## alternate simple command
plot(salary_data$YearsExperience, salary_data$Salary)

attach(salary_data)

# Correlation Coefficient
cor(YearsExperience, Salary)

# Covariance
cov(YearsExperience, Salary)

# Linear Regression model
reg <- lm(Salary ~ ., data = salary_data) # Y ~ X
summary(reg)

#confidence interval for model with level 95%
confint(reg, level = 0.95)

pred <- predict(reg, interval = "predict")
pred <- as.data.frame(pred)

# ggplot for adding Regression line for data
library(ggplot2)

ggplot(data = salary_data, aes(YearsExperience, Salary) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ x)

# Alternate way
ggplot(data = salary_data, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = salary_data, aes(x = YearsExperience, y = pred$fit))

# Evaluation the model for fitness 
cor(pred$fit, salary_data$Salary)

reg$residuals
rmse1 <- sqrt(mean(reg$residuals^2))
rmse1

# Transformation Techniques
# input = log(x); output = y
#Log Transformation
plot(log(YearsExperience), Salary)
cor(log(YearsExperience), Salary)

#model with log transformation
reg_log <- lm(Salary ~ log(YearsExperience), data = salary_data)
summary(reg_log)

#confidence interval with level 95%
confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval = "predict")

pred <- as.data.frame(pred)
cor(pred$fit, salary_data$Salary)

rmse2 <- sqrt(mean(reg_log$residuals^2))
rmse2

# Regression line for data
ggplot(data = salary_data, aes(log(YearsExperience), Salary) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ log(x))

# Alternate way
ggplot(data = salary_data, aes(x = log(YearsExperience), y = Salary)) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = salary_data, aes(x = log(YearsExperience), y = pred$fit))

# Log transformation applied on 'y'
# input = x; output = log(y)
plot(YearsExperience, log(Salary))
cor(YearsExperience, log(Salary))

reg_log1 <- lm(log(Salary) ~ YearsExperience, data = salary_data)
summary(reg_log1)

predlog <- predict(reg_log1, interval = "predict")
predlog <- as.data.frame(predlog)
reg_log1$residuals
sqrt(mean(reg_log1$residuals^2))

pred <- exp(predlog)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, salary_data$Salary)

res_log1 = Salary - pred$fit
rmse3 <- sqrt(mean(res_log1^2))
rmse3

# Regression line for data
ggplot(data = salary_data, aes(YearsExperience, log(Salary)) ) +
  geom_point() + stat_smooth(method = lm, formula = log(y) ~ x)

# Alternate way
ggplot(data = salary_data, aes(x = YearsExperience, y = log(Salary))) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = salary_data, aes(x = YearsExperience, y = predlog$fit))


# Non-linear models = Polynomial models
# input = x & x^2 (2-degree) and output = log(y)
reg2 <- lm(log(Salary) ~ YearsExperience + I(YearsExperience*YearsExperience), data = salary_data)
summary(reg2)

predlog <- predict(reg2, interval = "predict")
pred <- exp(predlog)

pred <- as.data.frame(pred)
cor(pred$fit, salary_data$Salary)

res2 = Salary - pred$fit
rmse4 <- sqrt(mean(res2^2))
rmse4

# Regression line for data
ggplot(data = salary_data, aes(YearsExperience, log(Salary)) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))

# Alternate way
ggplot(data = salary_data, aes(x = YearsExperience + I(YearsExperience*YearsExperience), y = log(Salary))) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = salary_data, aes(x = YearsExperience + I(YearsExperience^2), y = pred$fit))

#Printing all model RMSE
table_rmse <- data.frame("Model" =  c("SLR", "Log model", "Exp model", "Poly model"),"RMSE" = c(rmse1, rmse2, rmse3, rmse4))
table_rmse

# Data Partition
# Random Sampling
n <- nrow(salary_data)
n1 <- n * 0.8
n2 <- n - n1

train_ind <- sample(1:n, n1)
train <- salary_data[train_ind, ]
test <-  salary_data[-train_ind, ]

plot(train$YearsExperience, log(train$Salary))
plot(test$YearsExperience, log(test$Salary))

model <- lm(log(Salary) ~ YearsExperience + I(YearsExperience * YearsExperience), data = train)
summary(model)

confint(model,level=0.95)

log_res <- predict(model,interval = "confidence", newdata = test)

predict_original <- exp(log_res) # converting log values to original values
predict_original <- as.data.frame(predict_original)
test_error <- test$Salary - predict_original$fit # calculate error/residual
test_error

test_rmse <- sqrt(mean(test_error^2))
test_rmse

log_res_train <- predict(model, interval = "confidence", newdata = train)

predict_original_train <- exp(log_res_train) # converting log values to original values
predict_original_train <- as.data.frame(predict_original_train)
train_error <- train$Salary - predict_original_train$fit # calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))
train_rmse

#######################################Problem 5######################################
library(readr)
#Loading dataset
sat_data <- read.csv("C:\\Users\\DELL\\Downloads\\SAT_GPA.csv" , header = T)

#Performing EDA
summary(sat_data)

install.packages("Hmisc")
library(Hmisc)
describe(sat_data)

# Graphical exploration
install.packages("lattice")
library("lattice")
dotplot(sat_data$GPA, main = "Dot Plot of GPA")
dotplot(sat_data$SAT_Scores, main = "Dot Plot of SAT Scores")

boxplot(sat_data$GPA, col = "dodgerblue4")
boxplot(sat_data$SAT_Scores, col = "red", horizontal = T)

hist(sat_data$GPA)
hist(sat_data$SAT_Scores)

# Normal QQ plot
qqnorm(sat_data$GPA)
qqline(sat_data$GPA)

qqnorm(sat_data$SAT_Scores)
qqline(sat_data$SAT_Scores)

hist(sat_data$GPA, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(sat_data$GPA))             # add a density estimate with defaults
lines(density(sat_data$GPA, adjust = 2), lty = "dotted")   # add another "smoother" density

hist(sat_data$SAT_Scores, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(sat_data$SAT_Scores))             # add a density estimate with defaults
lines(density(sat_data$SAT_Scores, adjust = 2), lty = "dotted")   # add another "smoother" density

# Bivariate analysis
# Scatter plot
plot(sat_data$GPA, sat_data$SAT_Scores, main = "Scatter Plot", col = "Dodgerblue4", 
     col.main = "Dodgerblue4", col.lab = "Dodgerblue4", xlab = "Weight gained in grams", 
     ylab = "Calories Consumed", pch = 20)  # plot(x,y)

## alternate simple command
plot(sat_data$GPA, sat_data$SAT_Scores)

attach(sat_data)

# Correlation Coefficient
cor(GPA, SAT_Scores)

# Covariance
cov(GPA, SAT_Scores)

# Linear Regression model
reg <- lm(SAT_Scores ~ ., data = sat_data) # Y ~ X
summary(reg)

#confidence interval for model with level 95%
confint(reg, level = 0.95)

pred <- predict(reg, interval = "predict")
pred <- as.data.frame(pred)

# ggplot for adding Regression line for data
library(ggplot2)

ggplot(data = sat_data, aes(GPA, SAT_Scores) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ x)

# Alternate way
ggplot(data = sat_data, aes(x = GPA, y = SAT_Scores)) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = sat_data, aes(x = GPA, y = pred$fit))

# Evaluation the model for fitness 
cor(pred$fit, sat_data$SAT_Scores)

reg$residuals
rmse1 <- sqrt(mean(reg$residuals^2))
rmse1

# Transformation Techniques
# input = log(x); output = y
#Log Transformation
plot(log(GPA), SAT_Scores)
cor(log(GPA), SAT_Scores)

#model with log transformation
reg_log <- lm(SAT_Scores ~ log(GPA), data = sat_data)
summary(reg_log)

#confidence interval with level 95%
confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval = "predict")

pred <- as.data.frame(pred)
cor(pred$fit, sat_data$SAT_Scores)

rmse2 <- sqrt(mean(reg_log$residuals^2))
rmse2

# Regression line for data
ggplot(data = sat_data, aes(log(GPA), SAT_Scores) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ log(x))

# Alternate way
ggplot(data = sat_data, aes(x = log(GPA), y = SAT_Scores)) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = sat_data, aes(x = log(GPA), y = pred$fit))

# Log transformation applied on 'y'
# input = x; output = log(y)
plot(GPA, log(SAT_Scores))
cor(GPA, log(SAT_Scores))

reg_log1 <- lm(log(SAT_Scores) ~ GPA, data = sat_data)
summary(reg_log1)

predlog <- predict(reg_log1, interval = "predict")
predlog <- as.data.frame(predlog)
reg_log1$residuals
sqrt(mean(reg_log1$residuals^2))

pred <- exp(predlog)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, sat_data$SAT_Scores)

res_log1 = SAT_Scores - pred$fit
rmse3 <- sqrt(mean(res_log1^2))
rmse3

# Regression line for data
ggplot(data = sat_data, aes(GPA, log(SAT_Scores)) ) +
  geom_point() + stat_smooth(method = lm, formula = log(y) ~ x)

# Alternate way
ggplot(data = sat_data, aes(x = GPA, y = log(SAT_Scores))) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = sat_data, aes(x = GPA, y = predlog$fit))


# Non-linear models = Polynomial models
# input = x & x^2 (2-degree) and output = log(y)
reg2 <- lm(log(SAT_Scores) ~ GPA + I(GPA*GPA), data = sat_data)
summary(reg2)

predlog <- predict(reg2, interval = "predict")
pred <- exp(predlog)

pred <- as.data.frame(pred)
cor(pred$fit, sat_data$SAT_Scores)

res2 = SAT_Scores - pred$fit
rmse4 <- sqrt(mean(res2^2))
rmse4

# Regression line for data
ggplot(data = sat_data, aes(GPA, log(SAT_Scores)) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))

# Alternate way
ggplot(data = sat_data, aes(x = GPA + I(GPA*GPA), y = log(SAT_Scores))) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = sat_data, aes(x = GPA + I(GPA^2), y = pred$fit))

#Printing all model RMSE
table_rmse <- data.frame("Model" =  c("SLR", "Log model", "Exp model", "Poly model"),"RMSE" = c(rmse1, rmse2, rmse3, rmse4))
table_rmse

# Data Partition
# Random Sampling
n <- nrow(sat_data)
n1 <- n * 0.8
n2 <- n - n1

train_ind <- sample(1:n, n1)
train <- sat_data[train_ind, ]
test <-  sat_data[-train_ind, ]

plot(train$GPA, log(train$SAT_Scores))
plot(test$GPA, log(test$SAT_Scores))

model <- lm(SAT_Scores ~ log(GPA), data = train)
summary(model)

confint(model,level=0.95)

log_res <- predict(model,interval = "confidence", newdata = test)

predict_original <- as.data.frame(log_res)
test_error <- test$SAT_Scores - predict_original$fit # calculate error/residual
test_error

test_rmse <- sqrt(mean(test_error^2))
test_rmse

log_res_train <- predict(model, interval = "confidence", newdata = train)

predict_original_train <- as.data.frame(log_res_train)
train_error <- train$SAT_Scores - predict_original_train$fit # calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))
train_rmse

##########################################END###############################################
