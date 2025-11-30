library(ISLR2)
library(ggplot2)
library(ggrepel)
library(GGally)
library(glmnet)
library(tree)
library(gbm)
library(tidyverse)

head(Boston)


#Exploratory Data Analysis
#Questions 1(a):

cat("The number of rows in the Boston dataset:", nrow(Boston), "\n")
cat("The number of columns in the Boston dataset:", ncol(Boston), "\n")

names(Boston)

# dataset contains the housing values in 506 suburbs of Boston. Each row represents a suburb in Boston. 

#Question 1 (b):

pairs(Boston[, c("crim", "tax", "rm", "medv")])

#crim vs tax --> As the tax increases the crime rate also increases indicating a positive correlation. 
#crim vs rm --> The crime rate decreases as the average rooms per dwelling increases. Implying wealthier neighborhoods might have lower crime rates.
#rm vs medv --> As the number of rooms increases the median of the owner occupied houses also increases indicating a positive correlation.
#crim vs medv --> The crime rate decreases as the median value of the owner occupied houses rate increases, indicating a negative relationship. 


#Question 1 (c):

#By seeing the pairwise plots it was visible that when crim rates increase tax rates and the median of the owner occupied houses also increased, indicating a positive correlation. 
#The crime rates decreased as the average rooms per dwelling increased, indicating a negative correlation. 

par(mfrow = c(1, 3))  # 1 row, 3 columns layout

boxplot(Boston$crim, main = "Crime Rate", ylab = "crim", col = "lightblue")
boxplot(Boston$tax, main = "Property Tax Rate", ylab = "tax", col = "lightgreen")
boxplot(Boston$ptratio, main = "Pupil-Teacher Ratio", ylab = "ptratio", col = "lightcoral")

#Question 1 (d)

range(Boston$crim)
range(Boston$tax)
range(Boston$ptratio)

summary(Boston)

#The highest crime rate in the Boston data set is 88.97620. Similarly The highest tax rate goes up to 711 and the highest pupil teacher ratio is 22. 
#The range of crime and tax rate has the highest range from 0.006 to 88.97 for the former and 187 to 711 for the latter. 

#Question 1 (e):

chas_river <- Boston[ Boston$chas == 1, ]
table(Boston$chas)
cat("The number of census tracts that bounds Charles river is :" , nrow(chas_river))

#Question 1 (f):

cat("The median of the pupil teacher ratio among towns is: ", median(Boston$ptratio))

#Question 1 (g):

min(Boston$medv)

Boston [Boston$medv == 5, ]

summary(Boston)

#This census tract indicates higher crime rate when the median of owner occupied houses is minimum. The rad value is 24, which is at the maximum of the range, indicating high accessibility to highways. 
#Also the tax rates are closer to the maximum value. The ptratio is also high closer to its maximum value, indicating a crowded school service. 

#Question 1 (h):

rm_7 <- Boston[ Boston$rm > 7, ]
cat("The number of census tracts which averages more than seven rooms per dwelling:" , nrow(rm_7))

rm_8 <- Boston[ Boston$rm > 8, ]
cat("The number of census tracts which average more than eight rooms per dwelling:" , nrow(rm_8))

par(mfrow = c(1, 1))
plot(rm_8$rm, rm_8$crim, xlab = 'rm', ylab = 'crim' )

#As the average number of rooms per dwelling increases, the crime rate is stabilizes at a lower level, suggesting that these are wealthier neighborhood.
#However, there is one outlier with a crime rate of 3.4, indicating that even in wealthier areas, higher crime can still occur.




###################################################

#Linear Models
#Question 1:

fit_rm=lm(crim ~ rm,data=Boston)
summary(fit_rm)
coef_value_rm <- coef(fit_rm)[2]
print(coef_value_rm)

fit_tax=lm(crim ~ tax,data=Boston)
summary(fit_tax)
coef_value_tax <- coef(fit_tax)[2]
print(coef_value_tax)

fit_age=lm(crim ~ age,data=Boston)
summary(fit_age)
coef_value_age <- coef(fit_age)[2]
print(coef_value_age)

fit_lstat=lm(crim ~ lstat,data=Boston)
summary(fit_lstat)
coef_value_lstat <- coef(fit_lstat)[2]
print(coef_value_lstat)

fit_zn=lm(crim ~ zn,data=Boston)
summary(fit_zn)
coef_value_zn <- coef(fit_zn)[2]
print(coef_value_zn)

fit_medv=lm(crim ~ medv,data=Boston)
summary(fit_medv)
coef_value_medv <- coef(fit_medv)[2]
print(coef_value_medv)

fit_ptratio=lm(crim ~ ptratio,data=Boston)
summary(fit_ptratio)
coef_value_ptratio <- coef(fit_ptratio)[2]
print(coef_value_ptratio)

fit_rad=lm(crim ~ rad,data=Boston)
summary(fit_rad)
coef_value_rad <- coef(fit_rad)[2]
print(coef_value_rad)

fit_dis=lm(crim ~ dis,data=Boston)
summary(fit_dis)
coef_value_dis <- coef(fit_dis)[2]
print(coef_value_dis)

fit_indus=lm(crim ~ indus,data=Boston)
summary(fit_indus)
coef_value_indus <- coef(fit_indus)[2]
print(coef_value_indus)

fit_nox=lm(crim ~ nox,data=Boston)
summary(fit_nox)
coef_value_nox <- coef(fit_nox)[2]
print(coef_value_nox)

fit_black=lm(crim ~ black,data=Boston)
summary(fit_black)
coef_value_black <- coef(fit_black)[2]
print(coef_value_black)

fit_chas=lm(crim ~ chas,data=Boston)
summary(fit_chas)
coef_value_chas <- coef(fit_chas)[2]
print(coef_value_chas)

plot(Boston$lstat, Boston$crim,
     main = "Crime Rate vs. Lower Status Population",
     xlab = "Lower Status (%)", ylab = "Crime Rate",
     pch = 19, col = rgb(0.2, 0.5, 0.7, 0.5))

abline(fit_lstat, col = "red", lwd = 2)

###Interpretation of the Plots:

par(mfrow = c(2, 2))

# 1. lstat (significant)

plot(Boston$lstat, Boston$crim, main = "crim vs lstat",
     xlab = "Lower Status", ylab = "Crime Rate",
     pch = 19, col = "skyblue")
abline(fit_lstat, col = "red", lwd = 2)

# 2. rad (significant)

plot(Boston$rad, Boston$crim, main = "crim vs rad",
     xlab = "Radial Highways", ylab = "Crime Rate",
     pch = 19, col = "orange")
abline(fit_rad, col = "red", lwd = 2)

# 3. dis (significant)

plot(Boston$dis, Boston$crim, main = "crim vs dis",
     xlab = "Distance to Employment Centers", ylab = "Crime Rate",
     pch = 19, col = "lightgreen")
abline(fit_dis, col = "red", lwd = 2)

# 4. chas (not significant)

plot(Boston$chas, Boston$crim, main = "crim vs chas",
     xlab = "Charles River", ylab = "Crime Rate",
     pch = 19, col = "pink")
abline(fit_chas, col = "red", lwd = 2)


#Multiple Regression

multiple_regression_fit=lm(crim ~ .,data=Boston)
summary(multiple_regression_fit)
multi_coefs <- coef(multiple_regression_fit)[-1]
print(multi_coefs)


## Plot - Linear Regression and Multiple Regression 

linear_regression_coef <- c(
  zn = as.numeric(coef_value_zn),
  indus = as.numeric(coef_value_indus),
  chas = as.numeric(coef_value_chas),
  nox = as.numeric(coef_value_nox),
  rm = as.numeric(coef_value_rm),
  age = as.numeric(coef_value_age),
  dis = as.numeric(coef_value_dis),
  rad = as.numeric(coef_value_rad),
  tax = as.numeric(coef_value_tax),
  ptratio = as.numeric(coef_value_ptratio),
  black = as.numeric(coef_value_black),
  lstat = as.numeric(coef_value_lstat),
  medv = as.numeric(coef_value_medv)
)

#as.numeric is used so that only the numeric gets saved ignoring the names (eg:zn.zn)

#Combine the two columns - Linear and multiple regression coeffecients to plot. 

  df_coeff <- data.frame(
  Predictor = names(linear_regression_coef),
  Linear = linear_regression_coef,
  Multiple = multi_coefs[names(linear_regression_coef)]
)

#Question 3
  
par(mfrow = c(1, 1))

ggplot(df_coeff, aes(x = Linear, y = Multiple, label = Predictor)) +
  geom_point(color = "darkblue", size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
  geom_text_repel(size = 3, max.overlaps = Inf) +
  labs(title = "Coefficient Comparison", x = "Simple Regression Coefficients", y = "Multiple Regression Coefficients")
       
       
#Question 4 - Non Linearity 

fit_lstat_cubic <- lm(crim ~ lstat + I(lstat^2) + I(lstat^3), data = Boston)
summary(fit_lstat_cubic)

fit_rm_cubic <- lm(crim ~ rm + I(rm^2) + I(rm^3), data = Boston)
summary(fit_rm_cubic)

fit_zn_cubic <- lm(crim ~ zn + I(zn^2) + I(zn^3), data = Boston)
summary(fit_zn_cubic)

fit_tax_cubic <- lm(crim ~ tax + I(tax^2) + I(tax^3), data = Boston)
summary(fit_tax_cubic)

fit_dis_cubic <- lm(crim ~ dis + I(dis^2) + I(dis^3), data = Boston)
summary(fit_dis_cubic)

fit_black_cubic <- lm(crim ~ black + I(black^2) + I(black^3), data = Boston)
summary(fit_black_cubic)

fit_indus_cubic <- lm(crim ~ indus + I(indus^2) + I(indus^3), data = Boston)
summary(fit_indus_cubic)

fit_medv_cubic <- lm(crim ~ medv + I(medv^2) + I(medv^3), data = Boston)
summary(fit_medv_cubic)

ggplot(Boston, aes(x = medv, y = crim)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3)) + 
  labs(title = "Non-linear Relationship between medv and crim")


fit_age_cubic <- lm(crim ~ age + I(age^2) + I(age^3), data = Boston)
summary(fit_age_cubic)

fit_chas_cubic <- lm(crim ~ chas + I(chas^2) + I(chas^3), data = Boston)
summary(fit_chas_cubic)

fit_nox_cubic <- lm(crim ~ nox + I(nox^2) + I(nox^3), data = Boston)
summary(fit_nox_cubic)

ggplot(Boston, aes(x = nox, y = crim)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3)) + 
  labs(title = "Non-linear Relationship between nox and crim")

fit_ptratio_cubic <- lm(crim ~ ptratio + I(ptratio^2) + I(ptratio^3), data = Boston)
summary(fit_ptratio_cubic)

fit_rad_cubic <- lm(crim ~ rad + I(rad^2) + I(rad^3), data = Boston)
summary(fit_rad_cubic)

############################################################################

#Selection and Shrinkage Methods 

library(ISLR2)
library(glmnet)

data(Boston)
dim(Boston)

set.seed(5)

#Split into training and testing data set
train_index_1 <- sample(1:nrow(Boston), nrow(Boston)* 0.8)
boston_train <- Boston[train_index_1, ]
boston_test <- Boston[- train_index_1, ]

dim(boston_train)
dim(boston_test)

#Fit a Lasso model 

x_boston <- model.matrix(crim ~., boston_train)[, -1]
y_boston <- boston_train$crim

x_test <- model.matrix(crim ~ ., boston_test)[, -1]
y_test <- boston_test$crim

boston_lasso <- glmnet(x_boston, y_boston, alpha = 1)
boston_cv_lasso <- cv.glmnet(x_boston, y_boston, alpha = 1)
boston_min_lambda_lasso <- boston_cv_lasso$lambda.min

plot(boston_cv_lasso)
print("Lasso Coefficients:")
print(coef(boston_lasso))

#Ridge Regression 

boston_ridge <- glmnet(x_boston, y_boston, alpha = 0)
boston_cv_ridge <- cv.glmnet(x_boston, y_boston, alpha = 0)
boston_min_lambda_ridge <- boston_cv_ridge$lambda.min

plot(boston_cv_ridge)
print("Ridge Coefficients:")
print(coef(boston_ridge))

#Stepwise Selection 

library(MASS)

boston_model <- lm(crim ~ ., data = boston_train)
boston_step <- stepAIC(boston_model, direction = 'both', trace = FALSE)
summary(boston_step)

plot(boston_step, scale = 'adjr2')

#Question (b)

#Propose a model (or set of models) that seem to perform well on this data set, and justify your answer. 
#Make sure that you are evaluating model performance using validation set error, cross validation, or some other reasonable alternative, as opposed to using training error.

#Predict the Test MSE 

#Lasso Regression 

boston_lasso_pred <- predict(boston_cv_lasso, s = boston_min_lambda_lasso, newx = x_test)
boston_mse_lasso <- mean((boston_lasso_pred - y_test)^2)
print(paste("Lasso Test MSE:", round(boston_mse_lasso, 2)))

#Ridge Regression 

boston_ridge_pred <- predict(boston_cv_ridge, s = boston_min_lambda_ridge, newx = x_test)
boston_mse_ridge <- mean((boston_ridge_pred - y_test)^2)
print(paste("Ridge Test MSE:", round(boston_mse_ridge, 2)))

#Stepwise Selection

boston_step_pred <- predict(boston_step, newdata = boston_test)
boston_mse_stepwise <- mean((boston_step_pred - y_test)^2)
print(paste("Stepwise Test MSE:", round(boston_mse_stepwise, 2)))

AIC(boston_step)

#Plot the test MSE values of various models 

model_names <- c("Stepwise", "Lasso", "Ridge")
mse_values <- c(boston_mse_stepwise, boston_mse_lasso, boston_mse_ridge)

# Sort both mse and model names accordingly

sort_order <- order(mse_values)

mse_sorted_values <- mse_values[sort_order]
model_names_sorted <- model_names[sort_order]

barplot(mse_sorted_values, names.arg = model_names_sorted, 
        main = "Test MSE for Different Models",
        ylab = "Test MSE",
        xlab = "Models",
        las = 2,                  
        col = "skyblue",
        cex.names = 0.8)          

##########################################################################
#Logistic Regression 

#Question 4

#6.Suppose we collect data for a group of students in a statistics class with variables X1 = hours studied, X2 = undergrad GPA, and Y =
#receive an A. We fit a logistic regression and produce estimated coefficient, ˆ β0 = −6, ˆ β1 = 0.05, ˆ β2 = 1.

#Subdivision (a)
#Estimate the probability that a student who studies for 40 h and has an undergrad GPA of 3.5 gets an A in the class.

betacoeff0 <- -6
betacoeff1 <- 0.05
betacoeff2 <- 1

# Part (a) values
X1 <- 40    # hours studied
X2 <- 3.5   # GPA

# Calculate logit
logit <- betacoeff0 + betacoeff1 * X1 + betacoeff2 * X2

# Calculate probability
probability <- 1 / (1 + exp(-logit))
print(probability)

#0.37 is the probability of the boy getting an A in the class.

#Subdivision b

#p = 0.5 
#Solving the equation, 0.5 = 1/1+(e^-z) 
#We get the z value as 0. Since Z = 0, the provided values are substituted in the below formula. 

X1_value <- -(betacoeff0 + betacoeff2 * X2) / betacoeff1
print(X1_value)

#If the boy studies for 50 hours there is a 50% chance of getting an A.

# Question 4 - 9

#Subdivision (a):

odds <- 0.37 

probability_of_default <- odds/(1+odds)
print(probability_of_default)

#0.27 is the probability of customers defaulting their credit card payment.

#Subdivision (b):

probability = 0.16
odds_of_defaulting <- probability/(1-probability)
print(odds_of_defaulting)

#0.1904 is the odds of defaulting her credit card payment. 

#Question 13:

#Question 13(a):

head(Weekly)

dim(Weekly)

summary(Weekly)

cor(Weekly[, -9])

plot(Weekly$Year, Weekly$Volume, xlab = 'Year', ylab = 'Volume', main = 'Volume vs Year')  

#Question 13(b):

weekly_glm <- glm(Direction  ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume , data = Weekly , family = binomial)

summary(weekly_glm)

#Question 13(c):

weekly_probability <- predict(weekly_glm, type = 'response')
weekly_probability[1:10]

weekly_pred <- rep('Down', 1089)

weekly_pred[weekly_probability > 0.5] = 'Up'

#Initializing a confusion matrix

table(Predicted = weekly_pred, Actual = Weekly$Direction)

#Overall fraction of correct predictions 

(557+54)/1089 

#Question 13(d):

#Splitting the dataset to training and testing dataset.

weekly_train <- Weekly[Weekly$Year < 2009, ]
weekly_test <- Weekly[Weekly$Year >= 2009, ]

dim(weekly_test)
dim(weekly_train)

#Fit Logistic Regression on the testing data

weekly_fit2 <- glm(Direction ~ Lag2, data = weekly_train, family = binomial)
summary(weekly_fit2)

#Determining the probabilities on testing dataset

weekly_probability_test <- predict(weekly_fit2, weekly_test, type = 'response')

#Converting the probabilities to the Direction prediction 

weekly_predictions_test <- rep('Down', 104)
weekly_predictions_test[weekly_probability_test > 0.5] = 'Up'

#Initializing a confusion matrix

actual_direction <- weekly_test$Direction

table(Predicted = weekly_predictions_test, Actual = actual_direction)

#Overall fraction of correct predictions 

(9+56)/104

#Overall fraction of incorrect predictions

(34+5)/104


###############################################################

#Regression Trees 

#Use the Austin housing data from the prediction contest (austinhousing.csv) instead of the dataset in the book. 
#Use the following variables to generate predictions for log(latestPrice): latitude, longitude, hasAssociation, livingAreaSqFt, numOfBathrooms, numOfBedrooms. 
#When reporting your prediction errors, report them in terms of prices (not log prices).

#Question (a)

#Split the data set into a training set and a test set.

library(dplyr)

austin_house <- read.csv('austinhouses.csv')
head(austin_house)
dim(austin_house)

austin_house <- austin_house %>%
  dplyr::select(latitude, longitude, hasAssociation, livingAreaSqFt, numOfBathrooms, numOfBedrooms, latestPrice) %>%
  drop_na()

set.seed(123)

n <- nrow(austin_house)
train_index <- sample(1:n, size = 0.8 * n)

austin_train = austin_house[train_index, ]
austin_test = austin_house[-train_index, ]

dim(austin_train)
dim(austin_test)

#Question (b) Fit a regression tree to the training set. Plot the tree, and interpret the results. What test MSE do you obtain?
  
library(rpart)
library(rpart.plot)

#Fit a Regression tree

austin_tree <- rpart( log(latestPrice) ~ ., data = austin_train)
summary(austin_tree)
rpart.plot(austin_tree)

#Predict the House Prices in the testing data 

log_austin_pred <- predict(austin_tree, newdata = austin_test)
austin_pred <- exp(log_austin_pred)

#Calculate the test MSE

actual_house_price <- austin_test$latestPrice
test_mse <- mean((austin_pred - actual_house_price)^2)
print(paste("Test MSE:", round(test_mse, 2)))

#Question (c)
#Use cross-validation in order to determine the optimal level of tree complexity. Does pruning the tree improve the test MSE?

printcp(austin_tree)
plotcp(austin_tree)

#Take the minimum Complexity parameter
min_cp <- austin_tree$cptable[which.min(austin_tree$cptable[,"xerror"]), "CP"]

#Prune the tree

austin_prune <- prune(austin_tree, cp = min_cp)
plot(austin_prune)
text(austin_prune , pretty = 0)

#Calculate the test MSE

austin_pred_prune <- predict(austin_prune, newdata = austin_test)
austin_mse_prune <- mean((exp(austin_pred_prune) - austin_test$latestPrice)^2)
print(paste("Pruning Test MSE:", round(austin_mse_prune, 2)))

#Question (d) Use the bagging approach in order to analyze this data. What test MSE do you obtain? Use the importance() function to determine which variables are most important.

library(randomForest)

# Fit the bagging model

set.seed(123)

austin_bagging <- randomForest(
  log(latestPrice) ~ .,
  data = austin_train,
  mtry = 6,               # total number of predictors = 6
  importance = TRUE
)
summary(austin_bagging)


# Predict on test data
austin_bagging_pred <- predict(austin_bagging, newdata = austin_test)

# Convert log predictions back to actual price
austin_bagging_pred_actual <- exp(austin_bagging_pred)

# Calculate test MSE
bag_mse <- mean((austin_bagging_pred_actual - austin_test$latestPrice)^2)
print(paste("Bagging Test MSE:", round(bag_mse, 2)))

importance(austin_bagging)

# Plot variable importance
varImpPlot(austin_bagging)

#Question (e)

# Use random forests to analyze this data. What test MSE do you obtain? Use the importance() function to determine which variables are most important.
# Describe the effect of m, the number of variables considered at each split, on the error rate obtained.

#Fit a Random Forest model 

mtry_values <- 1:6 #Since there are six predictors. 

test_MSE_rf <- c()

#Initializing a loop for different mtry values 
for (i in seq_along(mtry_values)) {
  set.seed(2)
  austin_random_forest <- randomForest(
    log(latestPrice) ~ .,
    data = austin_train,
    mtry = mtry_values[i], 
    importance = TRUE
  )
  
  austin_rf_pred <- predict(austin_random_forest, newdata = austin_test)
  austin_pred_pric <- exp(austin_rf_pred)
  mse <- mean((austin_test$latestPrice - austin_pred_pric)^2)
  
  test_MSE_rf[i] <- mse
}

plot(mtry_values, test_MSE_rf, type = "b", pch = 19,
     xlab = "mtry", ylab = "Test MSE", main = "mtry Vs Test MSE")

importance(austin_random_forest)

varImpPlot(austin_random_forest)

#Question (f)

#Now analyze the data using BART, and report your results.

library(BART)

x_train <- austin_train[, 1:6]
x_test <- austin_test[, 1:6]

y_train <- log(austin_train$latestPrice)

#Fit BART model 

set.seed(42)
austin_bart <- gbart(x.train = x_train, y.train = y_train, x.test = x_test)

#Predict the data and calculate test MSE

austin_bart_pred <- exp(colMeans(austin_bart$yhat.test))
austin_bart_mse <- mean((austin_test$latestPrice - austin_bart_pred)^2)
print(paste("BART Test MSE:", round(austin_bart_mse, 2)))

#Bar Plot for comparing the results 

mse_values <- c(
  "Regression Tree" = test_mse,
  "Bagging" = bag_mse,               # replace with your actual value
  "RF (m=1)" = test_MSE_rf[1],
  "RF (m=2)" = test_MSE_rf[2],
  "RF (m=3)" = test_MSE_rf[3],
  "RF (m=4)" = test_MSE_rf[4],
  "RF (m=5)" = test_MSE_rf[5],
  "RF (m=6)" = test_MSE_rf[6],
  "BART" = austin_bart_mse
)

mse_values <- sort(mse_values)

# Plotting

par(mfrow = c(1, 1))

barplot(mse_values,
        main = "Test MSE for Different Models",
        ylab = "Test MSE",
        las = 2,                   # rotate x-axis labels for readability
        col = "skyblue",
        cex.names = 0.8)          # control label size


###############################################################

#Classification Trees 

#This question uses the Caravan data set.
#Question (a) Create a training set consisting of the first 1,000 observations,and a test set consisting of the remaining observations.

library(ISLR2)
library(gbm)
data("Caravan")

unique(Caravan$Purchase)

# Convert 'Purchase' to numeric in a new column
Caravan$Purchase_numeric <- ifelse(Caravan$Purchase == "Yes", 1, 0)

# Remove original Purchase column
Caravan$Purchase <- NULL

train_obs = 1:1000

#Split the data set to training and testing data set.
Caravan_train = Caravan[train_obs, ]
Caravan_test = Caravan[-train_obs, ]


#Question (b) Fit a boosting model to the training set with Purchase as the response and the other variables as predictors. 
#Use 1,000 trees,and a shrinkage value of 0.01. Which predictors appear to be the most important?

set.seed(342)
boost_caravan = gbm(Purchase_numeric ~ ., data = Caravan_train, n.trees = 1000, shrinkage = 0.01, 
                    distribution = "bernoulli")

summary(boost_caravan)

boost_prob = predict(boost_caravan, Caravan_test, n.trees = 1000, type = "response")
boost_pred = ifelse(boost_prob > 0.2, 1, 0)

#Initialize Confusion Matrix

table(Caravan_test$Purchase, boost_pred)

#Calculate Precision
34/(137 + 34)


#Question (c) Use the boosting model to predict the response on the test data.
#Predict that a person will make a purchase if the estimated probability of purchase is greater than 20%. Form a confusion matrix. What fraction of the people predicted to make a purchase do in fact make one? 
#How does this compare with the results obtained from applying KNN or logistic regression to this data set?

#Compare with Logistic Regression 
log_caravan = glm(Purchase_numeric ~ ., data = Caravan_train, family = binomial)
summary(log_caravan)

log_prob = predict(log_caravan, Caravan_test, type = "response")

log_pred = ifelse(log_prob > 0.2, 1, 0)

#Initialize Confusion matrix
table(Caravan_test$Purchase, log_pred)

#Calculate precision 
58/(350 + 58)

################################################################

