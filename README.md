
README: Machine Learning Models Assignment Analysis
This repository contains the R script and a summarized PDF of a machine learning assignment, primarily focusing on regression and classification tasks using various modeling techniques. The analysis uses the Boston, Weekly, and Caravan datasets from the ISLR2 package, as well as an external austinhouses.csv dataset.

1. Project Structure
assignment.R: The complete R script containing all data loading, preprocessing, model fitting, and evaluation code.

Machine_Learning_Models.pdf: A PDF summary of the results, including text interpretations, R output, and key plots.

2. echnical Requirements
The analysis was performed using R and requires the following packages:

Package	Purpose
ISLR2	Provides the Boston, Weekly, and Caravan datasets.
ggplot2, ggrepel	For advanced plotting and coefficient comparison.
GGally	For generating pairwise scatterplots.
glmnet	For Lasso and Ridge Regression.
tree, rpart, rpart.plot	For fitting and visualizing Regression Trees.
gbm	For fitting Boosting models.
randomForest	For Bagging and Random Forest models.
BART	For Bayesian Additive Regression Trees.
MASS	For Stepwise Selection (stepAIC).
dplyr	For data manipulation.
tidyverse	A collection of R packages designed for data science.

3. Analysis Overview
The project is structured into four main sections:

I. Exploratory Data Analysis (EDA) - (Boston Dataset)
Objective: Initial exploration of the Boston housing dataset.

Key Findings:

The dataset contains 506 rows and 14 columns.

Pairwise scatterplots revealed relationships, such as a positive correlation between crim and tax, and a negative correlation between crim and rm.

The median pupil-teacher ratio (ptratio) is 19.05.

35 census tracts bound the Charles River (chas=1).

II. Linear Models - (Boston Dataset)
Objective: Predicting per capita crime rate (crim) using various linear regression methods.

Simple Linear Regression (SLR):

All predictors except chas were found to be statistically significant in predicting crim.

Multiple Regression:

Predictors statistically significant at α=0.05 in the multiple regression model were zn, dis, rad, black, and medv.

Coefficient Comparison: A plot comparing SLR vs. Multiple Regression coefficients showed significant shifts and sign reversals for variables like nox and rm, indicating multicollinearity.

Non-Linearity: Testing cubic terms revealed non-linear associations for predictors like medv and nox.

III. Selection and Shrinkage Methods - (Boston Dataset)
Objective: Comparing penalized regression and variable selection models for predicting crim.

Models Evaluated: Lasso Regression, Ridge Regression, and Stepwise Selection (using AIC).

Test MSE Comparison (using a 80/20 train/test split):

Model	Test MSE
Ridge Regression	13.02
Lasso Regression	13.26
Stepwise Selection	14.47

Conclusion: Ridge Regression performed the best with the lowest Test MSE, suggesting it offers the most accurate predictions of crime rate among the three models on the test data.

IV. Logistic Regression and Classification Trees
A. Logistic Regression (Weekly Dataset - Question 13):

Objective: Predicting stock market Direction (Up/Down).

Full Model: Only Lag2 was found to be statistically significant.

Test Model (Lag2 only, 1990-2008 train, 2009-2010 test):

Overall correct prediction rate: 62.5%.

This simplified model with only Lag2 gave a higher accuracy than the full multiple logistic regression model.

B. Classification Trees (Caravan Dataset):

Objective: Predicting insurance plan Purchase using boosting.

Most Important Predictors: PPERSAUT, MKOOPKLA, and MOPLHOOG.

Boosting Precision (Prob>0.2): 19.8% of predicted purchasers actually purchased.

Logistic Regression Precision (Prob>0.2): 14.2% of predicted purchasers actually purchased.

Conclusion: Boosting showed a higher precision in identifying actual purchasers than Logistic Regression.

V. Regression Trees - (Austin Housing Data)
Objective: Predicting log(latestPrice) using trees, bagging, random forests, and BART.

Performance Comparison (Test MSE reported in terms of prices):

Model	Test MSE
BART	31,072.48
Bagging	37,015.13
Random Forest (m=5)	37,295.74
Regression Tree	69,292.13

Variable Importance (Bagging/Random Forest): livingAreaSqFt, latitude, and longitude were the most important predictors.

Random Forest (mtry) Effect: The Test MSE was lowest when m=5.

Conclusion: BART provided the lowest Test MSE, offering the most accurate predictions for the latest housing prices.
