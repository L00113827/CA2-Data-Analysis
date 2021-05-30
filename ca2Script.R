# Training and Testing Model
# L00113827 - Catherine de Andres Arceno


library(readr) # reading dataset
library(dplyr, warn.conflicts = F) # for Data manipulation
library(olsrr)  # ols stepwise regression and plotting
library(lm.beta)    # Standardize Regression Coefficients

if(!require(lm.beta)) {install.packages("lm.beta")} # downloading the package if not available
if(!require(olsrr)) {install.packages("olsrr")}


df <- read_csv("df.csv") # reading the dataset



glimpse(df)   # type, rows and columns, and general look of dataset


# Boxplot for Outliers

par(mfrow= c(3,3), col= "red", lwd=2)
for(i in 1:ncol(df)) {
    boxplot(df[,i], main= names(df[i]), col= i+2, horizontal = T, pch= 19, cex= 1)
}

# soci.s, generosity, p.corruption, and n.affect have some outliers
par(mfrow= c(2,2), lwd= 2)
boxplot(df$soci.s, main= "soci.s", horizontal = T, col= "tomato2", pch= 19)
boxplot(df$generosity, main= "generosity", horizontal = T, col= "gold", pch= 19)
boxplot(df$p.corruption, main= "p.corruption", horizontal = T, col= "steelblue", pch= 19)
boxplot(df$n.affect, main= "n.affect", horizontal = T, col= "palegreen4", pch= 19)


# Removing Outliers

df %>% filter(soci.s>.51) %>% 
    filter(generosity<.29) %>%
    filter(p.corruption>.61) %>% 
    filter(n.affect<.46) -> df


# Outliers are removed

boxplot(df$soci.s, main= "soci.s", horizontal = T, col= "tomato2", pch= 19)
boxplot(df$generosity, main= "generosity", horizontal = T, col= "gold", pch= 19)
boxplot(df$p.corruption, main= "p.corruption", horizontal = T, col= "steelblue", pch= 19)
boxplot(df$n.affect, main= "n.affect", horizontal = T, col= "palegreen4", pch= 19)


# Splitting the data into Training and Testing Set

set.seed(2021)     # it will give a fix random sample for reproducibility of analysis, we can use any random digit in the parenthesis.




# Splitting the data, 75 percent data for Training data set and 25 percent data for Testing data set.


df %>%
    sample_frac(.75) -> train_data               # 75 percent split for Training model
df %>% anti_join(train_data)-> test_data         # other 25 data for Testing


# Making Model: Multiple Stepwise Regerssion to select best predictor variables. Taking Happiness (happy) as critirion (dependent variable) and other all variables as Predictor variables (Independent variables)



fit <- lm(happy~., data= train_data) # Fitting the Model

summary(fit)    # Summary of the model

# 
# In the presence of other variables freedom, generosity, p.corruption, n.affect were not significant in the prediction of happiness (happy).
# 
# 
# To get significant variables, we are using Stepwise Regression in which we use both "foreword" & "backward" selection method. Setting the probability of Entry .10 and probability of remaining .05. For SR, we are using "olsrr" package (Arvind Hebbali, 2020).
# 
# 
# Aravind Hebbali (2020). olsrr: Tools for Building OLS Regression Models. R package version 0.5.3.https://CRAN.R-project.org/package=olsrr


olsrr::ols_step_both_p(fit, pent= .05, prem= .10) -> final_model  # This is our final model


final_model                  # Summary of final_model

# Final Model Summary

final_model$model -> final_fit      # This is our final fit
summary(final_fit)                  # Summary of our final fit


# Plotting the Model fitness

par(mfrow=c(2,2))
plot(final_fit, lwd= 2)



# 1. Residuals vs Fitted: follows assumption of Linearity
# 
# 
# 2. Normal Q-Q: Residuals are normally distributed
# 
# 
# 3. Scale-Location (also known as Spread-Location): To test Homoscedasity/ or Homogeneity of variance.Heterogeneity is bad for any parametric statistical test.
# 
# 
# 4. Residuals vs Leverage: To get information whether out model is in effect of any extreme value.


# "lm.beta" package (Stefan Behrendt, 2014) is used to get Standardize regression beta coefficient for final model.
# 
# 
# Stefan Behrendt (2014). lm.beta: Add Standardized Regression Coefficients to lm-Objects.R package version 1.5-1. https://CRAN.R-project.org/package=lm.beta
# 
# 
# Standardise Beta Coefficient


final_fit %>%
    lm.beta::lm.beta() %>%
    .$sta %>% data.frame() %>%
    `colnames<-`("Standardise Beta Coefficients") %>%
    print(digits=3)


# ANOVA Test for final model


anova(lm(happy~1, data= train_data), final_fit)


# Plotting the final Model


plot(final_model)


# 
# Train model is perfectly fine, having high R2 value which is good indicator of model fitness, and error is low i.e., low value of AIC, MSE, RMSE etc.



# Testing (forecasting and validation of the model)



observed= test_data$happy
forecast= predict(final_fit, test_data)           # Forested value for Testing dataset
data.frame(observed, forecast) -> newdata          # Saving it to a new variable
newdata


# Calculating the Error (residual) and square of residuals in forecasting


newdata %>% mutate(residual= observed-forecast,
                   sq_residual= residual^2) %>%
    print(digits=2) ->newdata


newdata$sq_residual %>% sum()/25 -> MSE
print(MSE)
RSE <- sqrt(MSE)
print(RSE)


# Since we have total 29 observations in validation data set (test_data), total degree of freedom (df) is 28, regression df is 3, residual df is 25.
# We use 25 degree of freedom of residual in calculating RSE and MSE. Low RSE and MSE indicates out model is good in forecasting the dependent variable (happiness).















