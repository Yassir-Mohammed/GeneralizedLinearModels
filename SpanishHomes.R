
library(tidyverse)
library(dplyr)
library(psych)
library(ggplot2)
library(scales)

# Data is taken from :https://www.kaggle.com/datasets/thedevastator/prices-characteristics-of-spanish-homes
# Data is provided by : THE DeVASTATOR , through: Kaggle 


data <- read.csv("D:Projects/Spanish Homes/pisos.csv")
data <- data.frame(data)

tracked_removals <- list()
labels <- list()

tracked_removals <- append(tracked_removals,dim(data)[1])
labels <- append(labels,"Original")


# Selecting Useful Columns
colnames(data)
data <- subset(data, select = c(7:12))
data <- data[,-4]
head(data)

# Converting data to numeric

data$price <- as.numeric(gsub("[^0-9]", "", data$price))
data$size <- as.numeric(gsub("[^0-9]", "", data$size))
data$rooms <- as.numeric(data$rooms)
data$bathrooms <- as.numeric(data$bathrooms)
data$Num.Photos <- as.numeric(data$Num.Photos)
head(data)


# Missing data

summary(data) 


# Outlier Removal 

rooms_sd <- sd(data$rooms,na.rm = T)
bathrooms_sd <- sd(data$bathrooms,na.rm = T)
size_sd <- sd(data$size, na.rm = T)
price_sd <- sd(data$price, na.rm = T)

data <- subset(data ,data$rooms <= 3*rooms_sd )
data <- subset(data ,data$size <= 3*size_sd )
data <- subset(data ,data$bathrooms <= 3*bathrooms_sd )
data <- subset(data ,data$price <= 3*price_sd )


tracked_removals <- append(tracked_removals,dim(data)[1])
labels <- append(labels,"Outlier Removal")



summary(data)

# Imputation

data$Num.Photos[is.na(data$Num.Photos)] <- 0
data$bathrooms[is.na(data$bathrooms)] <- median(data$bathrooms, na.rm = T)
data$rooms[is.na(data$rooms)] <- median(data$rooms, na.rm = T)

#######################################################
#Regression Imputation
data_subset <- data[!is.na(data$rooms) & !is.na(data$size), ]
# Fit a linear regression model with rooms as the response variable and size as the predictor variable
reg <- lm(rooms ~ size, data = data_subset)
summary(reg)
# R^2 is not high enough for imputation
########################################################

summary(data)  
dim(data)

data <- na.omit(data)

tracked_removals <- append(tracked_removals,dim(data)[1])
labels <- append(labels,"y NA removal")

summary(data)



# Linear Regression

model_linear <- lm(price ~ size + rooms +bathrooms+ Num.Photos + 
              I(size^2) +I(rooms^2) +I(Num.Photos^2) +
              I(size^3) +I(rooms^3) +I(Num.Photos^3) +
              I(size^4) +I(rooms^4) +I(Num.Photos^4), 
            data = data)
summary(model_linear)

model_gamma <- glm(price ~ size + rooms + bathrooms + Num.Photos + 
                    I(size^2) + I(rooms^2) + I(Num.Photos^2) +
                    I(size^3) + I(rooms^3) + I(Num.Photos^3) +
                    I(size^4) + I(rooms^4) + I(Num.Photos^4),
                  data = data, family = Gamma(link = "log"))


model_poisson <- glm(price ~ size + rooms + bathrooms + Num.Photos + 
                       I(size^2) + I(rooms^2) + I(Num.Photos^2) +
                       I(size^3) + I(rooms^3) + I(Num.Photos^3) +
                       I(size^4) + I(rooms^4) + I(Num.Photos^4),
                     data = data,family = poisson(link = "log"))
                     

  
summary(model_linear)
summary(model_gamma)
summary(model_poisson)



# Evaluation of Each Regression

#Residuals
resid_sum <- c(sum(resid(model_linear)), sum(resid(model_gamma)), sum(resid(model_poisson)))
# AIC and BIC
aic <- c(AIC(model_linear), AIC(model_gamma), AIC(model_poisson))
bic <-c(BIC(model_linear), BIC(model_gamma), BIC(model_poisson))

# R Squared and Adjusted R Squared
r_squared <- c(summary(model_linear)$r.squared, summary(model_gamma)$r.squared, summary(model_poisson)$r.squared)
adj_r_squared <- c(summary(model_linear)$adj.r.squared, summary(model_gamma)$adj.r.squared, summary(model_poisson)$adj.r.squared)



#Summary
summary_df <- data.frame("Sum of residuals" = resid_sum, 
                 "R-squared" = r_squared,
                  "Adjusted R-squared" = adj_r_squared,
                  "AIC" = aic,
                  "BIC" = bic,
                  row.names = c("Linear", "Gamma", "Poisson"))


summary_df

# In summary, the best fitting model is the Gamma , however it is still performing poorly with R^2 = 0.39,
# given that number of features in the data set is limited , although multiple techniques were used, such as 
# Linear, and Poisson fitting Models, scaling, normalizing, feature engineering (new feature creation or Squared or Tripled existing features)


