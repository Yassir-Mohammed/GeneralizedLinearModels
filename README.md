# GeneralizedLinearModels
GLM for Spanish homes prices prediction 

In summary, the best fitting model is the Gamma , however it is still performing poorly with R^2 = 0.39, given that number of features in the data set is limited , although multiple techniques were used, such as Linear, and Poisson fitting Models, scaling, normalizing, feature engineering (new feature creation or Squared or Tripled existing features)


       Sum.of.residuals R.squared Adjusted.R.squared         AIC         BIC
Linear     -1.010717e-04 0.3919381          0.3919272    19165315    19165488
Gamma      -8.107821e+04 0.3919381          0.3919272    18620003    18620175
Poisson    -1.873430e+07 0.3919381          0.3919272 49146561873 49146562034


# Data is taken from :https://www.kaggle.com/datasets/thedevastator/prices-characteristics-of-spanish-homes
# Data is provided by : THE DEVASTATOR , through: Kaggle 

