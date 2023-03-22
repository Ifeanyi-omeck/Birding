# Birding EIA Density Analysis
Using Generalised Linear Model to predict Animal density

This repository contains an R script for analyzing density of EIA (Environmental Impact Assessment) data. The analysis includes data cleaning, visualization, linear regression modeling, model optimization using AIC, BIC, and stepwise selection, checking for collinearity and autocorrelation, and making predictions.
Here's a brief overview of what the code does in each section:

* Data preparation: The script first loads the required R packages and reads in the CSV data file. It calculates the density by dividing the count by the area and converts the tidestate variable into a factor.

* Visualization: The script creates a heatmap to visualize the spatial distribution of density in the data.

* Linear regression modeling: The script fits several linear regression models with different combinations of predictors, including interactions and categorical variables, to find the best model based on the AIC and BIC criteria.

* Model optimization: Using AIC, BIC, and stepwise selection, the script identifies the optimal model. It also checks for multicollinearity using variance inflation factors (VIF) and tests for heteroscedasticity and autocorrelation in the residuals.

* Generalized least squares (GLS) modeling: The script fits GLS models with various correlation structures and variance functions to address potential issues with heteroscedasticity and autocorrelation.

* Model diagnostics: The script assesses the residuals of the GLS models and compares their AIC values to select the best model. It also conducts hypothesis tests to identify the significant variables in the final model.

* Prediction: Using the selected model, the script makes predictions for new data points, providing estimates of the density before and after a specific impact.
