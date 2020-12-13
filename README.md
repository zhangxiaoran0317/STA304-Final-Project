# STA304-Final-Project
This is my final project for STA304
Warning in eval(predvars, data, env): NAs introduced by coercion
---
title: "Data Analysis on Toronto Apartment Building Evaluation Score"
author: "Z1004115744"
date: "21/12/2020"
output: html_document
---

```{r, echo=FALSE}
library(opendatatoronto)
library(dplyr, warn.conflicts = FALSE)

package <- show_package("4ef82789-e038-44ef-a478-a8f3590c3eb1")

resources <- list_package_resources("4ef82789-e038-44ef-a478-a8f3590c3eb1")

datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

data <- filter(datastore_resources, row_number()==1) %>% get_resource()
```
## Authors, date, Repo

Author: Z1004115744

Date: December 21, 2020

Repo: (.)

## Keywords

The Keywords of this analysis: Linear regression, Apartment, Evaluation Score, Apartment Building Standard(RentSafeTO), Toronto Neighborhood.

## Abstract

This report will present how the evaluation score of Toronto apartments is conducted from different factors.The government of Toronto's "RentSafeTo" program gave all registered apartment buildings a score based on the standard of the buildings (Municipal Licensing & Standards). The standards compared how many balcony guards of each building, how many confirmed elevators and units of each building...etc. to ensure the safety and cleaness of Toronto buildings. The method used here is linear regression, by generating a multi-linear regression model, the coefficient of each variables illustrates how the variable contributed to the measure of evaluation score. Based on the model result, the numbers of laundry room, exterior walkways and stairwells are the most important factors, that more numbers of them the buildings have, the higher evaluation score they get. The report also contains lots of weakness that the linear regression model does not contains all the variables from the dataset(Toronto Open data), and some information is missing, recognized as N/A. The analysis can have further improvement on cleaning a more professional dataset and contains a regression model with more variables.

## Introduction

Many Toronto people today choose to live in apartments, but people barely have information to evaluate which apartment is the best option in Toronto. The Toronto government developed a program called "RentSafeTo" by law to ensure that the apartment buildings in Toronto is meeting a maintenance standards to ensure a safe and clean homes for tenants. (Municipal Licensing & Standards)
In this program, each participated apartments are given an evaluation score based on their building structures included the number of "entrance, exits, elevators, stairwells and securities...etc"(ABS program, City of Toronto). By comparing the evaluation scores, government and the building owners would determine whether the apartment building is meeting the standard. 
  
  However, The problem is how each aspects of the apartment buildings, like the number of balcony guards, the numbers of entrance...etc. contributed to the apartment building's evaluation score so the actual evaluation score conducted by the government could be a standard to determine the buildings. One most common way to find the relationships between explanatory variables and response variable is to conduct a linear regression model(Weisberg, 2005). In this analysis,it is better to use linear regression to determine how each aspects of the apartment building affects its evaluation scores, and answer the main question: Is it proper to use evaluation score to determine the standards of apartment? Therefore, the best apartment can be found based on the research.
  
  In the Methodology part, it includes the distribution of the apartment building evaluation score to see some general statistics of the score like maximum, minimum...etc. and a multi-linear regression model to find the relationship between the evaluation score and "BALCONY_GUARDS", "ELAVATOR", "ENTRANCE_DOORS_WINDOWS", "ENTRANCE_LOBBY", "EXTERIOR_WALKWAYS", "GRAFFITI", "LAUNDRY_ROOMS", "SECURITY", "STAIRWELLS". Results of the linear regression model is provided in the Results section, and the interpretation of this results is presented in Discussion section.  

## Methodology

# Data
```{r, echo=FALSE}
z = na.omit(data)
```

```{r, echo=FALSE}
hist(as.numeric(z$SCORE), main = "Table1", xlab = "Evaluation Score")
boxplot(as.numeric(SCORE)~BALCONY_GUARDS, data = z, main="Table2", ylab = "Evaluation Score")
boxplot(as.numeric(SCORE)~GRAFFITI, data = z, main="Table3", ylab = "Evaluation Score")
```

The dataset used here is the "Apartment Building Evaluation" dataset from open data Toronto. The data is open to collect on the official website: https://open.toronto.ca/dataset/apartment-building-evaluation/. The target population of the data is all apartment building in Toronto, and the sample frame is the apartment building that is registered for the evaluation program, and our sample is those registered apartment buildings with well information( contains less N/A). The non-respondents in the sample data is removed, and the respondents with few N/A in certain variable is still included in the study. The N/A responses cause the analysis not very statistically significant which is the weakness of the study. For the features of our data, some of the variables are similar to each other, and some of the variables contains too many non-responses, therefore the analysis only contains the explanatory variables that are significant to analysis the safety and cleaness of apartments. The table 1 depicts the distribution of the response variable "evaluation score", the histograms show the score is generally normal distributed, with a mean around 77 and a max 100. The Table 2 and Table 3 are boxplots where show a positive relationship between the number of the balcony guards, graffiti and the evaluation score, which has the same result as our regression model in follow-up steps.

# Model

```{r, echo=FALSE}
zmodel = lm(as.numeric(SCORE)~as.numeric(BALCONY_GUARDS)+as.numeric(ELEVATORS)+as.numeric(ENTRANCE_DOORS_WINDOWS)+as.numeric(ENTRANCE_LOBBY)+as.numeric(EXTERIOR_WALKWAYS)+as.numeric(GRAFFITI)+as.numeric(LAUNDRY_ROOMS)+as.numeric(SECURITY)+as.numeric(STAIRWELLS), data = z)
summary(zmodel)
```

The linear regression model is used here where our response variable is evaluation score and the explanatory variables (the predictors) are balcony guards, elevators, entrance_door_windows, entrance_lobby, exterior walkways, graffiti, laundry rooms, security and stairwells. This model can illustrate how each explanatory variables contributed to the calculation on evaluation score, to get the most important factors on the safety of apartment buildings. The mathematical form of the model is $y_{score}$ = 4.7216 + 2.1471$x_{BalconyGuards}$ + 1.7996$x_{Elevators}$ + 2.0495$x_{EntranceDoorsandWindows}$ + 1.5654$x_{EntranceLobby}$ + 2.4991$x_{ExteriorWalkways}$ + 1.7094$x_{Graffiti}$ + 2.5380$x_{LaudryRooms}$ + 1.6029$x_{Security}$ + 2.4301$x_{Stairwells}$ + $error$. The reason of why selected these explanatory variables as predictors of the model is they are features that people are mostly concerned about an apartment. Based on their  test statistics and p-values, and R squared equals to 0.919, very close to 1, shows all these predictors are statistically significant for this linear regression models, and have a very strong evidence to show that these predictors are highly correlated to the calculation of the evaluation score. And the software using here is R. Some of these predictors are categorical variable, like graffiti, but still has different levels, so it can be interpret as numerical in the model.The model also contains on weakness since some of the points have high leverages, or with NAs, therefore the model needs to be improved by adding more factors and removing NAs. 

## Results

```{r,echo=FALSE,include=FALSE}
summary(as.numeric(z$SCORE))
sd(as.numeric(z$SCORE))
```

The table 1 shows the evaluation score of Toronto Apartment buildings is normally distributed with mean 77.94, median 79 and standard deviation of 10.124. Table 2 shows a positive relationship between categorical variable Balcony guards and the evaluation score, and the table 3 shows a positive relationship between Graffiti and evaluation score as well. The linear regression model shows with every 1 unit increases in balcony guards, the evaluation score of that building increases by 2.1471; with every additional 1 unit of elevator, the evaluation score increases by 1.7996; with every 1 unit increase in doors and windows entrance, the evaluation score increases by 2.0495; with every additional 1 unit of lobby entrance, the evaluation score will increase by 1.5654; with every 1 more unit in exterior walkway, the evaluation score will increase by 2.4991; with every 1 unit increases in graffiti, the evaluation score increases by 1.7904; with additional 1 unit in laundry room, the evaluation score will increase by 2.5380; with every 1 unit increase in security, the evaluation score will increase by 1.6029; with every additional 1 unit increase in stairwells, the evaluation score will increase by 2.4301. Therefore, we can analysis from this model, the most important factors on evaluating the apartment buildings in Toronto is the numbers of laundry rooms the buildings have.  

## Discussion

# Summary

Since more and more people come to Toronto and look for an apartment to live, it is interesting to find which factor of the apartment buildings people care about the most. Therefore, the analysis retrieved the apartment evaluation data from the Toronto open data website to find out which is the most important factor for evaluating the apartment in Toronto. In the Methodology section, we plot all apartments evaluation score from the dataset, and shown as a histogram(table 1), we conclude the evaluation score is normally distributed and has a mean at around 77. Meanwhile, in order to find the individual variable's relationship to the evaluation score, the table 2, and 3 plot shows the variable Balcony_Guards and Graffiti are both positive correlated to the evaluation score. To find all individual variable and their estimate coefficient of affecting the measure of evaluation score, a linear regression model with multi-variables show each variable's relationship to the evaluation score, and the results is listed in the result section.

# Conclusion

According to the result of linear regression model, the important factors of measuring the evaluation score of Toronto apartment buildings are numbers of laundry room, exterior walkways and stairwells. Therefore, when government or renter evaluate the apartment, the things that they care the most are how many laundry rooms the apartment has? how many exterior walkways outside of the apartment? and how many stairwells the apartment has? Where the more laundry rooms, stairwells and exterior walkways the apartment has, the higher evaluation score the apartment can get according our analysis model. 

The p-value and test statistic supports my argument and the variables are statistically significant, there are very strong evidence against the null hypothesis that the relationship between each individual variables and evaluation score is 0. Therefore our regression model is statistically significant and proper here.

# Weakness & Next Steps

The weakness of this analysis is there are many missing data from each variables of the government collected dataset. These missing value has negative influence on the regression model and plotting, where it becomes a omitted variable bias in analysis. Some evaluation score needs to be re-conducted since it was conducted two years ago or earlier; the data also only contains the apartments that were registered for the "RentSafeTo" program, many new-build apartments and non-registered apartments are not included in the evaluation, which our sampling frame does not meet our target population, we cannot make a conclusion on all Toronto Apartment based on this analysis.


In future, to continue the research topics on finding the most safe and clean apartment buildings based on the evaluation, the methods of post-stratification or difference in differences should be considered here, or focusing on the data collection method, to implement some sampling techniques or even use a survey on finding which factor do people mostly care about the apartment. Therefore, a more specific and generalized analysis can be reproduced here.

## References
1. City Of Toronto. Apartment Building Standards. Retrieved from: https://www.toronto.ca/community-people/housing-shelter/rental-housing-tenant-information/rental-housing-standards/apartment-building-standards/.

2. Municipal Licensing & Standards. Apartment Building Evaluation. License hold by: Open Government Licence-Toronto. Retrieved from: 
https://open.toronto.ca/dataset/apartment-building-evaluation/.

3. Open Data Toronto Package, R.

4. Toronto Municipal Code. "Apartment Buildings." Chapter: 354. 

5. Weisberg, Sanford. "Scatterplots and Regression." Applied Linear Regression. John Wiley & Sons, 2005. 1-17.
