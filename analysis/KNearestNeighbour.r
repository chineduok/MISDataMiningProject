# Madhavi Shukla
# MIS 545 Section 01
# KNearestNeighbour.R
# This R application will predict the response of a customer to a marketing 
# campaign based on the K-nearest neighbour model.

# Install the tidyverse, class, corrplot, olsrr, fastDummies package 
# Have commented it after installing the package
# install.packages("tidyverse")
# install.packages("class")
# install.packages("corrplot")
# install.packages("olsrr")
# install.packages("fastDummies")


# Load the tidyverse, class, corrplot, olsrr, fastDummies library
library(tidyverse)
library(class)
library(corrplot)
library(olsrr)
library(fastDummies)

# Set the working directory to Lab07 Folder created specifically for this
# application
setwd("C:/Users/ual-laptop/Desktop/Madhavi Shukla/MIS 545 DM/Project/")

# Read MarketingCampaign.csv to the tibble variable marketingCampaign
# Mention the column types explicitly as per the requirements
# l for logical
# n for numeric
# i for integers
# c for characters
# f for factors
# D for dates
# T for datetimes
marketingCampaign  <- read_csv(file = "MarketingCampaign.csv", 
                               col_types = "nnccnnncnnnnnnnnnnnnnnnnnnnnl",
                               col_names = TRUE)

# Group Marital Status into 4 categories
marketingCampaign <- marketingCampaign %>% 
  mutate(Marital_Status = recode(Marital_Status,
                                 "Absurd" = "Single",
                                 "YOLO" = "Single",
                                 "Alone" = "Separated",
                                 "Together" ="Unmarried_couple"))

# Clean the year column
marketingCampaign <- marketingCampaign %>% 
  mutate(Year_Birth = ifelse(Year_Birth < 1940, 1940, Year_Birth))

# Calculate age assuming 2014 as year of data extraction
marketingCampaign$Age <- 2014 - marketingCampaign$Year_Birth

# Convert Dt_Customer in Date format
marketingCampaign$Dt_Customer <- 
  as.Date(marketingCampaign$Dt_Customer, '%d-%m-%Y')

# Convert Response to factor data type
marketingCampaign$Response <- 
  as.factor(marketingCampaign$Response)

# Impute missing values for Income using linear prediction
# Split data to train and test for missing values of income
marketingCampaignTrain <- marketingCampaign %>%
  filter(!is.na(Income))

marketingCampaignTest <- marketingCampaign %>%
  filter(is.na(Income))

# Build a linear model for income
model <- lm(Income ~. - ID, marketingCampaignTrain)

# Check linear model summary
summary(model)

# Predict the income based on the model
marketingCampaignPred <- predict(object = model,
                                 newdata = marketingCampaignTest)

# Assign predicted income to missing values of income
marketingCampaignTest$Income <- marketingCampaignPred

# Join datasets Train and Test
marketingCampaign <- rbind(marketingCampaignTrain,
                           marketingCampaignTest)

# Print summary of the marketingCampaign
summary(marketingCampaign)

# Create a new feature CustomerSinceDays
marketingCampaign$CustomerSinceDays <- 
  difftime("2014-12-31", marketingCampaign$Dt_Customer , units ="days")

marketingCampaign$CustomerSinceDays <- 
  round(as.numeric(str_sub(as.character(
    marketingCampaign$CustomerSinceDays, -4)
  )),0)

# Convert Education to factor data type
marketingCampaign$Education <- 
  as.factor(marketingCampaign$Education)

# Convert Marital_Status to factor data type
marketingCampaign$Marital_Status <- 
  as.factor(marketingCampaign$Marital_Status)

# Create dummy columns for Marital status and Education   
marketingCampaign <- 
  dummy_cols(marketingCampaign, 
             select_columns = c('Marital_Status', 'Education'))

# Display marketingCampaign in the console
print(marketingCampaign)

# Display the structure of the marketingCampaign in the console
str(marketingCampaign)

# Display the summary of marketingCampaign in the console
summary(marketingCampaign)

# Separate the tibble into two
# One called marketingCampaign with 3 variables 
# Another called marketingCampaignLabels
marketingCampaignLabels <- marketingCampaign %>%
  select(Response)

# Remove the continuous features from the tibble
marketingCampaign <- marketingCampaign %>% 
  select(-Year_Birth, 
         -Z_CostContact, 
         -Z_Revenue, 
         -Dt_Customer, 
         -Response,
         -Income,
		     -Marital_Status,
		     -Education)

# Create a function called displayAllHistograms that takes in a tibble 
# parameter that will displays histogram for all numeric features in the 
# tibble
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>% 
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x=value, fill=key),
                              color = "black") +
    facet_wrap (~ key, scales = "free") +
    theme_minimal()
}

# Call the displayAllHistograms() function, passing in marketingCampaign as an
# argument
displayAllHistograms(marketingCampaign)

# Split data into marketingCampaignTraining, marketingCampaignTrainingLabels & 
# marketingCampaignTesting, marketingCampaignTestingLabels dataset
# The set.seed() function is used to ensure that we can get the same result 
# every time we run a random sampling process
set.seed(123)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSet <- sample(nrow(marketingCampaign),
                    round(nrow(marketingCampaign) * 0.75),
                    replace = FALSE)

# Put the records from the 75% randomly sampled rows from the original dataset
marketingCampaignTraining <- marketingCampaign[sampleSet, ]
marketingCampaignTrainingLabels <- marketingCampaignLabels[sampleSet, ]

# Put all the other records (25%) into marketingCampaignTesting
marketingCampaignTesting <- marketingCampaign[-sampleSet, ]
marketingCampaignTestingLabels <- marketingCampaignLabels[-sampleSet, ]

# Generate the k-nearest neighbors model 
marketingCampaignPrediction <- knn(train = marketingCampaignTraining,
                                   test = marketingCampaignTesting,
                                   cl = marketingCampaignTrainingLabels$Response,
                                   k = 13)

# Display the prediction summary from the testing dataset on the console
summary(marketingCampaignPrediction)

# Display the predictions from the testing dataset on the console
print(marketingCampaignPrediction)

# Display summary of the predictions from the testing dataset
print(summary(marketingCampaignPrediction))

# Generate a confusion matrix of predictions
marketingCampaignConfusionMatrix <- table(marketingCampaignTestingLabels$Response,
                                          marketingCampaignPrediction)

# Display the confusion matrix on the console
print(marketingCampaignConfusionMatrix)

# Calculate the model predictive accuracy and store it into a variable 
predictiveAccuracy <- sum(diag(marketingCampaignConfusionMatrix)) / 
  nrow(marketingCampaignTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracy)

# Create a matrix of k-values with their predictive accuracy 
kValueMatrix <- matrix(data = NA,
                       nrow = 0,
                       ncol = 2)

# Assign column names to the matrix
colnames(kValueMatrix) <- c("k value", "Predictive Accuracy")
nrow(marketingCampaignTraining)
# Loop through different values of k to determine the best fitting model
# using odd numbers from 1 to number of observations in the training data set
for (kValue in 1:nrow(marketingCampaignTraining)) {
  # Only calculate predictive accuracy if the k value is odd
  if(kValue %% 2 != 0) {
    # Generate the model
    marketingCampaignPrediction <- 
      knn(train = marketingCampaignTraining,
          test = marketingCampaignTesting,
          cl = marketingCampaignTrainingLabels$Response,
          k = kValue)
    
    # Generate a confusion matrix of predictions
    marketingCampaignConfusionMatrix <-
      table(marketingCampaignTestingLabels$Response,
            marketingCampaignPrediction)
    
    # Calculate the model predictive accuracy and store it into a variable 
    predictiveAccuracy <- sum(diag(marketingCampaignConfusionMatrix)) / 
      nrow(marketingCampaignTesting)
    
    # Add a new row to the kValueMatrix
    kValueMatrix <- rbind(kValueMatrix, c(kValue, predictiveAccuracy))
  }
}

# Display the kValueMatrix on the console to determine the best k-value
print(kValueMatrix)
