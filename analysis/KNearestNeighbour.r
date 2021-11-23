# Madhavi Shukla
# MIS 545 Section 01
# Lab07ShuklaM.R
# This R application will predict Sedan Size for a given Price($USD), 
# Road Test Score(0-100) and Reliability(1-5) score by using k-NN method

# Install the tidyverse package 
# Have commented it after installing the package
# install.packages("tidyverse")
# install.packages("fastDummies")

# Load the tidyverse, class library
library(tidyverse)
library(class)
library(corrplot)
library(olsrr)
library(fastDummies)

# Set the working directory to Lab07 Folder created specifically for this
# application
setwd("C:/Users/ual-laptop/Desktop/Madhavi Shukla/MIS 545 DM/Project/")

# Read SedanSize.csv to the tibble variable sedanSize
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

marketingCampaign <- marketingCampaign %>% 
  mutate(Marital_Status = recode(Marital_Status,
                                 "Absurd" = "Single",
                                 "YOLO" = "Single",
                                 "Alone" = "Separated",
                                 "Together" ="Unmarried_couple")
  )

# select columns to convert and assign to an object
# cols <- c("Education","Marital_Status", "AcceptedCmp3", "AcceptedCmp4",
# "AcceptedCmp5", "AcceptedCmp1", "AcceptedCmp2", "Complain", "Response")

# convert the columns in the assigned object using the mutate_at function
# marketingCampaign <- marketingCampaign %>% 
# mutate_at(cols, factor)

# Address year
marketingCampaign <- marketingCampaign %>% 
  mutate(Year_Birth = ifelse(Year_Birth < 1940, 1940, Year_Birth))

# calculate age, assume 2014 as year of data extraction
marketingCampaign$Age <- 2014 - marketingCampaign$Year_Birth

# convert to date 
marketingCampaign$Dt_Customer <- 
  as.Date(marketingCampaign$Dt_Customer, '%d-%m-%Y')

marketingCampaign$Response <- 
  as.factor(marketingCampaign$Response)

# Impute missing values for Income using linear prediction
# Split data to train and test for missing values of income
marketingCampaignTrain <- marketingCampaign %>%
  filter(!is.na(Income))

# Build model
model <- lm(Income ~. - ID, marketingCampaignTrain)

# Check model summary
summary(model)

# Predict income
marketingCampaignTest <- marketingCampaign %>%
  filter(is.na(Income))

marketingCampaignPred <- predict(object = model,
                                 newdata = marketingCampaignTest)

# Assign predicted income to missing values
marketingCampaignTest$Income <- marketingCampaignPred

summary(marketingCampaignTest)

# Join datasets Train and Test
marketingCampaign <- rbind(marketingCampaignTrain,
                           marketingCampaignTest)

summary(marketingCampaign)

marketingCampaign$CustomerSinceDays <- 
  difftime("2014-12-31", marketingCampaign$Dt_Customer , units ="days")

marketingCampaign$CustomerSinceDays <- 
  round(as.numeric(str_sub(as.character(
    marketingCampaign$CustomerSinceDays, -4)
  )),0)


marketingCampaign$Education <- 
  as.factor(marketingCampaign$Education)

marketingCampaign$Marital_Status <- 
  as.factor(marketingCampaign$Marital_Status)
  
marketingCampaign <- dummy_cols(marketingCampaign, select_columns = c('Marital_Status', 'Education'))

# Display sedanSize in the console
print(marketingCampaign)

# Display the structure of the sedanSize in the console
str(marketingCampaign)

# Display the summary of sedanSize in the console
summary(marketingCampaign)

# Separate the tibble into two
# One called sedanSize with 3 variables 
# Another called sedanSizeLabels
marketingCampaignLabels <- marketingCampaign %>%
  select(Response)

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

# Call the displayAllHistograms() function, passing in sedanSize as an
# argument
displayAllHistograms(marketingCampaign)

# Split data into sedanSizeTraining, sedanSizeTrainingLabels & 
# sedanSizeTesting, sedanSizeTestingLabels dataset
# The set.seed() function is used to ensure that we can get the same result 
# every time we run a random sampling process
set.seed(517)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSet <- sample(nrow(marketingCampaign),
                    round(nrow(marketingCampaign) * 0.75),
                    replace = FALSE)

# Put the records from the 75% randomly sampled rows from the original dataset
marketingCampaignTraining <- marketingCampaign[sampleSet, ]
marketingCampaignTrainingLabels <- marketingCampaignLabels[sampleSet, ]

# Put all the other records (25%) into mobilePhoneTesting
marketingCampaignTesting <- marketingCampaign[-sampleSet, ]
marketingCampaignTestingLabels <- marketingCampaignLabels[-sampleSet, ]

# Generate the k-nearest neighbors model 
marketingCampaignPrediction <- knn(train = marketingCampaignTraining,
                                   test = marketingCampaignTesting,
                                   cl = marketingCampaignTrainingLabels$Response,
                                   k = 13)

summary(marketingCampaignTraining)

str(marketingCampaignTraining)

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

# Loop through different values of k to determine the best fitting model
# using odd numbers from 1 to number of observations in the training data set
for (kValue in 1:nrow(marketingCampaignTraining)) {
  # Only calculate predictive accuracy if the k value is odd
  if(kValue %% 2 != 0) {
    # Generate the model
    marketingCampaignPrediction <- knn(train = marketingCampaignTraining,
                                       test = marketingCampaignTesting,
                                       cl = marketingCampaignTrainingLabels$Response,
                                       k = kValue)
    
    # Generate a confusion matrix of predictions
    marketingCampaignConfusionMatrix <- table(marketingCampaignTestingLabels$Response,
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