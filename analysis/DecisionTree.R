# Aliptha Nagaraju
# MIS545 001

# Install required packages
# install.packages("rpart.plot")

# Load libraries
library(tidyverse)
library(rpart)
library(rpart.plot)


# set the working directory
setwd("~/Desktop/Eller/Data Mining/Project")

# Read the csv file 
markcamp <- read_csv (file = "mktCmpgn.csv",
                       col_types = "iicciffiiiiiiiiiiiiiffffffiif",
                       col_names = TRUE)

# Display markcamp  tibble on the console window
print(markcamp)

# Display structure of markcamp tibble on the console window
str(markcamp)

# Display structure of markcamp tibble on the console window
summary(markcamp)

# Splitting data into training and testing data set and ensuring same random 
# sampling process through setseet
set.seed(1103)

# Create a vector of 75% randomly sampled rows from original dataset
sampleset <- sample(nrow(markcamp),
                    round(nrow(markcamp) * 0.75),
                    replace = FALSE)

# Put the records from 75% sample into training
markcampTraining <- markcamp[sampleset, ]

# Put the records from 25% sample into training
markcampTesting <- markcamp[-sampleset, ]

# Train the Decision tree model with complexity of 0.01
markcampTypeModel <- rpart(formula =  Response ~ ., 
                            method  = "class",
                            cp      = 0.01,
                            data    = markcampTraining)

# Display the decision tree plot.
rpart.plot(markcampTypeModel)

# Predict classes for each record in the dataset
markcampPrediction <- predict(markcampTypeModel,
                               markcampTesting,
                               type = "class")

# Display the prediction from dwellingTypePrediction on the console
print(markcampPrediction)

# Evaluate the model by forming a confusion matrix
markcampConfusionMatrix <- table(markcampTesting$Response,
                                 markcampPrediction)

# Display the prediction from riceFarmsPrediction as a confusion matrix
# on the console
print(markcampConfusionMatrix)

# calculate model's predictive accuracy
predictiveAccuracy <- sum(diag(markcampConfusionMatrix)) / 
  nrow(markcampTesting)

# Display predictive accuracy on the console
print(predictiveAccuracy)




# Train the Decision tree model with complexity of 0.007
markcampTypeModel1 <- rpart(formula =  Response ~ ., 
                           method  = "class",
                           cp      = 0.007,
                           data    = markcampTraining)

# Display the decision tree plot.
rpart.plot(markcampTypeModel1)

# Predict classes for each record in the dataset
markcampPrediction1 <- predict(markcampTypeModel1,
                              markcampTesting,
                              type = "class")

# Display the prediction from dwellingTypePrediction on the console
print(markcampPrediction1)

# Evaluate the model by forming a confusion matrix
markcampConfusionMatrix1 <- table(markcampTesting$Response,
                                 markcampPrediction1)

# Display the prediction from riceFarmsPrediction as a confusion matrix
# on the console
print(markcampConfusionMatrix1)

# calculate model's predictive accuracy
predictiveAccuracy1 <- sum(diag(markcampConfusionMatrix1)) / 
  nrow(markcampTesting)

# Display predictive accuracy on the console
print(predictiveAccuracy1)





