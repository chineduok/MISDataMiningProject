# Install required packedges and load them
install.packages("tidyverse")
install.packages("corrplot")
install.packages("olsrr")
install.packages("smotefamily")
install.packages("dplyr")
library(tidyverse)
library(corrplot)
library(olsrr)
library(smotefamily)
library(dplyr)

# Set the file dictionary
setwd("/Users/liguanqi/Desktop/MIS 545/project")

# Convert csv file into tibble
df <- read.csv(file = "mktCmpgn.csv",
               header = TRUE)

summarise_at(group_by(df, Income_group),vars(MntMeatProducts),funs(mean(.,na.rm=TRUE)))
summarise_at(group_by(df, Income_group),vars(MntFishProducts),funs(mean(.,na.rm=TRUE)))
summarise_at(group_by(df, Income_group),vars(MntSweetProducts),funs(mean(.,na.rm=TRUE)))
summarise_at(group_by(df, Income_group),vars(MntGoldProds),funs(mean(.,na.rm=TRUE)))
summarise_at(group_by(df, Income_group),vars(MntFruits),funs(mean(.,na.rm=TRUE)))
summarise_at(group_by(df, Income_group),vars(MntWines),funs(mean(.,na.rm=TRUE)))

# Change the columns' types
df$Income <- as.numeric(df$Income)
df$Teenhome <- as.numeric(df$Teenhome)
df$Recency <- as.numeric(df$Recency)
df$AcceptedCmp1 <- as.numeric(df$AcceptedCmp1)
df$AcceptedCmp2 <- as.numeric(df$AcceptedCmp2)
df$AcceptedCmp3 <- as.numeric(df$AcceptedCmp3)
df$AcceptedCmp4 <- as.numeric(df$AcceptedCmp4)
df$AcceptedCmp5 <- as.numeric(df$AcceptedCmp5)
df$Complain <- as.numeric(df$Complain)
df$Response <-as.numeric(df$Response)

# Check the summary and strcture of the tibble
print(df)
summary(df)
str(df)

# Remove the columnes which are not required for this algorithm
df2 <- select(df, -c(X, ID, Education, Marital_Status, Dt_Customer,
                     Age_group, Income_group, CustomerSinceGroup))
# Create a new tibble to sotre target data
data2 <- df2       

# Display the sturcture of data2
str(data2)
print(data2)
summary(data2)

# Create a correlation plot
corrplot(cor(data2),
         method = "shade",
         type = "lower",
         diag = TRUE,
         tl.col = "black",
         bg = "white",
         title = "Correlation Plot",
         col = NULL)

# Create sample for for testing and training
sampleSet <- sample(nrow(df2),
                    round(nrow(df2) * 0.75),
                    replace = FALSE)

# Put the records from 75% sample into mobilePhoneTraining
Training <- df2[sampleSet, ]

# Put all other records (25%) into mobilePhoneTesting
Testing <- df2[-sampleSet, ]

summary(Training$Complain)

# Generate the logistic regression model
Model <- glm(data = df2,
            family = binomial,
            formula = Response ~ .)

# Display the output of the logistic regression model
summary(Model)

# Use the model to predict outcomes in the testing dataset
prediction <- predict(Model,
                      Testing,
                      type = "response")

# Display the prediction on the concole
print(Prediction)

# Treat anything below or equal to 0.5 as a 0, anything above 0.5 as 1
Prediction <- ifelse(Prediction >= 0.5, 1, 0)

# Create a confusion matrix
ConfusionMatrix <- table(Testing$Response,
                          Prediction)

# Display the confusion matrix
print(ConfusionMatrix)

# Calculate the false positive rate
ConfusionMatrix[1, 2] /
  (ConfusionMatrix[1, 2] + 
     ConfusionMatrix[1, 1])

# Calculate the true positive rate
ConfusionMatrix[2, 1] /
  (ConfusionMatrix[2, 1] + 
     ConfusionMatrix[2, 2])

# Calculate the accuracy of the models
sum(diag(ConfusionMatrix)) / nrow(Testing)
