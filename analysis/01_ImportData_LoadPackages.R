
# Load Packages Required --------------------------------------------------



library(tidyverse)
library(e1071)
library(rpart.plot)
library(rpart)
library(estimatr)
library(neuralnet)
library(dummies)
library(fastDummies)


# Import Dataset ----------------------------------------------------------

df <- read_csv("input/marketing_campaign.csv")

glimpse(df)
names(arrange(df))
#MISDataMiningProject