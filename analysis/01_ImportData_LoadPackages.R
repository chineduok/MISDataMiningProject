
# Load Packages Required --------------------------------------------------



library(tidyverse)
library(e1071)
library(rpart.plot)
library(rpart)
library(estimatr)
library(neuralnet)
library(dummies)
library(fastDummies)
library(plm)
library(maxLik)
library(pglm)


# Import Dataset ----------------------------------------------------------

df <- read_csv("input/marketing_campaign.csv")

