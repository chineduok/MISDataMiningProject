mktgCmpgn <- mktgCmpgn%>%select(-ID, - Z_Revenue, - Z_CostContact)

# Split Data into Training and Testing set --------------------------------

# set random seed to 154
set.seed(154)

# Randomly create 75% index rows to use for split 
sampleSet <- sample(nrow(mktgCmpgn),
                    round(nrow(mktgCmpgn) * 0.75),
                    replace = FALSE)

# Put the records from 75%  sample into mktgCmpgnTraining
mktgCmpgnTraining <- mktgCmpgn[sampleSet,]


# Put the records from 75%  sample into mktgCmpgnTesting
mktgCmpgnTesting <- mktgCmpgn[-sampleSet,]


# Build the model ---------------------------------------------------------

# Generate the Naive Bayes model to predict mktgCmpgn

mktgCmpgnModel <- naiveBayes(formula = Response ~ .,
                                data = mktgCmpgnTraining,
                                laplace = 1)

# Build probability for each record in testing dataset

mktgCmpgnProbability <- predict(mktgCmpgnModel,
                                   mktgCmpgnTesting,
                                   type = "raw")

# Display probability 
print(mktgCmpgnProbability)


# Predict classes for each record in testing dataset
mktgCmpgnPrediction<- predict(mktgCmpgnModel,
                                 mktgCmpgnTesting,
                                 type = "class")


# Display prection in console
print(mktgCmpgnPrediction)


# Evaluating Performance --------------------------------------------------

# Evaluate model with confusion matrix

mktgCmpgnConfusionMatrix <- table(mktgCmpgnTesting$Response,
                                     mktgCmpgnPrediction)

# Display confusion matrix

print(mktgCmpgnConfusionMatrix)


# Determine the predictive accuracy
predictiveAccuracy <- sum(diag(mktgCmpgnConfusionMatrix)) / 
  nrow(mktgCmpgnTesting)

# Display the predictive accuracy in the console
print(predictiveAccuracy)

