# Display mktgCmpgn in the console
mktgCmpgn <- read_csv("output/mktCmpgn.csv")

mktgCmpgn <- mktgCmpgn%>%select(-ID,- Dt_Customer)

print(mktgCmpgn)

# Display the structure of mktgCmpgn in the console
str(mktgCmpgn)

# Display the summary of mktgCmpgn in the console
summary(mktgCmpgn)

# Split Data into Training and Testing set --------------------------------
# Feature Scaling 

mktgCmpgnScaled <- mktgCmpgn%>%
  mutate(Income = scale(Income),
         Recency = scale(Recency),
         MntWines = scale(MntWines),
         MntFruits = scale(MntFruits),
         MntFishProducts = scale(MntMeatProducts),
         MntMeatProducts = scale(MntMeatProducts),
         MntGoldProds = scale(MntGoldProds),
         MntSweetProducts = scale(MntSweetProducts),
         NumDealsPurchases = scale(NumDealsPurchases),
         NumWebPurchases =scale(NumWebPurchases),
         NumCatalogPurchases = scale(NumCatalogPurchases),
         NumStorePurchases = scale(NumStorePurchases),
         NumWebVisitsMonth = scale(NumWebVisitsMonth))



# Dummy code the Position using dummy.data.frame(),and convert back to a tibble 
mktgCmpgnScaled <- dummy_cols(mktgCmpgnScaled,
                               select_columns = c("Education","Marital_Status"))                                    

mktgCmpgnScaled<- mktgCmpgnScaled%>%select(-Education, -Marital_Status , -Age_group)
# set random seed to 154
set.seed(591)

# Randomly create 75% index rows to use for split 
sampleSet <- sample(nrow(mktgCmpgnScaled),
                    round(nrow(mktgCmpgnScaled) * 0.75),
                    replace = FALSE)

# Put the records from 75%  sample into mktgCmpgnTraining
mktgCmpgnTraining <- mktgCmpgnScaled[sampleSet,]


# Put the records from 75%  sample into mktgCmpgnTesting
mktgCmpgnTesting <- mktgCmpgnScaled[-sampleSet,]

str(mktgCmpgnTraining)

mktgCmpgnTraining <- mktgCmpgnTraining%>% rename(Education_2n_Cycle = `Education_2n Cycle`)
mktgCmpgnTesting <- mktgCmpgnTesting%>% rename(Education_2n_Cycle = `Education_2n Cycle`)
names(mktgCmpgnTraining)
mktgCmpgnTraining%>%select()
# Generate Neural network model
mktgCmpgnNeuralNet <- neuralnet(
  formula = Response ~ Income + Kidhome + Teenhome + Recency + MntWines + 
    MntFruits + MntMeatProducts + MntFishProducts + MntSweetProducts +
    MntGoldProds + NumDealsPurchases + NumWebPurchases + NumCatalogPurchases +
    NumStorePurchases + NumWebVisitsMonth +  AcceptedCmp3 + AcceptedCmp4 +
    AcceptedCmp5 + AcceptedCmp1 + AcceptedCmp2 + Complain + Age +
    Education_2n_Cycle + Education_Basic + Education_PhD + Education_Master +
    Education_Graduation + Marital_Status_Absurd + Marital_Status_Alone +
    Marital_Status_Divorced + Marital_Status_Married + Marital_Status_Single +
    Marital_Status_Together + Marital_Status_Widow + Marital_Status_YOLO,
  data = mktgCmpgnTraining, hidden = 4, act.fct = "logistic",
  linear.output = FALSE , threshold = 0.02)  

# Display neural network results
print(mktgCmpgnNeuralNet$result.matrix)

# Visualize the neural network
plot(mktgCmpgnNeuralNet)

# Use model to generate probabilities
mktgCmpgnProbability <- compute(mktgCmpgnNeuralNet,
                                     mktgCmpgnTesting)

# Display predictions
print(mktgCmpgnProbability$net.result)

# convert probabilities into 0/1 predictions
mktgCmpgnPredictions <- ifelse(
  mktgCmpgnProbability$net.result > 0.5, 1, 0) 

# Display 0/1 predictions on console
print(mktgCmpgnPredictions)

# Evaluate model by getting a confusion matrix
mktgCmpgnConfusionMatrix <- table(mktgCmpgnTesting$Response,
                                       mktgCmpgnPredictions)

# Display confusion matrix on console
print(mktgCmpgnConfusionMatrix)

# Determine the predictive accuracy
predictiveAccuracy <- sum(diag(mktgCmpgnConfusionMatrix)) / 
  nrow(mktgCmpgnTesting)

# Display the predictive accuracy in the console
print(predictiveAccuracy)
