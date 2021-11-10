# Display mktgCmpgn in the console
print(mktgCmpgn)

# Display the structure of mktgCmpgn in the console
str(mktgCmpgn)

# Display the summary of mktgCmpgn in the console
summary(mktgCmpgn)

# Split Data into Training and Testing set --------------------------------
# Feature Scaling 
names(mktgCmpgn)

# Scale AnnualIncome and CatchRate
mktgCmpgn <- mktgCmpgn%>%
  mutate(IncomeScaled = scale(Income),
         RecencyScaled = scale(Recency),
         MntWinesScaled = scale(MntWines),
         MntFruitsScaled = scale(MntFruits),
         MntFishProductsScaled = scale(MntMeatProducts),
         MntMeatProductsScaled = scale(MntMeatProducts),
         MntGoldProdsScaled = scale(MntGoldProds),
         MntSweetProductsScaled = scale(MntSweetProducts),
         NumDealsPurchasesScaled = scale(NumDealsPurchases),
         NumWebPurchasesScaled =scale(NumWebPurchases),
         NumCatalogPurchasesScaled = scale(NumCatalogPurchases),
         NumStorePurchasesScaled = scale(NumStorePurchases),
         NumWebVisitsMonthScaled = scale(NumWebVisitsMonth))


mktgCmpgnScaled <- mktgCmpgn%>%
  select(-Income,-Recency,-MntWines,-MntFruits,-MntMeatProducts,
         -MntFishProducts,-MntGoldProds, -MntSweetProducts, -NumDealsPurchases,
         -NumWebPurchases, -NumStorePurchases, -NumWebVisitsMonth,
         -NumCatalogPurchases, -Dt_Customer)
         
# Dummy code the Position using dummy.data.frame(),and convert back to a tibble 
mktgCmpgnScaled2 <- dummy_cols(mktgCmpgnScaled,
                               select_columns = c("Education","Marital_Status"))                                    

mktgCmpgnScaled2<- mktgCmpgnScaled2%>%select(-Education, -Marital_Status)
# set random seed to 154
set.seed(591)

# Randomly create 75% index rows to use for split 
sampleSet <- sample(nrow(mktgCmpgnScaled2),
                    round(nrow(mktgCmpgnScaled2) * 0.75),
                    replace = FALSE)

# Put the records from 75%  sample into mktgCmpgnTraining
mktgCmpgnTraining <- mktgCmpgnScaled2[sampleSet,]


# Put the records from 75%  sample into mktgCmpgnTesting
mktgCmpgnTesting <- mktgCmpgnScaled2[-sampleSet,]

str(mktgCmpgnTraining)



# Generate Neural network model
mktgCmpgnNeuralNet <- neuralnet(
  formula = Response ~ -ID -Year_Birth,
  data = mktgCmpgnTraining, hidden = 50, act.fct = "logistic",
  linear.output = FALSE)  

# Display neural network results
print(mktgCmpgnNeuralNet$result.matrix)

# Visualize the neural network
plot(mktgCmpgnNeuralNet)

# Use model to generate probabilities
mktgCmpgnProbability <- compute(mktgCmpgnNeuralNet,
                                     mktgCmpgnTesting)
mktgCmpgnProbability
# Display predictions
print(mktgCmpgnProbability$net.result)

# convert probabilities into 0/1 predictions
mktgCmpgnPredictions <- ifelse(
  mktgCmpgnProbability$net.result > 0.5,1,0) 

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
