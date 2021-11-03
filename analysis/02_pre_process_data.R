# Convert 11 columns to factors
glimpse(df)
names(df)

# select columns to convert and assign to an object
cols <- c("Education","Marital_Status","AcceptedCmp3","AcceptedCmp4",
          "AcceptedCmp5","AcceptedCmp1", "AcceptedCmp2","Complain","Response" )

# convert the columns in the assigned object using the mutate_at function
df1 <- df%>%mutate_at(cols, factor)
df1$ID <- as.character(df1$ID)

# Check to see converted columns
glimpse(df1)

# Display data on console
print(df1)

# Display data summary
summary(df1)



# Impute missing values for Income using linear prediction
# Split data to train and test for missing values of income
dfTrain <- df1%>%filter(!is.na(Income))

# Build model
model <- lm_robust(Income ~. -ID -Dt_Customer -Z_CostContact, dfTrain)

# Check model summary
summary(model)

# Predict income
dfTest <- df1%>%filter(is.na(Income))

pred <- predict(object = model,newdata = dfTest)

# Assign predicted income to missing values
dfTest$Income <- pred
summary(dfTest)

# Join datasets Train and Test
mktgCmpgn <- rbind(dfTrain,dfTest)
summary(mktgCmpgn)

