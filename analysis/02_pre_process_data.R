# Convert 11 columns to factors
glimpse(df)
names(df)

# select columns to convert and assign to an object
cols <- c("Education","Marital_Status","AcceptedCmp3","AcceptedCmp4",
          "AcceptedCmp5","AcceptedCmp1", "AcceptedCmp2","Complain","Response" )

# convert the columns in the assigned object using the mutate_at function
df1 <- df%>%mutate_at(cols, factor)


# Check to see converted columns
glimpse(df1)

# Display data on console
print(df1)

# Display data summary
summary(df1)


# Address year

df1 <- df1%>%mutate(Year_Birth = ifelse(Year_Birth < 1940, 1940, Year_Birth))

# calculate age, assume 2014 as year of data extraction


# Calculate age
df1$Age <- 2014 - df1$Year_Birth

# Group age
df1 <- df1%>%
  mutate(Age_group = 
           case_when(Age <= 25 ~ "<=25 years",
                     Age <= 50 ~ "26- 50 years",
                     Age > 50 ~ "Above 50 years")
         )
df1$Age_group <- factor(df1$Age_group)

df1%>%select(Age, Age_group)%>%filter(Age>50)

# convert to date 
df1$Dt_Customer <- as.Date(df1$Dt_Customer, '%m/%d/%Y')
summary(df1)

# de-select columns not required

# Display Histograms of the data
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>% 
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes( x = value, fill = key),
                              color = "black") +
    facet_wrap(~ key, scales = "free") +
    theme_minimal()
}
displayAllHistograms(df1)

# Drop z_CostContact and z_Revenue no variance and year of birth

df1 <- df1%>%select( -Year_Birth, -Z_CostContact, -Z_Revenue)

# Impute missing values for Income using linear prediction
# Split data to train and test for missing values of income
dfTrain <- df1%>%filter(!is.na(Income))

# Build model
model <- lm(Income ~. - ID , dfTrain)

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

write.csv(mktgCmpgn,"output/mktCmpgn.csv")
