# Convert columns to factors

# select columns to convert and assign to an object
cols <- c("Education","Marital_Status","AcceptedCmp3","AcceptedCmp4",
          "AcceptedCmp5","AcceptedCmp1", "AcceptedCmp2","Complain","Response" )

mktgCmpgnUnProcessed <- mktgCmpgnUnProcessed%>%mutate(Marital_Status = recode(Marital_Status,
                                            "Absurd" = "Single",
                                            "YOLO" = "Single",
                                            "Alone" = "Separated",
                                            "Together" ="Unmarried_couple"))

# convert the columns in the assigned object using the mutate_at function
mktgCmpgnUnProcessed1 <- mktgCmpgnUnProcessed%>%mutate_at(cols, factor)


# Check to see converted columns
glimpse(mktgCmpgnUnProcessed1)

# Display data on console
print(mktgCmpgnUnProcessed1)

# Display data summary
summary(mktgCmpgnUnProcessed1)


# Address year
mktgCmpgnUnProcessed1 <- mktgCmpgnUnProcessed1%>%mutate(Year_Birth = ifelse(Year_Birth < 1940, 1940, Year_Birth))

# calculate age, assume 2014 as year of data extraction
mktgCmpgnUnProcessed1$Age <- 2014 - mktgCmpgnUnProcessed1$Year_Birth

# Group age
mktgCmpgnUnProcessed1 <- mktgCmpgnUnProcessed1%>%
  mutate(Age_group = 
           case_when(Age <= 25 ~ "<=25 years",
                     Age <= 50 ~ "26- 50 years",
                     Age > 50 ~ "Above 50 years")
         )
mktgCmpgnUnProcessed1$Age_group <- factor(mktgCmpgnUnProcessed1$Age_group)

# check grouped age column
mktgCmpgnUnProcessed1%>%select(Age, Age_group)%>%filter(Age>50)

# convert to date 
mktgCmpgnUnProcessed1$Dt_Customer <- as.Date(mktgCmpgnUnProcessed1$Dt_Customer, '%m/%d/%Y')
summary(mktgCmpgnUnProcessed1)


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
displayAllHistograms(mktgCmpgnUnProcessed1)

# Drop z_CostContact and z_Revenue no variance and year of birth

mktgCmpgnUnProcessed1 <- mktgCmpgnUnProcessed1%>%select( -Year_Birth, -Z_CostContact, -Z_Revenue)

# Impute missing values for Income using linear prediction
# Split data to train and test for missing values of income
mktgCmpgnUnProcessedTrain <- mktgCmpgnUnProcessed1%>%filter(!is.na(Income))

# Build model
model <- lm(Income ~. - ID , mktgCmpgnUnProcessedTrain)

# Check model summary
summary(model)

# Predict income
mktgCmpgnUnProcessedTest <- mktgCmpgnUnProcessed1%>%filter(is.na(Income))

pred <- predict(object = model,newdata = mktgCmpgnUnProcessedTest)

# Assign predicted income to missing values
mktgCmpgnUnProcessedTest$Income <- pred
summary(mktgCmpgnUnProcessedTest)

# Join datasets Train and Test
mktgCmpgn <- rbind(mktgCmpgnUnProcessedTrain,mktgCmpgnUnProcessedTest)
summary(mktgCmpgn)
head(mktgCmpgn)

# Group Income
mktgCmpgn <- mktgCmpgn%>%
  mutate(Income_group = 
           case_when(Income <= mean(Income) - sd(Income) ~ "Low Income",
                     Income > mean(Income) - sd(Income) & Income < mean(Income) + sd(Income) ~ "Middle Income",
                     Income >= mean(Income) + sd(Income) ~ "High Income")
  )
mktgCmpgn$Income_group <- factor(mktgCmpgn$Income_group)
mktgCmpgn$Kidhome <- factor(mktgCmpgn$Kidhome)
mktgCmpgn$Teenhome <- factor(mktgCmpgn$Teenhome)


mktgCmpgn$CustomerSinceDays <- difftime("2014-12-31", mktgCmpgn$Dt_Customer , units ="days")

mktgCmpgn$CustomerSinceDays <- round(as.numeric(str_sub(as.character(mktgCmpgn$CustomerSinceDays, -4))),0)
mktgCmpgn <- mktgCmpgn%>%
  mutate(CustomerSinceGroup = 
           case_when(CustomerSinceDays <= 365 ~ "<= 12 months",
                     CustomerSinceDays <= 547 ~ "<= 18 months",
                     CustomerSinceDays > 547 ~ "> 18 months")
         
  )

mktgCmpgn$CustomerSinceGroup <- factor(mktgCmpgn$CustomerSinceGroup)

write.csv(x = mktgCmpgn, file = "output/mktCmpgn.csv")

