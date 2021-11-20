mktgCmpgnWide <- read_csv("output/mktCmpgn.csv")

# Rename Response to AcceptedCmp6
mktgCmpgnWide <- mktgCmpgnWide%>%
  rename(AcceptedCmp6 = Response)
# reshape from wide to long
mktgCmpgn <- gather(mktgCmpgnWide, key = Campaign, 
                        value = Response, AcceptedCmp1, 
                        AcceptedCmp2, AcceptedCmp3, 
                        AcceptedCmp4, AcceptedCmp5, AcceptedCmp6)

mktgCmpgn$Campaign <-str_sub(mktgCmpgn$Campaign,9) 


# Split to Train (Cmp1 - Cmp5) and Test(Cmp6) 
mktgCmpgnTrain <- mktgCmpgn%>%filter(Campaign != "Cmp6")

mktgCmpgnTest <- mktgCmpgn %>%filter(Campaign == "Cmp6")

plm_model <- pglm(Response ~ Education + Marital_Status + Income + Kidhome + Teenhome +
                    Recency + MntWines + MntFruits + MntMeatProducts + MntFishProducts + 
                    MntSweetProducts + MntGoldProds + NumDealsPurchases + NumWebPurchases +
                    NumCatalogPurchases + NumStorePurchases + NumWebVisitsMonth +
                    Complain  , data = mktgCmpgnTrain,
                  family = "binomial", model = "within", method = "bfgs", 
                  index = c("ID","Campaign"), effect = "twoways",start = NULL)
summary(plm_model)
