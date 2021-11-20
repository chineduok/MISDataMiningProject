library(corrplot)
library(tidyverse)
mktCmpgn <- read_csv("output/mktCmpgn.csv")

str(mktCmpgn)

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
displayAllHistograms(mktCmpgn)



numericMtkgCmpgn <- mktgCmpgn %>%select_if(is.numeric)%>%select(-...1)

str(numericMtkgCmpgn)  
mktgCmpgnCorr <- round(cor(numericMtkgCmpgn),2)

mktgCmpgnCorr

# Display a correlation plot using the "number" method and output bottom left

corrplot(corr = mktgCmpgnCorr,method = "number", type = "lower")


# Get some insights with group by summaries
mktCmpgn %>%
  filter(Response ==1)%>%
  group_by(AcceptedCmp1,AcceptedCmp2,AcceptedCmp3,AcceptedCmp4,AcceptedCmp5,Response)%>%
  summarise(n())


df<- mktCmpgn %>%
  filter(Response ==1)%>%
  group_by(Education, AcceptedCmp1,AcceptedCmp2,AcceptedCmp3,AcceptedCmp4,AcceptedCmp5,Response)%>%
  summarise(n())

# close to half of those that responded did not accept previous campaigns

mktCmpgn %>%
  filter(Response ==0)%>%
  group_by(AcceptedCmp1,AcceptedCmp2,AcceptedCmp3,AcceptedCmp4,AcceptedCmp5,Response)%>%
  summarise(n())

# 

mktCmpgn%>%group_by(Response)%>%summarise(n())
