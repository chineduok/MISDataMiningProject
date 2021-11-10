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
displayAllHistograms(mktgCmpgn)

# Drop z_CostContact and z_Revenue no variance
mktgCmpgn <- mktgCmpgn%>%select(-Z_CostContact, -Z_Revenue)

# Get some insights with group by summaries