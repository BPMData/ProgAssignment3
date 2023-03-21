library(data.table)
library(tibble)
library(tidyverse)

outcomes <- as.data.table(fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                                 header = TRUE, sep = ",", stringsAsFactors = FALSE,
                                 na.strings = "Not Available", select = c(2,7,11,17,23)) %>%
                                 naniar::replace_with_na_all(condition = ~.x %in% "Not Available") %>% # Doing it with :: removes the need to load another package we're only going to use for one function call one time.
                                 as_tibble %>%
                                 mutate(across(3:5, as.numeric)) %>%
                                 mutate(across(2, as.factor)) %>%
                                 setnames(c("Hospital","State","Heart Attack","Heart Failure","Pneumonia")))

best_hospital <- function(x, y) {
      statesub <- outcomes[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      index <- as.numeric(which(names(outcomes) == y))
      head(dplyr::select(sorted, c(1,index)),1)
}
best_hospital("NY", "Heart Attack")
best_hospital("CT", "Heart Failure")
best_hospital("AZ", "Pneumonia")
# I didn't actually need to create that index, but I like it this way, and it
# came in handy for rankhospital()



