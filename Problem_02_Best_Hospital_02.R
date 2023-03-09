rm(list=ls())

library(data.table)
library(dplyr)
library(naniar)

# Weirdly this inputs the NAs properly but doesn't allow for the select argument
# so we're not gonna use it.

# tripleoutcomes <- read.csv("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
#                            header = TRUE, sep = ",", stringsAsFactors = FALSE,
#                            na.strings = "Not Available")


tripleoutcomes <- fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                                          header = TRUE, sep = ",", stringsAsFactors = FALSE,
                                          na.strings = "Not Available", select = c(2,11,17,23))


outcomes_cleaned <- tripleoutcomes %>%
      replace_with_na_all(condition = ~.x %in% "Not Available") # Wondering about the weird ~.x syntax?
# check the help page for the function it says:

# condition required to be TRUE to set NA. Here, the condition is specified with a formula,
# following the syntax: ⁠~.x {condition}⁠. For example, writing ~.x < 20 would mean
# "where a variable value is less than 20, replace with NA".

?replace_with_na_all

outcomes <- as.numeric(unlist(outcomes_cleaned[,c(2:4)]))

matrix <- as.matrix(outcomes_cleaned[,c(2:4)]) # Almost right, but we lost column one. We could cbind it back in, or...

class(outcomes_cleaned) #tbl_df
is.list(outcomes_cleaned) # TRUE

dfoutcomes <- as.data.frame(outcomes_cleaned)

class(dfoutcomes)
# data.frame
sapply(outcomes_cleaned, typeof) # All character
sapply(dfoutcomes, typeof) # Obviously still all character

typeof(dfoutcomes) # list

tib <- as_tibble(dfoutcomes)

### OMG THANK YOU ###
tibmutated <- tib %>%
      mutate(across(2:4,as.numeric))
######################################
sapply(tibmutate, typeof) # Woot - we got character/numeric/numeric/numeric, like we wanted.

rm(tibmutated)
rm(tib)





colnames(tibmutated) <- c("Hospital","Attack","Failure","Pneumonia") # Oddly this IS the
# actual syntax for colnames - you pass it the names of the columns as a vector outside the parantheses.
# The original object is *IMMEDIATELY OVERWRITTEN*, so be careful with this.

str(tibmutated)

usethis::create_from_github(
      "https://github.com/BPMData/ProgAssignment3",
      destdir = "C:/Users/corma/OneDrive/Documents/R/Assignment3Git"
)
1
