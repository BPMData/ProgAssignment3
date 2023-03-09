getwd()
#"C:/Users/corma/OneDrive/Documents/R/Assignment3"


browseVignettes("data.table")
browseVignettes("dplyr")

library(dplyr)
library(data.table)
library(naniar)
library(styler)

?fread
outcomes <- fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                  header = TRUE, sep = ",", stringsAsFactors = FALSE,
                  na.strings = "Not Available")

head(outcomes)

is.na(outcomes[4,11])

is.na(outcomes[4,4])

outcomes[4,11]

class(outcomes)

outcomes2 <- fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                  header = TRUE, stringsAsFactors = FALSE,
                  na.strings = "Not Available")

install.packages("naniar")
library(naniar)

na_strings <- c("Not Available")

outcomes_cleaned <- outcomes %>%
      replace_with_na_all(condition = ~.x %in% na_strings)



outcomes3 <- outcomes2 <- fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                                stringsAsFactors = FALSE, sep = ",",
                                na.strings = "Not Available")

str(outcomes_cleaned)

rm(outcomes2, outcomes3)

outcomes_cleaned[, 11] <- as.numeric(outcomes_cleaned[, 11])


class(outcomes_cleaned)
class(outcomes)

#### FULL SOLUTION STARTS HERE####

library(data.table)
library(dplyr)
library(naniar)

#Trying to get the columns classified as numeric in the fread function.
outcomes <- fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                  header = TRUE, sep = ",", stringsAsFactors = FALSE,
                  colClasses=list(numeric=11))
# And failing.... moving on anyway.


na_strings <- c("Not Available")

outcomes_cleaned <- outcomes %>%
      replace_with_na_all(condition = ~.x %in% na_strings)

#Weird error message -  Attempt to override column 11 <<Hospital 30-Day Death
#(Mortality) Rates from Heart Attack>> of inherent type 'string' down to 'float64' ignored.
#Only overrides to a higher type are currently supported. If this was intended,
#please coerce to the lower type afterwards.

outcomes_cleaned[, 11] <- as.numeric(unlist(outcomes_cleaned[, 11]))




# Here's our plot of the 30-day mortality rates for heart attack, finally.
# This should've taken like 5 minutes lol.

hist(unlist(outcomes_cleaned[,11]), main = "Distribution of 30 Day Mortality Rates for Heart Attack
     Across US Hospitals",
     xlab = "30 Day Mortality Rates for Heart Attack", ylab = "Number of Hospitals")


### Testing something - delete if it doesn't work...
rm(outcomes_cleaned)


outcomes_cleaned <- as.numeric(unlist(outcomes_cleaned[,11]))

#Okay, if you set the left side of the assignment to just outcomes_cleaned and
# NOT outcomes_cleaned[, 11], you just get the vector of column 11. We don't want that!


# Everything below is BS

str(outcomes_cleaned)

variable11 <- as.numeric(unlist(outcomes_cleaned[, 11]))


hist(variable11)

is.numeric(outcomes_cleaned[,11])

class(outcomes_cleaned[,11])

typeof(outcomes_cleaned[,11])

# Testing

