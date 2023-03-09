rm(list=ls())

library(data.table)
library(dplyr)
library(naniar)

tripleoutcomes <- read.csv("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                   header = TRUE, sep = ",", stringsAsFactors = FALSE,
                   na.strings = "Not Available")

tripleoutcomes_the_fuck <- fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                                          header = TRUE, sep = ",", stringsAsFactors = FALSE,
                                          na.strings = "Not Available", select = c(2,11,17,23))

na_strings <- c("Not Available")

outcomes_cleaned <- tripleoutcomes_the_fuck %>%
      replace_with_na_all(condition = ~.x %in% na_strings)


outcomes <- as.numeric(unlist(outcomes_cleaned[,c(2:4)]))

matrix <- as.matrix(outcomes_cleaned[,c(2:4)])

class(outcomes_cleaned)

dfoutcomes <- as.data.frame(outcomes_cleaned)

class(dfoutcomes)


sapply(dfoutcomes, typeof)
class(dfoutcomes)

typeof(dfoutcomes)

tib <- as_tibble(dfoutcomes)

### OMG THANK YOU ###
tibmutate <- tib %>%
      mutate(across(2:4,as.numeric))
######################################

?rename

renamed <- rename(tibmutate, hospital name = Hospital, 2=Attack, 3=Failure, 4=Pneumonia)

renamed <- colnames(tibmutate) <- c("Hospital","Attack","Failure","Lungies")


str(tibmutate)

dataframe <- data.frame(dfoutcomes)
str(dataframe)

is.list(dataframe)
type?dplyr
?fread
?read.csv

?as.data.table


