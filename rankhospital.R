library(tidyverse)

# Read in our data
outcomes <- data.table::fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                              header = TRUE, sep = ",", stringsAsFactors = FALSE,
                              na.strings = "Not Available", select = c(2,7,11,17,23)) %>%
      naniar::replace_with_na_all(condition = ~.x %in% "Not Available") %>% # Doing it with :: removes the need to load another package we're only going to use for one function call one time.
      as_tibble %>%
      mutate(across(3:5, as.numeric)) %>%
      mutate(across(2, as.factor)) %>%
      data.table::setnames(c("Hospital","State","Heart Attack","Heart Failure","Pneumonia"))

# Create our list of data frames split by state
outcomes.split <- split(outcomes, outcomes$State)


# Create our outcome ranks vectors...

getRank <- function(df, column.name) {
      ColumnRank <- rank(df[[column.name]], ties.method = "first", na.last = "keep")
      ColumnRank
}

PneumoniaRanks <- sapply(X = outcomes.split, FUN = getRank, column.name = "Pneumonia")
AttackRanks <- sapply(X = outcomes.split, FUN = getRank, column.name = "Heart Attack")
FailureRanks <- sapply(X = outcomes.split, FUN = getRank, column.name = "Heart Failure")

# And bind these vectors back to their respective data frames...

for (i in 1:length(outcomes.split)) {outcomes.split[[i]] <- cbind(outcomes.split[[i]], PneumoniaRanks[[i]])
colnames(outcomes.split[[i]])[ncol(outcomes.split[[i]])] <- "PneumoniaRank"
}

for (i in 1:length(outcomes.split)) {outcomes.split[[i]] <- cbind(outcomes.split[[i]], AttackRanks[[i]])
colnames(outcomes.split[[i]])[ncol(outcomes.split[[i]])] <- "AttackRank"
}

for (i in 1:length(outcomes.split)) {outcomes.split[[i]] <- cbind(outcomes.split[[i]], FailureRanks[[i]])
colnames(outcomes.split[[i]])[ncol(outcomes.split[[i]])] <- "FailureRank"
}

# Now for the actual rankhospital function...

rankhospital <- function(x,y,num) {
      statesubset <- outcomes.split[[x]]
      statesubset
      statenames <- names(outcomes.split)
      if (!(x %in% statenames)){
            stop("invalid state")
      }
      index <- as.numeric(which(names(statesubset) == y))
      if (length(index) == 0) {
            stop("invalid outcome")
      } else if (index == 5) {
            rank <- 6
      } else if (index == 3) {
            rank <- 7
      } else if (index == 4) {
            rank <- 8
      } else if (index %in% c(1, 2, 6, 7, 8)) {
            stop("invalid outcome")
      }
      outcome <- select(statesubset, c(1,rank, index))

      # Now to calculate z based off of num, including best and worst

      if (num == "best") {
            z = 1
      } else if (num == "worst") {
            z = max(na.omit(outcome[,2]))
      } else {
            z = num
      }
      output <- outcome[outcome[,2] == z, 1]
      finaloutput <- head(output[complete.cases(output)],1)

      if (length(finaloutput) == 0) {
            print("NA")
      }
      else finaloutput
}

rankhospital("CA","Heart Failure", 1)

rankhospital("CA","Heart Failure", "best")

rankhospital("CA","Heart Failure", 285)

rankhospital("CA","Heart Failure", 286)

rankhospital("CA","Heart Failure", "worst")

rankhospital("Alaska","Heart Failure",1)

rankhospital("AK","Motion Sickness",1)

rankhospital("AK", "Heart Failure", 1)
rankhospital("AK", "Heart Failure", 2)
rankhospital("AK", "Heart Failure", 3)
rankhospital("AK", "Heart Failure", 4)
rankhospital("AK", "Heart Failure", 5)

