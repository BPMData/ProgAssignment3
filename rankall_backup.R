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

# The actual function itself:

rankall <- function(outcome, num) {
      # Let's first get our invalid outcome stop message out of the way...
      if(!(outcome %in% c("Heart Failure","Heart Attack","Pneumonia")))
            stop("invalid outcome")

      # Create an empty data frame to store the results
      results <- data.frame(hospital = character(),
                            state = factor(),
                            stringsAsFactors = FALSE)

      # Create the index number that tells the function which column to look for
      # the appropriate rank depending on the outcome entered
      if(outcome == "Heart Attack"){index = 7}
      else if (outcome == "Heart Failure"){index = 8}
      else if (outcome == "Pneumonia"){index = 6}
      # Wow that was so much cleaner than how I did it in the other function

      # Now our for loop to create our results data frame

      for (i in 1:length(outcomes.split)) {

            # First, to determine which rank # constitutes "worst" for
            # each state and outcome combination:

            worstdeterminant <- outcomes.split[[i]][index]

            if (num == "best") {
                  z = 1
            } else if (num == "worst") {
                  z = max(na.omit(worstdeterminant[,1]))
            } else {
                  z = num
            }

            # Then to find the appropriate hospital for each state depending on the arguments entered

            statesubset <- subset(outcomes.split[[i]], outcomes.split[[i]][index] == z)[1:2]
            data.table::setnames(statesubset, c("hospital", "state"))
            # We're going to have to rename the columns eventually, doing it here prevents
            # an rbind error when creating the dummy rows for states with NAs.

            if (nrow(statesubset) == 0) {
                  results <- rbind(results, data.frame(hospital = NA, state = names(outcomes.split[i])))
            }
            # Using setnames above solved our rbind error

            else {results <- rbind(results, statesubset)}
      }

      rownames(results) <- NULL # The row names will be nonsensical as they're derived from 54 data frame subsets
                                # so best to just remove them

      # Actually though, we can add back in sensical rownames:
      statenames <- as.character(unlist(as.vector(results[2])))
      rownames(results) <- statenames

      results
}


rankall("Heart Failure",1)

rankall("Heart Failure","best")
rankall("Heart Failure","worst")
rankall("Heart Failure",150)

rankall("Heart Attack", 15)
rankall("Pneumonia", 27)
