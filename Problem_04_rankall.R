# Problem 4, last problem

# Tidy up from rankhospital...
detach("package:data.table", unload = TRUE)

rm(PneumoniaRanks, FailureRanks, AttackRanks, outcomes, outcomes.split, getRank)

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

# Now to coalesce it all back into one large data frame:
outcomes2 <- unsplit(outcomes.split, outcomes$State)

data(mtcars)

temp <- split(mtcars, mtcars$cyl)

unsplittemp <- unsplit(temp, mtcars$cyl)

# That was easy thanks to data.table.
rankedoutcomes <- data.table::rbindlist(outcomes.split)

# But I don't want a data.table....

rankedoutcomes <- as_tibble(rankedoutcomes)

str(rankedoutcomes)

# New function...

library(dplyr)

testoutput1 <- rankedoutcomes %>%
      filter(.[[6]] == 1) %>%
      .[c(1,2,6)]

testoutput1

testoutput20 <- rankedoutcomes %>%
      filter(.[[6]] == 20) %>%
      .[c(1,2,6)]

testoutput20


# The trick is getting it to return NAs for states where there is no 20... I'll think
# about it tomorrow, or later tonight. # Later: I think to make this easier will mean we should use outcomes.split
# not rankedoutcomes


#I might not actually want to coalesce the data honestly, might be better to keep it as 54 separate
# data frames

# Probably want to write a for loop for this...

rankall <- function(outcome, num) {
# Let's first get our invalid outcome stop message out of the way...
      if(!(outcome %in% c("Heart Failure","Heart Attack","Pneumonia")))
            stop("invalid outcome")

# Create an empty data frame to store the results
      results <- data.frame(hospital = character(),
                            state = factor(),
                            stringsAsFactors = FALSE)

# Create that index number...
 if(outcome == "Heart Attack"){index = 7}
      else if (outcome == "Heart Failure"){index = 8}
      else if (outcome == "Pneumonia"){index = 5}
      # Wow that was so much cleaner than how I did it in the other function

      # Now our for loop

      for (i in 1:length(outcomes.split)) {
           worstdeterminant <- outcomes.split[[i]][index]

                       if (num == "best") {
                             z = 1
                       } else if (num == "worst") {
                             z = max(na.omit(worstdeterminant[,1]))
                       } else {
                             z = num
                       }

            statesubset <- subset(outcomes.split[[i]], outcomes.split[[i]][index] == z)[1:2]

            if (nrow(statesubset) == 0) {
                  results <- rbind(results, data.frame(Hospital = NA, State = names(outcomes.split[i])))
            }
            # Not sure why I needed to capitalize Hospital and State here, but hey, it worked.

            else {results <- rbind(results, statesubset)}
      }

      rownames(results) <- NULL
      data.table::setnames(results, c("hospital","state"))
      results
}

rankall("Heart Failure",1)

rankall("Heart Failure","best")
rankall("Heart Failure","worst")
rankall("Heart Failure",150)


7 %in% c(1,2,7,4)
7 %in% c(1,2,4)

"Dog" %in% c(1, "cat",1:50, "Dog")

"Heart Attack" %in% c("Heart Failure","Heart Attack","Pneumonia")

paste0(disease, "Rank")

?subset

names(outcomes.split[7])
# Just CT

names(outcomes.split[[7]])
# Gives you the entire 7th state data frame - Connecticut
# Names = the names of the columns

outcomes.split[[7]][7]
# Gives you just the 7th column of the Connecticut data frame - attackRank

subset(outcomes.split[[7]], outcomes.split[[7]][7] == 5)

# Gives you only the entire row in the Connecticut subset where the 7th column's value for that row is 5.
# I.E. where heart attack rank is 5.

subset(outcomes.split[[7]], outcomes.split[[7]][7] == 5)[1:2]
#Gives you just the first two columns of that subset, i.e. just the hospital name and state abbreviation.

index <- 7

names(outcomes.split)

names(outcomes.split[7])


stateoutcomesubset <- outcomes.split[[7]][7]

max(na.omit(stateoutcomesubset[,1]))

stateoutcomesubset[, 1]

stateoutcomesubset

testframe1 <- data.table::setnames(data.frame(hospital = NA, state = "CT"), c("Applesauce","Aardvark"))
testframe1

testframe2 <- data.table::setnames(data.frame(hospital = NA, state = names(outcomes.split[7])), c("Applesauce","Aardvark"))
testframe2

CTSubset <- outcomes.split[[7]]

colnames(CTSubset)
