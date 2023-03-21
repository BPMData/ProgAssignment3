#Not as data table?

outcomes.df <- fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                     header = TRUE, sep = ",", stringsAsFactors = FALSE,
                     na.strings = "Not Available", select = c(2,7,11,17,23)) %>%
      naniar::replace_with_na_all(condition = ~.x %in% "Not Available") %>% # Doing it with :: removes the need to load another package we're only going to use for one function call one time.
      as_tibble %>%
      mutate(across(3:5, as.numeric)) %>%
      mutate(across(2, as.factor)) %>%
      setnames(c("Hospital","State","Heart Attack","Heart Failure","Pneumonia"))

outcomes.df.split <- split(outcomes.df, outcomes.df$State)


class(outcomes.df)

class(iris)

class(outcomes)

outcomes <- as.data.table(fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                                header = TRUE, sep = ",", stringsAsFactors = FALSE,
                                na.strings = "Not Available", select = c(2,7,11,17,23)) %>%
                                naniar::replace_with_na_all(condition = ~.x %in% "Not Available") %>% # Doing it with :: removes the need to load another package we're only going to use for one function call one time.
                                as_tibble %>%
                                mutate(across(3:5, as.numeric)) %>%
                                mutate(across(2, as.factor)) %>%
                                setnames(c("Hospital","State","Heart Attack","Heart Failure","Pneumonia")))


rankhospital <- function(x,y,z) {
      statesub <- outcomes[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      index <- as.numeric(which(names(outcomes) == y))
      head(dplyr::select(sorted, c(1,index)),1)
}

rank(outcomes$`Heart Failure`)


#Thoughts:

# I'll have to use split by state, create a new column called rank and populate it
# for each state AND each condition, so I'll actually need 3 columns called rank
# and then rbind all that stuff back together

# Lots of lapply I think.


#Let's create a practice dataset for creating the Rank row...####
rm(list=ls())
vector1 <- sample(letters, 15, replace = TRUE)
vector2 <- sample(letters, 15, replace = TRUE)
vector1
vector2

dfletters <- data.frame(vector1, vector2)

dim(dfletters)

dfletters

dfletters[, "vector2"]

rank(dfletters$vector2)

dfletters$Rank1 <- rank(dfletters$vector1,ties.method = "min" )

dfletters

dfletters$Rank2 <- rank(dfletters$vector2, ties.method = "min")


dfletters[c("vector1", "Rank1", "vector2", "Rank2")] -> dfletters2


dfletters2

?rank

# Split Practice - NY Only ####

nyoutcomes <- as.data.table(outcomes)[outcomes$State == "NY"]

# Let's get a rank for Heart Attack and Bind It:

nyoutcomes$AttackRank <- rank(nyoutcomes$`Heart Attack`, ties.method = "min")

nyoutcomes

# Now the other two:

nyoutcomes$FailureRank <- rank(nyoutcomes$`Heart Failure`, ties.method = "min")
nyoutcomes$PneumoniaRank <- rank(nyoutcomes$Pneumonia, ties.method = "min")

# Find the best for Pneumonia:

nyoutcomes[nyoutcomes$PneumoniaRank == 1]

# 3 Hospitals have the same rate (10.0), so it should be decided alphabetically, so Albany Medical Center Hospital.

# Now to rank and bind for all 54 states ####

# Iris dataset:

iris.split <- split(iris.df, iris.df$Species)
iris.split

str(iris.split)

get.mean.of.specified.column <- function(df, column.name) {

      mean.of.given.column <- mean(df[[column.name]])

      return(mean.of.given.column)
}


sapply(X = iris.split, FUN = get.mean.of.specified.column, column.name = "Sepal.Length")

get.AttackRank <- function(df, column.name) {
      AttackRank <- rank(df[[column.name]], ties.method = "min")
      return(AttackRank)
}

sapply(X = outcomes.split, FUN = get.AttackRank, column.name = "Heart Attack")

# Stupid, screw this...
get.PneumoniaRankTable <- function(df, column.name) {
      PneumoniaRank <- rank(df[ , .(column.name = column.name)], ties.method = "min")
      return(PneumoniaRank)
}

sapply(X = outcomes.split, FUN = get.AttackRank, column.name = "Heart Attack")



# Try it for the newly created outcomes.df.split...

sapply(X = outcomes.df.split, FUN = get.PneumoniaRank, column.name = "Pneumonia")

# Bro what, it worked on my tibble? Why did it being a data table break it?

lapply(X = outcomes.split, FUN = get.PneumoniaRank, column.name = "Pneumonia")

# Let's start from the beginning without using the accursed data.table format that keeps breaking things... ####
rm(outcomes)
rm(outcomes.split)
rm(get.PneumoniaRank)
rm(outcomes.df)
rm(outcomes.df.split)
rm(AttackRanks)
rm(FailureRanks)
rm(PneumoniaRanks)

outcomes <- fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                     header = TRUE, sep = ",", stringsAsFactors = FALSE,
                     na.strings = "Not Available", select = c(2,7,11,17,23)) %>%
      naniar::replace_with_na_all(condition = ~.x %in% "Not Available") %>% # Doing it with :: removes the need to load another package we're only going to use for one function call one time.
      as_tibble %>%
      mutate(across(3:5, as.numeric)) %>%
      mutate(across(2, as.factor)) %>%
      setnames(c("Hospital","State","Heart Attack","Heart Failure","Pneumonia"))

outcomes.split <- split(outcomes, outcomes$State)

get.AttackRank <- function(df, column.name) {
      AttackRank <- rank(df[[column.name]], ties.method = "min")
      return(AttackRank)
}
sapply(X = outcomes.split, FUN = get.AttackRank, column.name = "Heart Attack")

# That gives us our vectors, now to see how we can bind them back to their respective
# data frames...

# Ignore this:

# getandbind.Rank <- function(df, column.name) {
#       ColumnRank <- rank(df[[column.name]], ties.method = "min")
#       outcomes.split$OutcomeRank <- ColumnRank
# }
# PneumoniaRanks <- sapply(X = outcomes.split, FUN = getandbind.Rank, column.name = "Pneumonia")

# THIS IS THE ORIGINAL FORMULA BUT IT DOESN'T IGNORE HOSPITALS WITH NO DATA
# JUST KIDDING EASILY FIXED, ADD na.last = "keep"
getRank <- function(df, column.name) {
      ColumnRank <- rank(df[[column.name]], ties.method = "min", na.last = "keep")
      ColumnRank
}

PneumoniaRanks <- sapply(X = outcomes.split, FUN = getRank, column.name = "Pneumonia")
AttackRanks <- sapply(X = outcomes.split, FUN = getRank, column.name = "Heart Attack")
FailureRanks <- sapply(X = outcomes.split, FUN = getRank, column.name = "Heart Failure")

names(outcomes.split)
names(PneumoniaRanks)
names(FailureRanks)

# EXCEPT ties.method = "min" led to errors, I have to fix it.

?rank

ranktest <- c("a","z","b","b","c","d","d","d","e","f","g")


aaaba
rank(ranktest)

rank(ranktest, ties.method = "min")
rank(ranktest, ties.method = "max")
rank(ranktest, ties.method = "average")

rank(ranktest, ties.method = "first")

rank(ranktest, ties.method = "last")

# I think what I need is "first"...?


# LET'S FIGURE OUT HOW TO GET IT TO IGNORE HOSPITALS WITH NO DATA

?rank


# Let's practice binding with one state ####

# BindRanks <- function(df, vector) {
#       df$Rank1 <-
# }



nyoutcomes <- as.data.table(outcomes)[outcomes$State == "NY"]

nyoutcomes <- as_tibble(nyoutcomes)

is.data.table(nyoutcomes)

is_tibble(nyoutcomes)

PneumoniaRankNY <- rank(nyoutcomes$Pneumonia, ties.method = "min")

nyoutcomes$PneumoniaRank <- PneumoniaRankNY

nyoutcomes

# Okay, binding worked with one state. Now let's do it using sapply for all states... ####
getRank <- function(df, column.name) {
      ColumnRank <- rank(df[[column.name]], ties.method = "min")
      ColumnRank
}

BindRanks <- function(df, vector) {
      df$Rank1 <- vector
}



sapply(X = outcomes.split, FUN = BindRanks, vector = PneumoniaRanks)

cbind(nyoutcomes, PneumoniaRankNY)

# Binding using gpt output - THIS ACTUALLY WORKED. Don't change the order of these! Run top to bottom!####

for (i in 1:length(outcomes.split)) {outcomes.split[[i]] <- cbind(outcomes.split[[i]], PneumoniaRanks[[i]])
colnames(outcomes.split[[i]])[ncol(outcomes.split[[i]])] <- "PneumoniaRank"
}

for (i in 1:length(outcomes.split)) {outcomes.split[[i]] <- cbind(outcomes.split[[i]], AttackRanks[[i]])
colnames(outcomes.split[[i]])[ncol(outcomes.split[[i]])] <- "AttackRank"
}

for (i in 1:length(outcomes.split)) {outcomes.split[[i]] <- cbind(outcomes.split[[i]], FailureRanks[[i]])
colnames(outcomes.split[[i]])[ncol(outcomes.split[[i]])] <- "FailureRank"
}

outcomes.split$AK


# Everything is read in, ranked and bound! Now to write the actual function.... ####

# First, get out statesubset...
rankhospital <- function(x) {
statesubset <- outcomes.split[[x]]
statesubset

}

# Index time...
rm(rankAK)

rankAK <- rankhospital("AK")
names(rankAK)
index <- as.numeric(which(names(rankAK) == "AttackRank"))

rankAK[, c(1,index)]


rankhospital <- function(x,y) {
      statesubset <- outcomes.split[[x]]
      statesubset
      index <- as.numeric(which(names(statesubset) == y))
      select(statesubset, c(1,3, index))
}

rankhospital("AK","FailureRank")

# Now to make the 2nd argument in c(1,2,3) into a variable as well...

rankhospital <- function(x,y) {
      statesubset <- outcomes.split[[x]]
      statesubset
      index <- as.numeric(which(names(statesubset) == y))
      if (index == 6) {
            rate <- 5
      } else if (index == 7) {
            rate <- 3
      } else if (index == 8) {
            rate <- 4
      }
      select(statesubset, c(1,rate, index))
}

rankhospital("AK","AttackRank")

# Whoops except I set these in the wrong order, let's fix it...

rankhospital <- function(x,y) {
      statesubset <- outcomes.split[[x]]
      statesubset
      index <- as.numeric(which(names(statesubset) == y))
      if (index == 5) {
            rank <- 6
      } else if (index == 3) {
            rank <- 7
      } else if (index == 4) {
            rank <- 8
      } else if (length(index == 0)) {
            stop("invalid outcome")
      }
      select(statesubset, c(1,rank, index))
}

## Trying to get that invalid outcome stop message to work - copy and pasting the above in case
# I mess stuff up....
rankhospital <- function(x,y) {
      statesubset <- outcomes.split[[x]]
      statesubset
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

      select(statesubset, c(1,rank, index))
}


which(names(rankAK) == "Heart Failure")

which(names(rankAK) == "Anime")

length(which(names(rankAK) == "Heart Failure"))

length(which(names(rankAK) == "Anime"))

?stop
names(rankAK)
rankhospital("AK","Heart Failure")
rankhospital("AK","Anime Girls")
rankhospital("AK","State")

# Now we need to add the "invalid state" stop message...
rankhospitalfull <- function(x,y) {
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

      select(statesubset, c(1,rank, index))
}



rankhospitalfull("AK","Heart Failure")


rankhospital("AK","Anime Girls")
rankhospital("AK","State")
rankhospital("Obama","Heart Failure")

names(outcomes.split)

# Yo I figured that out fast! Now...


# MOST RECENT POINT IS THE FORMULA ABOVE ####

# To do:
# Accept a numerical input for z
# Set it so NA's for the Y results in the entire row being dropped and thus not ranked...
# Fuck I have to change outcomes.split for that to work, oh well...
# All that best, worst stuff, etc.

# Now to just finalize function variable z and I think we have it!
rankhospital <- function(x,y,z) {
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
      stateoutcomesubset <- select(statesubset, c(1,rank, index)) %>%
            filter(stateoutcomesubset[ , 2] == z) %>%
            select(1)
}



# Trying without the filter() function ...

rankhospital <- function(x,y,z) {
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
      finaloutput <- outcome[outcome[,2] == z, 1]
      head(finaloutput[complete.cases(finaloutput)],1)
}
rankhospital("AK","Heart Failure", 5)

# DID IT! DONE!! OH EXCEPT FOR BEST OR WORST... #####
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
      finaloutput <- outcome[outcome[,2] == num, 1]
      head(finaloutput[complete.cases(finaloutput)],1)
}
rankhospital("AK","Heart Failure", 5)
rankhospital("AK","Heart Failure", 2)


rankCT <- rankhospital("CT","Heart Failure")

rankCT

rankCT[,2]

rankCT[rankCT[ ,2] == 5, ]
rankCT[rankCT[ ,2] == 5, 1]

BPT <- na.omit(rankCT[rankCT[ ,2] == 5, 1])

head(BPT)

cases <- rankhospital("AK","Heart Failure", 5)

cases

complete.cases(cases)

cases[complete.cases(cases)]


rankhospital("AK","Anime Girls")
rankhospital("AK","State")
rankhospital("Obama","Heart Failure")
rankhospital("AK","Heart Attack")
rankhospital("AK","Pneumonia")


# ACTUALLY DONE ####
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

# EVERYTHING BELOW HERE IS TRASH ####
# So the Rank is always the second column... so I can subset by the column index...?







# Just make sure it's setting rate properly...

rankhospital <- function(x,y) {
      statesubset <- outcomes.split[[x]]
      statesubset
      index <- as.numeric(which(names(statesubset) == y))
      if (index == 6) {
            rate <- 5
      } else if (index == 7) {
            rate <- 3
      } else if (index == 8) {
            rate <- 4
      }
 print(index)
 print(rate)
}
rankhospital("AK","PneumoniaRank")


# Now,find only the rows where rank = the number specified.
rankhospital <- function(x, y) {
      statesubset <- outcomes.split[[x]]
      statesubset

      if (y == "Heart Attack")
             OutcomeRank = AttackRank
      else if (y == "Heart Failure")
             OutcomeRank = FailureRank
      else if (y == "Pneumonia")
             OutcomeRank = PneumoniaRank

OutcomeRank
}

rankhospital("AK","Heart Failure")


AKSubset$FailureRank

AKSubset %>% subset(State)


AKSubset <- outcomes.split$AK

typeof(AKSubset)

# Yo what the hell, my function works on the list of data frames, iris.split ####
sapply(X = iris.split, FUN = get.PneumoniaRank, column.name = "Sepal.Length")

dim(iris.df)
dim(outcomes)
class(iris.df)
class(outcomes)

# The function works properly if there's only one data frame...
get.PneumoniaRank(nyoutcomes, "Pneumonia")

get.PneumoniaRank(outcomes.split$CT, "Pneumonia")

# Does iris.split have a different class or type than outcomes.split?

class(iris.split)
typeof(iris.split)
is.data.frame(iris.split)
str(iris.split)

class(outcomes.split)
typeof(outcomes.split)
is.data.frame(outcomes.split)
str(outcomes.split)


outcomes.split[[]]

class(outcomes.split)


outcomes.split <- split(outcomes, outcomes$State)
str(outcomes.split)

nyoutcomes$AttackRank2 <- rank(nyoutcomes[["Heart Attack"]], ties.method = "min")

# Try to bind attack rank....

outcomes.split$AttackRank <- rank(outcomes.split$`Heart Attack`, ties.method = "min")

# Or Pneumonia

outcomes.split$PneumoniaRank <- rank(outcomes.split$, ties.method = "min")
