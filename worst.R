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

      outcome
}

worstsubset <- rankhospital("CA","Heart Failure", 5)

max(na.omit(worstsubset[, 2]))



####
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

      # Now to calculate z based of num, including best and worst


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
