outcomes <- as.data.table(fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                                header = TRUE, sep = ",", stringsAsFactors = FALSE,
                                na.strings = "Not Available", select = c(2,7,11,17,23)) %>%
                                naniar::replace_with_na_all(condition = ~.x %in% "Not Available") %>% # Doing it with :: removes the need to load another package we're only going to use for one function call one time.
                                as_tibble %>%
                                mutate(across(3:5, as.numeric)) %>%
                                mutate(across(2, as.factor)) %>%
                                setnames(c("Hospital","State","Heart Attack","Heart Failure","Pneumonia")))


rankhospital <- function(x, y,z) {
      statesub <- outcomes[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      index <- as.numeric(which(names(outcomes2) == y))
      head(dplyr::select(sorted, c(1,index)),1)
}

rank(outcomes$`Heart Failure`)


#Thoughts:

# I'll have to use split by state, create a new column called rank and populate it
# for each state AND each condition, so I'll actually need 3 columns called rank
# and then rbind all that stuff back together

# Lots of lapply I think.


# Let's create a practice dataset for creating the Rank row...

vector1 <- sample(letters, 15, replace = TRUE)
vector2 <- sample(letters, 15, replace = TRUE)
vector1
vector2

dfletters <- data.frame(vector1, vector2)

dim(dfletters)

dfletters[, "vector2"]

rank(dfletters$vector2)

dfletters$Rank1 <- rank(dfletters$vector1,ties.method = "min" )

dfletters

dfletters$Rank2 <- rank(dfletters$vector1, ties.method = "min")


dfletters[c("vector1", "Rank1", "vector2", "Rank2")] -> dfletters2


dfletters2

?rank

