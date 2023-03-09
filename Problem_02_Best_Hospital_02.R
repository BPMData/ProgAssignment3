rm(list=ls())

library(data.table)
library(dplyr)
library(naniar)
library(tibble)
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

tib <- as_tibble(dfoutcomes) # Don't even need dfoutcomes, see below.


### THIS IS ALL YOU NEED SO FAR #####################################################################################
tripleoutcomes <- fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                        header = TRUE, sep = ",", stringsAsFactors = FALSE,
                        na.strings = "Not Available", select = c(2,11,17,23))


outcomes_cleaned <- tripleoutcomes %>%
      replace_with_na_all(condition = ~.x %in% "Not Available") %>%
      as_tibble


rm(list=ls())
# The above works so far but can I write it as one nice piping block? Let's save the above in case
# I break it...


outcomes2 <- fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                        header = TRUE, sep = ",", stringsAsFactors = FALSE,
                        na.strings = "Not Available", select = c(2,11,17,23)) %>%
      replace_with_na_all(condition = ~.x %in% "Not Available") %>%
            as_tibble %>%
                  mutate(across(2:4, as.numeric))

colnames(outcomes2) <- c("Hospital","Attack","Failure","Pneumonia")

sapply(outcomes2, typeof)

is_tibble(outcomes2)

rm(list=ls())
# I want it all in one piping!

outcomes2 <- fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                   header = TRUE, sep = ",", stringsAsFactors = FALSE,
                   na.strings = "Not Available", select = c(2,7,11,17,23)) %>%
      naniar::replace_with_na_all(condition = ~.x %in% "Not Available") %>%
      as_tibble %>%
      mutate(across(3:5, as.numeric)) %>%
      setnames(c("Hospital","State","Attack","Failure","Pneumonia"))

sapply(outcomes2, typeof)
# Woo I did it, thank you data.table::setnames()
# Also obviously I need state data so I went back and added it in, and due to my
# clean code, it was very easy to do so! Just added ,7, and "State",

# Now to just actually, you know, get to coding the function itself rather than
# a beautiful code to retrieve the data I need, lol.

?`::`



# Holy shit it worked! That's way nicer, and my environment has literally only one object in it right now.
# Beautiful stuff.

















################### ALL BS BELOW THIS POINT #########################################
?colnames<-


rm(list=ls())

            mutate(across(2:4,as.numeric)) -> tibmutated


rm(tibmutated)
3+3


rm(tib)
tib <- as_tibble(outcomes_cleaned)
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
