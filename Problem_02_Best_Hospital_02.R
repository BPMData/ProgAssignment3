rm(list=ls())

library(data.table)
library(dplyr)
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


### THIS WAS ALL I NEEDED (AT FIRST)  #####################################################################################
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


# Kinda cool but not needed obviously:
system.time(outcomes2 <- fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                               header = TRUE, sep = ",", stringsAsFactors = FALSE,
                               na.strings = "Not Available", select = c(2,7,11,17,23)) %>%
                  naniar::replace_with_na_all(condition = ~.x %in% "Not Available") %>% # Doing it with :: removes the need to load another package we're only going to use for one function call one time.
                  as_tibble %>%
                  mutate(across(3:5, as.numeric)) %>%
                  setnames(c("Hospital","State","Attack","Failure","Pneumonia")))

rm(list=ls())

rm(outcomes2)
# Data Import in Single Piping Block ####

outcomes2 <- as.data.table(fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                   header = TRUE, sep = ",", stringsAsFactors = FALSE,
                   na.strings = "Not Available", select = c(2,7,11,17,23)) %>%
      naniar::replace_with_na_all(condition = ~.x %in% "Not Available") %>% # Doing it with :: removes the need to load another package we're only going to use for one function call one time.
      as_tibble %>%
      mutate(across(3:5, as.numeric)) %>%
      mutate(across(2, as.factor)) %>%
      setnames(c("Hospital","State","Heart Attack","Heart Failure","Pneumonia")))

sapply(outcomes2, typeof)
is.factor(outcomes2$State)
str(outcomes2)
summary(outcomes2)
# Why is State a factor with 54 levels, not 50? Let's check it out:


levels(outcomes2$State)

table(outcomes2$State)

# Oh, it's because it includes DC, GU (Guam),VI (Virgin Islands) and PR (Puerto Rico).

# Woo I did it, thank you data.table::setnames()
# Also obviously I need state data so I went back and added it in, and due to my
# clean code, it was very easy to do so! Just added ,7, and "State",

# Now to just actually, you know, get to coding the function itself rather than
# a beautiful code to retrieve the data I need, lol.

# Attempt at actually coding the required function ####

# Changing variable names to not have spaces

outcomes3 <- as.data.table(fread("C:/Users/corma/OneDrive/Documents/R/Assignment3/hospitaldata/outcomes.csv",
                                 header = TRUE, sep = ",", stringsAsFactors = FALSE,
                                 na.strings = "Not Available", select = c(2,7,11,17,23)) %>%
                                 naniar::replace_with_na_all(condition = ~.x %in% "Not Available") %>% # Doing it with :: removes the need to load another package we're only going to use for one function call one time.
                                 as_tibble %>%
                                 mutate(across(3:5, as.numeric)) %>%
                                 mutate(across(2, as.factor)) %>%
                                 setnames(c("Hospital","State","Attack","Failure","Pneumonia")))



orderbyattack <- setorder(outcomes2,"Heart Attack", na.last = TRUE)

orderattackny <- orderbyattack[, "State" == "NY"]


?order

best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      statesub[order(statesub, y), ]
}
best_hospital("NY", y = "Failure")

best_hospital("NY", Attack)

best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      statesub[order(statesub, y), ]
      print(y)
}
best_hospital("NY", y = "Failure")

best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      statesub[order(statesub, Failure), ]
}
best_hospital("NY")

best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      y2 <- noquote(y)
      statesub[order(statesub, y2), ]
      print(y2)
}
best_hospital("NY", "Failure")

best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      y2 <- noquote(y)
      subset(statesub(, 4)
            statesub[drop = FALSE, order(statesub, y2), ]
}
?subset()

# Currently this returns the entire list of NY sorted by the variable specified
best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      y2 <- substitute(y)
      statesub[order(statesub, eval(y2)), ]
}
best_hospital("NY", Failure)

# But I want my 2nd argument to be in quotes and can't figure out how to do it.

# WOOO THIS WORKS, I JUST NEED TO INTRODUCE INDEX NOW ####
best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      sorted
}
best_hospital("NY", "Attack")

which(names(outcomes3) == "Failure")

# Here's the index created properly, now I need to subset by index without dropping data. ####
best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      index <- which(names(outcomes3) == y)
      sorted
      print(index)
}

typeof(best_hospital("NY", "Attack"))
best_hospital("NY", "Attack") == 3

# Here's my attempt to subset by index. Here's proof it works with the number: ####
best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      index <- which(names(outcomes3) == y)
      head(sorted[,c(1,3)],1)
}

best_hospital("NY", "Attack")

# Now to get it to work with index instead of 3.
best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      index <- as.double(which(names(outcomes3) == y))
      head(sorted[ ,c(1,3), drop = FALSE],1)
}

best_hospital("NY", "Attack")
# Hospital              Attack
# 1: NYU HOSPITALS CENTER   10.1


best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      index <- as.double(which(names(outcomes3) == y))
      head(sorted[ ,c(1,3), drop = FALSE],1)
      print(index)
}

best_hospital("NY","Attack")
# 3

best_hospital("NY","Attack") == 3
# TRUE

# So why on earth does this happen?
best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      index <- as.double(which(names(outcomes3) == y))
      head(sorted[ ,c(1,index), drop = FALSE],1)

}
is.vector(best_hospital("NY","Attack"))

# 1. Just the number 1.

# This also works and is potenially easier:
best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      index <- which(names(outcomes3) == y)
      sorted[,c(1,4)][1,]
}

best_hospital("NY", "Failure")
# #  Hospital                             Failure
# 1: KINGSBROOK JEWISH MEDICAL CENTER       7

best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      index <- which(names(outcomes3) == y)
      sorted[,c(1,4)][1,]
      print(index)
}
best_hospital("NY", "Failure")
# WHY DOESN'T IT WORK WITH INDEX:

# Bing attempt #4 or whatever damn IDK

# BING FUCKING FIXED IT! THANK YOU BING!!!! ####
library(dplyr)
best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      index <- which(names(outcomes3) == y)
      head(sorted %>% select(c(1,index)),1)
}

# Rewritten without piping or requiring library(dplyr)##############################
best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      index <- as.numeric(which(names(outcomes3) == y))
      head(dplyr::select(sorted, c(1,index)),1)
}
best_hospital("NY", "Pneumonia")
######################################
best_hospital <- function(x, y) {
      statesub <- outcomes2[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      index <- as.numeric(which(names(outcomes2) == y))
      head(dplyr::select(sorted, c(1,index)),1)
}
best_hospital("NY", "Heart Attack")

best_hospital("NY", "Heart Failure")

best_hospital("GA", "Pneumonia")

best_hospital("MD","Heart")
# I DID IT ####

# OH MY GOD I DIDN'T NEED TO DO ANY OF THAT INDEX VARIABLE CREATION AND SORTING, TECHNICALLY!!!

?Syntax

outcomes2

?select
colnames(outcomes3)
is.numeric(index)


best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      index <- which(names(outcomes3) == y)
      sorted[,c(1,4)][1,]
      print(index)
      sorted[, c(1,eval(index))][1, ]
}
best_hospital("NY", "Failure")

# Here's chatgpt round 2. Nevermind it's a fucking mess and doesn't work.

best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x, ]
      sorted <- statesub[order(statesub[, match(y, colnames(outcomes3))]), ]
      index <- match(y, colnames(outcomes3))
      head(sorted[, c(1, index)], 1)
}
best_hospital("NY", "Failure")



# Here's bing:


#or
best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      index <- which(names(outcomes3) == y)
      head(sorted[,c(1,index)])
}
best_hospital("NY", "Attack")

# Here's chatGPT:

best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      index <- which(colnames(outcomes3) == y)
      head(sorted[,c(1,index)],1)
}
best_hospital("NY", "Attack")

typeof(3)
best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      sorted <- statesub[order(statesub, get(y)), ]
      index <- which(names(outcomes3) == y)
}

best_hospital("NY","Attack")


z
<- "Attack"
z
?unquote
typeof(noquote(z))

best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      statesub[order(statesub,y), ]
}
best_hospital("NY", `Failure`)

best_hospital("NY", "Failure")

best_hospital <- function(x, y) {
      statesub <- outcomes3[State == y]
      statesub[order(statesub,y), ]
}

# Trying to get it to give only one row:
best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      y2 <- substitute(y)
      sorted <- statesub[order(statesub, eval(y2)), ]
      subset(sorted[,], select = c(1,4))[1,]
}

subset(statesub[,], select = c(1,4))[1,]

best_hospital("NY", Attack)

# YES I DID IT. NOW TO INTRODUCE INDEX WITHOUT BREAKING EVERYTHING.
# Trying to get it to give only one row:
best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      y2 <- substitute(y)
      sorted <- statesub[order(statesub, eval(y2)), ]
      subset(sorted[,], select = c(1,3))[1,]
      print(which(names(outcomes3) == y))
}

best_hospital("NY", "Failure")

# Trying to get it to give only one row:
best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      y2 <- substitute(y)
      sorted <- statesub[order(statesub, eval(y2)), ]
      y3 <- paste("y")
      index <- which(names(outcomes3) == "y")
      subset(sorted[,], select = c(1,4))[1,]

}


?concat
which(names(outcomes3) == "Attack")

best_hospital("NY", Attack)

# Trying to get it to give only one row:
best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      index <- which(names(statesub)== eval("y"))
      index <- as.double(index)
      y2 <- substitute(y)
      sorted <- statesub[order(statesub, eval(y2)), ]
      sorted
      subset(sorted, select = c(1,4), drop = FALSE)[1,]
}

best_hospital("NY", Attack)

# Trying to get it to give only one row:
best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x]
      index <- which(names(outcomes3)== y)
      index <- as.double(index)
      y2 <- substitute(y)
      sorted <- statesub[order(statesub, eval(y2)), ]
      sorted
}
subset(sorted, drop = FALSE, select = c(1,4))[1, drop = FALSE]

index2 <- which(names(outcomes3) == "Attack")
index2

best_hospital("NY", "Attack")



best_hospital <- function(x, y) {
      statesub <- outcomes3[State == x][order(y)]
      statesub
}


best_hospital <- function(x, y) {
      y2 <- substitute(y)
      statesub <- outcomes3[State == x][order(eval(y2))]
      statesub
}

best_hospital("NY", Failure)
# IT WORKS BUT I HATE IT. FIX?

# Can I remove one of those calls?
best_hospital <- function(x, y) {
      y2 <- substitute(y)
      statesub <- outcomes3[State == x][order(eval(y2))]
      statesub[1,c(1,3)]
}

best_hospital("NY", "Failure")

best_hospital("NY", Failure)

# What if the variable name is two words. I.E. use outcomes2

best_hospital <- function(x, y) {
      y2 <- substitute(y)
      statesub <- outcomes3[State == x][order(eval(y2))]
      statesub[1,c(1,3)]
}

# Making this work for a multiple word column title...
best_hospital <- function(x, y){
      y2 <- substitute(y)
      index <- which(names(outcomes3) == eval(y2))
      statesub <- subset(outcomes3[State == x][order(eval(y2))], select = c(1,4))[1,]
      statesub
}

best_hospital("NY",Failure)


best_hospital <- function(x, y) {
      y2 <- substitute(y)
      statesub <- outcomes3[State == x][order(eval(y2))]
      subset(statesub[,], select = c(1,4))[1,]
}

best_hospital("NY",Failure)

# Introducing index without breaking shit I hope:

best_hospital <- function(x, y) {
      y2 <- substitute(y)
      index <- which(names(outcomes3) == eval(y2))
      statesub <- outcomes3[State == x][order(eval(y2))]
      subset(statesub[,], select = c(1,index))[1,]
}

best_hospital("NY",Failure)


best_hospital("NY","Failure")

# Work for a single word column title first lol

best_hospital <- function(x, y){
      y2 <- substitute(y)
      statesub <- outcomes3[State == x][order(eval(y2))]
      index <- which(names(outcomes3) == y)
      statesub[1,c(1,index)]
}
best_hospital("NY","Failure")

best_hospital <- function(x, y){
      y2 <- substitute(y)
      index <- which(names(outcomes2) == eval(y))
      subset(outcomes2[State == x][order(eval(y2))], select = c(1,4))[1,]
}

best_hospital("NY", `Heart Failure`)


index <- which(names(outcomes2) == "Heart Failure")
index

subset(outcomes3[State == "NY"][order(Attack)], select = c(1,fixedindex))[1,]

best_hospital("NY", "Failure")

best_hospital(NY, "Heart Failure")

best_hospital("NY",`Heart Failure`)


outcomes3[State == "NY"][order(Attack)][1,c(1,3)]

outcomes3[State == "NY"][order(Attack)][1,c(1,outcomes3$Failure)]

index<- which(names(outcomes3) == "Failure")
index
typeof(index)
class(index)
typeof(4)
class(4)
fixedindex <- as.double(index)
fixedindex
fixedindex == 4

is.list(outcomes3[State == "NY"][order(Attack)][1,c(1,fixedindex), drop = FALSE])

is.list(outcomes3[State == "NY"][order(Attack)][1,c(1,4)])

outcomes3[State == "NY", drop = FALSE][order(Attack), drop = FALSE][1, c(1, fixedindex), drop = FALSE]
outcomes3[State == "NY"][order(Attack)][1, c(1, fixedindex), drop = FALSE]

subset(outcomes3[State == "NY"][order(Attack)], select = c(1,index))[1,]

?`[`

# noquote is NOT WORKING
best_hospital <- function(x, z) {
      z2 <- noquote(z)
      statesub <- outcomes3[State == x]
      statesub[order(z2)]
}


noquote("Failure")
best_hospital("NY", Attack)

best_hospital("NY")[order(Failure)]


outcomes3[State == "NY"][order(Failure)]

rm(best_hospital)
?noquote

best_hospital <- function(x, y) {
      y_expr <- substitute(y)
      statesub <- outcomes3[State == x][order((eval(y_expr)]
      statesub
}


?substitute
statesub[order(statesub$y), ]
?order

statesub <- outcomes3[State == "NY"]
statesub
statesub[order(statesub, "Attack")]

statesub[order(statesub$Failure)]

best_hospital("NY", y = "Attack")

best_hospital("NY", y = Attack)
best_hospital("NY", Attack)

rm(best_hospital)

ny2[order(ny2$Failure), ]

nysort

rm(ny)

ny2 <- outcomes3[State == "NY"]
ny2
ny2[order(ny2$Attack), ]

ny2[order("Attack"), ]

ny <- as.data.frame(ny)
ny
is.data.frame(ny)

ny[do.call(order, ny[ny$Attack],)]

ny[order(Failure), ]

ny[ny$Attack]

typeof(ny$Attack)
is.list(ny$Attack)
ny$Attack
rm(ny)

?order

rm(best_hospital)

best_hospital("NY")

outcomes3[State == "NY"][order(Attack)]

best_hospital("NY","Pneumonia")

best_hospital_manual <- outcomes3[State == "NY"][drop = FALSE, order(Attack)][1,c(1,Attack)]
best_hospital_manual
print(outcomes3[State == "NY"][order(Attack)][1,c(1,3)])

orderattackny2 <- outcomes2[State == "NY"][order(`Heart Attack`)]

outcomes2[State == "PR"][order(`Heart Attack`)]

rm(best_hospital)




2+2
# Holy shit?? ####
# You can create skip-to-able subsections in your code by starting a comment block with:
#  the following: # TITLE ####. Must be four #'s after the title, and a space or nothing after the four #'s.




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
