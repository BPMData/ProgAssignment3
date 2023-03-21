# This is actual data frame sorting practice...

# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-sortFunctionsExample.md

library(datasets)

head(mtcars)
library(plyr)

# use plyr::arrange() to sort the data frame by cyl and mpg

arrangedData <- arrange(mtcars, cyl, mpg)
head(arrangedData)

# One of the productivity aids from plyr is the ability to reference columns in
# the data frame directly, without the need for the extract operator $, as in mtcars$mpg.

# Also note the car names were stored as ROW names, so they're not in arrangedData anymore.

# Doing the same thing without dplyr or plyr is possible, but messier looking.

baseorderedData <- mtcars[order(mtcars$cyl, mtcars$mpg), ]

head(baseorderedData)

# This does keep the car names (row names) though, which is nice.

# To include the car names, we need to:
# 1) Output the row names as a vector
# 2) Bind this vector back to the data frame
# 3) Set row names to NULL

carNames <- rownames(mtcars)

carNames

mtcars <- cbind(carNames, mtcars)
mtcars

rownames(mtcars) <- NULL
head(mtcars)

arrangedData2 <- arrange(mtcars, cyl, mpg)
head(arrangedData2)

# Now here's actual split practice:

# V RMD V
# https://github.com/AlanBerger/Practice-programming-exercises-for-R/blob/master/Eleventh-R-Practice-exercise-using-sapply-and-split-and-also-use-of-ellipsis-to-pass-in-additional-argumentsFeb11.Rmd

# V PDF V
# https://github.com/AlanBerger/Practice-programming-exercises-for-R/blob/master/Eleventh-R-Practice-exercise-using-sapply-and-split-and-also-use-of-ellipsis-to-pass-in-additional-argumentsFeb11.pdf

# sapply, simple case:

v <- 1:15

sapply(X = v, FUN = function(y) y^2)

# I think including the curly brackets to denote the function in a formal/dummy function
# is a lot more legible - remember this confused tf out of me in Problem 2:
sapply(X = v, FUN = function(y) {y^2})

# If the anonymous/formal/dummy function is short, then including it this way is okay
# but if the function is longer, you might want to define it outside of your sapply call,
# then use the defined name inside your sapply call.

# Use of ellipses to pass in additional arguments to sapply:

2+2

sapply(X = v, FUN = function(x,power)
      {x^power}, power = 3)

# Another example using lexical scoping:

power <- 4
sapply(X = v, FUN = function(x)
      {x^power})

# Two additional arguments example, output is a matrix, using enter to make
# the anonymous function call really clear:

v <- 1:5
sapply(X = v, FUN = function(x, power1, power2)
                              {c(x^power1,x^power2)}, power1 = 2, power2 = 3)

# Now let's demonstrate sapply on the "iris" dataset, which has dimensions (150, 5)

data(iris)

dim(iris)

iris.df <- as_tibble(iris)

head(iris.df)

is.data.frame(iris.df)

tail(iris.df)

# IDK putting an NA in here for some reason

iris.df[2,2] <- NA

head(iris.df)

# Okay I did it idk y

?mean

# The sapply command below will;
#     * Successively pass EACH column of iris.df{1, 1:4}
#           as a VECTOR into the mean function.

sapply(x = iris.df[ , 1:4], FUN = mean, na.rm = TRUE, trim = 0.11)

# Error?  Oh, because with sapply, the x MUST be an X.

sapply(X = iris.df[ , 1:4], FUN = mean, na.rm = TRUE, trim = 0.11)

x <- sort(na.omit(iris.df[[1]]))

mean(x[17:length(x) - 16])

# When using an anonymous function in sapply, AND using an ellipsis argument (...)
# one needs to have any additional arguments be specifically declared arguments of the function
# as in the power examples above, OR

# have an ellpisis in the argument list of the function specified by FUN, AND used
# appropriately within the anonymous function.

sapply(X = iris.df[, 1:4],
       FUN = function(x, ...) {mean(x, ...)},
       na.rm = TRUE, trim = 0.11)

# Sepal.Length  Sepal.Width Petal.Length  Petal.Width
# 5.807627     3.043590     3.762712     1.183898

# If ... was not included within the anonymous function, this won't work:

sapply(X = iris.df[, 1:4],
       FUN = function(x, ...) {mean(x)},
       na.rm = TRUE, trim = 0.11)
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width
# 5.843333           NA     3.758000     1.199333

# Critically, R doesn't actually give an error message, but you can see by looking
# at the output that the arguments you thought you passed in were simply ignored,
# leading to quietly incorrect outputs.

# This will also work, however - the declarative form of the code above:

sapply(X = iris.df[, 1:4],
       FUN = function(x, na.rm.value, trim.value)
                  {mean(x, na.rm = na.rm.value, trim = trim.value)},
       na.rm.value = TRUE, trim.value = 0.11)

# Sepal.Length  Sepal.Width Petal.Length  Petal.Width
# 5.807627     3.043590     3.762712     1.183898

# Now on to the practice exercise itself...

# Assignment: Given the name of one of the four columns in iris,
# Then: compute the mean for that column, FOR EACH of the 3 species in the data set.

# First, let's recreate iris.df without that NA we inserted:

data(iris)
iris.df <- iris
head(iris.df)

# The split:

iris.split <- split(iris.df, iris.df$Species)
iris.split

str(iris.split)

get.mean.of.specified.column <- function(df, column.name) {

mean.of.given.column <- mean(df[[column.name]])

return(mean.of.given.column)
}

iris[["Sepal.Length"]]


sapply(X = iris.split, FUN = get.mean.of.specified.column, column.name = "Sepal.Length")


# Guess it runs that over each data frame as its own entity automatically, which is neat.

sapply(X = iris.split, FUN = get.mean.of.specified.column, column.name = "Sepal.Width")

sapply(X = iris.split, FUN = get.mean.of.specified.column, column.name = "Petal.Length")

sapply(X = iris.split, FUN = get.mean.of.specified.column, column.name = "Petal.Width")

# Now program the function to be used in sapply to return the vector of all 4 means.
# Then sapply will return a matrix.

get.all.4.means.in.df <- function(df){
      the.4.means <- sapply(df[,1:4], mean)
      return(the.4.means)
}

# Use sapply and this function to get the matrix of means...

result <- sapply(X = iris.split, FUN = get.all.4.means.in.df)
result

# Notice sapply saves row and column names.
rownames(result)
colnames(result)

# Transpose:

print(t(result))
