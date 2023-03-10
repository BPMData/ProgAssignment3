install.packages("devtools")
library(devtools)

devtools::install_github("arunsrinivasan/flights")

library(flights)

input <- if (file.exists("flights14.csv")) {
      "flights14.csv"
} else {
      "https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv"
}
flights <- fread(input)

class(flights)
class(orderbyattack)

typeof(flights)
typeof(orderbyattack)
ans <- flights[origin == "JFK"][order(air_time)]
ans

order2 <- as.data.table(orderbyattack)

class(order2)

myans <- order2[State == "NY"]
myans
