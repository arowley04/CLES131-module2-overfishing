install.packages("nycflights13")
library(nycflights13)
library(tidyverse)


# Primary keys of airlines, airports, planes, weather
class(airlines)
str(airlines)
head(airlines)

#Base R: looking for primary key
length(unique(airlines$carrier)) == nrow(airlines) 

#Tidyverse 
airlines |> 
  count(carrier) |> 
  filter(n >1)

#WEATHER - primary key = compount (origin + time_hour)
class(weather)
str(weather)
head(weather)

weather |> 
  count(origin) |> 
  filter(n > 1)

weather |> 
  count(origin, time_hour) |> 
  filter(n > 1)

# Identify foreign keys
View(flights)
dim(flights)

intersect(colnames(airlines), colnames(flights))

intersect(colnames(weather), colnames(origin))
intersect(colnames(weather), colnames(time_hour))


#Left_joins (mutating join -> adds new column)
dim(flights) #serves fuction of mutate: adding new columns from another table
flights|> 
  left_join(airlines) |> 
  dim()

flights |> 
  left_join(planes) |> 
  View() #shows too many NAs

flights |> 
  left_join(planes, join_by(tailnum)) |> 
  View() 

flights |> 
  left_join(airports, join_by(dest == faa)) |> 
  View() 

# Filtering joins -> filters # rows
dim(airports)

airports |> 
  semi_join(flights, join_by(faa == origin)) |> 
  dim()

airports |> 
  semi_join(flights, join_by(faa == dest)) |> 
  dim()


#anti_join
flights |> 
  anti_join(airports, join_by(dest == faa)) |> 
  distinct(dest) |> #tells you distinct destinations 
  pull() #converts tibble -> vector

mising_airports %in% airports$faa
