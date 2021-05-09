# ------------------------------------------------------
# coding with dplyr
# 28 Apr 2021
# Sarah K. Morris
# ------------------------------------------------------
library(dplyr)
data(starwars)
class(starwars)

glimpse(starwars) # similar to str() but better

# cleaning the data
starwarsClean <- starwars[complete.cases(starwars[,1:10]),]
# what does this do? gets rid of NAs?

anyNA(starwars)
# tells you if there are any NAs in the dataset (T/F)

glimpse(starwarsClean)


# filter() subsets data
filter(starwarsClean, gender == "masculine", height < 180, height > 100)
# could use commas instead of '&'

# arrange() reorder rows)
arrange(starwarsClean, by = height) # ascending
arrange(starwarsClean, by = desc(height)) # descending
arrange(starwarsClean, height, desc(mass)) # each additional column is used to break ties         

# select() choose variables by their names
