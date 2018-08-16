library(nycflights13)
library(tidyverse)

flights
jan1 <- filter(flights, month == 1, day == 1)

# hasta pg 50