library(readr)
library(dplyr)

##loading data into R
setwd("C:/Users/LENOVO/Documents/Data science project/caflights15")
caflights15 <- read_csv("data/caflight15.csv")


## California cities
colnames(caflights15)
caflights15 <- caflights15 %>% 
  select(1,2,3,8,9,10,11,12,13,4,14,5,6,7,15) %>% 
  rename(year = YEAR, month = MONTH, day = DAY_OF_MONTH, dep_time = DEP_TIME, 
         dep_delay = DEP_DELAY, dep_del15 = DEP_DEL15, arr_time = ARR_TIME, arr_delay = 8, arr_del15 = ARR_DEL15,
         carrier = 10, flight = 11, tailnum = 12, origin = 13, dest = 14, distance = 15)
##Loading in airlines
airlines <- read_csv("data/airlines.csv")
airports <- read_csv("data/airports.csv")

### Working with joins
db_flights <- caflights15 %>% 
  left_join(airlines, by = "carrier") %>% 
  left_join(airports, by = c("origin" = "iata_code")) %>% 
  rename(origin_name = airport_name) %>% 
  select(-latitude, -longitude, -state, -country, -city) %>% 
  left_join(airports, by = c("dest" = "iata_code")) %>% 
  rename(dest_name = airport_name)


##Number of flights
db_flights %>% 
  tally() %>% 
  pull() %>% 
  as.integer()

##average per day
db_flights %>%
  group_by(day, month) %>% 
  tally() %>% 
  ungroup() %>% 
  summarise(avg = mean(n, na.rm = TRUE)) %>% 
  pull() %>% 
  round()

## Percent delayed
db_flights %>% 
  filter(!is.na(dep_del15)) %>% 
  summarise(
    delays = sum(dep_del15, na.rm = TRUE),
    total = n()
  ) %>% 
  mutate(percent = (delays / total) * 100) %>% 
  pull() %>% 
  round()

### dest dest_name and 
db_flights %>% 
  group_by(dest, dest_name) %>% 
  tally() %>% 
  collect() %>% 
  arrange(desc(n)) %>% 
  rename(`No of flights` = n) %>% 
  head(10)




