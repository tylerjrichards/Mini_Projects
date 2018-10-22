library(here)
library(tidyverse)
setwd(dir = here())
df <- read_csv("Full_DSI_History.csv")
library(lubridate)

#first, we have two different time formats for the dataframe which is super annoying. Let's fix that here
df$timestamp <- parse_date_time(x = df$timestamp,
                orders = c("m/d/y h:m:s", "y-m-d h:m:s"))

#we can do this analysis by month and day, but we really want two things
#first we want Fall or Spring, and next we want how many weeks of school have gone by. If we just do month, then we're confounding those variables

library(zoo)
yq <- as.yearqtr(as.yearmon(df$timestamp) + 1/12)

test = df %>% 
  mutate(day = weekdays(timestamp),
         season = factor(format(yq, "%q"), levels = 1:4, 
                         labels = c("Spring", "Spring", "Fall", "Fall")),
         month = months(timestamp),
         time_of_year = ifelse(season == "Fall", yday(timestamp) - 233, yday(timestamp))) %>% 
  group_by(workshop_type, season, month, time_of_year) %>% 
  summarise(count = n()) %>% 
  as.data.frame()

#stopping here, this isn't working as well as I thought, there are surveys, RSVPs, etc in there which shouldn't be, and with only 179 rows (there are actually going to be much fewer), idk how well this ML will work
#another idea, given how many people show up in the first ten minutes, how many are going to show up for the workshop?