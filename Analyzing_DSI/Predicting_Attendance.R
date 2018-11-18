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
library(janitor)
yq <- as.yearqtr(as.yearmon(df$timestamp) + 1/12)

Workshop_dataframe = df %>%  
  mutate(yday = yday(timestamp),
         season = factor(format(yq, "%q"), levels = 1:4, 
                         labels = c("Spring", "Spring", "Fall", "Fall")),
         month = months(timestamp),
         year = year(timestamp),
         num_days_into_sem = ifelse(season == "Fall", yday(timestamp) - 233, yday(timestamp))) %>% 
  filter(!grepl("Survey|Elections|RSVP|Q&A|Symposium|Involvement|Applications|Election|Post", workshop_type)) %>% 
  rename(workshop_name = workshop_type) %>% 
  mutate(wk_type = ifelse(grepl("Python 0|Python0", workshop_name),"Python 0",
                          ifelse(grepl("Python 1|Python1|Python_1", workshop_name), "Python 1",
                                 ifelse(grepl("Python2|Python 2", workshop_name), "Python 2",
                                        ifelse(grepl("Hadoop", workshop_name), "Hadoop",
                                               ifelse(grepl("Data Visualization", workshop_name), "Data Visualization",
                                                      ifelse(grepl("SQL", workshop_name), "SQL",
                                                             ifelse(grepl("GBM", workshop_name), "GBM",
                                                                    ifelse(grepl("Data Science Wednesday", workshop_name), "DSW",
                                                                           ifelse(grepl("R-0|R0|R 0", workshop_name), "R0",
                                                                                  ifelse(grepl("R1|R Workshop", workshop_name), "R1",
                                                                                         ifelse(grepl("R2", workshop_name), "R2",
                                                                                                ifelse(grepl("Finance", workshop_name), "Finance",
                                                                                                       ifelse(grepl("API", workshop_name), "API",
                                                                                                              ifelse(grepl("NLP|Natural Language Processing", workshop_name), "Natural Language Processing",
                                                                                                                     ifelse(grepl("Spark", workshop_name), "Spark",
                                                                                                                            ifelse(grepl("Neural", workshop_name), "Neural Nets", 
                                                                                                                                   ifelse(grepl("Practical", workshop_name), "Practical Data Science", "None")))))))))))))))))) %>% 
  group_by(workshop_name, season, month, year, num_days_into_sem, wk_type) %>% 
  summarise(count = n()) %>% 
  filter(count > 5) %>% 
  filter(!grepl("Spring 2018 Python0 Sign-In", workshop_name)) %>% 
  as.data.frame() %>% 
  add_row(workshop_name = "Spring 2018 Python0 Sign-In (1/24/2018) (Responses)", season= "Spring", month = "January", year = 2018, num_days_into_sem = 18, wk_type = "Python 0", count = 42) %>% 
  add_row(workshop_name = "Spring 2018 Python0 Sign-In (1/25/2018) (Responses)", season= "Spring", month = "January", year = 2018, num_days_into_sem = 19, wk_type = "Python 0", count = 43)
  

write.csv(Workshop_dataframe, "Grouped_workshop_data.csv")

df = read.csv("Fall_Workshops.csv")

#now on to prediction

linear_model = lm(data = Workshop_dataframe, count ~ season + month + as.factor(year) + num_days_into_sem + wk_type)
plot(linear_model)
summary(linear_model)

linear_model
