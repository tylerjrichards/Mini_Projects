downloads_data = read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-30/r-downloads.csv")
r_downloads = read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-30/r_downloads_year.csv")


head(downloads_data)

table(downloads_data$os)

library(dplyr)
library(ggplot2)
windows = downloads_data %>% 
  filter(os == "win") %>% 
  {table(.$ip_id)} %>% 
  as.data.frame() %>% 
  {prop.table(table(.$Freq))} %>% 
  as.data.frame()

windows
mac = downloads_data %>% 
  filter(os == "osx") %>% 
  {table(.$ip_id)} %>% 
  as.data.frame() %>% 
  {prop.table(table(.$Freq))} %>% 
  as.data.frame()

  ggplot(aes(x = Freq)) + geom_histogram()
mac

mac$os = "mac"
windows$os = "windows"

total_binded = rbind(mac, windows)
total_binded

ggplot(data = total_binded, aes(x = Freq)) + 
  geom_histogram() +
  transition_states(
    os,
    transition_length = 2,
    state_length = 1
  ) +
  labs(title = '{closest_state}') +
  enter_fade() + 
  exit_shrink()

library(gganimate)
ggplot(aes(x = v, y = Freq)) + 
  geom_histogram(stat = "identity") 
