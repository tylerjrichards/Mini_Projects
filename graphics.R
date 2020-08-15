library(readxl)
library(ggtech)
library(tidyverse)
library(ggpomological)
df_quar <- read_excel("Quarantine at the Movies.xlsx")
glimpse(df_quar)

year_df = df_quar %>% 
  group_by(Year) %>% 
  summarise(cnt = n()) %>% 
  mutate(color_chr = 'equal')


year_plot = ggplot(data = year_df, aes( x = Year, y = cnt, fill = cnt)) +
  geom_bar(stat = 'identity') + 
  scale_x_continuous(breaks = seq(1940,2020,10)) +
  ylab("Count") +
  xlab("Year") + 
  theme(legend.position = "none")
year_plot +
  **theme_void() +**
  theme_pomological_fancy()

theme(legend. position = "none")

+
  scale_fill_pomological()

year_plot + theme_tech()


density_plot + theme_pomological("Homemade Apple", 16)
+
  theme_tech(theme="airbnb")



group_by(yrmn) %>%
  summarise(cnt = n()) %>%
  ggplot(aes(x = (yrmn), y = cnt)) +
  geom_bar(stat = "identity") +
  theme_tech(theme="airbnb")

ggplot(data = df_quar) +
  geom_bar(aes('Year'), stat = 'count')

'''
Year
Rating
Medium Seen On
Genre - the pie graph and average rating per genre bar graph
Both misc comparison graphs (not as high a priority if the others are taking more time)
'''



