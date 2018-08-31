library(readr)
library(tidyverse)
library(wordcloud)
library(data.table)
Russian_tweets <- read_csv("tweets.csv")
ID_counts <- as.data.frame(table(Russian_tweets$user_key))
Common_users <- ggplot(subset(ID_counts, Freq > 1000), aes(x = reorder(Var1, Freq), y = Freq)) + geom_bar(stat = "identity") + coord_flip() + theme_bw() + labs(x = "Twitter Handle", y ="Number of Tweets", title = "Russian Influenced Twitter Accounts Ranked by Number of Flagged Tweets")
Common_users

Hashtags <- as.data.frame(table(unlist(str_extract_all(Russian_tweets$text, "#\\S+"))))

Common_hashtags <- ggplot(subset(Hashtags, Freq > 300), aes(x = reorder(Var1, Freq), y = Freq)) + geom_bar(stat = "identity") + coord_flip() + theme_bw() + labs(x = "Hashtag", y ="Hashtag Count", title = "Most Used Hashtags by Russian Influenced Twitter Accounts")
Common_hashtags

Black_Lives_Matter <- Russian_tweets[Russian_tweets$text %like% "#BlackLivesMatter",]
View(Black_Lives_Matter[c(30,31, 84,94),8])

Black_Lives_Matter[c(30,31, 84,94),8]

library(lubridate)
Russian_tweets$created_str <- as.Date(Russian_tweets$created_str)
Dates_freq <- as.data.frame(table(Russian_tweets$created_str))
Dates_freq$Week <- as.Date(cut(as.Date(Dates_freq$Var1), breaks = "week"))
New_Dates <- Dates_freq %>% 
  group_by(Var1) %>% 
  summarise(Frequency = sum(Freq))

ggplot( data = New_Dates, aes(as.Date(Var1), Frequency)) + geom_line() + labs(x = "Date", y = "Number of Tweets", title = "Foreign Influenced Tweets Over Time") + scale_x_date(date_breaks = "3 months") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


Russian_tweets$Week <- as.Date(cut(as.Date(Russian_tweets$created_str), breaks = "week"))
Russian_tweets$Week
ymd(Black_Lives_Matter$created_str)
format.str <- "%a %b %d %H:%M:%S %z %Y"
Russian_tweets$created_at <- as.POSIXct(strptime(Russian_tweets[,"created_str"], format.str, tz = "GMT"), tz = "GMT")

mtcars[grep("^Merc",rownames(mtcars)),]