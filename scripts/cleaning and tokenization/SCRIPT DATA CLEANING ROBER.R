#DATA CLEANING

library(readr)
train <- read_csv("~/Equipo/Twitter sentiment analysis/train.csv")
View(train)

library(readr)
test <- read_csv("~/Equipo/Twitter sentiment analysis/test.csv")
View(test)

require(dplyr)
sum(is.na(train))
sum(is.na(test))

unique(train$Sentiment)
0 1

1 feliz
0 triste

Mismos tweets test y train



all<- paste(train$SentimentText, sep = ",", collapse = NULL)

require(dplyr)

text_df <- data_frame(line = 1:99989, text = all)

text_df


library(tidytext)

words<- text_df %>%
  unnest_tokens(word, text)


