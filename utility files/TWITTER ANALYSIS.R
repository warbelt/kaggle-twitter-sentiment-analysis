#####R Data Mining of Twitter#####

##############################################################################
#1.- Retrieve Tweets
## Option 1: retrieve tweets from Twitter
library(twitteR)
library(ROAuth)
## Twitter authentication
consumer_key<-"4DXhrTcqypTUqKRfWrUl25bBC"
consumer_secret<-"PSPd0zWy0Bck2Cp8qaZ1HTCLPN08f9BcW6KrJP25j3S0oCAEP6"
access_token<-"827820085-rgRdk9YCB7kvEdIp9r9PeotYx69v5BqeCoDXwpJx"
access_secret<-"GB6Nrg6OBtHPLOYBRBeErBxGwOu7oiaYBNS5bjkk7Nwr0"
setup_twitter_oauth(consumer_key, consumer_secret, access_token,
                    access_secret)

## 3200 is the maximum to retrieve
tweets <- userTimeline("RDataMining", n = 3200)

## Option 2: download @RDataMining tweets from RDataMining.com
url <- "http://www.rdatamining.com/data/RDataMining-Tweets-20160212.rds"
download.file(url, destfile = "RDataMining-Tweets-20160212.rds")
#Best to download manually

## load tweets into R
tweets <- readRDS("RDataMining-Tweets-20160212.rds")

#See how many tweets do we have.
(n.tweet <- length(tweets))

############################################################################
# 2.-Convert tweets to a data frame
tweets.df <- twListToDF(tweets)
dim(tweets.df)


###############################################################################
#3.- Cleaning our text

library(tm)
# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(tweets.df$text))
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, tolower)
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
myCorpus <- tm_map(myCorpus, removeURL)

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords('english'), "available", "via")
# remove "r" and "big" from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
# keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)


# inspect the first 5 documents (tweets)
inspect(myCorpus[1:5])
# The code below is used for to make text fit for paper width
for (i in c(1:2, 320)) {
  cat(paste0("[", i, "] "))
  writeLines(strwrap(as.character(myCorpus[[i]]), 60))
}


stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
  # a word in dictionary. Remove empty string to avoid above issue.
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}
myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
myCorpus <- Corpus(VectorSource(myCorpus))

#############################################################################

#4.- Frequency analysis
# count frequency of "mining"
miningCases <- lapply(myCorpusCopy,
                      function(x) { grep(as.character(x), pattern = "\\<mining")} )
sum(unlist(miningCases))

# count frequency of "miner"
minerCases <- lapply(myCorpusCopy,
                     function(x) {grep(as.character(x), pattern = "\\<miner")} )
sum(unlist(minerCases))

# replace "miner" with "mining"
myCorpus <- tm_map(myCorpus, content_transformer(gsub),
                   pattern = "miner", replacement = "mining")
tdm <- TermDocumentMatrix(myCorpus,
                          control = list(wordLengths = c(1, Inf)))
tdm

idx <- which(dimnames(tdm)$Terms == "r")
inspect(tdm[idx + (0:5), 101:110])

# inspect frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq = 15, highfreq = 180))
#We have a superior limit, because some terms aren´t inside the tweets themselves.
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=15)
term.freq <- subset(term.freq, term.freq <=180)
df <- data.frame(term = names(term.freq), freq = term.freq)

##Plot of frequency
library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()

# which words are associated with 'r'?
findAssocs(tdm, "r", 0.2)

# which words are associated with 'mining'?
findAssocs(tdm, "mining", 0.25)


## try http:// if https:// URLs are not supported
#source("https://bioconductor.org/biocLite.R")
#biocLite("graph")
library(graph)

## try http:// if https:// URLs are not supported
#source("https://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
library(Rgraphviz)

#Plot of relations
plot(df, term = freq.terms, corThreshold = 0.1, weighting = T)

#The words are very small so we delete the least important words.

# inspect frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq = 22, highfreq = 180))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=22)
term.freq <- subset(term.freq, term.freq <=180)
df <- data.frame(term = names(term.freq), freq = term.freq)

library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()


plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
require(RColorBrewer)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
# plot word cloud
library(wordcloud)

#Create matrix freq without minimum
# inspect frequent words

term.freq2 <- rowSums(as.matrix(tdm))
term.freq2 <- subset(term.freq2, term.freq2 >=3)
term.freq2 <- subset(term.freq2, term.freq2 <=180)
df2 <- data.frame(term = names(term.freq2), freq = term.freq2)

#Wordcloud
wordcloud(words = df2$term, freq = df2$freq, min.freq = 3,
          random.order = F, colors = pal)

#############################################################################

#5.- Sentimental analysis

# install package sentiment140

require(digest)
require(devtools)
require(Rcpp)

#install_github("okugami79/sentiment140")

# sentiment analysis
library(sentiment)
sentiments <- sentiment(tweets.df$text)
table(sentiments$polarity)

# sentiment plot
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
require(data.table)
sentiments$date <- as.IDate(tweets.df$created)
result <- aggregate(score ~ date, data = sentiments, sum)
ggplot(result, aes(result$date, result$score)) +geom_line()+labs(x = "Date")+labs(y="Score")

############################################################################33

#6.- Rewteets analysis
##Most retweeted tweets
user <- getUser("RDataMining")
user$toDataFrame()
friends <- user$getFriends() # who this user follows
followers <- user$getFollowers() # this user's followers
followers2 <- followers[[1]]$getFollowers() # a follower's followers

plot(followers)
# select top retweeted tweets
table(tweets.df$retweetCount)
selected <- which(tweets.df$retweetCount >= 9)
# plot them
dates <- strptime(tweets.df$created, format="%Y-%m-%d")
plot(x=dates, y=tweets.df$retweetCount, type="l", col="grey",
     xlab="Date", ylab="Times retweeted")
colors <- rainbow(10)[1:length(selected)]
points(dates[selected], tweets.df$retweetCount[selected],
       pch=19, col=colors)
text(dates[selected], tweets.df$retweetCount[selected],
     tweets.df$text[selected], col=colors, cex=.9)

tweets[[1]]
retweeters(tweets[[1]]$id)
retweets(tweets[[1]]$id)




