#load text mining libraries
install.packages("tm")
install.packages("SnowballC")
library(tm)
library(SnowballC)

#set working directory (modify path as needed)
#setwd("/Users/apple/Desktop/Spring Sem/WRI\ 108/Essay 3/Data")

#load files into corpus
#get listing of .txt files in directory
filenames2 <- list.files(getwd(),pattern="authority-ol.txt")

#read files into a character vector
files2 <- lapply(filenames2,readLines)

#create corpus from vector
docs <- Corpus(VectorSource(files2))

#inspect a particular document in corpus
inspect(docs[[1]])

#start preprocessing
docs <-tm_map(docs,content_transformer(tolower))
docs <- tm_map(docs, removePunctuation)

docs <- tm_map(docs, removeWords, stopwords("english"))
inspect(docs[[1]])

docs <- tm_map(docs, stripWhitespace)

#define and eliminate all custom stopwords
myStopwords <- c("can","will","thats", "dont", "also", "nahi", "can", "doesnt", "said", "already", "like")
docs <- tm_map(docs, removeWords, myStopwords)

# STOP WITH PRE PROCESSING HERE


# trying some sentiment analysis
library(dplyr)
library(stringr)
dtm <- DocumentTermMatrix(docs)
tidied_text <- tidy(dtm)
sentiments <- tidied_text %>% inner_join(get_sentiments("bing"), by = c(term = "word"))
sentiments

# install.packages("ggplot2")
library(ggplot2)

count(sentiments)

sentiments %>%
  count(sentiment, term, wt = count) %>%
  filter(n >= 3) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment") + ggtitle("Influential users based on Hub Score")



write.csv(sentiments,"auth-ol-sent.csv")

#Create document-term matrix
dtm <- DocumentTermMatrix(docs)

#convert rownames to filenames
#rownames(dtm) <- filenames

#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
freq[ord]
write.csv(freq[ord],"authority-ol-freq.csv")

#load topic models library
# install.packages("topicmodels")
# install.packages("tidytext")
library(topicmodels)
library(tidytext)



#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 2

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

#write out results

#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,5))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"auth-ol-TopicsToTerms.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"auth-ol-TopicProbabilities.csv"))

#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))





docs <- tm_map(docs, stemDocument, language = "english")


#Transform to lower case
docs <-tm_map(docs,content_transformer(tolower))

#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "'")

#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)

#Stem document
docs <- tm_map(docs,stemDocument)


#inspect a document as a check
writeLines(as.character(docs[[1]]))





lda <- LDA(dtm, k = 4, control = list(seed = 1234))
topics <- tidy(lda, matrix = "gamma")
topics

topics_2 <- tidy(lda, matrix = "beta")
topics_2



