install.packages("topicmodels")
library (base64enc)
library(tidyverse)
library(twitteR)
library(ROAuth)
library(dplyr)
library(httr)
library(stringr)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(SnowballC)
----------------------------------------

consumer_key <- "iEfyueZn0AIqOupAbru7KQ4hO"
consumer_secret <- "5mDRidCTb037SYRWvDGcjGD1LOFr8mnGvE2bXvSg8vapRKnw3S"
access_token <-"1269900012269551617-tbTtzvo74EfbdyskCA5S7Lo49abxKm"
access_secret <-"q3qAU8gIb3WW2hJ3vlwd3z2VrdiQDEMD86eDgcYkmUc1W"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
-------------------------------------------------------------------------------------------------
#EXTRACT TWEETS
# Grab latest tweets
tweet_text <- function(x) x$getText()
# Submit a search query (terms separated by "+") and get a return
# set of data (corpus).
tweet_corpus <- function(search, n = 1000, ...) {
  payload <- searchTwitter(search, n = n, ...)
  sapply(payload, tweet_text)
}

# Search for some key terms, try to grab a lot if you want. Twitter will 
# limit you as it sees fit (can find). Also has spatial options.
# Try these Sacramento coordinates: '38.630404,-121.293535,50mi'

tweets <- tweet_corpus("#safoorazargar",n=1000,lang="en")


# Save your corpus (because you're limited in how often you can do this for free!)
saveRDS(tweets, file = "C:/Users/Sheeja Ayoob/Documents/tweets.Rds", compress = 'xz')

# Okay, read that corpus back in from disk. I'm sure you have a
# different save location, right?
tweets <- readRDS("C:/Users/Sheeja Ayoob/Documents/tweets.Rds")
--------------------------------------------------------------------------------------------------------
#CLEANING TWEETS
# Here we pre-process the data in some standard ways. I'll post-define each step
tweets <- iconv(tweets, to = "ASCII", sub = " ")  # Convert to basic ASCII text to avoid silly characters
tweets <- tolower(tweets)  # Make everything consistently lower case
tweets <- gsub("rt", " ", tweets)  # Remove the "RT" (retweet) so duplicates are duplicates
tweets <- gsub("@\\w+", " ", tweets)  # Remove user names (all proper names if you're wise!)
tweets <- gsub("http.+ |http.+$", " ", tweets)  # Remove links
tweets <- gsub("[[:punct:]]", " ", tweets)  # Remove punctuation
tweets <- gsub("[ |\t]{2,}", " ", tweets)  # Remove tabs
tweets <- gsub("amp", " ", tweets)  # "&" is "&amp" in HTML, so after punctuation removed ...
tweets <- gsub("^ ", "", tweets)  # Leading blanks
tweets <- gsub(" $", "", tweets)  # Lagging blanks
tweets <- gsub(" +", " ", tweets) # General spaces (should just do all whitespaces no?)
tweets <- unique(tweets)  # Now get rid of duplicates!

corpus <- Corpus(VectorSource(tweets))  # Create corpus object

# Remove English stop words. This could be greatly expanded!
# Don't forget the mc.cores thing
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus=tm_map(corpus, removeWords,c("safoora", "zargar","safoorazargar", "delhi", "made", "can", "im", "peopl", "just", "i"))  

# Remove numbers. This could have been done earlier, of course.
corpus <- tm_map(corpus, removeNumbers)

# Stem the words. Google if you don't understand
corpus <- tm_map(corpus, stemDocument)
---------------------------------------------------------------------------------------------------------------------------
#WORDCLOUD
pal <- brewer.pal(8, "Dark2")
wordcloud(corpus, min.freq=2, max.words = 150, random.order = TRUE, col ="#006633")
---------------------------------------------------------------------------------------------------------------------------------------
#LDA

# Get the lengths and make sure we only create a DTM for tweets with
# some actual content
doc.lengths <- rowSums(as.matrix(DocumentTermMatrix(corpus)))
dtm <- DocumentTermMatrix(corpus[doc.lengths > 0])


#Load Topic models
library(topicmodels)
#Run Latent Dirichlet Allocation (LDA) using Gibbs Sampling
#set burn in
burnin <-1000
#set iterations
iter<-2000
#thin the spaces between samples
thin <- 500
#set random starts at 5
nstart <-5
#use random integers as seed 
seed <- list(254672,109,122887,145629037,2)
# return the highest probability as the result
best <-TRUE
#set number of topics 
k <-3
#run the LDA model
ldaOut <- LDA(dtm,k, method="Gibbs", control=
                list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
zaf=terms(ldaOut,10)
zaf

library(tidytext)

ap_topics <- tidy(ldaOut, matrix = "beta")
ap_topics

ap_documents <- tidy(ldaOut, matrix = "gamma")
ap_documents
