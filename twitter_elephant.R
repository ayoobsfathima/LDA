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
library(tidytext)
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

tweets1 <- tweet_corpus("#keralaelephant",n=1000,lang="en")


# Save your corpus (because you're limited in how often you can do this for free!)
saveRDS(tweets1, file = "C:/Users/Sheeja Ayoob/Documents/tweets1.Rds", compress = 'xz')

# Okay, read that corpus back in from disk. I'm sure you have a
# different save location, right?
tweets1 <- readRDS("C:/Users/Sheeja Ayoob/Documents/tweets1.Rds")
--------------------------------------------------------------------------------------------------------
#CLEANING TWEETS
# Here we pre-process the data in some standard ways. I'll post-define each step
tweets1 <- iconv(tweets1, to = "ASCII", sub = " ")  # Convert to basic ASCII text to avoid silly characters
tweets1 <- tolower(tweets1)  # Make everything consistently lower case
tweets1 <- gsub("rt", " ", tweets1)  # Remove the "RT" (retweet) so duplicates are duplicates
tweets1 <- gsub("@\\w+", " ", tweets1)  # Remove user names (all proper names if you're wise!)
tweets1 <- gsub("http.+ |http.+$", " ", tweets1)  # Remove links
tweets1 <- gsub("[[:punct:]]", " ", tweets1)  # Remove punctuation
tweets1 <- gsub("[ |\t]{2,}", " ", tweets1)  # Remove tabs
tweets1 <- gsub("amp", " ", tweets1)  # "&" is "&amp" in HTML, so after punctuation removed ...
tweets1 <- gsub("^ ", "", tweets1)  # Leading blanks
tweets1 <- gsub(" $", "", tweets1)  # Lagging blanks
tweets1 <- gsub(" +", " ", tweets1) # General spaces (should just do all whitespaces no?)
tweets1 <- unique(tweets1)  # Now get rid of duplicates!

corpus1 <- Corpus(VectorSource(tweets1))  # Create corpus object

# Remove English stop words. This could be greatly expanded!
# Don't forget the mc.cores thing
corpus1 <- tm_map(corpus1, removeWords, stopwords("en"))  
corpus1=tm_map(corpus1, removeWords,c("…","hea","cow","god","also", "get","like", "kerala", "keralaeleph", "eleph", "im", "pregnant", "just", "i","pineappl","fed","now","human","one","incid","can","don","anim","explos"))


# Remove numbers. This could have been done earlier, of course.
corpus1 <- tm_map(corpus1, removeNumbers)

# Stem the words. Google if you don't understand
corpus1 <- tm_map(corpus1, stemDocument)
---------------------------------------------------------------------------------------------------------------------------
#WORDCLOUD
pal <- brewer.pal(8, "Dark2")
wordcloud(corpus1, min.freq=2, max.words = 100, random.order = TRUE, col = "#FFCC00")
----------------------------------------------------------------------------------------------------------------------------
#LDA

# Get the lengths and make sure we only create a DTM for tweets with
# some actual content
doc.lengths <- rowSums(as.matrix(DocumentTermMatrix(corpus1)))
dtm1 <- DocumentTermMatrix(corpus1[doc.lengths > 0])


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
ldaOut1 <- LDA(dtm1,k, method="Gibbs", control=
                list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
eleph=terms(ldaOut1,10)
eleph

library(tidytext)

ap_topics1 <- tidy(ldaOut1, matrix = "beta")
ap_topics1

ap_documents1 <- tidy(ldaOut1, matrix = "gamma")
ap_documents1

library(ggplot2)
library(dplyr)

ap_top_terms1 <- ap_topics1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()