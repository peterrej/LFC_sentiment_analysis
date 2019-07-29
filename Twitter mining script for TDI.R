#####Twitter mining#####
install.packages("twitteR")
install.packages("RCurl")
library(twitteR)
library(RCurl)


###Loading credentials
api_key <- "PrqlFHXgH6eGNno31GOD9WjgW"
api_secret <- "n3MH0TAMvy7YLd3yd7oYhiOtdOhgFl7D6cIfbp7lGJVrzlFTSF"
access_token <- "853156752-1vQos0Yn2ACCWj4mcr6BkN89CmkVSmIrAvKghqaQ"
access_token_secret <- "UNFpIVcWm4oElThJP7YD3yP8lsMLgrxNCbDNvVMZ5tzmz"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

###Practice pulling data
LFC_tweets<-searchTwitter("LFC", n = 100, lang = "en")
LFC_tweets


#########SENTIMENT ANALYSIS#########
###Running a sentiment analysis, which is a form of supervised learning
###Supervised learning is one of 3 types of ML

##The sentiment analysis determines if a comment is positive or negative (or neutral)
#2 lists of words (positve and negative). 
#Neutral words fall outside of these lists
#Binary sentiment analysis: accounts for conjunctions (e.g., food was great, but service was bad)
#Best sentiment analyses still have 80% error

####DEMO

##Problem statement: deciding from twitter data, does LFC or MUFC have more positive
#tweets!

##Loading additional packages
#install.packages("purrr")
#install.packages("dplyr")
#install.packages("plyr")
#install.packages("ROAuth")
#install.packages("stringr")
library(purrr)
library(dplyr)
library(plyr)
library(ROAuth)
library(stringr)

##Sentiment function
score.sentiment<-function(sentences, pos.words, neg.words, .progress = 'none')
{
  require(plyr)
  require(stringr)
  scores <- laply (sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub ('[[:punct:]]', "", sentence)#removing punctuation from sentence: "global substitution" from srtingr
    sentence <- gsub ('[[:cntrl:]]', "", sentence)#removing control words
    sentence <- gsub ('\\d+', "", sentence)#removing digits (subbing w/null space)
    sentence <- tolower(sentence)#converted to lowercase
    word.list <-str_split(sentence, '\\s+')#splitting statement into separate words
    words<-unlist(word.list)#stored in list
    pos.matches <- match (words, pos.words)#converted lists into vectors - pos words
    neg.matches <- match (words, neg.words)#neg words--match will tell me which item in list of words matches
    pos.matches <- !is.na(pos.matches)#indicates if not an NA, store it as a TRUE (w/out ! it's the opposite)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches) ##sums number of TRUE values for pos and subtracts w/neg
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score = scores, text = sentences)
  return(scores.df)
}

##Define positive & negative words
pos.words = scan("/Users/peterrej/Desktop/positive_words_for_sentiment_analysis.txt", what = 'character', comment.char = ';')
neg.words = scan("/Users/peterrej/Desktop/negative_words_for_sensitivity_analysis.txt", what = 'character', comment.char = ';')


##Getting Twitter data
tweet <- userTimeline("@LFC", n = 1000) #getting tweet text from LFC
tweet2<- userTimeline("@ManUtd", n = 1000)#United
tweet_df <- tbl_df(map_df(tweet,as.data.frame))#text is one of the columns
tweet2_df<-tbl_df(map_df(tweet2, as.data.frame))


##runing function
lfcscore <- score.sentiment(tweet_df$text, pos.words, neg.words, .progress = 'text')
mufcscore <- score.sentiment(tweet2_df$text, pos.words, neg.words, .progress = 'text')
hist(lfcscore$score)
hist(mufcscore$score)

sum(lfcscore$score)
sum(mufcscore$score)
##All togehter, United twitter handle is tweeting exactly 2.5X more positive
#tweets than LFC. 

#Next I'm going to pull tweets where #LFC and #ManUtd are mentioned,
#so not just from the official handles. We'll see if the sentiment of
#general Twitter accounts mirrors the official accounts...

##runing function 2.0
fantweet <- searchTwitter("#LFC", n = 1000, lang = "en") #getting tweet text from LFC
fantweet2<- searchTwitter("#ManUtd", n = 1000, lang = "en")#United
fantweet_df <- tbl_df(map_df(fantweet,as.data.frame))#text is one of the columns
fantweet2_df<-tbl_df(map_df(fantweet2, as.data.frame))
lfcfanscore <- score.sentiment(fantweet_df$text, pos.words, neg.words, .progress = 'text')
mufcfanscore <- score.sentiment(fantweet2_df$text, pos.words, neg.words, .progress = 'text')
hist(lfcfanscore$score)
hist(mufcfanscore$score)

sum(lfcfanscore$score)
sum(mufcfanscore$score)

##Wow. Not what I expected. Fans talk a lot of smack about LFC.
#For the last 1000 tweets the vast majority included a negative word.
#Total score LFC = -18, United = 214

