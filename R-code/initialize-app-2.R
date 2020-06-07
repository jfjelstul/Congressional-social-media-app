# Josh Fjelstul

# libraries
library(stringr)
library(dplyr)
library(keras)

setwd("~/Dropbox/Data Science/Machine Learning Demo/Data/")

##################################################
# read in data
##################################################

# read in data
# tweets <- read.csv("tweet_data.csv", stringsAsFactors = FALSE)

# create a label for each member
# tweets$label <- str_c(tweets$member, " (", tweets$party_short, "-", tweets$state_short, ")")

##################################################
# choose tweet examples
##################################################

# # read in data
# text <- read.csv("tweet_text.csv", stringsAsFactors = FALSE)
# 
# # make data frame
# examples <- tweets
# examples$text <- text$raw_text
# 
# # choose tweets
# mccain <- c(31839, 125798, 240422, 180679, 169030, 244077)
# schumer <- c(156032, 207964, 100809, 232534, 11728, 189458)
# mcconnell <- c(185862, 238780, 98651, 86727, 105485, 160072)
# 
# # subset data
# examples <- examples[c(schumer, mccain, mcconnell),]
# 
# # labels
# examples$label <- str_c("Tweet ", 1:nrow(examples), ": ", examples$label)
# 
# write.csv(examples, "tweet_examples.csv", row.names = FALSE)

# read in data
examples <- read.csv("tweet_examples.csv", stringsAsFactors = FALSE)

##################################################
# member-level data
##################################################

members <- tweets %>%
  group_by(member) %>%
  summarize(score = mean(score),
            party = party[1],
            party_short = party_short[1],
            state = state[1],
            state_short = state_short[1],
            label = label[1]) %>%
  arrange(member)

##################################################
# time-varying data
##################################################

members_month <- tweets %>%
  group_by(member, month) %>%
  summarize(score = mean(score),
            party = party[1],
            party_short = party_short[1],
            state = state[1],
            state_short = state_short[1],
            label = label[1])

members_month$month <- as.numeric(factor(members_month$month))

##################################################
# load model information 
##################################################

# read in the tokenizer
tokenizer <- load_text_tokenizer("tokenizer")

# read in the model
model <- load_model_hdf5("model")

# read in the model
model_uncertainty <- load_model_hdf5("model_dropout")

##################################################
# functions to analyze a tweet
##################################################

tweet_distribution <- function(text, model, tokenizer, iter = 100) {
  
  # clean
  text <- str_to_lower(text)
  text <- str_replace(text, "http.*", "")
  text <- str_replace_all(text, "@[a-z0-9_]+", "")
  text <- str_replace(text, "^rt", "")
  text <- str_replace_all(text, "[^a-z ]+", " ")
  text <- str_squish(text)
  
  # convert texts to sequences
  sequence <- texts_to_sequences(tokenizer, text)
  
  # pad sequencess
  sequence <- pad_sequences(sequence, maxlen = 60, padding = "post", truncating = "post")
  
  # run interations
  out <- NULL
  for(i in 1:iter) {
    score <- as.numeric(predict(model, sequence))
    out <- c(out, score)
  }
  
  # return
  return(out)
} 

tweet_simulation <- function(text, model, tokenizer) {
  
  # clean
  text <- str_to_lower(text)
  text <- str_replace(text, "http.*", "")
  text <- str_replace_all(text, "@[a-z0-9_]+", "")
  text <- str_replace(text, "^rt", "")
  text <- str_replace_all(text, "[^a-z ]+", " ")
  text <- str_squish(text)
  
  # make a vector of words
  words <- str_split(text, " ")
  words <- unlist(words)
  
  # convert texts to sequences
  sequence0 <- texts_to_sequences(tokenizer, text)
  
  # pad sequencess
  sequence0 <- pad_sequences(sequence0, maxlen = 60, padding = "post", truncating = "post")
  
  # predict score
  score0 <- as.numeric(predict(model, sequence0))
  
  # loop through words
  score1 <- NULL
  for(i in 1:length(words)) {
    
    # make new text
    text1 <- str_c(words[-i], collapse = " ")
    
    # convert texts to sequences
    sequence1 <- texts_to_sequences(tokenizer, text1)
    
    # pad sequencess
    sequence1 <- pad_sequences(sequence1, maxlen = 60, padding = "post", truncating = "post")
    
    # predict score
    score1_temp <- as.numeric(predict(model, sequence1))
    
    # append
    score1 <- c(score1, score1_temp)
  }
  
  # make data frame
  out <- data.frame(word = words, score1 = score1, score0 = score0)
  
  # difference
  out$delta <- -(out$score1 - out$score0)
  
  # return
  return(out)
} 

# out <- tweet_distribution(examples$text[14], model_uncertainty, tokenizer)
# out <- tweet_simulation(examples$text[14], model, tokenizer)
