# Josh Fjelstul

# libraries
library(stringr)
library(lubridate)
library(dplyr)
library(keras)
library(ggplot2)

# set working directory
setwd("~/Dropbox/Data Science/Machine Learning Demo")

##################################################
# model architecture
##################################################

# function to train model
train_model <- function(train_x, train_y, test_x, test_y, embedding_matrix) {
  
  # define flags
  FLAGS <- flags(
    flag_numeric("max_length", 60),
    flag_numeric("max_words", 10000),
    flag_numeric("dropout_rate", 0.5),
    flag_numeric("kernel_size", 7),
    flag_numeric("filters", 100),
    flag_numeric("embedding_dimensions", 100),
    flag_numeric("batch_size", 32),
    flag_numeric("learning_rate", 0.001),
    flag_numeric("epochs", 1)
  )
  
  # define dropout layer
  dropout <- layer_dropout(rate = FLAGS$dropout_rate)
  
  # define model
  input <- layer_input(shape = FLAGS$max_length)
  output <- input %>%
    layer_embedding(input_dim = FLAGS$max_words, output_dim = FLAGS$embedding_dimensions, input_length = FLAGS$max_length, weights = list(embedding_matrix), trainable = TRUE, name = "embeddings") %>%
    layer_conv_1d(filters = FLAGS$filters, kernel_size = FLAGS$kernel_size, strides = 1, activation = "relu") %>%
    layer_max_pooling_1d() %>%
    layer_conv_1d(filters = FLAGS$filters, kernel_size = FLAGS$kernel_size, strides = 1, activation = "relu") %>%
    layer_global_max_pooling_1d() %>%
    dropout(training = FALSE) %>%
    layer_dense(FLAGS$filters, activation = "relu") %>%
    dropout(training = FALSE) %>%
    layer_dense(1, activation = "sigmoid", name = "output")
  model <- keras_model(input, output)
  
  # compile model
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_adam(lr = FLAGS$learning_rate),
    metrics = c("accuracy")
  )
  
  # fit model
  model %>% fit(
    x = train_x,
    y = train_y,
    batch_size = FLAGS$batch_size,
    epochs = FLAGS$epochs,
    validation_data = list(test_x, test_y)
  )
  
  # return
  return(model)
}

# function to train model
copy_model <- function(model1) {
  
  # define flags
  FLAGS <- flags(
    flag_numeric("max_length", 60),
    flag_numeric("max_words", 10000),
    flag_numeric("dropout_rate", 0.5),
    flag_numeric("kernel_size", 7),
    flag_numeric("filters", 100),
    flag_numeric("embedding_dimensions", 100),
    flag_numeric("batch_size", 32),
    flag_numeric("learning_rate", 0.001),
    flag_numeric("epochs", 1)
  )
  
  # define dropout layer
  dropout <- layer_dropout(rate = FLAGS$dropout_rate)
  
  # define model
  input <- layer_input(shape = FLAGS$max_length)
  output <- input %>%
    layer_embedding(input_dim = FLAGS$max_words, output_dim = FLAGS$embedding_dimensions, input_length = FLAGS$max_length, weights = list(embedding_matrix), trainable = TRUE, name = "embeddings") %>%
    layer_conv_1d(filters = FLAGS$filters, kernel_size = FLAGS$kernel_size, strides = 1, activation = "relu") %>%
    layer_max_pooling_1d() %>%
    layer_conv_1d(filters = FLAGS$filters, kernel_size = FLAGS$kernel_size, strides = 1, activation = "relu") %>%
    layer_global_max_pooling_1d() %>%
    dropout(training = TRUE) %>%
    layer_dense(FLAGS$filters, activation = "relu") %>%
    dropout(training = TRUE) %>%
    layer_dense(1, activation = "sigmoid", name = "output")
  model2 <- keras_model(input, output)
  
  # compile model
  model2 %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_adam(lr = FLAGS$learning_rate),
    metrics = c("accuracy")
  )
  
  # copy weights
  set_weights(model2, get_weights(model1))
  
  # return
  return(model2)
}

##################################################
# prepare data
##################################################

# read in tweets
senate <- read.csv("Data/senate_tweets.csv", stringsAsFactors = FALSE)
house <- read.csv("Data/house_tweets.csv", stringsAsFactors = FALSE)
senate$chamber <- "Senate"
house$chamber <- "House"
tweets <- rbind(senate, house)

# select variables
tweets <- select(tweets, user_screen_name, text, created_at)

# rename
tweets <- rename(tweets, user_name = user_screen_name, date = created_at)

# date
tweets$date <- str_replace(tweets$date, "^[A-Z][a-z]+", "")
tweets$date <- str_replace(tweets$date, "[0-9]+:[0-9]+:[0-9]+", "")
tweets$date <- str_replace(tweets$date, "[+]0000", "")
tweets$date <- str_squish(tweets$date)
tweets$date <- str_replace_all(tweets$date, " ", "-")
tweets$date <- mdy(tweets$date)
tweets$month <- str_extract(tweets$date, "[0-9]+-[0-9]+")

# filter
tweets <- filter(tweets, date >= ymd("2017-01-03") & date <= ymd("2019-01-03"))

# clean
tweets$clean_text <- tweets$text
tweets$clean_text <- str_to_lower(tweets$clean_text)
tweets$clean_text <- str_replace(tweets$clean_text, "http.*", "")
tweets$clean_text <- str_replace_all(tweets$clean_text, "@[a-z0-9_]+", "")
tweets$clean_text <- str_replace(tweets$clean_text, "^rt", "")
tweets$clean_text <- str_replace_all(tweets$clean_text, "'", "")
tweets$clean_text <- str_replace_all(tweets$clean_text, "[^a-z ]+", " ")
tweets$clean_text <- str_squish(tweets$clean_text)

# word count
tweets$words <- str_count(tweets$clean_text, "[a-z]+")

# filter
tweets <- filter(tweets, words >= 5)

# read in senator data
# congress <- read.csv("Data/115th_congress_old.csv", stringsAsFactors = FALSE)
# 
# congress$district <- str_pad(congress$district, width = 2, side = "left", pad = "0")
# 
# congress$district[congress$chamber == "Senate"] <- ""
# congress$district <- str_c(congress$state_short, "", congress$district)
# 
# congress$last <- str_replace_all(congress$last, "'", " ")
# 
# congress$member <- str_c(congress$last, " (", congress$party_short, "-", congress$district, ")")
# 
# congress_collapsed <- congress %>% 
#   group_by(member) %>% 
#   summarize(user_name = str_c(user_name, collapse = ", "),
#             chamber = chamber[1],
#             last = last[1],
#             first = first[1],
#             party = party[1],
#             party_short = party_short[1],
#             state = state[1],
#             state_short = state_short[1],
#             district = district[1],
#             note = note[1])

# congress_collapsed <- select(congress, -user_name)
# congress_collapsed$duplicated <- duplicated(congress_collapsed)
# congress_collapsed <- filter(congress_collapsed, !duplicated)
# congress_collapsed <- select(congress_collapsed, )

# setwd("~/Dropbox/Data Science/Machine Learning Demo/Data/")
# 
# write.csv(congress, "115th_congress.csv", row.names = FALSE)
# write.csv(congress_collapsed, "115th_congress_collapsed.csv", row.names = FALSE)

# read in congress data
congress <- read.csv("Data/115th_congress.csv", stringsAsFactors = FALSE)

# filter
congress <- filter(congress, user_name != "MISSING")
congress <- filter(congress, !duplicated(user_name))

# choose variables
congress <- select(congress, -note)

# merge
tweets <- left_join(tweets, congress, by = c("user_name"))

# drop missing
tweets <- na.omit(tweets)

# x
text <- tweets$clean_text

# y
y <- as.numeric(tweets$party == "Republican")

# tokenize texts
tokenizer <- text_tokenizer(num_words = 10000) %>% fit_text_tokenizer(text)

# convert texts to sequences
sequences <- texts_to_sequences(tokenizer, text)

# pad sequencess
sequences <- pad_sequences(sequences, maxlen = 60, padding = "post", truncating = "post")

# indexes
train_indexes <- sample(1:nrow(tweets), floor(nrow(tweets) * 0.7))
test_indexes <- setdiff(1:nrow(tweets), train_indexes)

# train data
train_x <- sequences[train_indexes,]
train_y <- y[train_indexes]

# test data
test_x <- sequences[test_indexes,]
test_y <- y[test_indexes]

# out <- select(tweets, member, text, clean_text)
# write.csv(out, "tweet_text.csv", row.names = FALSE)

##################################################
# glove embeddings
##################################################

# # read in GloVe data
# glove_data <- read.delim("glove.6B.100d.txt", sep = " ", header = FALSE)
# glove_matrix <- as.matrix(glove_data[,-1])
# rownames(glove_matrix) <- glove_data$V1
# colnames(glove_matrix) <- str_c("D", 1:100)
# 
# # get the word indexes for the 10000 most common words from our tokenizer
# word_index <- tokenizer$word_index
# word_index <- unlist(word_index)
# word_index <- word_index[1:10000]
# 
# # make embedding matrix
# embedding_matrix <- matrix(0, nrow = 10000, ncol = 100)
# for (i in 1:10000) {
#   word <- names(word_index)[i]
#   embedding_vector <- as.numeric(glove_matrix[rownames(glove_matrix) == word,])
#   if (length(embedding_vector) != 0) {
#     embedding_matrix[i,] <- embedding_vector
#   }
# }
# rm(i)
# 
# dim(embedding_matrix)
# 
# embedding_data <- as.data.frame(embedding_matrix)
# names(embedding_data) <- str_c("dimension_", 1:100)
# 
# write.csv(embedding_data, "embedding_data.csv", row.names = FALSE)

embedding_matrix <- read.csv("Data/embedding_data.csv", stringsAsFactors = FALSE)
embedding_matrix <- as.matrix(embedding_matrix)

##################################################
# train with embeddings
##################################################

# fit model
model <- train_model(train_x, train_y, test_x, test_y, embedding_matrix)
model_uncertainty <- copy_model(model)

# save the tokenizer
save_text_tokenizer(tokenizer, "Trained Model/tokenizer")

# save the model
save_model_hdf5(model, "Trained Model/model")
save_model_hdf5(model_uncertainty, "Trained Model/model_uncertainty")

##################################################
# read in saved data
##################################################

# read in the tokenizer
tokenizer <- load_text_tokenizer("Trained Model/tokenizer")

# read in the model
model <- load_model_hdf5("Trained Model/model")
model_uncertainty <- load_model_hdf5("Trained Model/model_uncertainty")

##################################################
# predicted probabilities
##################################################

predict_scores <- function(model, sequences) {
  as.numeric(predict(model, sequences))
}

predict_scores_dropout <- function(model, sequences, iter) {
  matrix <- matrix(NA, nrow = nrow(sequences), ncol = iter)
  for(i in 1:iter) {
    matrix[, i] <- as.numeric(predict(model, sequences))
  }
  return(matrix)
}

# test <- sequences[1:10,]

# pred <- predict_scores_dropout(model, test, 500)
# pred_uncertainty <- predict_scores_dropout(model_uncertainty, test, 500)

# add tweet-level scores
tweets$score <- predict_scores(model, sequences)

# chamber
# tweets$chamber <- "House"
# tweets$chamber[!str_detect(tweets$district, "[0-9]+")] <- "Senate"

# label
# tweets$member <- str_c(tweets$last, " (", tweets$party_short, "-", tweets$district, ")")

# arrange
tweets <- arrange(tweets, chamber, member, date)

# choose variables
out <- select(tweets, chamber, member, last, first, state, state_short, district, party, party_short, date, month, score, text, clean_text, words)
out_small <- select(tweets, chamber, member, last, first, state, state_short, district, party, party_short, date, month, score)

# export data
write.csv(out, "Data/tweet_data.csv", row.names = FALSE)
write.csv(out_small, "Data/tweet_data_small.csv", row.names = FALSE)

# ##############################################
# # rank ordering
# ##############################################
# 
# # chamber
# tweets$chamber <- "Senate"
# tweets$chamber[tweets$user_name %in% house$user_screen_name] <- "House"
# 
# # district
# tweets$district <- str_pad(tweets$district, side = "left", width = 2, pad = "0")
# 
# # create a label for each member
# tweets$label <- str_c(tweets$member, " (", tweets$party_short, "-", tweets$state_short, ifelse(tweets$chamber == "House", tweets$district, ""), ")")
# 
# # members data
# members <- tweets %>%
#   group_by(label) %>%
#   summarize(score = mean(score),
#             chamber = chamber[1],
#             party = party[1],
#             party_short = party_short[1],
#             state = state[1],
#             state_short = state_short[1],
#             district = district[1]) %>%
#   arrange(chamber, state, district, label)
# 
# # make plot data
# plot_data <- members
# 
# # order factor
# plot_data$label <- factor(plot_data$label, levels = plot_data$label[order(plot_data$score)])
# 
# # plot positions
# plot <- ggplot(plot_data) +
#   geom_point(aes(x = score, y = as.numeric(label), color = party), position = position_jitter(width = 0.01, height = 0), alpha = 0.7, size = 2) +
#   scale_color_manual(name = NULL, values = c(colors[1], "gray50", colors[2])) +
#   scale_x_continuous(breaks = seq(0, 1, 0.1)) +
#   scale_y_continuous(expand = expand_scale(0.05, 0)) +
#   my_titles(x = "Member-Level Score\n(Predictably Democrat to Predictably Republican)", y = "Member", main = "Member-Level Scores (Rank Ordered)") +
#   my_theme() + 
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())
# plot
# 
# ##################################################
# # analyze a tweet
# ##################################################
# 
# text <- tweets$text[2]
# 
# analyze_tweet <- function(text, model, iter = 100) {
#   
#   # clean
#   text <- str_to_lower(text)
#   text <- str_replace(text, "http.*", "")
#   text <- str_replace(text, "^rt", "")
#   text <- str_replace_all(text, "[^a-z ]+", " ")
#   text <- str_squish(text)
#   
#   # convert texts to sequences
#   sequence <- texts_to_sequences(tokenizer, text)
#   
#   # pad sequencess
#   sequence <- pad_sequences(sequence, maxlen = 60, padding = "post", truncating = "post")
# 
#   out <- NULL
#   for(i in 1:iter) {
#     pred <- as.numeric(predict(model, sequence))
#     out <- c(out, pred)
#   }
#   return(out)
# } 
# 
# out <- analyze_tweet(text, model_uncertainty, iter = 500)
# hist(out, xlim = c(0, 1))
# 