###########################################################################
# Josh Fjelstul, Ph.D.
# Congressional Social Media (CSM) Project
###########################################################################

##################################################
# read in data
##################################################

# read in data
tweet_data <- read.csv("data/tweet-data.csv", stringsAsFactors = FALSE)

# read in congress data
congress_data <- read.csv("data/115th-congress.csv", stringsAsFactors = FALSE)

# read in HTML templates
HTML_text <- read_html("www/HTML/app-text.html")

# read in tweet examples
example_tweets <- read.csv("data/example-tweets.csv", stringsAsFactors = FALSE)

##################################################
# month data
##################################################

# month data
month_data <- tweet_data %>% 
  group_by(member, month) %>% 
  summarize(score = mean(score))
month_data$month <- as.numeric(as.factor(month_data$month))
month_data <- filter(month_data, month != 25)

# slope data
slope_data <- month_data %>% 
  group_by(member) %>% 
  summarize(slope =  coef(lm(score ~ month))[2])
slope_data$slope <- round(slope_data$slope, 3)

# score data
score_data <- tweet_data %>% group_by(member) %>%
  summarize(score_MEAN = round(mean(score), 2),
            score_SD = round(IQR(score, na.rm = TRUE), 2))

##################################################
# clean congress data
##################################################

# clean congress data
congress_data <- left_join(congress_data, score_data, by = "member")
congress_data <- left_join(congress_data, slope_data, by = "member")
congress_data <- rename(congress_data, score = score_MEAN)
congress_data <- congress_data %>% mutate(congress_percentile = round(rank(score)/n(), 2) * 100, congress_percentile_SD = round(rank(score_SD)/n(), 2) * 100)
congress_data <- congress_data %>% group_by(chamber) %>% mutate(chamber_percentile = round(rank(score)/n(), 2) * 100, chamber_percentile_SD = round(rank(score_SD)/n(), 2) * 100)
congress_data <- congress_data %>% group_by(party) %>% mutate(party_percentile = round(rank(score)/n(), 2) * 100, party_percentile_SD = round(rank(score_SD)/n(), 2) * 100)
congress_data <- congress_data %>% group_by(state) %>% mutate(state_percentile = round(rank(score)/n(), 2) * 100, state_percentile_SD = round(rank(score_SD)/n(), 2) * 100)
congress_data$slope_percentile <- round(rank(abs(congress_data$slope))/nrow(congress_data), 2) * 100

##################################################
# prepare map data
##################################################

# state-level data
state_data <- congress_data %>% 
  group_by(state) %>% 
  summarize(
    score_all = mean(score, na.rm = TRUE), 
    score_house = mean(score[chamber == "House"], na.rm = TRUE), 
    score_senate = mean(score[chamber == "Senate"], na.rm = TRUE)
  ) %>%
  ungroup()
state_data$score_house[is.nan(state_data$score_house)] <- NA

# district-level data
district_data <- congress_data %>%
  filter(chamber == "House") %>%
  group_by(district) %>% 
  summarize(score = mean(score, na.rm = TRUE)) %>%
  ungroup()
district_data$score[is.nan(district_data$score)] <- NA

# cut state data
state_data$score_all_fill <- cut(state_data$score_all, breaks = seq(0, 1, 0.1))
state_data$score_house_fill <- cut(state_data$score_house, breaks = seq(0, 1, 0.1))
state_data$score_senate_fill <- cut(state_data$score_senate, breaks = seq(0, 1, 0.1))

# cut district data
district_data$score_fill <- cut(district_data$score, breaks = seq(0, 1, 0.1))

##################################################
# load trained neural networks
##################################################

# function to loead model
load_CNN <- function() {
  tokenizer <- load_text_tokenizer("trained-model/tokenizer")
  model <- load_model_hdf5("trained-model/model")
  model_dropout <- load_model_hdf5("trained-model/model-dropout")
  out <- list(tokenizer = tokenizer, deterministic = model, stochastic = model_dropout)
  return(out)
}

# load trained neural network
model <- load_CNN()

###########################################################################
# end R script
###########################################################################
