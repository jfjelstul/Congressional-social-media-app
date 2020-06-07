###########################################################################
# Josh Fjelstul, Ph.D.
# Congressional Social Media (CSM) Project
###########################################################################

##################################################
# function to clean the text of a tweet
##################################################

clean_tweet <- function(raw) {
  raw <- str_squish(raw)
  raw <- str_replace_all(raw, "[’]", "'")
  raw <- str_replace_all(raw, "<.*?>", "")
  raw <- str_replace_all(raw, "<.*?>", "&")
  cleaned <- str_to_lower(raw)
  cleaned <- str_replace(cleaned, "http.*", "")
  cleaned <- str_replace_all(cleaned, "@[a-z0-9_]+", "")
  cleaned <- str_replace(cleaned, "^rt", "")
  cleaned <- str_replace_all(cleaned, "['’]", "")
  cleaned <- str_replace_all(cleaned, "[^a-z ]+", " ")
  cleaned <- str_squish(cleaned)
  out <- list(raw = raw, cleaned = cleaned)
  return(out)
}

##################################################
# function to scrape a tweet
##################################################

scrape_tweet <- function(url) {
  json_url <- str_c("https://api.twitter.com/1/statuses/oembed.json?url=", url, collapse = "")
  json <- try(read_json(json_url))
  if(class(json) == "try-error") {
    return(NULL)
  }
  html <- read_html(json$html)
  p <- html %>% html_node("p") %>% as.character()
  text <- str_replace_all(p, "<.?p.*?>", "")
  text <- str_replace_all(text, "<br>", " ")
  author <- str_extract(as.character(html), "<.p>.*?<a.*?>")
  author <- str_replace_all(author, "<.*?>", "")
  author <- str_replace_all(author, "—", "")
  author <- str_squish(author)
  out <- clean_tweet(text)
  twitter_handle <- str_extract(author, "@[A-Za-z0-9_ ]+")
  out$twitter_handle <- twitter_handle
  return(out)
}

##################################################
# get member data
##################################################

get_member_data <- function(tweet, congress_data, tweet_data) {
  selected <- unique(congress_data$member[str_detect(congress_data$user_name, tweet$twitter_handle)])
  out <- filter(tweet_data, member == selected)
  return(out)
}

get_member <- function(tweet, congress_data) {
  out <- unique(congress_data$member[str_detect(congress_data$user_name, tweet$twitter_handle)])
  return(out)
}

##################################################
# function to make the tweet table
##################################################

make_tweet_table <- function(words, scores) {
  
  # set up a template
  template <- includeHTML("www/HTML/tweet-table-cell.html")
  template <- as.character(template)
  
  # color palette
  cut <- cut(scores, breaks = c(-1, -0.05, -0.02, -0.01, -0.001, 0.001, 0.01, 0.02, 0.05, 1), include.lowest = TRUE)
  
  # color palette
  palette <- colorRampPalette(c("#2D8CFF", "white", "#FF4242"))
  colors <- palette(9)
  
  # get stopwords
  stopwords <- stopwords()
  stopwords <- str_replace_all(stopwords, "'", "")
  
  # format
  words <- as.character(words)
  scores <- format(round(scores, 3), nsmall = 3)
  borders <- rep("#FFFFFF", length(words))
  borders[as.numeric(cut) == 5] <- "#DADADA"
  
  # loop througuh words in the tweet
  table_HTML <- NULL
  for(i in 1:length(words)) {
    cell_HTML <- template
    if(words[i] %in% stopwords) {
      cell_HTML <- str_replace(cell_HTML, "SCORE", "")
    }
    if(words[i] %in% stopwords) {
      cell_HTML <- str_replace(cell_HTML, "<div class=\"circle\" style=\"background-color: COLOR; border-color: BORDER;\"></div>", "")
    }
    cell_HTML <- str_replace(cell_HTML, "WORD", words[i])
    cell_HTML <- str_replace(cell_HTML, "SCORE", scores[i])
    cell_HTML <- str_replace(cell_HTML, "COLOR", colors[cut[i]])
    cell_HTML <- str_replace(cell_HTML, "BORDER", borders[i])
    table_HTML <- c(table_HTML, cell_HTML)
  }
  table_HTML <- str_c(table_HTML, collapse = "")
  table_HTML <- str_c("<div class=\"tweet-table\">", table_HTML, "</div>", collapse = "")
  return(HTML(table_HTML))
}

##################################################
# function to calculate tweet score
##################################################

tweet_score <- function(text, model) {
  
  # convert texts to sequences
  sequence <- texts_to_sequences(model$tokenizer, text$clean)
  
  # pad sequencess
  sequence <- pad_sequences(sequence, maxlen = 60, padding = "post", truncating = "post")
  
  # calculate score
  score <- as.numeric(predict(model$deterministic, sequence))
  
  # return
  return(score)
} 

##################################################
# function to calculate tweet distribution
##################################################

tweet_distribution <- function(text, model, iter = 100) {

  # convert texts to sequences
  sequence <- texts_to_sequences(model$tokenizer, text$clean)
  
  # pad sequencess
  sequence <- pad_sequences(sequence, maxlen = 60, padding = "post", truncating = "post")
  
  # run interations
  out <- NULL
  for(i in 1:iter) {
    score <- as.numeric(predict(model$stochastic, sequence))
    out <- c(out, score)
  }
  
  # return
  return(out)
} 

##################################################
# function to calculate word scores
##################################################

calculate_word_scores <- function(text, model) {

  # make a vector of words
  words <- str_split(text$clean, " ")
  words <- unlist(words)
  
  # convert texts to sequences
  sequence0 <- texts_to_sequences(model$tokenizer, text$clean)
  
  # pad sequencess
  sequence0 <- pad_sequences(sequence0, maxlen = 60, padding = "post", truncating = "post")
  
  # predict score
  score0 <- as.numeric(predict(model$deterministic, sequence0))
  
  # loop through words
  score1 <- NULL
  for(i in 1:length(words)) {
    
    # make new text
    text1 <- str_c(words[-i], collapse = " ")
    
    # convert texts to sequences
    sequence1 <- texts_to_sequences(model$tokenizer, text1)
    
    # pad sequencess
    sequence1 <- pad_sequences(sequence1, maxlen = 60, padding = "post", truncating = "post")
    
    # predict score
    score1_temp <- as.numeric(predict(model$deterministic, sequence1))
    
    # append
    score1 <- c(score1, score1_temp)
  }
  
  # make data frame
  out <- data.frame(word = words, score1 = score1, score0 = score0)
  
  # difference
  out$word_score <- round(-(out$score1 - out$score0), 3)
  
  # return
  return(out)
} 

##################################################
# function to get wikipedia page
##################################################

get_wikipedia_info <- function(url) {
  
  # make URL
  url <- str_c("https://en.wikipedia.org", url)
  
  # scrape the page
  html <- read_html(url)
  
  # extract body of article
  body <- html %>% html_nodes(xpath = "//*[@class=\"mw-parser-output\"]")
  
  # get children
  elements <- body %>% html_children()
  
  # select intro paragraphs
  class <- elements %>% html_attr("class")
  index_info_card <- which(class == "infobox vcard")
  index_toc <- which(class == "toc")
  intro <- elements[(index_info_card + 1):(index_toc - 1)]
  
  # extract text from HTML
  text <- intro %>% html_text()
  
  # drop article series paragaph
  if(str_detect(text[1], "This article is part of a series")) {
    text <- text[-1]
  }
  
  # clean text
  text[1] <- str_replace(text[1], "\\(.*?\\)", "")
  text <- str_replace_all(text, "\\[[0-9]+\\]", "")
  text <- str_squish(text)

  # make HTML
  text <- str_c("<p>", text, "</p>")

  # return HTML
  return(HTML(text))
}

##################################################
# function to convert numeric to ordinal
##################################################

to_ordinal <- function(x) {
  x <- as.character(x)
  if(x == "11") {
    x <- "11th"
  } else if(x == "12") {
    x <- "12th"
  } else if(x == "13") {
    x <- "13th"
  } else if(str_detect(x, "1$")) {
    x <- str_c(x, "st")
  } else if(str_detect(x, "2$")) {
    x <- str_c(x, "nd")
  } else if(str_detect(x, "[03-9]$")) {
    x <- str_c(x, "th")
  }
  return(x)
}

##################################################
# member description
##################################################

insert_member_description <- function(selected) {
  
  # get HTML template
  html <- insert_HTML_text("member-description")
  
  # get member data
  selected_data <- congress_data[congress_data$member == selected,]
  
  # representation text
  representation <- str_extract(selected_data$district, "[0-9]+")
  representation <- str_replace(representation, "^0+", "")
  if(selected_data$chamber == "House") {
    if(representation == "") {
      representation <- str_c(selected_data$state, "as an at-large member in the House", sep = " ") 
    } else {
      representation <- str_c(selected_data$state, "'s ", to_ordinal(representation), " Congressional district in the House", sep = "")      
    }
  } else if(selected_data$chamber == "Senate") {
    representation <- str_c(selected_data$state, "in the Senate", sep = " ")
  }
  
  # party name
  party_name <- ifelse(selected_data$party == "Democrat", "Democratic Party", "Republican Party")
  
  # score
  score <- str_pad(selected_data$score, width = 4, side = "right", pad = "0")
  
  # partisanship
  distance <- abs(selected_data$score - mean(congress_data$score, na.rm = TRUE))
  partisanship <- ""
  if(distance >= 0.3) {
    partisanship <- "strongly partisan"
  } else if(distance >= 0.2) {
    partisanship <- "moderately partisan"
  } else if(distance >= 0.1) {
    partisanship <- "weakly partisan"
  } else {
    partisanship <- "relatively non-partisan"
  }
  
  # clean percentiles
  selected_data$congress_percentile <- to_ordinal(selected_data$congress_percentile)
  selected_data$chamber_percentile <- to_ordinal(selected_data$chamber_percentile)
  selected_data$party_percentile <- to_ordinal(selected_data$party_percentile)
  selected_data$state_percentile <- to_ordinal(selected_data$state_percentile)
  
  # edit HTML
  html <- str_replace_all(html, "CONGRESS_PERCENTILE", selected_data$congress_percentile)
  html <- str_replace_all(html, "CHAMBER_PERCENTILE", selected_data$chamber_percentile)
  html <- str_replace_all(html, "PARTY_PERCENTILE", selected_data$party_percentile)
  html <- str_replace_all(html, "STATE_PERCENTILE", selected_data$state_percentile)
  html <- str_replace_all(html, "FIRST", selected_data$first)
  html <- str_replace_all(html, "LAST", selected_data$last)
  html <- str_replace_all(html, "STATE", selected_data$state)
  html <- str_replace_all(html, "PARTY_NAME", party_name)
  html <- str_replace_all(html, "PARTY", selected_data$party)
  html <- str_replace_all(html, "REPRESENTATION", representation)
  html <- str_replace_all(html, "SCORE", score)
  html <- str_replace_all(html, "CHAMBER", selected_data$chamber)
  html <- str_replace_all(html, "PARTISANSHIP", partisanship)
  html <- str_replace_all(html, "INVERSE_PERCENT_CHANCE", as.character(100 - round(selected_data$score * 100)))
  html <- str_replace_all(html, "PERCENT_CHANCE", as.character(round(selected_data$score * 100)))
  
  # convet text to HTML
  html <- HTML(html)
  
  # return
  return(html)
}

###########################################################################
# end R script
###########################################################################
