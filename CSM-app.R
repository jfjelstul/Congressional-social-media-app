###########################################################################
# Josh Fjelstul, Ph.D.
# Congressional Social Media (CSM) Project
###########################################################################

# libraries
library(stringr)
library(backpipe)
library(ggplot2)
library(plotly)
library(ggiraph)
library(shiny)
library(SnowballC)
library(tm)
library(rvest)
library(jsonlite)
library(httr)
library(keras)
library(extrafont)
library(rworldmap)
library(rworldxtra)
library(raster)
library(dplyr)
library(ggiraph)
library(sf)
library(rgdal)
library(maptools)
library(rgeos)

# set working directory
# setwd("~/Documents/Congressional-social-media-app")

# source files
source("R-code/HTML-functions.R")
source("R-code/plot-theme.R")
source("R-code/app-functions.R")
source("R-code/US-maps.R")
source("R-code/initialize-app.R")

##################################################
# UI
##################################################

##################################################
# UI: intro page
##################################################

intro_page <- insert_page(
  page = 1, active = TRUE,
  insert_full_page(
    insert_block(
      insert_row_center(
        insert_banner(
          insert_HTML_text("banner") 
        )
      ),
      insert_spacer(75),
      insert_major_heading("The Congressional Social Media (CSM) Project"),
      insert_minor_heading("By Josh Fjelstul, Ph.D."),
      insert_text_block(
        insert_HTML_text("intro-0") 
      ),
      insert_row_center(
        insert_contained_button_link("website", "MY WEBSITE", url = , "https://www.joshuafjelstul.com"),
        insert_contained_button_link("linkedin", "MY LINKED-IN", url = , "https://www.linkedin.com/in/jfjelstul/"),
        insert_contained_button_link("github", "MY GITHUB", url = , "https://github.com/jfjelstul?tab=repositories")
      )
    ),
    insert_divider(),
    insert_block(
      insert_major_heading("An Introduction to CSM Scores"),
      insert_text_block(
        insert_HTML_text("intro-1") 
      ),
      insert_row_center(
        insert_plot_container(
          height = 700, ratio = 0.7,
          insert_girafe("senate_map")
        )
      ),
      insert_row_center(
        insert_download_button("PNG_senate_map", label = "DOWNLOAD PNG"),
        insert_download_button("CSV_senate_map", label = "DOWNLOAD CSV")
      ),
      insert_text_block(
        insert_HTML_text("intro-2") 
      ),
      insert_row_center(
        insert_plot_container(
          height = 700, ratio = 0.7,
          insert_girafe("house_map")
        )
      ),
      insert_row_center(
        insert_download_button("PNG_house_map", label = "DOWNLOAD PNG"),
        insert_download_button("CSV_house_map", label = "DOWNLOAD CSV")
      ),
      insert_text_block(
        insert_HTML_text("intro-3") 
      )
    ),
    # insert_divider(),
    insert_block_shaded(
      insert_minor_heading("How to Interpret CSM Scores"),
      insert_text_block(
        insert_HTML_text("interpretation")
      )
    ),
    # insert_divider(),
    insert_block(
      insert_minor_heading("Partisanship vs Ideology"),
      insert_text_block(
        insert_HTML_text("ideology")
      )
    ),
    insert_footer(
      insert_row_center(
        insert_HTML_text("footer")
      )
    )
  )
)

##################################################
# UI: chamber page
##################################################

chamber_page <- insert_page(
  page = 2, active = FALSE,
  insert_full_page(
    insert_spacer(25),
    insert_block(
      insert_row_center(
        insert_major_heading("Visualize Position-Taking in the House and Senate")
      ),
      insert_text_block(
        insert_HTML_text("chamber-page-intro")
      ),
      insert_row_center(
        insert_menu("chamber", choices = c("House", "Senate"))
      )
      # insert_row_center(
      #   insert_contained_button("chambers_page_update_button", "UPDATE")
      # )
    ),
    insert_HTML("chambers_page_HTML"),
    insert_footer(
      insert_row_center(
        insert_HTML_text("footer") 
      )
    )
  )
)

##################################################
# UI: member page
##################################################

member_page <- insert_page(
  page = 3, active = FALSE,
  insert_full_page(
    insert_spacer(25),
    insert_block(
      insert_major_heading("Analyze the Tweets of a Member of Congress"),
      insert_text_block(
        insert_HTML_text("member-page-intro")
      ),
      insert_row_center(
        insert_menu("select_chamber_menu", choices = c("House", "Senate")), 
        insert_menu("select_member_menu", choices = congress_data$member[congress_data$chamber == "House"])
      )
      # insert_row_center(
      #   insert_contained_button("members_page_update_button", "UPDATE")
      # )
    ),
    insert_HTML("members_page_HTML"),
    insert_footer(
      insert_row_center(
        insert_HTML_text("footer") 
      )
    )
  )
)

##################################################
# UI: tweet page
##################################################

tweet_page <- insert_page(
  page = 4, active = FALSE,
  insert_full_page(
    insert_spacer(25),
    insert_block(
      insert_major_heading("Provide a Tweet to Analyze"),
      insert_text_block(
        insert_HTML_text("tweet-page-intro")
      ),
      insert_row_center(
        insert_menu("input_options", choices = c("Provide a tweet URL", "Provide tweet text", "Use an example tweet"))
      ),
      conditionalPanel(
        condition = "input.input_options == 'Provide a tweet URL'",
        insert_text_block(
          insert_HTML_text("tweet-input-info-1") 
        ),
        insert_row_center(
          insert_text_box("tweet_URL_box", placeholder = "Paste URL here", width = "500px")
        )
      ),
      conditionalPanel(
        condition = "input.input_options == 'Provide tweet text'",
        insert_text_block(
          insert_HTML_text("tweet-input-info-2") 
        ),
        insert_row_center(
          insert_text_area("tweet_text_box", height = "150px", placeholder = "Paste text here", width = "400px")
        ),
        insert_text_block(
          insert_HTML_text("tweet-input-info-3") 
        ),
        insert_row_center(
          insert_menu("tweets_page_select_member_menu", c("None", congress_data$member))
        )
      ),
      conditionalPanel(
        condition = "input.input_options == 'Use an example tweet'",
        insert_text_block(
          insert_HTML_text("tweet-input-info-4") 
        ),
        insert_row_center(
          insert_menu("example_tweet_menu", choices = example_tweets$tweet_ID)
        )
      ),
      insert_row_center(
        insert_contained_button("tweets_page_update_button", "UPDATE")
      )
    ),
    insert_HTML("tweets_page_HTML"),
    insert_footer(
      insert_row_center(
        insert_HTML_text("footer") 
      )
    )
  )
)

##################################################
# UI: methodology page
##################################################

methodology_page <- insert_page(
  page = 5, active = FALSE,
  insert_full_page(
    insert_spacer(25),
    insert_block(
      insert_minor_heading("Methodology"),
      insert_text_block(
        insert_HTML_text("methodology")
      )
    ),
    insert_block_shaded(
      insert_minor_heading("Uncertainty"),
      insert_text_block(
        insert_HTML_text("uncertainty") 
      )
    ),
    insert_footer(
      insert_row_center(
        insert_HTML_text("footer")
      )
    )
  )
)

##################################################
# UI: about page
##################################################

about_page <- insert_page(
  page = 6, active = FALSE,
  insert_full_page(
    insert_spacer(25),
    insert_block(
      insert_minor_heading("Josh Fjelstul, Ph.D."),
      insert_text_block(
        insert_HTML_text("about")
      )
    ),
    insert_block_shaded(
      insert_minor_heading("About This App"),
      insert_text_block(
        insert_HTML_text("app-info") 
      )
    ),
    insert_footer(
      insert_row_center(
        insert_HTML_text("footer")
      )
    )
  )
)

##################################################
# UI: create site
##################################################

ui <- create_site(
  list(
    insert_app_header(
      "Josh Fjelstul, Ph.D."
    ),
    insert_title_bar(tabs = c("INTRODUCTION", "CHAMBERS", "MEMBERS", "TWEETS", "METHODOLOGY", "ABOUT")),
    insert_branding("The CSM Project"),
    insert_header_row(
      insert_header_button("website_header", "WEBSITE", url = , "https://www.joshuafjelstul.com"),
      insert_header_button("linkedin_header", "LINKED-IN", url = , "https://www.linkedin.com/in/jfjelstul/"),
      insert_header_button("github_header", "GITHUB", url = , "https://github.com/jfjelstul?tab=repositories")
    ) ,
    insert_page_container(
      intro_page,
      chamber_page,
      member_page,
      tweet_page,
      methodology_page,
      about_page
    )
  )
)

##################################################
# server
##################################################

server <- function(input, output, session) {
  
  ##################################################
  # create reactive values
  ##################################################
  
  # reactive values
  reactive <- reactiveValues()

  ####################################################################################################
  # reactive expressions
  ####################################################################################################
  
  ##################################################
  # REACTIVE: chamber page update button
  ##################################################
  
  # chambers_page_update_button
  observeEvent(input$chamber, {
    
    if(input$chamber == "House") {
      updateSelectInput(session, inputId = "select_member", choices = congress_data$member[congress_data$chamber == "House"])
    } else if(input$chamber == "Senate") {
      updateSelectInput(session, inputId = "select_member", choices = congress_data$member[congress_data$chamber == "Senate"])
    }
    
    # selected chamber
    reactive$chambers_page_selected_chamber <- input$chamber
    
    # udpate HTML
    reactive$chambers_page_HTML <- list(
      # insert_divider(),
      insert_block_shaded(
        insert_minor_heading("Rank Order of Members by CSM-M Score"),
        insert_text_block(
          insert_HTML_text("rank-order-plot-before")
        ),
        insert_row_center(
          insert_plot_container(
            height = 600, ratio = 0.8,
            insert_girafe("rank_ordering") 
          )
        ),
        insert_row_center(
          insert_outlined_button("download_button", "DOWNLOAD PNG"),
          insert_outlined_button("download_button", "DOWNLOAD CSV")
        ),
        insert_text_block(
          insert_HTML_text("rank-order-plot-after-1")
        ),
        insert_row_center(
          insert_switch("show_party_means", "Party means"),
          insert_switch("show_sample_mean", "Sample mean")
        ),
        insert_text_block(
          insert_HTML_text("rank-order-plot-after-2")
        ),
        insert_row_center(
          insert_menu("select_member", choices = congress_data$member[congress_data$chamber == "House"])
        ),
        insert_text_block(
          insert_HTML("member_description_1")
        )
      ),
      # insert_divider(),
      insert_block(
        insert_minor_heading("Geographic Variation in CSM-M Scores"),
        insert_text_block(
          insert_HTML_text("chamber-maps-before")
        ),
        insert_row_center(
          insert_plot_container(
            height = 700, ratio = 0.7,
            insert_girafe("chamber_map")
          )
        ),
        insert_row_center(
          insert_outlined_button("download_button", "DOWNLOAD PNG"),
          insert_outlined_button("download_button", "DOWNLOAD CSV")
        ),
        insert_text_block(
          insert_HTML_text("chamber-maps-after")
        )
      ),
      # insert_divider(),
      insert_block_shaded(
        insert_minor_heading("Party Distributions of CSM-M Scores"),
        insert_text_block(
          insert_HTML_text("party-distribution-plot-before")
        ),
        insert_row_center(
          insert_plot_container(
            height = 600, ratio = 0.8,
            insert_girafe("party_distributions") 
          )
        ),
        insert_row_center(
          insert_outlined_button("download_button", "DOWNLOAD PNG"),
          insert_outlined_button("download_button", "DOWNLOAD CSV")
        ),
        insert_text_block(
          insert_HTML_text("party-distribution-plot-after")
        ),
        insert_row_center(
          insert_switch("distribution_party_means", "Party means"),
          insert_switch("distribution_sample_mean", "Sample mean")
        )
      )
    )
  })
  
  ##################################################
  # REACTIVE: members page update button
  ##################################################
  
  # members_page_update_button
  observeEvent(input$select_member_menu, {
    
    # selected member
    reactive$members_page_selected_member <- input$select_member_menu
    
    # get member data
    reactive$members_page_selected_member_data <- congress_data[congress_data$member == reactive$members_page_selected_member,]
    
    # selected chamber
    selected_chamber <- congress_data$chamber[congress_data$member == reactive$members_page_selected_member]
    
    # if house
    if(selected_chamber == "House") {
      member_maps_HTML <- insert_row_center(
        insert_plot_container(
          height = 300, ratio = 1,
          insert_girafe("member_map_country")
        ),
        insert_plot_container(
          height = 400, ratio = 1,
          insert_girafe("member_map_state_house")
        ),
        insert_plot_container(
          height = 200, ratio = 1,
          insert_girafe("member_map_district")
        )
      )
    }
    
    # if senate
    if(selected_chamber == "Senate") {
      member_maps_HTML <- insert_row_center(
        insert_plot_container(
          height = 500, ratio = 1,
          insert_girafe("member_map_country")
        ),
        insert_plot_container(
          height = 400, ratio = 1,
          insert_girafe("member_map_state_senate")
        )
      )
    }
    
    # update HTML
    reactive$members_page_HTML <- list(
      insert_divider(),
      insert_block(
        insert_minor_heading("About This Member"),
        insert_text_block(
          insert_HTML("member_description_2")
        ),
        member_maps_HTML,
        insert_row_center(
          insert_outlined_button("download_button", "DOWNLOAD PNG"),
          insert_outlined_button("download_button", "DOWNLOAD CSV")
        )
        # insert_text_block(
        #   insert_HTML("wikipedia_entry_1")
        # ),
        # insert_row_center(
        #   insert_contained_button("wikipedia_link", "READ MORE")
        # )
      ),
      # insert_divider(),
      insert_block_shaded(
        insert_minor_heading("How Consistent Is This Member?"),
        insert_text_block(
          insert_HTML("member_distribution_before")
        ),
        insert_row_center(
          insert_plot_container(
            height = 600, ratio = 0.8,
            insert_girafe("member_distribution")
          )
        ),
        insert_row_center(
          insert_outlined_button("download_button", "DOWNLOAD PNG"),
          insert_outlined_button("download_button", "DOWNLOAD CSV")
        ),
        insert_text_block(
          insert_HTML("member_distribution_after")
        )
      ),
      # insert_divider(),
      insert_block(
        insert_minor_heading("Congressional Delegation"),
        insert_text_block(
          insert_HTML("delegation_plot_before")
        ),
        insert_row_center(
          insert_plot_container(
            height = 200, ratio = 0.25,
            insert_girafe("delegation")
          )
        ),
        insert_row_center(
          insert_outlined_button("download_button", "DOWNLOAD PNG"),
          insert_outlined_button("download_button", "DOWNLOAD CSV")
        ),
        insert_text_block(
          insert_HTML("delegation_plot_after")
        )
        # insert_row_center(
        #   insert_plot_container(
        #     height = 800, ratio = 0.8,
        #     insert_girafe("delegation_map")
        #   )
        # ),
        # insert_row_center(
        #   insert_outlined_button("download_button", "DOWNLOAD PNG"),
        #   insert_outlined_button("download_button", "DOWNLOAD CSV")
        # ),
        # insert_text_block(
        #   insert_HTML("delegation_maps_after")
        # )
      ),
      # insert_divider(),
      insert_block_shaded(
        insert_minor_heading("Variation in Position-Taking Over Time"),
        insert_text_block(
          insert_HTML("timeline_plot_before")
        ),
        insert_row_center(
          insert_plot_container(
            height = 600, ratio = 0.8,
            insert_girafe("member_timeline")
          )
        ),
        insert_row_center(
          insert_outlined_button("download_button", "DOWNLOAD PNG"),
          insert_outlined_button("download_button", "DOWNLOAD CSV")
        ),
        insert_text_block(
          insert_HTML("timeline_plot_after")
        )
      )
    )
  })
  
  ##################################################
  # REACTIVE: tweets page update button
  ##################################################x

  observeEvent(input$tweets_page_update_button, {
    
    # scrape tweet
    if(input$input_options == "Provide a tweet URL") {
      
      # selected tweet
      reactive$tweets_page_tweet <- scrape_tweet(input$tweet_URL_box)
      
      if(is.null(reactive$tweets_page_tweet)) {
        
        reactive$tweets_page_HTML <- insert_block(
          insert_row_center(
            HTML("<p><b>Please enter a valid tweet URL.</b></p>")
          )
        )
        
        return()
      }
      
      # selected member
      reactive$tweets_page_selected_member <- get_member(reactive$tweets_page_tweet, congress_data)

    } else if(input$input_options == "Provide tweet text") {
      
      # selected tweet
      reactive$tweets_page_tweet <- clean_tweet(input$tweet_text_box)
      
      # selected member
      if (input$tweets_page_select_member_menu != "None") {
        reactive$tweets_page_selected_member <- input$tweets_page_select_member_menu
      } else {
        reactive$tweets_page_selected_member <- NULL
      }
      
    } else if(input$input_options == "Use an example tweet") {
      
      # selected tweet
      reactive$tweets_page_tweet <- clean_tweet(example_tweets$text[example_tweets$tweet_ID == input$example_tweet_menu])
      
      # selected member
      reactive$tweets_page_selected_member <- example_tweets$member[example_tweets$tweet_ID == input$example_tweet_menu]
    }

    # tweet score
    reactive$tweets_page_score <- tweet_score(reactive$tweets_page_tweet, model)

    # tweet distribution
    reactive$tweets_page_distribution <- tweet_distribution(reactive$tweets_page_tweet, model)
    
    # tweet versions text
    reactive$tweets_page_version_A_text <- reactive$tweets_page_tweet$raw
    reactive$tweets_page_version_B_text <- reactive$tweets_page_tweet$raw

    # tweet versions data
    version_A <- reactive$tweets_page_tweet$raw
    version_B <- reactive$tweets_page_tweet$raw
    version_A <- clean_tweet(version_A)
    version_B <- clean_tweet(version_B)
    version_A <- tweet_distribution(version_A, model)
    version_B <- tweet_distribution(version_B, model)
    version_A <- data.frame(score = version_A, version = "Version A")
    version_B <- data.frame(score = version_B, version = "Version B")
    reactive$tweets_page_version_A_data <- version_A
    reactive$tweets_page_version_B_data <- version_B

    # update values in text boxes
    updateTextAreaInput(session, inputId = "text_version_A", value = reactive$tweets_page_tweet$raw)
    updateTextAreaInput(session, inputId = "text_version_B", value = reactive$tweets_page_tweet$raw)
    
    # recode if member is missing
    if(identical(reactive$tweets_page_selected_member, character(0))) {
      reactive$tweets_page_selected_member <- NULL
    }
    
    # member comparison
    member_comparison_HTML <- list(
      # insert_divider(),
      insert_block_shaded(
        insert_minor_heading("How Representative is this Tweet?"),
        insert_text_block(
          insert_HTML("tweet_member_distribution_before")
        ),
        insert_row_center(
          insert_plot_container(
            height = 600, ratio = 0.8,
            insert_girafe("tweet_member_distribution")
          )
        ),
        insert_row_center(
          insert_outlined_button("download_button", "DOWNLOAD PNG"),
          insert_outlined_button("download_button", "DOWNLOAD CSV")
        ),
        insert_text_block(
          insert_HTML("tweet_member_distribution_after")
        )
      )
    )
    
    # member description
    member_description_HTML <- list(
      insert_spacer(50),
      insert_text_block(
        insert_HTML("member_description_3")
      )
    )
    
    # if no member is selected, omit description and comparison
    if(is.null(reactive$tweets_page_selected_member)) {
      member_description_HTML <- NULL
      member_comparison_HTML <- NULL
    }
    
    # update HTML
    reactive$tweets_page_HTML <- list(
      # insert_divider(),
      insert_block_shaded(
        insert_text_block(
          insert_HTML("tweet_text")
        ),
        member_description_HTML
      ),
      # insert_divider(),
      insert_block(
        insert_minor_heading("How Partisan is this Tweet?"),
        insert_text_block(
          insert_HTML_text("tweet-distribution-plot-before")
        ),
        insert_row_center(
          insert_plot_container(
            height = 600, ratio = 0.8,
            insert_girafe("tweet_distribution")
          )
        ),
        insert_row_center(
          insert_outlined_button("download_button", "DOWNLOAD PNG"),
          insert_outlined_button("download_button", "DOWNLOAD CSV")
        ),
        insert_text_block(
          insert_HTML("tweet_distribution_plot_after")
        )
      ),
      member_comparison_HTML,
      # insert_divider(),
      insert_block(
        insert_minor_heading("Word Sensitivity Analysis"),
        insert_text_block(
          insert_HTML_text("tweet-table-before")
        ),
        insert_row_center(
          style = "padding: 0% 15%;",
          insert_HTML("tweet_table") 
        ),
        insert_text_block(
          insert_HTML_text("tweet-table-after")
        )
      ),
      # insert_divider(),
      insert_block_shaded(
        insert_minor_heading("Open-Ended Phrase Sensitivity Analysis"),
        insert_text_block(
          insert_HTML_text("tweet-versions-before-1")
        ),
        # insert_text_block(
        #   insert_HTML_text("tweet-distribution-plot-before")
        # ),
        insert_row_center(
          insert_text_area(id = "text_version_A", height = "150px", width = "300px", placeholder = NULL),
          insert_text_area(id = "text_version_B", height = "150px", width = "300px", placeholder = NULL)
        ),
        insert_row_center(
          insert_contained_button(id = "reset_tweet_versions", label = "RESET"),
          insert_contained_button(id = "update_tweet_versions", label = "UPDATE")
        ),
        insert_text_block(
          insert_HTML_text("tweet-versions-before-2")
        ),
        insert_row_center(
          insert_plot_container(
            height = 600, ratio = 0.8,
            plot = insert_girafe("tweet_versions")
          )
        ),
        insert_row_center(
          insert_outlined_button("download_button", "DOWNLOAD PNG"),
          insert_outlined_button("download_button", "DOWNLOAD CSV")
        ),
        insert_text_block(
          insert_HTML("tweet_versions_after")
        ),
      )
    )
  })

  ##################################################
  # REACTIVE: chambers page - select chamber
  ##################################################
  
  observeEvent(input$chamber, {
    if(input$chamber == "House") {
      updateSelectInput(session, inputId = "select_member", choices = congress_data$member[congress_data$chamber == "House"])
    } else if(input$chamber == "Senate") {
      updateSelectInput(session, inputId = "select_member", choices = congress_data$member[congress_data$chamber == "Senate"])
    }
    reactive$chambers_page_selected_chamber <- input$chamber
  })
  
  ##################################################
  # REACTIVE: chambers page - select member on plot
  ##################################################
  
  observeEvent(input$select_member, {
    session$sendCustomMessage(type = "rank_ordering_set", message = input$select_member)
    reactive$chambers_page_selected_member <- input$select_member
  })
  
  observeEvent(input$rank_ordering_selected, {
    # session$sendCustomMessage(type = 'plot_set', message = character(0))
    reactive$chambers_page_selected_member <- input$rank_ordering_selected
    if(!is.null(input$rank_ordering_selected)) {
      updateSelectInput(session, inputId = "select_member", selected = input$rank_ordering_selected)
    }
  })
  
  ##################################################
  # REACTIVE: members page - select chamber
  ##################################################
  
  observeEvent(input$select_chamber_menu, {
    if(input$select_chamber_menu == "House") {
      updateSelectInput(session, inputId = "select_member_menu", choices = congress_data$member[congress_data$chamber == "House"])
    } else if(input$select_chamber_menu == "Senate") {
      updateSelectInput(session, inputId = "select_member_menu", choices = congress_data$member[congress_data$chamber == "Senate"])
    }
  })
  
  ##################################################
  # REACTIVE: tweets page - reset versions
  ##################################################
  
  observeEvent(input$reset_tweet_versions, {
    updateTextAreaInput(session, inputId = "text_version_A", value = reactive$tweets_page_tweet$raw)
    updateTextAreaInput(session, inputId = "text_version_B", value = reactive$tweets_page_tweet$raw)
  })
  
  ##################################################
  # REACTIVE: tweets page - update versions
  ##################################################
  
  observeEvent(input$update_tweet_versions, {
    version_A <- input$text_version_A
    version_B <- input$text_version_B
    version_A <- clean_tweet(version_A)
    version_B <- clean_tweet(version_B)
    version_A <- tweet_distribution(version_A, model)
    version_B <- tweet_distribution(version_B, model)
    version_A <- data.frame(score = version_A, version = "Version A")
    version_B <- data.frame(score = version_B, version = "Version B")
    reactive$tweets_page_version_A_data <- version_A
    reactive$tweets_page_version_B_data <- version_B
  })

  ####################################################################################################
  # HTML output
  ####################################################################################################
  
  ##################################################
  # HTML: chambers page HTML
  ##################################################
  
  output$chambers_page_HTML <- renderUI({
    reactive$chambers_page_HTML
  })
  
  ##################################################
  # HTML: members page HTML
  ##################################################
  
  output$members_page_HTML <- renderUI({
    reactive$members_page_HTML
  })
  
  ##################################################
  # HTML: tweets page HTML
  ##################################################
  
  output$tweets_page_HTML <- renderUI({
    reactive$tweets_page_HTML
  })
  
  ##################################################
  # HTML: multiple pages - member description
  ##################################################

  output$member_description_1 <- renderUI({
    insert_member_description(reactive$chambers_page_selected_member)
  })
  
  output$member_description_2 <- renderUI({
    insert_member_description(reactive$members_page_selected_member)
  })
  
  output$member_description_3 <- renderUI({
    insert_member_description(reactive$tweets_page_selected_member)
  })
  
  ##################################################
  # HTML: multiple pages - wikipedia intro
  ##################################################
  
  output$wikipedia_entry_1 <- renderUI({
    link <- congress_data$wikipedia_link[congress_data$member == reactive$members_page_selected_member]
    get_wikipedia_info(link)
  })
  
  output$wikipedia_entry_2 <- renderUI({
    link <- congress_data$wikipedia_link[congress_data$member == reactive$tweets_page_selected_member]
    get_wikipedia_info(link)
  })
  
  ##################################################
  # HTML: members page - distribution HTML
  ##################################################
  
  output$member_distribution_before <- renderUI({
    
    # get template
    html <- insert_HTML_text("member-distribution-before")

    # replace text
    html <- str_replace_all(html, "LAST", reactive$members_page_selected_member_data$last)
    
    # convet text to HTML
    html <- HTML(html)
    
    # return HTML
    return(html)
  })
  
  output$member_distribution_after <- renderUI({
    
    # get template
    html <- insert_HTML_text("member-distribution-after")
    
    # selected member
    selected_member <- reactive$members_page_selected_member
    
    # get member data
    selected_data <- reactive$members_page_selected_member_data

    # party name
    party_name <- ifelse(selected_data$party == "Democrat", "Democratic Party", "Republican Party")
    
    # consistency
    percentile <- selected_data$congress_percentile_SD
    consistency <- ""
    if(percentile >= 80) {
      consistency <- "very inconsistent"
    } else if(percentile >= 60 & percentile < 80) {
      consistency <- "moderately inconsistent"
    } else if(percentile >= 40 & percentile < 60) {
      consistency <- "neither consistent nor inconsistent"
    } else if(percentile >= 20 & percentile < 40) {
      consistency <- "moderately consistent"
    } else {
      consistency <- "very consistent"
    }
    
    # score
    score_SD <- str_pad(selected_data$score_SD, width = 4, side = "right", pad = "0")
    
    # clean percentiles
    selected_data$congress_percentile_SD <- to_ordinal(selected_data$congress_percentile_SD)
    selected_data$chamber_percentile_SD <- to_ordinal(selected_data$chamber_percentile_SD)
    selected_data$party_percentile_SD <- to_ordinal(selected_data$party_percentile_SD)
    selected_data$state_percentile_SD <- to_ordinal(selected_data$state_percentile_SD)
    
    # replace text
    html <- str_replace_all(html, "LAST", selected_data$last)
    html <- str_replace_all(html, "CONGRESS_PERCENTILE", selected_data$congress_percentile_SD)
    html <- str_replace_all(html, "CHAMBER_PERCENTILE", selected_data$chamber_percentile_SD)
    html <- str_replace_all(html, "PARTY_PERCENTILE", selected_data$party_percentile_SD)
    html <- str_replace_all(html, "STATE_PERCENTILE", selected_data$state_percentile_SD)
    html <- str_replace_all(html, "STATE", selected_data$state)
    html <- str_replace_all(html, "CHAMBER", selected_data$chamber)
    html <- str_replace_all(html, "PARTY_NAME", party_name)
    html <- str_replace_all(html, "CONSISTENCY", consistency)
    html <- str_replace_all(html, "SCORE_SD", score_SD)

    # convet text to HTML
    html <- HTML(html)
    
    # return HTML
    return(html)
  })
  
  ##################################################
  # HTML: members page - delegation HTML
  ##################################################
  
  output$delegation_plot_before <- renderUI({
    
    # get template
    html <- insert_HTML_text("delegation-plot-before")
    
    # selected member
    selected_member <- reactive$members_page_selected_member
    
    # get member data
    selected_data <- reactive$members_page_selected_member_data
    
    # replace text
    html <- str_replace_all(html, "LAST", selected_data$last)
    html <- str_replace_all(html, "STATE", selected_data$state)
    
    # convet text to HTML
    html <- HTML(html)
    
    # return HTML
    return(html)
  })
  
  output$delegation_plot_after <- renderUI({
    
    # get template
    html <- insert_HTML_text("delegation-plot-after")

    # replace text
    html <- str_replace_all(html, "LAST", reactive$members_page_selected_member_data$last)
    html <- str_replace_all(html, "STATE", reactive$members_page_selected_member_data$state)
    
    # convet text to HTML
    html <- HTML(html)
    
    # return HTML
    return(html)
  })
  
  output$delegation_maps_after <- renderUI({
    
    # get template
    html <- insert_HTML_text("delegation-maps-after")

    # replace text
    html <- str_replace_all(html, "LAST", reactive$members_page_selected_member_data$last)
    html <- str_replace_all(html, "STATE", reactive$members_page_selected_member_data$state)
    
    # convet text to HTML
    html <- HTML(html)
    
    # return HTML
    return(html)
  })

  ##################################################
  # HTML: members page - timeline HTML
  ##################################################
  
  output$timeline_plot_before <- renderUI({
    
    # get template
    html <- insert_HTML_text("timeline-plot-before")

    # replace text
    html <- str_replace_all(html, "LAST", reactive$members_page_selected_member_data$last)
    
    # convet text to HTML
    html <- HTML(html)
    
    # return HTML
    return(html)
  })
  
  output$timeline_plot_after <- renderUI({
    
    # get template
    html <- insert_HTML_text("timeline-plot-after")
    
    # selected member
    selected_member <- reactive$members_page_selected_member
    
    # get member data
    selected_data <- reactive$members_page_selected_member_data

    # change
    change <- "is constant over time"
    if(selected_data$slope > 0) {
      change <- str_c("increases by approximately", abs(selected_data$slope), "points per month", sep = " ")
    } else if(selected_data$slope < 0) {
      change <- str_c("decreases by approximately", abs(selected_data$slope), "points per month", sep = " ")
    }
    
    # magnitude
    magnitude <- ""
    if(selected_data$slope_percentile >= 80 & selected_data$slope_percentile < 100) {
      magnitude <- "very large"
    } else if(selected_data$slope_percentile >= 60 & selected_data$slope_percentile < 80) {
      magnitude <- "large"
    } else if(selected_data$slope_percentile >= 40 & selected_data$slope_percentile < 60) {
      magnitude <- "average"
    } else if(selected_data$slope_percentile >= 20 & selected_data$slope_percentile < 40) {
      magnitude <- "small"
    } else if(selected_data$slope_percentile >= 0 & selected_data$slope_percentile < 20) {
      magnitude <- "very small"
    }
    
    # replace text
    html <- str_replace_all(html, "LAST", selected_data$last)
    html <- str_replace_all(html, "CHANGE", change)
    html <- str_replace_all(html, "MAGNITUDE", magnitude)
    html <- str_replace_all(html, "PERCENTILE", to_ordinal(selected_data$slope_percentile))
    
    # convet text to HTML
    html <- HTML(html)
    
    # return HTML
    return(html)
  })
  
  ##################################################
  # HTML: tweets page - text of the tweet
  ##################################################
  
  output$tweet_text <- renderUI({
    author <- reactive$tweets_page_selected_member
    if(is.null(author)) {
      author <- "Unknown"
    }
    HTML(str_c("<p>", reactive$tweets_page_tweet$raw, "</p><p><b>— ", author, "</b></p>", collapse = ""))
  })
  
  ##################################################
  # HTML: tweet page - distribution HTML
  ##################################################
  
  output$tweet_distribution_plot_after <- renderUI({
    
    # get template
    html <- insert_HTML_text("tweet-distribution-plot-after")
    
    # selected member
    selected_member <- reactive$tweets_page_selected_member
    
    # get member data
    selected_data <- congress_data[congress_data$member == selected_member,]
    
    # standard deviation
    sd <- sd(reactive$tweets_page_distribution)
    
    # partisanship
    distance <- abs(reactive$tweets_page_score - mean(tweet_data$score))
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
    
    # confidence
    confidence <- ""
    if(sd >= 0.15 & sd < 0.5) {
      confidence <- "low"
    } else if(sd >= 0.05 & sd < 0.15) {
      confidence <- "medium"
    } else if(sd >= 0 & sd < 0.05) {
      confidence <- "high"
    }
    
    # clean scores
    tweet_score <- str_pad(round(reactive$tweets_page_score, 2), width = 4, side = "right", pad = "0")
    
    # replace text
    html <- str_replace_all(html, "LAST", selected_data$last)
    html <- str_replace_all(html, "SCORE", tweet_score)
    html <- str_replace_all(html, "CONFIDENCE", confidence)
    html <- str_replace_all(html, "PARTISANSHIP", partisanship)
    
    # convet text to HTML
    html <- HTML(html)
    
    # return HTML
    return(html)
  })
  
  ##################################################
  # HTML: tweets page - tweet table
  ##################################################
  
  output$tweet_table <- renderUI({
    if(!is.null(reactive$tweets_page_tweet)) {
      out <- calculate_word_scores(reactive$tweets_page_tweet, model)
      make_tweet_table(out$word, out$word_score)
    }
  })
  
  ##################################################
  # HTML: tweet page - versions HTML
  ##################################################
  
  output$tweet_versions_before <- renderUI({
    
    # get template
    html <- insert_HTML_text("tweet-versions-before")
    
    # selected member
    selected_member <- reactive$tweets_page_selected_member
    
    # get member data
    selected_data <- congress_data[congress_data$member == selected_member,]
    
    # replace text
    html <- str_replace_all(html, "LAST", selected_data$last)
    
    # convet text to HTML
    html <- HTML(html)
    
    # return HTML
    return(html)
  })
  
  output$tweet_versions_after <- renderUI({
    
    # get template
    html <- insert_HTML_text("tweet-versions-after")
    
    # means
    mean_version_A <- round(mean(reactive$tweets_page_version_A_data$score), 2)
    mean_version_B <- round(mean(reactive$tweets_page_version_B_data$score), 2)

    # clean scores
    score_A <- str_pad(mean_version_A, width = 4, side = "right", pad = "0")
    score_B <- str_pad(mean_version_B, width = 4, side = "right", pad = "0")
    
    # direction
    direction <- ""
    if(mean_version_B > mean_version_A) {
      direction <- "higher"
    } else if(mean_version_B < mean_version_A) {
      direction <- "lower"
    }
    
    # interpretation
    if(mean_version_B < 0.5 & mean_version_A < 0.5 & mean_version_B > mean_version_A) {
      interpretation <- "both tweets are partisan in a Democratic direction and version B is less partisan than version A."
    } else if(mean_version_B < 0.5 & mean_version_A < 0.5 & mean_version_B < mean_version_A) {
      interpretation <- "both tweets are partisan in a Democratic direction and version B is more partisan than version A."
    } else if(mean_version_B > 0.5 & mean_version_A > 0.5 & mean_version_B > mean_version_A) {
      interpretation <- "both tweets are partisan in a Republican direction and version B is more partisan than version A."
    } else if(mean_version_B > 0.5 & mean_version_A > 0.5 & mean_version_B < mean_version_A) {
      interpretation <- "both tweets are partisan in a Republican direction and version B is less partisan than version A."
    } else if(mean_version_B > 0.5 & mean_version_A < 0.5) {
      interpretation <- "version A is partisan in a Democratic direction and version B is partisan in a Republican direction."
    } else if(mean_version_B < 0.5 & mean_version_A > 0.5) {
      interpretation <- "version A is partisan in a Republican direction and version B is partisan in a Democratic direction."
    } else {
      interpretation <- "ERROR"
    }
    
    # replace text
    html <- str_replace_all(html, "DIRECTION", direction)
    html <- str_replace_all(html, "INTERPRETATION", interpretation)
    html <- str_replace_all(html, "SCORE_A", score_A)
    html <- str_replace_all(html, "SCORE_B", score_B)
    
    # convet text to HTML
    html <- HTML(html)
    
    # return HTML
    return(html)
  })
  
  ##################################################
  # HTML: tweet page - member distribution HTML
  ##################################################
  
  output$tweet_member_distribution_before <- renderUI({
    
    # get template
    html <- insert_HTML_text("tweet-member-distribution-before")
    
    # selected member
    selected_member <- reactive$tweets_page_selected_member
    
    # get member data
    selected_data <- congress_data[congress_data$member == selected_member,]
    
    # replace text
    html <- str_replace_all(html, "LAST", selected_data$last)
    
    # convet text to HTML
    html <- HTML(html)
    
    # return HTML
    return(html)
  })
  
  output$tweet_member_distribution_after <- renderUI({
    
    # get template
    html <- insert_HTML_text("tweet-member-distribution-after")
    
    # selected member
    selected_member <- reactive$tweets_page_selected_member
    
    # get member data
    selected_data <- congress_data[congress_data$member == selected_member,]
    
    # clean scores
    tweet_score <- str_pad(round(reactive$tweets_page_score, 2), width = 4, side = "right", pad = "0")
    member_score <- str_pad(selected_data$score, width = 4, side = "right", pad = "0")
    
    # replace text
    html <- str_replace_all(html, "LAST", selected_data$last)
    html <- str_replace_all(html, "TWEET_SCORE", tweet_score)
    html <- str_replace_all(html, "MEMBER_SCORE", member_score)
    
    # convet text to HTML
    html <- HTML(html)
    
    # return HTML
    return(html)
  })
  
  ####################################################################################################
  # plots
  ####################################################################################################
  
  ##################################################
  # PLOT: house chamber plot
  ##################################################
  
  output$house_chamber_plot <- renderGirafe({
    plot_data <- filter(congress_data, chamber == "House")
    insert_chamber_plot(plot_data, rows = 7, blue = blue, red = red)
  })
  
  ##################################################
  # PLOT: senate chamber plot
  ##################################################
  
  output$senate_chamber_plot <- renderGirafe({
    plot_data <- filter(congress_data, chamber == "Senate")
    insert_chamber_plot(plot_data, rows = 4, blue = blue, red = red)
  })

  ##################################################
  # PLOT: senate map
  ##################################################
  
  output$senate_map <- renderGirafe({
    
    # get data
    plot_data <- congress_data %>%
      filter(chamber == "Senate" & !is.na(score)) %>%
      group_by(state) %>%
      arrange(last) %>%
      summarize(
        mean_score = round(mean(score, na.rm = TRUE), 2),
        score_1 = round(score[1], 2),
        score_2 = round(score[2], 2), 
        senator_1 = member[1],
        senator_2 = member[2],
        last_1 = last[1],
        last_2 = last[2],
        party_short_1 = party_short[1],
        party_short_2 = party_short[2],
        district_1 = district[1],
        district_2 = district[2],
        count = n()
      )
    
    # make fill variable
    plot_data$score_fill <- cut(plot_data$mean_score, seq(0, 1, 0.1))
    
    # format score
    plot_data$mean_score <- str_pad(plot_data$mean_score, width = 4, side = "right", pad = "0")
    plot_data$score_1 <- str_pad(plot_data$score_1, width = 4, side = "right", pad = "0")
    plot_data$score_2 <- str_pad(plot_data$score_2, width = 4, side = "right", pad = "0")
    
    # make tooltip
    plot_data$tooltip <- str_c(
      "<div class=plot-tooltip-score>", plot_data$mean_score, "</div>", 
      "<div class=plot-tooltip-member>", plot_data$state, "</div>", 
      "<div class=plot-tooltip-info>", "(Mean)", "</div>",
      "<div style=\"height:15px;\"></div>",
      "<div class=plot-tooltip-score>", plot_data$score_1, "</div>", 
      "<div class=plot-tooltip-member>", plot_data$last_1, "</div>", 
      "<div class=plot-tooltip-info>", "(", plot_data$party_short_1, "-", plot_data$district_1, ")", "</div>",
      "<div style=\"height:15px;\"></div>",
      "<div class=plot-tooltip-score>", plot_data$score_2, "</div>", 
      "<div class=plot-tooltip-member>", plot_data$last_2, "</div>", 
      "<div class=plot-tooltip-info>", "(", plot_data$party_short_2, "-", plot_data$district_2, ")", "</div>"
    )
    
    plot <- make_US_map(plot_data, tooltip = "tooltip", fill = "score_fill") 
    
    reactive$DATA_senate_map <- select(plot_data, state, mean_score, senator_1, score_1, senator_2, score_2)
    reactive$PLOT_senate_map <- plot
    
    plot %>% make_interactive(width = 10, height = 7)
  })
  
  ##################################################
  # PLOT: house map
  ##################################################
  
  output$house_map <- renderGirafe({
    
    # get data
    plot_data <- congress_data %>%
      filter(chamber == "House") %>%
      group_by(state) %>%
      summarize(
        mean_score = round(mean(score, na.rm = TRUE), 2),
        count = n()
      )
    
    # make fill variable
    plot_data$mean_score_fill <- cut(plot_data$mean_score, seq(0, 1, 0.1))
    
    # format score
    plot_data$mean_score <- str_pad(plot_data$mean_score, width = 4, side = "right", pad = "0")
    plot_data$mean_score[str_detect(plot_data$mean_score, "NaN")] <- "NA"
    
    # make tooltip
    plot_data$tooltip <- str_c(
      "<div class=plot-tooltip-score>", plot_data$mean_score, "</div>", 
      "<div class=plot-tooltip-member>", plot_data$state, "</div>",
      "<div class=plot-tooltip-info>", plot_data$count, ifelse(plot_data$count == 1, " member", " members"), "</div>"
    )
    
    # make map
    plot <- make_US_map(plot_data, fill = "mean_score_fill", tooltip = "tooltip")
    
    # save output
    reactive$DATA_house_map <- select(plot_data, state, mean_score, count)
    reactive$PLOT_house_map <- plot
    
    # make interactive
    plot %>% make_interactive(width = 10, height = 7)
  })
  
  ##################################################
  # PLOT: rank ordering
  ##################################################
  
  output$rank_ordering <- renderGirafe({
    
    # get data that depends on the selected chamber
    plot_data <- NULL
    breaks <- NULL
    colors <- NULL
    if(reactive$chambers_page_selected_chamber == "House") {
      plot_data <- filter(congress_data, chamber == "House")
      breaks <- seq(0, 460, 25)
      colors <- c("#2D8CFF", "#FF4242")
    } else if(reactive$chambers_page_selected_chamber == "Senate") {
      plot_data <- filter(congress_data, chamber == "Senate")
      breaks <- seq(0, 110, 10)
      colors <- c("#2D8CFF", "#BABACC", "#FF4242")
    }
    
    # order factor
    plot_data$member <- factor(plot_data$member, levels = plot_data$member[order(plot_data$score)])
    
    # tooltip
    plot_data$tooltip <- str_c(
      "<div class=plot-tooltip-score>", round(plot_data$score, 2), "</div>", 
      "<div class=plot-tooltip-member>", plot_data$last, "</div>", 
      "<div class=plot-tooltip-info>", "(", plot_data$party_short, "-", plot_data$district, ")", "</div>"
    )
    
    # set seed
    set.seed(12345)
    
    # plot positions
    plot <- ggplot(plot_data) +
      geom_point_interactive(aes(x = score, y = as.numeric(member), color = party, tooltip = tooltip, data_id = member), position = position_jitter(width = 0.01, height = 0), alpha = 0.5, size = 3, na.rm = TRUE) +
      scale_color_manual(name = NULL, values = colors) +
      scale_x_continuous(breaks = seq(0, 1, 0.1)) +
      scale_y_continuous(breaks = breaks, expand = expansion(0.05, 0)) +
      my_titles(x = "CSM-M Score", y = "Member") +
      my_theme(background_color = "#FAFAFA") + 
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank()
      )

    # party means
    if(input$show_party_means) {
      plot <- plot +
        geom_vline(xintercept = mean(plot_data$score[plot_data$party == "Democrat"], na.rm = TRUE), color = blue, linetype = "dashed", size = 0.5) +
        geom_vline(xintercept = mean(plot_data$score[plot_data$party == "Republican"], na.rm = TRUE), color = red, linetype = "dashed", size = 0.5)
    }

    # sample mean
    if(input$show_sample_mean) {
      plot <- plot +
        geom_vline(xintercept = mean(plot_data$score, na.rm = TRUE), color = "#414155", linetype = "solid", size = 0.5)
    }
    
    # convert to D3
    plot <- girafe(print(plot), width_svg = 10, height_svg = 8,
           options = list(
             opts_hover(css = "r:9;"),
             opts_selection(type = "single", css = "fill:#414155;stroke:#414155;r:11;opacity:1;"),
             opts_tooltip(css = tooltip_css, offx = 20, offy = 20, opacity = 1, delay_mouseover = 100, delay_mouseout = 100), 
             opts_toolbar(saveaspng = FALSE), 
             opts_sizing(rescale = TRUE, width = 1)
           )
    )
    
    # return
    return(plot)
  })

  ##################################################
  # PLOT: chamber map
  ##################################################
  
  output$chamber_map <- renderGirafe({
    
    if(input$chamber == "House") {
      
      # get data
      plot_data <- congress_data %>%
        filter(chamber == "House") %>%
        group_by(state) %>%
        summarize(
          mean_score = round(mean(score, na.rm = TRUE), 2),
          count = n()
        )
      
      # make fill variable
      plot_data$mean_score_fill <- cut(plot_data$mean_score, seq(0, 1, 0.1))
      
      # format score
      plot_data$mean_score <- str_pad(plot_data$mean_score, width = 4, side = "right", pad = "0")
      plot_data$mean_score[str_detect(plot_data$mean_score, "NaN")] <- "NA"
      
      # make tooltip
      plot_data$tooltip <- str_c(
        "<div class=plot-tooltip-score>", plot_data$mean_score, "</div>", 
        "<div class=plot-tooltip-member>", plot_data$state, "</div>", 
        "<div class=plot-tooltip-info>", plot_data$count, ifelse(plot_data$count == 1, " member", " members"), "</div>"
      )
      
      # make map
      make_US_map(plot_data, fill = "mean_score_fill", tooltip = "tooltip") %>% make_interactive(width = 10, height = 7)
    } else {
      # get data
      plot_data <- congress_data %>%
        filter(chamber == "Senate" & !is.na(score)) %>%
        group_by(state) %>%
        arrange(last) %>%
        summarize(
          mean_score = round(mean(score, na.rm = TRUE), 2),
          score_1 = round(score[1], 2),
          score_2 = round(score[2], 2), 
          senator_1 = member[1],
          senator_2 = member[2],
          last_1 = last[1],
          last_2 = last[2],
          party_short_1 = party_short[1],
          party_short_2 = party_short[2],
          district_1 = district[1],
          district_2 = district[2],
          count = n()
        )
      
      # make fill variable
      plot_data$score_fill <- cut(plot_data$mean_score, seq(0, 1, 0.1))
      
      # format score
      plot_data$mean_score <- str_pad(plot_data$mean_score, width = 4, side = "right", pad = "0")
      plot_data$score_1 <- str_pad(plot_data$score_1, width = 4, side = "right", pad = "0")
      plot_data$score_2 <- str_pad(plot_data$score_2, width = 4, side = "right", pad = "0")
      
      # make tooltip
      plot_data$tooltip <- str_c(
        "<div class=plot-tooltip-score>", plot_data$mean_score, "</div>", 
        "<div class=plot-tooltip-member>", plot_data$state, "</div>", 
        "<div class=plot-tooltip-info>", "(Mean)", "</div>",
        "<div style=\"height:15px;\"></div>",
        "<div class=plot-tooltip-score>", plot_data$score_1, "</div>", 
        "<div class=plot-tooltip-member>", plot_data$last_1, "</div>", 
        "<div class=plot-tooltip-info>", "(", plot_data$party_short_1, "-", plot_data$district_1, ")", "</div>",
        "<div style=\"height:15px;\"></div>",
        "<div class=plot-tooltip-score>", plot_data$score_2, "</div>", 
        "<div class=plot-tooltip-member>", plot_data$last_2, "</div>", 
        "<div class=plot-tooltip-info>", "(", plot_data$party_short_2, "-", plot_data$district_2, ")", "</div>"
      )
      
      make_US_map(plot_data, tooltip = "tooltip", fill = "score_fill") %>% make_interactive(width = 10, height = 8)
    }
  })
  
  ##################################################
  # PLOT: party distributions
  ##################################################
  
  output$party_distributions <- renderGirafe({
    
    # make plot data
    plot_data <- NULL
    if(reactive$chambers_page_selected_chamber == "House") {
      plot_data <- filter(congress_data, chamber == "House")
    } else if(reactive$chambers_page_selected_chamber == "Senate") {
      plot_data <- filter(congress_data, chamber == "Senate" & party != "Independent")
    }
    
    # calculate mean
    plot_data <- plot_data %>% group_by(party) %>% mutate(mean_score = mean(score, na.rm = TRUE), count = n())
    
    # tooltip
    plot_data$tooltip <- str_c(
      "<div class=plot-tooltip-score>", round(plot_data$mean_score, 2), "</div>",
      "<div class=plot-tooltip-member>", plot_data$party, " mean</div>",
      "<div class=plot-tooltip-info>", plot_data$count, " members</div>"
    )

    # plot positions
    plot <- ggplot(plot_data) +
      geom_density_interactive(aes(x = score, color = party, fill = party, tooltip = tooltip, data_id = party), alpha = 0.3, size = 0.5, na.rm = TRUE) +
      scale_color_manual(name = NULL, values = c("#2D8CFF", "#FF4242"), guide = FALSE) +
      scale_fill_manual(name = NULL, values = c("#2D8CFF", "#FF4242"), guide = FALSE) +
      scale_x_continuous(breaks = seq(0, 1, 0.1)) +
      scale_y_continuous(expand = expansion(0.05, 0)) +
      my_titles(x = "CSM-M Score", y = "Density") +
      my_theme(background_color = "#FAFAFA") +
      theme(
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
      )
    
    if(input$chamber == "House" & input$distribution_party_means) {
      plot <- plot + 
        geom_vline_interactive(xintercept = mean(congress_data$score[congress_data$chamber == "House" & congress_data$party == "Democrat"], na.rm = TRUE), color = "#2D8CFF", linetype = "dashed", size = 0.5) + 
        geom_vline_interactive(xintercept = mean(congress_data$score[congress_data$chamber == "House" & congress_data$party == "Republican"], na.rm = TRUE), color = "#FF4242", linetype = "dashed", size = 0.5)
    }
    
    if(input$chamber == "Senate" & input$distribution_party_means) {
      plot <- plot + 
        geom_vline_interactive(xintercept = mean(congress_data$score[congress_data$chamber == "Senate" & congress_data$party == "Democrat"], na.rm = TRUE), color = "#2D8CFF", linetype = "dashed", size = 0.5) + 
        geom_vline_interactive(xintercept = mean(congress_data$score[congress_data$chamber == "Senate" & congress_data$party == "Republican"], na.rm = TRUE), color = "#FF4242", linetype = "dashed", size = 0.5)
    }
    
    if(input$chamber == "House" & input$distribution_sample_mean) {
      plot <- plot + 
        geom_vline(xintercept = mean(congress_data$score[congress_data$chamber == "House"], na.rm = TRUE), color = "#414155", linetype = "solid", size = 0.5)
    }
    
    if(input$chamber == "Senate" & input$distribution_sample_mean) {
      plot <- plot + 
        geom_vline(xintercept = mean(congress_data$score[congress_data$chamber == "Senate"], na.rm = TRUE), color = "#414155", linetype = "solid", size = 0.5)
    }

    # convert to D3
    girafe(print(plot), width_svg = 10, height_svg = 8,
           options = list(
             opts_hover(css = "stroke-width:2;"),
             opts_selection(type = "none"),
             opts_tooltip(css = tooltip_css, offx = 20, offy = 20, opacity = 1, delay_mouseover = 100, delay_mouseout = 100), 
             opts_toolbar(saveaspng = FALSE), 
             opts_sizing(rescale = TRUE, width = 1)
           )
    )
  })
  
  ##################################################
  # PLOT: tweet score
  ##################################################
  
  output$tweet_distribution <- renderGirafe({
    
    # tweet distribution data
    plot_data <- data.frame(score = reactive$tweets_page_distribution)
    
    # format scores
    tweet_score <- str_pad(round(mean(plot_data$score), 2), width = 4, side = "right", pad = "0")
    
    # tooltips
    plot_data$tooltip <- str_c(
      "<div class=plot-tooltip-score>", tweet_score, "</div>",
      "<div class=plot-tooltip-member>", "This tweet", "</div>",
      "<div class=plot-tooltip-info>", "100 iterations", "</div>"
    )

    # make plot
    plot <- ggplot(filter(plot_data)) +
      geom_density_interactive(aes(score, tooltip = tooltip, data_id = tooltip), alpha = 0.3, size = 0.5, color = "#C182D1", fill = "#C182D1") +
      geom_vline(xintercept = mean(plot_data$score), linetype = "solid", size = 0.5, color = "#C182D1") +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
      scale_color_manual(values = c("#C182D1", "#51C5F7"), guide = FALSE) + 
      scale_fill_manual(values = c("#C182D1", "#51C5F7"), guide = FALSE) + 
      my_theme() +
      my_titles(x = "CSM-T Score", y = "Density") +
      theme(
        panel.grid.major.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
      )
    
    # convert to D3
    plot_interactive <- girafe(
      print(plot), width_svg = 10, height_svg = 8,
      options = list(
        opts_hover(css = "stroke-width:2;"),
        opts_selection(type = "none"),
        opts_tooltip(css = tooltip_css, offx = 20, offy = 20, opacity = 1, delay_mouseover = 100, delay_mouseout = 100), 
        opts_toolbar(saveaspng = FALSE), 
        opts_sizing(rescale = TRUE, width = 1)
      )
    )
    
    # return
    return(plot_interactive)
  })
  
  ##################################################
  # PLOT: tweet member distribution
  ##################################################
  
  output$tweet_member_distribution <- renderGirafe({
    
    # selected member
    selected_member <- reactive$tweets_page_selected_member
    
    # member distribution data
    plot_data_A <- filter(tweet_data, member == selected_member)
    
    # tweet distribution data
    plot_data_B <- data.frame(score = reactive$tweets_page_distribution)

    # format scores
    member_score <- str_pad(round(mean(plot_data_A$score), 2), width = 4, side = "right", pad = "0")
    tweet_score <- str_pad(round(mean(plot_data_B$score), 2), width = 4, side = "right", pad = "0")
    
    # tooltips
    plot_data_A$tooltip <- str_c(
      "<div class=plot-tooltip-score>", member_score, "</div>",
      "<div class=plot-tooltip-member>", plot_data_A$last, "</div>",
      "<div class=plot-tooltip-info>", "(", plot_data_A$party_short, "-", plot_data_A$district, ")", "</div>"
    )
    plot_data_B$tooltip <- str_c(
      "<div class=plot-tooltip-score>", tweet_score, "</div>",
      "<div class=plot-tooltip-member>", "This tweet", "</div>",
      "<div class=plot-tooltip-info>", "100 iterations", "</div>"
    )
    
    # finish cleaning data    
    plot_data_A <- select(plot_data_A, score, tooltip)
    plot_data_A$group <- "member"
    plot_data_B$group <- "tweet"
    
    # stack data
    plot_data <- rbind(plot_data_A, plot_data_B)
    
    # make plot
    plot <- ggplot(filter(plot_data)) +
      geom_density_interactive(aes(score, group = group, fill = group, color = group, tooltip = tooltip, data_id = tooltip), alpha = 0.3, size = 0.5) +
      geom_vline(xintercept = congress_data$score[congress_data$member == selected_member], linetype = "solid", size = 0.5, color = "#C182D1") +
      geom_vline(xintercept = mean(plot_data_B$score), linetype = "solid", size = 0.5, color = "#51C5F7") +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
      scale_color_manual(values = c("#C182D1", "#51C5F7"), guide = FALSE) + 
      scale_fill_manual(values = c("#C182D1", "#51C5F7"), guide = FALSE) + 
      my_theme() +
      my_titles(x = "CSM Score", y = "Density") +
      theme(
        panel.grid.major.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
      )
    
    # convert to D3
    plot_interactive <- girafe(
      print(plot), width_svg = 10, height_svg = 8,
      options = list(
        opts_hover(css = "stroke-width:2;"),
        opts_selection(type = "none"),
        opts_tooltip(css = tooltip_css, offx = 20, offy = 20, opacity = 1, delay_mouseover = 100, delay_mouseout = 100), 
        opts_toolbar(saveaspng = FALSE), 
        opts_sizing(rescale = TRUE, width = 1)
      )
    )
    
    # return
    return(plot_interactive)
  })
  
  ##################################################
  # PLOT: tweet versions
  ##################################################
  
  output$tweet_versions <- renderGirafe({
    
    # make plot data
    plot_data <- rbind(reactive$tweets_page_version_A_data, reactive$tweets_page_version_B_data)
    
    # format scores
    plot_data <- plot_data %>% 
      group_by(version) %>% 
      mutate(
        formatted_score = str_pad(round(mean(score), 2), width = 4, side = "right", pad = "0")
      )

    # tooltips
    plot_data$tooltip <- str_c(
      "<div class=plot-tooltip-score>", plot_data$formatted_score, "</div>",
      "<div class=plot-tooltip-member>", plot_data$version, "</div>",
      "<div class=plot-tooltip-info>", "100 iterations", "</div>"
    )
    
    # make plot
    plot <- ggplot(plot_data) +
      geom_density_interactive(aes(x = score, group = version, color = version, fill = version, tooltip = tooltip, data_id = tooltip), alpha = 0.2) +
      geom_vline(xintercept = mean(plot_data$score[plot_data$version == "Version A"], na.rm = TRUE), color = "#C182D1", linetype = "solid", size = 0.5) +
      geom_vline(xintercept = mean(plot_data$score[plot_data$version == "Version B"], na.rm = TRUE), color = "#51C5F7", linetype = "solid", size = 0.5) +
      scale_color_manual(values = c("#C182D1", "#51C5F7"), guide = FALSE) + 
      scale_fill_manual(values = c("#C182D1", "#51C5F7"), guide = FALSE) + 
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
      my_theme(background_color = "#FAFAFA") +
      my_titles(x = "CSM-T Score", y = "Density") +
      theme(
        panel.grid.major.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
      )
    
    # convert to D3
    interactive_plot <- girafe(
      print(plot), width_svg = 10, height_svg = 8,
      options = list(
        opts_hover(css = "stroke-width:2;"),
        opts_selection(type = "none"),
        opts_tooltip(css = tooltip_css, offx = 20, offy = 20, opacity = 1, delay_mouseover = 100, delay_mouseout = 100), 
        opts_toolbar(saveaspng = FALSE), 
        opts_sizing(rescale = TRUE, width = 1)
      )
    )
    
    # return
    return(interactive_plot)
  })
  
  ##################################################
  # PLOT: member distribution
  ##################################################

  output$member_distribution <- renderGirafe({

    # selected member
    selected_member <- reactive$members_page_selected_member
    
    # get plot data
    plot_data <- filter(tweet_data, member == selected_member)

    # make group
    plot_data$higher <- ceiling(plot_data$score * 20) / 20
    plot_data$lower <- plot_data$higher - 0.05
    plot_data$middle <- plot_data$lower + 0.025
    
    # number of tweets in each bin
    plot_data <- plot_data %>% group_by(higher) %>% summarize(count = n(), lower = lower[1], middle = middle[1])
    
    # complete range
    x <- data.frame(id = 1:20, higher = seq(0.05, 1, 0.05))
    x$higher <- as.character(x$higher)
    plot_data$higher <- as.character(plot_data$higher)
    plot_data <- left_join(x, plot_data, by = "higher")
    
    # format
    plot_data$higher <- str_pad(plot_data$higher, width = 4, side = "right", pad = "0")
    plot_data$lower <- str_pad(plot_data$lower, width = 4, side = "right", pad = "0")
    
    # percent
    plot_data$percent <- round(plot_data$count / sum(plot_data$count, na.rm = TRUE) * 100)

    # clean format
    plot_data[plot_data == "0000"] <- "0.00"
    plot_data[plot_data == "1000"] <- "1.00"
    
    # tooltip
    plot_data$tooltip <- str_c(
      "<div class=plot-tooltip-score>", plot_data$percent, "%</div>",
      "<div class=plot-tooltip-member>Scores between ", plot_data$lower, " and ", plot_data$higher, "</div>",
      "<div class=plot-tooltip-info>", plot_data$count, ifelse(plot_data$count == 1, " tweet", " tweets"), "</div>"
    )

    # color
    color <- ifelse(congress_data$party[congress_data$member == selected_member] == "Democrat", blue, red)
    
    # plot positions
    plot <- ggplot(plot_data) +
      geom_bar_interactive(stat = "identity", aes(x = factor(higher), y = count, tooltip = tooltip, data_id = tooltip), fill = color, color = color, alpha = 0.3, size = 0.5, width = 1, na.rm = TRUE) +
      scale_y_continuous(expand = c(0.01, 0.01)) +
      my_titles(x = "CSM-T Score", y = "Number of Tweets") +
      my_theme(background_color = "#FAFAFA") +
      theme(
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()
      )

    # convert to D3
    interactive_plot <- girafe(
      print(plot), width_svg = 10, height_svg = 8,
      options = list(
        opts_hover(css = "stroke-width:2;fill-opacity:0.5;"),
        opts_selection(type = "none"),
        opts_tooltip(css = tooltip_css, offx = 20, offy = 20, opacity = 1, delay_mouseover = 100, delay_mouseout = 100),
        opts_toolbar(saveaspng = FALSE),
        opts_sizing(rescale = TRUE, width = 1)
      )
    )
    
    # return
    return(interactive_plot)
  })
 
  ##################################################
  # PLOT: member timeline
  ##################################################
  
  output$member_timeline <- renderGirafe({

    # selected member
    selected_member <- reactive$members_page_selected_member
    
    # party
    party <- congress_data$party[congress_data$member == selected_member]
    
    # plot data
    plot_data <- filter(tweet_data, member == selected_member)
    # plot_data <- filter(tweet_data, label == selected)
    plot_data$month_label <- plot_data$month
    plot_data$month_label <- str_replace(plot_data$month_label, "([0-9]+)-([0-9]+)", "\\2-\\1")
    
    plot_data$month <- as.Date(str_c(plot_data$month, "-1"))
    plot_data <- plot_data %>% group_by(month) %>% summarize(mean_score = mean(score), count = n(), month_label = month_label[1])
    
    # drop last month
    plot_data <- filter(plot_data, month_label != "01-2019")
    
    # format score
    scores <- str_pad(round(plot_data$mean_score, 2), width = 4, side = "right", pad = "0")
    
    # tooltip
    plot_data$tooltip <- str_c(
      "<div class=plot-tooltip-score>", scores, "</div>",
      "<div class=plot-tooltip-member>Month: ", plot_data$month_label, "</div>",
      "<div class=plot-tooltip-info>", plot_data$count, " tweets</div>"
    )

    # make categories
    plot_data$cut <- cut(plot_data$mean_score, breaks = seq(0, 1, 0.1), labels = FALSE)
    used_colors <- unique(plot_data$cut)
    used_colors <- used_colors[order(used_colors)]
    used_colors <- used_colors[used_colors <= 10]
    colors <- palette[used_colors]
    plot_data$cut <- as.factor(plot_data$cut)
    
    # plot positions
    plot <- ggplot(plot_data) +
      geom_line(aes(x = month, y = mean_score), color = "#262626", size = 0.5) +
      geom_smooth(method = "lm", formula = y ~ x, aes(x = month, y = mean_score), linetype = "dashed", color = "#262626", size = 0.5, alpha = 0.2, se = FALSE) +
      geom_point_interactive(aes(x = month, y = mean_score, fill = cut, tooltip = tooltip, data_id = month), color = "#262626", size = 4, pch = 21, stroke = 1) +
      scale_fill_manual(values = colors, guide = FALSE) +
      scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
      scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months") +
      my_titles(x = "Month", y = "CSM-MM Score") +
      my_theme(background_color = "#FAFAFA")

    # convert to D3
    plot <- girafe(print(plot), width_svg = 10, height_svg = 8,
           options = list(
             opts_hover(css = "r:8;"),
             opts_selection(type = "none"),
             opts_tooltip(css = tooltip_css, offx = 20, offy = 20, opacity = 1, delay_mouseover = 100, delay_mouseout = 100),
             opts_toolbar(saveaspng = FALSE),
             opts_sizing(rescale = TRUE, width = 1)
           )
    )
    
    plot
  })
  
  ##################################################
  # PLOT: congressional delegation
  ##################################################
  
  output$delegation <- renderGirafe({

    # selected member
    selected_member <- reactive$members_page_selected_member
    
    # selected state
    selected_state <- str_extract(selected_member, "-[A-Z]{2}")
    selected_state <- str_extract(selected_state, "[A-Z]{2}")
    
    # make plot data
    plot_data <- filter(congress_data, state_short == selected_state)
    plot_data <- na.omit(plot_data)

    # calculate y value
    plot_data$y <- 1:nrow(plot_data)
    plot_data$y <- (plot_data$y - min(plot_data$y)) / (max(plot_data$y) - min(plot_data$y))
    
    # make categories
    plot_data$cut <- cut(plot_data$score, breaks = seq(0, 1, 0.1), labels = FALSE)
    plot_data$cut[plot_data$member == selected_member] <- 98
    used_colors <- unique(plot_data$cut)
    used_colors <- used_colors[order(used_colors)]
    used_colors <- used_colors[used_colors <= 10]
    colors <- c(palette[used_colors], "#414155")
    plot_data$cut <- as.factor(plot_data$cut)
    
    # tooltip
    plot_data$tooltip <- str_c(
      "<div class=plot-tooltip-score>", round(plot_data$score, 2), "</div>", 
      "<div class=plot-tooltip-member>", plot_data$last, "</div>", 
      "<div class=plot-tooltip-info>", "(", plot_data$party_short, "-", plot_data$district, ")", "</div>"
    )

    # plot_data$difference <- abs(plot_data$score - 0.5)
    # yintercept <- plot_data$y[which(plot_data$difference == min(plot_data$difference))]
    
    plot <- ggplot(plot_data) + 
      geom_segment(aes(x = 0, xend = 1, y = 0.5, yend = 0.5), color = "#262626", size = 0.5) +
      geom_point_interactive(aes(x = score, y = y, fill = cut, size = chamber, tooltip = tooltip, data_id = tooltip), pch = 21, stroke = 1) +
      scale_fill_manual(values = colors, guide = FALSE) +
      scale_size_manual(values = c(4, 8), guide = FALSE) +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
      scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, 0.25)) +
      my_theme() + 
      my_titles(x = "CSM-M Score") +
      theme(
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()
      )
    
    # convert to D3
    plot_interactive <- girafe(
      print(plot), width_svg = 10, height_svg = 2.5,
      options = list(
        opts_hover(css = "fill:#262626;stroke:#262626;"),
        opts_selection(type = "none"),
        opts_tooltip(css = tooltip_css, offx = 20, offy = 20, opacity = 1, delay_mouseover = 100, delay_mouseout = 100),
        opts_toolbar(saveaspng = FALSE),
        opts_sizing(rescale = TRUE, width = 1)
      )
    )
    
    # return
    return(plot_interactive)
  })

  ##################################################
  # PLOT: delegation map
  ##################################################
  
  output$delegation_map <- renderGirafe({

    # selected data
    selected_member <- reactive$members_page_selected_member
    selected_state <- congress_data$state[congress_data$member == selected_member]
    selected_district <- congress_data$district[congress_data$member == selected_member]
    selected_chamber <- congress_data$chamber[congress_data$member == selected_member]
    
    # make map
    if(selected_chamber == "Senate") {
      make_delegation_map(
        make_US_map(state_data, selected_state = selected_state, box = TRUE, shaded = TRUE),
        make_district_map(district_data, selected_state = selected_state, fill = "score_fill"),
        make_state_map(state_data, selected_state = selected_state, fill = "score_senate_fill"),
        make_state_map(state_data, selected_state = selected_state, fill = "score_senate_fill")
      ) %>% make_interactive(width = 10, height = 12)  
    } else if(selected_chamber == "House") {
      make_delegation_map(
        make_US_map(state_data, selected_state = selected_state, box = TRUE, shaded = TRUE),
        make_district_map(district_data, selected_state = selected_state, selected_district = selected_district, fill = "score_fill", box = TRUE, shaded = TRUE),
        make_state_map(state_data, selected_state = selected_state, fill = "score_senate_fill"),
        make_state_map(state_data, selected_state = selected_state, fill = "score_senate_fill")
      ) %>% make_interactive(width = 10, height = 12)
    }
  })
  
  ##################################################
  # PLOT: member maps
  ##################################################
  
  output$member_map_country <- renderGirafe({
    
    # selected data
    selected_member <- reactive$members_page_selected_member
    selected_state <- congress_data$state[congress_data$member == selected_member]

    # make map
    make_US_map(state_data, selected_state = selected_state, box = TRUE, fill = "score_all_fill") %>% make_interactive(width = 10, height = 7)
  })
  
  output$member_map_state_senate <- renderGirafe({
    
    # selected data
    selected_member <- reactive$members_page_selected_member
    selected_state <- congress_data$state[congress_data$member == selected_member]
    
    # make map
    make_state_map(state_data, selected_state = selected_state, fill = "score_all_fill") %>% make_interactive(width = 8, height = 8)
  })
  
  output$member_map_state_house <- renderGirafe({
    
    # selected data
    selected_member <- reactive$members_page_selected_member
    selected_state <- congress_data$state[congress_data$member == selected_member]
    selected_district <- congress_data$district[congress_data$member == selected_member]
    
    # make map
    make_district_map(district_data, selected_state = selected_state, selected_district = selected_district, box = TRUE, fill = "score_fill") %>% make_interactive(width = 8, height = 8)
  })
  
  output$member_map_district <- renderGirafe({
    
    # selected data
    selected_member <- reactive$members_page_selected_member
    selected_district <- congress_data$district[congress_data$member == selected_member]
    
    # make map
    make_single_district_map(district_data, selected_district = selected_district, fill = "score_fill") %>% make_interactive(width = 8, height = 8)
  })
  
  # output$member_map <- renderGirafe({
  #   
  #   # selected data
  #   selected_member <- reactive$members_page_selected_member
  #   selected_state <- congress_data$state[congress_data$member == selected_member]
  #   selected_district <- congress_data$district[congress_data$member == selected_member]
  #   selected_chamber <- congress_data$chamber[congress_data$member == selected_member]
  #   
  #   # make map
  #   if(selected_chamber == "Senate") {
  #     make_state_map_with_inset(
  #       make_US_map(state_data, selected_state = selected_state, box = TRUE, shaded = TRUE, fill = "score_all_fill"),
  #       make_state_map(state_data, selected_state = selected_state, fill = "score_senate_fill")
  #     ) %>% make_interactive(width = 8, height = 8) 
  #   } else if (selected_chamber == "House") {
  #     make_single_district_map_with_inset(
  #       make_US_map(state_data, selected_state = selected_state, box = TRUE, shaded = TRUE, fill = "score_all_fill"),
  #       make_district_map(district_data, selected_state = selected_state, selected_district = selected_district, box = TRUE, shaded = TRUE, fill = "score_fill"),
  #       make_single_district_map(district_data, selected_district = selected_district, fill = "score_fill")
  #     ) %>% make_interactive(width = 8, height = 8)
  #   }
  # })
  
  ####################################################################################################
  # downloads
  ####################################################################################################
  
  # senate map
  
  output$CSV_senate_map <- downloadHandler(
    filename = paste("CSM-data-", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      write.csv(reactive$DATA_senate_map, file, row.names = FALSE)
    }
  )
  
  output$PNG_senate_map <- downloadHandler(
    filename = paste("CSM-plot-", Sys.Date(), ".png", sep = ""),
    content = function(file) {
      ggsave(filename = file, plot = reactive$PLOT_senate_map, device = "png", scale = 1.5, width = 10, height = 7)
    }
  )
  
  # house map
  
  output$CSV_house_map <- downloadHandler(
    filename = paste("CSM-data-", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      write.csv(reactive$DATA_house_map, file, row.names = FALSE)
    }
  )
  
  output$PNG_house_map <- downloadHandler(
    filename = paste("CSM-plot-", Sys.Date(), ".png", sep = ""),
    content = function(file) {
      ggsave(filename = file, plot = reactive$PLOT_house_map, device = "png", scale = 1.5, width = 10, height = 7)
    }
  )
  
  ##################################################
  # end server
  ################################################## 
}

##################################################
# run app
##################################################

shinyApp(ui, server)
