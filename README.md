# Congressional Social Media (CSM) App

This app showcases a new quantitative methodology I've developed that uses machine learning (deep convolutional neural networks) to measure the political positions that members of Congress take on Twitter based on the text of members' tweets.

You can view the app at https://fjelstul.shinyapps.io/CSM-app/. 

I developed the app in R using Shiny. I trained the deep convolutional neural network that I use to create CSM scores using Keras and Google's Tensor Flow. All of the maps in the app use GIS data from the Census Bureau (https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.2017.html). The raw tweet data from the 115th Congress that I use to train the model was collected by Justin Littman at George Washington University Libraries. It is available from the George Washington University Libraries Dataverse (https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UIVHQR).

# Introduction

When members of Congress tweet, they take political positions on a wide variety of issues. Congressional Social Media (CSM) scores are a new quantitative estimate of how partisan members of Congress are on Twitter in terms of the positions they take, which are reflected in the language members use in their tweets. A partisan member of Congress is one who consistently articulates positions that are consistent with their party's positions. This is an experimental methodology and is subject to change.

This app provides a variety of interactive tools that you can use to visualize CSM scores for members of the 115th Congress. You can visualize the distribution of CSM scores for the House of Representatives and the Senate, you can analyze the tweets of specific members of Congress, and you can analyze specific tweets posted by members of Congress or any other tweets that have political content.

I estimate CSM scores based on the text of members' tweets. I use machine learning (deep convolutional neural networks) to score all tweets posted by members of the 115h Congress (January 3, 2017 to January 3, 2019). See the methodology section for more information on how CSM scores are estimated. The sample includes approximately 250,000 tweets by senators and approximately 630,000 tweets by representatives. This works out to around 2,500 tweets per senator and around 1,450 tweets per representative.

I generate one CSM score per tweet in the sample, called a CSM-T score. I then average these CSM-T scores by member to create member-level scores, called CSM-M scores, that characterize each member's position-taking behavior on Twitter over the course of the entire 115th Congress. I can also average the tweet-level scores by member and by month, called CSM-MM scores, to create a measure of position-taking that varies over time.

In addition to producing a single score for each member I also generate a distribution of estimates for each member that characterizes their position-taking behavior across all of their tweets. Members with narrower distributions are more consistent in their position-taking. CSM scores are directly comparable over time and across members of Congress. See the methodology section for more information.


