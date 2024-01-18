#library(shiny)
#library(rCharts)
library(lubridate)
library(highcharter)
library(leaflet)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(ggplot2)
library(dplyr)
library(readr)

# UI
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  # Application title
  titlePanel("Analyzing the #fakenews hashtag on Twitter"),
  
  p(
    class = "text-muted",
    paste("This app analyzes the '#fakenews' hashtag using different types of visualizations.")
  ),
  p("By: Ananya Pujary"),
    
  tabsetPanel(
    tabPanel("About",
      mainPanel(
        h1("About this app"),
        h3("Background"),
        p("\'Fake news\' has been an emerging topic of discussion over the past few years.
          It can be understood as \"information that is fabricated, misleading, and verifiably false\" (Chuai & Zhao, 2020, p.1).
           This term has been used in various contexts, from vaccine information (Rohan, 2022) to political campaigns (BBC, 2018), and features prominently on different social media, such as Twitter.
          Several studies have pointed to the emotional impact that fake news has on its readers, commonly inciting anxiety, anger, fear, sadness, and suspicion (Erdelyi, 2020; Fernández-López & Perea, 2020).
          Hence, it would be worthwhile to analyze social media data, particularly from Twitter, of posts using the hashtag \'#fakenews\'.
          I expect that the hashtag would be more popular in Western countries like North America and Europe,
          and there would be a higher degree of negative sentiments attached to posts using it."),
        h3("Research Objectives"),
        tags$ul(
    tags$li("Understand the worldwide prevalence of \'#fakenews\' on Twitter"),
    tags$li("Examine the top hashtags and usernames that are associated with \'#fakenews\'"), 
    tags$li("Determine which sentiments are closely associated with \'#fakenews\' and how stable they are over time")
    ),
        h3("Methodology"),
        ("The data used in these visualizations were collected by focusing on the hashtag \'#fakenews\' using the"), em("search_tweet()"),
        ("function on R. The command was written to search for 10,000 tweets twice. 9,989 tweets were collected for the period of November 28th-December 3rd and 9,666 for the period of December 4th-8th.
         A total of 19,655 tweets were collected and analyzed for this project. The dataset used also included retweets.
         Three visualization techniques were used in this project: geomapping using"), em("leaflet()"),(", wordcloud using the document-feature matrix"),em("(dfm())"),(", and sentiment analysis using the"), em("syuzhet"),("package."),
        h3("References"),
    tags$ul(
      tags$li("BBC. (2018, November 12). How president Trump took 'fake news' into the mainstream. BBC News. Retrieved December 14, 2022, from https://www.bbc.com/news/av/world-us-canada-46175024."),
      tags$li("Chuai, Y., & Zhao, J. (2020). Anger makes fake news viral online. Retrieved 4 October 2021, from https://arxiv.org/pdf/2004.10399.pdf."),
      tags$li("Erdelyi, K. (2020). The Psychological Impact of Information Warfare & Fake News. Psycom. Retrieved 13 December 2022, from https://www.psycom.net/iwar.1.html."),
      tags$li("Fernández-López, M., & Perea, M. (2020). Language does not modulate fake news credibility, but emotion does. Psicológica, 41(2), 84-102. https://doi.org/10.2478/psicolj-2020-0005."),
      tags$li("Rohan, A. J. (2022). Facts, fake news, and covid-19 vaccination. MCN: The American Journal of Maternal/Child Nursing, 47(2), 65–65. https://doi.org/10.1097/nmc.0000000000000794.")
      )
    )
    ), 
tabPanel("Geomapping",
         sidebarLayout(
           sidebarPanel(
             #helpText(h5("Filter tweets by their minimum retweet count here:", style = "font-family: 'arial'; font-si12pt")),
             sliderInput("slider20", h4("Filter by minimum retweet count here:"),
                         min = 0, max = 150, value = 1),
             p(
               class = "text-muted",
               paste("Click on the pins on the map to view tweets that used the #fakenews hashtag.")
             )
           ),
           mainPanel(
             h1("Geomapping"),
             leafletOutput("mymap0"),
             h3("Observations"),
             p("From this visualization, we can observe that this hashtag is particularly popular in Europe, North America, and South America.
               The geotagged tweets in Europe seem to be addressing various issues, from vaccines (Switzerland) to politics (France).
               The geotagged tweets in North America seem to talk about politics and critiquing news media content and the president.
               Finally, in South America, the goetagged tweets using this hashtag talk about public health (Peru) and politics (Chile).
               The most retweeted geotagged tweet is one from Switzerland about journalism on vaccines (approximately 95 retweets).")
           )
         )
),
tabPanel("Wordcloud",
         sidebarLayout(
           sidebarPanel(        
             sliderInput("slider10", h4("Filter by minimum retweet count here:"),
                         min = 0, max = 1000, value = 1),
             p(
               class = "text-muted",
               paste("This visualization shows us other hashtags that are typically used with \'#fakenews\' and the users that use this hashtag most."
               )
             )
           ),
           mainPanel(
             h1("Wordcloud"),
             plotOutput("wordcloud0"),
             h3("Observations"),
             p("This visualization helps us view users that commonly use this hashtag as well as other hashtags that are used with it.
             The terms are plotted in a way that their sizes are proportional to their numerical values in the document-feature matrix.
             At the retweet count of 1,000, the user @corinnereverbel seems to be closely associated with \'#fakenews\'.
             This user tweets in French. Their most popular tweet calls for submissions from Twitter users of fake news that were propagated by mainstream media and fact-checkers in France related to the COVID-19 pandemic.
             It was posted on November 27th and has over 1,400 retweets. There are several tweets in the dataset that are retweets of this tweet, and many of them concern perceived vaccine misinformation from mainstream media and the politicians.
               The original tweet by this user has the hashtags #mediamainstream and #factcheckeu, which are visible in the wordcloud too.")
           #  p(class = "text-muted",paste("Observations: 400+ retweets - @aitofficial, @beatrix_vstorch, #relotius, #lugenpresse, @tagesspiegel, @kanchangupta")
             )
           )
         ),
tabPanel("Sentiment Analysis",
         sidebarLayout(
           sidebarPanel(
           #  helpText(h4("Choose a sentiment (or more) here:", style = "font-family: 'arial'; font-si12pt")),
             h4("Choose a sentiment (or more) here:"),
             selectInput("variable0", "Sentiment:", choices = c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive"), multiple = TRUE),
             p(
               class = "text-muted",
               paste("Hover over the points on the chart to view the exact average sentiment value on a particular day."
               )
             )
          ),
           mainPanel(
             h1("Sentiment Analysis"),
             highchartOutput('sentiments0'),
             h3("Observations"),
             p("This graph shows the average sentiment value on each of the 10 days (November 28th-December 8th).
               There are ten sentiments that were extracted from the tweets collected: anger, anticipation, disgust, fear, joy, negative, positive, sadness, surprise, and trust.
               Overall, we observe that negative sentiments are higher in value in tweets containing this hashtag compared to the other sentiments. The sentiment with the lowest value across this time period was joy, closely followed by surprise.
               Below are the general trends we observe for each of the ten sentiments:"),
             tags$ul(
               tags$li("Anger: a wide bell curve between December 1st and 5th reaching 0.27, followed by lower levels on 7th (0.11) and 8th (0.14)."),
               tags$li("Anticipation: a decline in average value between November 28th and 30th (0.15 to 0.102), followed by a sharp increase on December 4th (0.2). Then there is a gradual decline from 6th to reach its lowest value on 8th (0.05)."),
               tags$li("Disgust: the average value for this sentiment reached its peak on December 3rd (0.27), with a gradual decline till the 5th (0.14), plateauing on 6th, and then reducing further to around 0.1 on the 7th and 8th."),
               tags$li("Fear: this sentiment's average value starts off at a relatively high value of just under 0.4, declines a bit to 0.3 on the 1st and bounces back on the 3rd, to gradually decline through the 8th."),
               tags$li("Joy: the average value fluctuated constantly over the 10-day period. There were lows on November 30th, December 3rd, 5th, and 7th, and highs on the remaining days except for the 8th (0.0606)."),
               tags$li("Sadness: its average value peaks on 29th (0.302), plateaus between 30th and 1st to then increase on 3rd and gradually decline till the 7th. There is a sharp increase in value on the 8th."),
               tags$li("Surprise: its average value peaks on 3rd (0.183), while the value fluctuates between 0.05 and 0.11 on the other days."),
               tags$li("Trust: this sentiment's average value peaks on November 30th (0.44), declines to 0.3 the next day to increase back up for the next few days, and declines to 0.202 on 7th and 8th."),
               tags$li("Negative: its average value starts with a gradual decline between the 28th and 1st (0.63 to 0.52), and rises back up to peak on 4th (0.66). Then, the value gradually declines to around 0.4 till the 8th."),
               tags$li("Positive: its average value gradually declines between the 28th and 1st (0.43 to 0.33), rises up to 0.5 on 3rd and plateaus through the 4th. After this, the value gradually declines to 0.25 on the 8th. Interestingly, this sentiment's value varies in a similar manner to the negative sentiment.")
             ),
             h3("Common terms for each sentiment"),
             plotOutput("sentiment_word_counts")
           )
         )
),
tabPanel("Reflections",
         mainPanel(
           h1("Reflections"),
           h3("Findings"),
           p("The three visualizations chosen to represent data on \'#fakenews\' provided some interesting insights.
             The map showcased that this hashtag is more commonly used on some continents (Europe, North America, and South America) than others. This could be due to many factors, such as differences in internet and social media access.
             \'#fakenews\' has also been used in various contexts, such as politics and public health.
             The wordcloud further demonstrated that this hashtag has been used in these contexts as well, and includes discourse on mainstream media reporting.
             Lastly, the sentiment analysis confirmed much of what existing literature has to say about the link between emotionality and fake news.
             Tweets with this hashtag were rated higher on negative sentiment value compared to the other nine sentiments examined, while joy and surprise were at lower levels on average."),
           h3("Limitations"),
           p("Due to a limited timeframe given for it, this project only analyzed data from Twitter over a relatively short time period. This would not have been able to capture long-term trends with the hashtag.
             In terms of the methods used, not all tweets are geotagged, so the geomapping technique may not be a completely accurate representation of the locations where the hashtag is popular.
             The sentiment analysis may not be completely accurate either. For example, in the visualization of common terms for each sentiment,\"trump\" is the most frequently occurring term under \'surprise\'.
             However, the tweets using this word were more likely to be talking about President Trump rather than the verb \'trump\'.
             In the data collected, no tweets containing the hashtag were found to be posted on December 2nd, indicating a temporal gap in the visualizations."),
           h3("Conclusion and Future Directions"),
           p("The visualizations used in this app were insightful in that they indicated the geographic regions in which fake news is being talked about, the users and other hashtags closely associated with it, and the types of sentiments commonly associated with it.
           However, with certain improvements, they could be made more representative. This project exclusively used Twitter data, so future apps analyzing the hashtag \'#fakenews\' can incorporate data
             from Tiktok, Instagram, and other social media platforms to compare the ways and contexts in which it is discussed.
             Additionally, data can be collected over a longer time period to assess the stability of trends.
             Other methods such as social network analysis could be used to understand whether this hashtag has a tightly-clustered or loosely-clustered system, and the key account users that are engaged with this hashtag.
             Besides the retweet count of the tweets, other parameters such as language and continent could be used to filter the data in the visualizations to provide deeper insights.")
         )
  
)
)
)



# SERVER 
server <- function(input, output) {
  thematic::thematic_shiny()

  geocodes0 <- read_csv("#fakenews_geocodes_all.csv")
  geocodes0$text <- as.character(geocodes0$text)
  
  tweets0 <- read_csv("#fakenews_tweets_all.csv")
  tweets0$text <- as.character(tweets0$text)
  
  sentiments0 <- read_csv("#fakenews_sentiments_all.csv")
  sentiments0$date_label <- as.Date(sentiments0$date_label)
  
  sentiment_word_counts <- read_csv("#fakenews_sentiments_words.csv")
  
  
  output$mymap0 <- renderLeaflet({
    
    usericon <- makeIcon(
      iconUrl = geocodes0$profile_image_url,
      iconWidth = 15, iconHeight = 15
    )
    
    
    leaflet(data = geocodes0[geocodes0$retweet_count >= input$slider20,]) %>% 
      addTiles() %>%
      setView(lng = -98.35, lat = 39.50, zoom = 2) %>% 
      addMarkers(lng = ~lng, lat = ~lat,popup = ~ as.character(text),icon = usericon) %>% 
      addProviderTiles("Stamen.TonerLite") %>%  #more layers:http://leaflet-extras.github.io/leaflet-providers/preview/
      addCircleMarkers(
        stroke = FALSE, fillOpacity = 0.5)
  })
  
  output$wordcloud0 <- renderPlot({
    dfm0 <- dfm(tweets0[tweets0$retweet_count >= input$slider10,]$text, remove = c(stopwords("english"), remove_numbers = TRUE, remove_symbols = TRUE, remove_punct = TRUE))
    dfm0 <- dfm_select(dfm0, pattern = ("#*|@*"))
    set.seed(100)
    textplot_wordcloud(dfm0, min_size = 1.5, min_count = 10, max_words = 100,color = rev(RColorBrewer::brewer.pal(10, "PiYG")))
  })
  

  
  output$sentiments0 <- renderHighchart({
    highchart() %>%
      hc_add_series(data= sentiments0[sentiments0$variable %in% input$variable0,],"line", hcaes(x = date_label, y = value, group=variable)) %>%
      hc_xAxis(type = "datetime")%>%
      hc_title(text = "Sentiment(s) for #fakenews over time", align = "left")
    
  }
  )
  
  # visualizing key sentimental terms
 output$sentiment_word_counts <- renderPlot({
  sentiment_word_counts %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(title = "Sentiment terms",
         y = "Contribution to sentiment",
         x = NULL) +
    coord_flip()
})

}

# Run the application 
shinyApp(ui = ui, server = server)

