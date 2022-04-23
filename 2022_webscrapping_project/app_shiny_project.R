#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(stringr)
library(wordcloud2)
library(igraph)
library(ggraph)# library for the plot a network
library(widyr)
library(tidytext)
library(textdata)
library(topicmodels)

friend <- read_csv(file = "friends.csv")

friend_guess <- friend %>% filter(!character%in% c("rachel", "monica", "chandler", "phoebe", "ross", "joey", "all","chan","phoe","mnca","rach")) 

count_guess <- friend_guess %>% group_by(character) %>% count(character, name = "guess_character_n", sort = TRUE)

friend.colors = c("#c28464","#ebded5", "#54545d" )
friend.background = "#e7ceb8"
friend_lead <- friend %>% filter(character == c("rachel", "monica", "chandler", "phoebe", "ross", "joey"))
stop_words <- tidytext::stop_words
unnested_com <- friend_lead %>% tidytext :: unnest_tokens(input = script,
                                                          output = "word",
                                                          token = "words")

clean_com <- unnested_com %>% anti_join(stop_words, by = "word")
clean_com_filter <- clean_com %>% filter(!word %in% c("uhm", "it's", "ll", "im", "don't", "i'm", "that's", "ve", "that's","you're",
                                                      "woah", "didn", "what're", "alright", "she's", "we're", "dont", "c'mere", "wouldn",
                                                      "isn","pbs", "can't", "je", "youre", "doesn", "007", "haven", "whoah", "whaddya", 
                                                      "somethin", "yah", "uch", "i'll","there's", "won't", "didn't", "you'll", "allright",
                                                      "yeah", "hey", "uh", "gonna", "umm","um", "y'know", "ah", "ohh", "wanna", "ya", "huh", "wow",
                                                      "whoa", "ooh")) %>% 
  mutate(word = str_remove_all(word, "'s")) 



##### count number words at least used 50 times by the lead characters
com_count <- clean_com_filter  %>% count(word, sort = TRUE) %>% filter(n >= 50) 


word_correlations <- clean_com_filter %>% semi_join(com_count, by ="word") %>% 
  pairwise_cor(item = word, feature = character) %>% filter( correlation > 0.65 ) 



bing <- get_sentiments(lexicon = "bing")
sentiment_bing <- unnested_com %>% inner_join(y = bing)
count_bing <- sentiment_bing %>% count(sentiment, word, sort = TRUE) %>% 
  group_by(sentiment) %>% slice_max(n, n = 10) %>% ungroup() %>% mutate(word = reorder(word, n))

dtm <- clean_com_filter %>% 
  select(season, word) %>% 
  group_by(season, word) %>% 
  count() %>% 
  cast_dtm(season, word, n)

# set a seed so that the output of the model is predictable

lda <- LDA(dtm, k = 5, control = list(seed = 1234))
topics <- tidy(lda, matrix = "beta")

top_terms <- topics %>%group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Text Mining Friends Series DASHBOARD"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
            column (12,navbarPage("Text analysis :",
                               tabPanel( "Guess character wordcloud", 
                                         wordcloud2Output("wordcloud")),
                               tabPanel( "Words association",
                                         plotOutput("network")),
                               tabPanel( "Sentiment analysis",
                                         plotOutput("sentiment")),
                               tabPanel( "Topics Modeling",
                                         plotOutput("topics"))
                             )
                             )))

        

# Define server logic required to draw a histogram
server <- function(input, output) {
  


  output$wordcloud <- renderWordcloud2({wordcloud2(data = count_guess, 
               shape = "Pentagon",
               color = friend.colors,
               backgroundColor = friend.background
    )} )
    
  
  output$network <- renderPlot({
    graph_from_data_frame(d = word_correlations,
                          vertices = com_count %>% semi_join(word_correlations, by = c("word" = "item1"))) %>% 
      ggraph(layout = "fr")+
      geom_edge_link(aes(alpha = correlation)) + 
      geom_node_point()+
      geom_node_text(aes(color = n, label = name), repel = TRUE) + 
      ggtitle("words association across lead character") +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))},
    width = "auto",
    height = "auto",
    res = 65)
    
  output$sentiment <- renderPlot({
    count_bing %>% ggplot(mapping = aes(x = n, y = word, fill = sentiment))+
      geom_col(show.legend = FALSE) + 
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Words used related to negative/positive sentiment", x = NULL) +  
      ggtitle("lead character - Sentiment analysis")+
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))},
    width = "auto",
    height = "auto",
    res = 72)
    
  output$topics <- renderPlot({
    top_terms %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = TRUE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip() +
      scale_x_reordered()+
      labs(title = "Word-Topic Probabilities")+
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))},
    width = "auto",
    height = "auto",
    res = 72)

}

# Run the application 
shinyApp(ui = ui, server = server)
