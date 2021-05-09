##using the newsanchor package to extract some random washingtonpost articles 
##that containt the keyword "trump" and then subquently perform some basic sentiment analysis
##Not shown, I had to use the CsS selector in safari to figure out the correct format
##to scrape the washington post


#load required packages
library(newsanchor)
library(robotstxt)
library(tidytext)
library(textdata)
library(httr)
library(rvest)
library(stringr)
library(dplyr)
library(forcats)
library(ggplot2)


##set api key##
##you will need to obtain your own key
set_api_key(api_key = "input your key", 
            path = "~/.Renviron")

##query all articles about trump on April 30th 
trump <- get_everything_all(query   = "trump", language = 'en', sources = "the-washington-post",
                            sort_by = 'popularity', from = '2021-05-06')

# extract response data frame
articles <- trump$results_df
articles1 <- as.data.frame(articles)
View(articles)

##look at one random article
samp <- sample(1:nrow(articles), 1, replace = FALSE)
articles2 <- articles[samp,]
View(articles2)

##create function to scrape body text

get_article_body <- function (url) {
  
  # download article page
  response <- GET(url)
  
  # check if request was successful
  if (response$status_code != 200) return(NA)
  
  # extract html
  html <- content(x        = response,
                  type     = "text",
                  encoding = "UTF-8")
  
  # parse html
  parsed_html <- read_html(html)
  
  # define paragraph DOM selector
  selector <- ".pb-md"
  
  # parse content
  parsed_html %>%
    html_nodes(selector) %>%      # extract all paragraphs within class 'article-section'
    html_text() %>%               # extract content of the <p> tags
    str_replace_all("\n", "") %>% # replace all line breaks
    paste(collapse = " ")         # join all paragraphs into one string
}


##apply function
articles2$body <- NA

# initialize progress bar
pb <- txtProgressBar(min     = 0,
                     max     = nrow(articles2),
                     initial = 1,
                     style   = 3)

# loop through articles and "apply" function
for (i in 1:nrow(articles2)) {
  
  # "apply" function to i url
  articles2$body[i] <- get_article_body(articles2$url[i])
  
  # update progress bar
  setTxtProgressBar(pb, i)
  
  # sleep for 1 sec
  Sys.sleep(1)
}

##lexicon for sentiment analysis
nrc <- get_sentiments(lexicon = "bing")
nrc
articles2$body
sentiment_by_day <- articles2 %>%
  select(url, body) %>%                                  # extract required columns
  unnest_tokens(word, body) %>%                          # split each article into single words
  anti_join(stop_words) %>%            # remove stopwords
  inner_join(nrc, by = "word")%>%
  filter(sentiment%in% c("positive", "negative"))%>%
  count(word, sentiment,sort = TRUE)%>%
  top_n(20)


#initial plot
ggplot(sentiment_by_day, aes(x=word,y=n, fill=sentiment))+geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales='free')+
  coord_flip()

sentiment_by_day2 <- articles2 %>%
  select(url, body) %>%                                  # extract required columns
  unnest_tokens(word, body) %>%                          # split each article into single words
  anti_join(stop_words) %>%            # remove stopwords
  inner_join(nrc, by = "word")%>% #add in sentiment words
  filter(sentiment%in% c("positive", "negative"))%>%
  count(word, sentiment,sort = TRUE)%>%
  mutate(n=ifelse(sentiment=="negative",-n,n))%>%
  mutate(word=reorder(word,n))


#contribution to sentiment plot
ggplot(sentiment_by_day2, aes(x=word,y=n, fill=sentiment))+geom_col(show.legend = FALSE) +
  coord_flip()+theme_minimal() + labs(y="contribution to sentiment")




