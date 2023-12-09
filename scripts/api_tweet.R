usethis::edit_r_environ()

# Referencia
# https://developer.twitter.com/en/docs/tutorials/getting-started-with-r-and-v2-of-the-twitter-api 
# en particular ver https://developer.twitter.com/en/docs/twitter-api/tweets/search/introduction 

library(httr)
library(jsonlite)
library(tidyverse)
library(glue)
library(rtweet)


clientID <- ""
clientSecret <- ""
client <- rtweet_client(clientID, clientSecret, "Application_name")
oauth2 <- rtweet_oauth2(client)
while(TRUE) {
  tweet_post("test", token = oauth2)
  Sys.sleep(3600)
  oauth2 <- rtweet:::auth_renew(oauth2)
  Sys.sleep(3600)
  oauth2 <- rtweet:::auth_renew(oauth2)
  Sys.sleep(3600)
  oauth2 <- rtweet:::auth_renew(oauth2)
}


client <- rtweet_client(client_id = "bHhaMEdFQjRMcENFcWY2SHFrbEY6MTpjaQ",
                        client_secret= "QIvN2GwrPGT8o_WS_0WRAH6KOAwzO9f5rNzX3RXvPfIPpFy-Yb")


headers <- c(Authorization = glue::glue('Bearer {Sys.getenv("BEARER_TOKEN")}'))

params <- list(`user.fields` = 'description',
               `expansions` = 'pinned_tweet_id')

# params <- list(query = 'from:SergioMassa',
#                max_results = '5',
#                tweet.fields = 'created_at,lang,conversation_id')

response <- httr::GET(url = url_handle,
              httr::add_headers(.headers = headers),
              query = params)
obj <- httr::content(response, as = "text")
print(obj)


url_handle <- glue('https://api.twitter.com/2/tweets/search/recent')

response <- httr::GET(url = url_handle,
              httr::add_headers(.headers = headers),
              query = params)

contenido <- httr::content(response, as = "text")

print(contenido)

json_data <- fromJSON(contenido, flatten = TRUE) %>% as_tibble()

View(json_data)
