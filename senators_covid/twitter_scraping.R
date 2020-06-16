library(rtweet)
library(readr)
library(rstudioapi)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
# create credentials as objects (these are FAKE CREDENTIALS)
# you need to replace them with your own.

path = dirname(getSourceEditorContext()$path)
setwd(path)

api_key <- ""
api_secret_key <- ""


token <- create_token(
  app = "",
  consumer_key = api_key,
  consumer_secret = api_secret_key)


senator_info = read_csv('us-senate.csv')

# n = 1:100
# 
# for (i in 1:100){
# senator_tweets =  get_timelines(senator_info$twitter_handle[i], n = 3200,
#                                   since = '2019-12-01',
#                                   until = '2020-06-16')
# 
# senator_tweets$party = senator_info$party[i]
# senator_tweets$state = senator_info$state_name[i]
# 
# if (i == 1) {
#   senators_tweets = senator_tweets
# } else {
#   senators_tweets = rbind(senators_tweets_all, senator_tweets)
# }

# 
# saveRDS(senators_tweets_all, 'sentators_tweets_all.rds')
senators_tweets_all = readRDS('sentators_tweets_all.rds')


head(senators_tweets_all$text)
sub = senators_tweets_all
sub$logical = grepl('corona',sub$text) | grepl('covid',sub$text) | grepl('virus',sub$text) | 
  grepl('lockdown', sub$text) | grepl('quarantine', sub$text) | grepl('distancing', sub$text)


dated = '2019-12-01 00:00:01'
sub = sub %>%
  filter(created_at > dated)
  
sub$ix = 1:dim(sub)[1]

sub = sub %>% 
  mutate(party = factor(party, levels = c('republican', 'independent','democrat')))

sub %>%
  filter(logical ==TRUE) %>%
  group_by(party) %>%
  ts_plot(., "weeks") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets about COVID-19 by Senators",
    subtitle = "Tweet counts aggregated by week",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

sub %>%
  mutate(time = floor_date(created_at, "week"))%>%
  group_by(screen_name, time, party) %>%
  summarise(freq = mean(logical,na.rm = T)) %>%
  group_by(time, party) %>%
  summarise(freq_m= mean(freq,na.rm = T)) %>%
  ggplot(.) +
  geom_line(aes(x = time, y = freq_m, color = party))

sub %>%
  mutate(time = floor_date(created_at, "week"))%>%
  group_by(screen_name, time, party,state) %>%
  summarise(freq = mean(logical,na.rm = T)) %>%
  ggplot(.) +
  geom_line(aes(x = time, y = freq, color = party,group = screen_name),alpha = .8) +
  facet_wrap(.~state, ncol = 5)




