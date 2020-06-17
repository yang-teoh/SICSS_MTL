library(tidyverse)
library(tidytext)
library(dplyr)
library(SnowballC)
library(lubridate)

load(url("https://cbail.github.io/Trump_Tweets.Rdata"))

#Tweets -> words
tidy_trump_tweets<- trumptweets %>%
  select(created_at,text) %>%
  unnest_tokens("word", text)
head(tidy_trump_tweets)

tidy_trump_tweets %>%
  count(word) %>%
  arrange(desc(n))

#removing stop words
data("stop_words")
tidy_trump_tweets<-tidy_trump_tweets %>%
  anti_join(stop_words)

tidy_trump_tweets %>%
  count(word) %>%
  arrange(desc(n))

#Stemming 
tidy_trump_tweets<-tidy_trump_tweets %>%
  mutate_at("word", funs(wordStem((.), language="en")))


tidy_trump_tweets %>%
  count(word) %>%
  arrange(desc(n))


library(ggplot2)

#removal of twitter-specific language
top_words<-
  tidy_trump_tweets %>%
  anti_join(stop_words) %>%
  filter(!(word=="https"|
             word=="rt"|
             word=="t.co"|
             word=="amp")) %>%
  count(word) %>%
  arrange(desc(n))


top_words %>%
  slice(1:20) %>%
  ggplot(aes(x=reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Frequency")+
  xlab("")+
  ggtitle("Most Frequent Words in Trump Tweets")+
  guides(fill=FALSE)



#Re-dating the tweets
tidy_trump_tweets$date<-as.Date(tidy_trump_tweets$created_at, 
                                format="%Y-%m-%d %x")

#sentiment analysis using dictionary
trump_tweet_sentiment <- tidy_trump_tweets %>%
  filter(!(word=="https"|
             word=="rt"|
             word=="t.co"|
             word=="amp")) %>%
  inner_join(get_sentiments("bing")) %>%
  count(date, sentiment) 



ggplot(trump_tweet_sentiment, aes(x=date, y=n, color = sentiment))+
  geom_line(size=.5)+
  geom_hline(yintercept = mean(trump_tweet_sentiment$n[trump_tweet_sentiment$sentiment == 'positive']), 
             color = 'dodgerblue')+
  geom_hline(yintercept = mean(trump_tweet_sentiment$n[trump_tweet_sentiment$sentiment == 'negative']), 
             color = 'red3')+
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Number of Words")+
  xlab("")+
  ggtitle("Sentiment in Trump Tweets")+
  theme(aspect.ratio=1/4)

tidy_trump_DTM<-
  tidy_trump_tweets %>%
  filter(!(word=="https"|
                                  word=="rt"|
                                  word=="t.co"|
                                  word=="amp")) %>%
  count(created_at, word) %>%
  cast_dtm(created_at, word, n)


######## COLLAPSING ALL TWEETS IN 1 DAY INTO A SINGLE "DOCUMENT" ########
tidy_trump_DTM_byday <-
  tidy_trump_tweets %>%
  filter(!(word=="https"|
             word=="rt"|
             word=="t.co"|
             word=="amp")) %>%
  count(date, word) %>%
  cast_dtm(date, word, n)




#topic modelling with 5 topics on day-to-day tweets
trump_topic_model<-LDA(tidy_trump_DTM_byday, k=5, control = list(seed = 321))

trump_topics <- tidy(trump_topic_model, matrix = "beta")

trump_top_terms <- 
  trump_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)



#topic 1: taxes 
#topic 2: nationalism
#topic 3: election
#topic 4: immigration
#topic 5: foreign affairs
trump_top_terms %>%
  mutate(term = reorder(term, beta),
         topic = factor(topic, levels = c(1:5),
                        labels = c('taxes','nationalism','election',
                                   'immigration','foreign affairs'))) %>%
  ggplot(aes(term, beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()



####### COMBINING SENTIMENT ANALYSES & TOPIC MODELLING ######
trump_tweet_sentiment <- tidy_trump_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(date, sentiment, .drop = FALSE)%>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(date, sentiment,
           fill = list(n = 0))

trump_tweet_sentiment_full = trump_tweet_sentiment %>% 
  complete(date,nesting(sentiment), fill = list(n = 0))

trump_documents_topic = tidy(trump_topic_model, matrix = 'gamma') 

trump_tweet_sentiment_full = trump_tweet_sentiment_full %>%
  spread(sentiment, n)

trump_documents_topic = trump_documents_topic[
  str_detect(trump_documents_topic$document,
             paste(trump_tweet_sentiment_full$date, collapse = '|')),]
  
trump_documents_sentiment = trump_tweet_sentiment_full %>%
  group_by(date) %>%
  mutate(taxes = trump_documents_topic$gamma[trump_documents_topic$document == date &
                                               trump_documents_topic$topic == 1],
         nationalism = trump_documents_topic$gamma[trump_documents_topic$document == date &
                                               trump_documents_topic$topic == 2],
         election = trump_documents_topic$gamma[trump_documents_topic$document == date &
                                               trump_documents_topic$topic == 3],
         immigration = trump_documents_topic$gamma[trump_documents_topic$document == date &
                                               trump_documents_topic$topic == 4],
         foreign_affairs = trump_documents_topic$gamma[trump_documents_topic$document == date &
                                               trump_documents_topic$topic == 5])


head(trump_documents_sentiment)


####### EVOLUTION OF TOPICS OVER TIME ########
test1 = trump_documents_topic %>%
  ungroup() %>%
  mutate(document = as.Date(document, format = "%Y-%m-%d"),
           date = floor_date(document,
                           "month")) %>%
  group_by(date,topic) %>%
  summarise(gamma = mean(gamma,na.rm =T))

test1 %>%
  mutate(topic = factor(topic, levels = c(1:5),
                labels = c('taxes','nationalism','election',
                           'immigration','foreign affairs')))  %>%  
  ggplot(.) +
  geom_line(aes(x = date, y = gamma, color = topic))+ theme_minimal()
  


  
  ####### TWEETS ABOUT IMMIGRATION TENDS TO USE MORE POSITIVE LANGUAGE ########
g1 = glm(positive ~ nationalism + election + immigration + 
           foreign_affairs + taxes, trump_documents_sentiment, 
             family = 'poisson')
summary(g1)

ggeffects::ggpredict(g1, c('immigration')) %>%
  data.frame() %>%
  ggplot(.) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, fill = 'darkgoldenrod1') +
  geom_line(aes(x = x, y = predicted)) + 
  labs(x = 'Document loading on Immigration Topic',
       y = 'Counts of positive words') + theme_minimal() 
  


####### TWEETS ABOUT IMMIGRATION & ELECTION TENDS TO USE MORE NEGATIVE LANGUAGE ########
g2 = glm(negative ~ nationalism + election + immigration + 
           foreign_affairs + taxes , trump_documents_sentiment, 
         family = 'poisson')
summary(g2)


ggeffects::ggpredict(g2, c('immigration')) %>%
  data.frame() %>%
  ggplot(.) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, fill = 'dodgerblue') +
  geom_line(aes(x = x, y = predicted)) + 
  labs(x = 'Document loading on Immigration Topic',
       y = 'Counts of negative words') + theme_minimal() 


ggeffects::ggpredict(g2, c('election')) %>%
  data.frame() %>%
  ggplot(.) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = .5, fill = 'dodgerblue') +
  geom_line(aes(x = x, y = predicted)) + 
  labs(x = 'Document loading on Election Topic',
       y = 'Counts of negative words') + theme_minimal() 

####### ANALYZING RETWEET & LIKE DATA ########
trump_documents_sentiment

tidy_trump_likes_retweets = trumptweets %>%
  select(created_at,retweet_count,favorite_count) %>%
  mutate(date = as.Date(created_at,  format="%Y-%m-%d %x")) %>%
  group_by(date) %>%
  summarise(likes = sum(favorite_count,na.rm = T),
            retweets = sum(retweet_count, na.rm = T))

tidy_trump_likes_retweets


trump_documents_sentiment = 
  trump_documents_sentiment %>%
  group_by(date) %>%
  mutate(likes = tidy_trump_likes_retweets$likes[tidy_trump_likes_retweets$date == date],
         retweets = tidy_trump_likes_retweets$retweets[tidy_trump_likes_retweets$date == date])


trump_documents_sentiment

sentiment_glm_likes = lm(likes ~ positive + negative, trump_documents_sentiment)
                    #family = 'quasipoisson')
summary(sentiment_glm)


sentiment_glm = glm(likes ~ positive + negative, trump_documents_sentiment,
       family = 'quasipoisson')
summary(sentiment_glm)

topics_glm_retweet = glm(retweets ~ nationalism + election + immigration + 
                           foreign_affairs + taxes , trump_documents_sentiment,
                         family = 'quasipoisson')
summary(topics_glm_retweet)


topics_glm_retweet = glm(retweets ~ nationalism , trump_documents_sentiment,
                         family = 'quasipoisson')
summary(topics_glm_retweet)

topics_glm_retweet = glm(retweets ~ election, trump_documents_sentiment,
                         family = 'quasipoisson')
summary(topics_glm_retweet)



topics_glm_likes = glm(likes ~ nationalism + election + immigration + 
                         foreign_affairs + taxes , trump_documents_sentiment,
                       family = 'quasipoisson')
summary(topics_glm_likes)


sentiment_glm_retweet = glm(retweets ~ positive + negative, trump_documents_sentiment,
                            family = 'quasipoisson')
summary(sentiment_glm_retweet)


####PREDICTING LIKES: SENTIMENTS*IMMIGRATION(TOPIC)
sentiment_glm_likes_immigration = glm(likes ~ positive*immigration + negative*immigration, 
                                      trump_documents_sentiment,
                    family = 'quasipoisson')
summary(sentiment_glm_likes_immigration)
interactions::interact_plot(sentiment_glm_likes_immigration, 
                            pred = 'positive', modx = 'immigration')


####PREDICTING LIKES: SENTIMENTS*FOREIGNAFFAIRS(TOPIC)
sentiment_glm_likes_foreignaffairs = glm(likes ~ positive*foreign_affairs + negative*foreign_affairs, 
                                         trump_documents_sentiment,
                                      family = 'quasipoisson')
summary(sentiment_glm_likes_foreignaffairs)

interactions::interact_plot(sentiment_glm_likes_foreignaffairs, 
                            pred = 'positive', modx = 'foreign_affairs')



####PREDICTING LIKES: SENTIMENTS*ELECTION(TOPIC)
sentiment_glm_likes_election = glm(likes ~ positive*election + negative*election, 
                                         trump_documents_sentiment,
                                         family = 'quasipoisson')
summary(sentiment_glm_likes_election)
interactions::interact_plot(sentiment_glm_likes_election, 
                            pred = 'positive', modx = 'election')




####PREDICTING LIKES: SENTIMENTS*TAXES(TOPIC)
sentiment_glm_likes_taxes = glm(likes ~ positive*taxes + negative*taxes, 
                                   trump_documents_sentiment,
                                   family = 'quasipoisson')
summary(sentiment_glm_likes_taxes)
interactions::interact_plot(sentiment_glm_likes_taxes, 
                            pred = 'positive', modx = 'taxes')



######################## POSITIVE COVERAGE OF NATIONALISM IS LIKED ON TWITTER #############################
####PREDICTING LIKES: SENTIMENTS*NATIONALISM(TOPIC)
sentiment_glm_likes_nationalism = glm(likes ~ positive*nationalism + negative*nationalism, 
                                trump_documents_sentiment,
                                family = 'quasipoisson')
summary(sentiment_glm_likes_nationalism)
interactions::interact_plot(sentiment_glm_likes_nationalism, 
                            pred = 'positive', modx = 'nationalism')

sentiment_glm_likes_nationalism %>%
  ggeffects::ggpredict(., c('positive', 'nationalism [.1, .5, .9]')) %>%
  data.frame() %>%
  ggplot(.) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = .3) +
  geom_line(aes(x = x, y = predicted, group = group, linetype = group)) +
  theme_minimal() + 
  guides(fill = guide_legend('Nationalism'),linetype = guide_legend('Nationalism')) + 
  labs(x = 'Positive word count', y = 'Like count')




####PREDICTING RETWEETSS: SENTIMENTS*IMMIGRATION(TOPIC)
sentiment_glm_retweets_immigration = glm(retweets ~ positive*immigration + negative*immigration, 
                                      trump_documents_sentiment,
                                      family = 'quasipoisson')
summary(sentiment_glm_retweets_immigration)
interactions::interact_plot(sentiment_glm_retweets_immigration, 
                            pred = 'positive', modx = 'immigration')


####PREDICTING RETWEETSS: SENTIMENTS*FOREIGNAFFAIRS(TOPIC)
sentiment_glm_retweets_foreignaffairs = glm(retweets ~ positive*foreign_affairs + negative*foreign_affairs, 
                                         trump_documents_sentiment,
                                         family = 'quasipoisson')
summary(sentiment_glm_retweets_foreignaffairs)

interactions::interact_plot(sentiment_glm_retweets_foreignaffairs, 
                            pred = 'positive', modx = 'foreign_affairs')

####PREDICTING RETWEETSS: SENTIMENTS*ELECTION(TOPIC)
sentiment_glm_retweets_election = glm(retweets ~ positive*election + negative*election, 
                                   trump_documents_sentiment,
                                   family = 'quasipoisson')
summary(sentiment_glm_retweets_election)
interactions::interact_plot(sentiment_glm_retweets_election, 
                            pred = 'positive', modx = 'election')


####PREDICTING RETWEETSS: SENTIMENTS*TAXES(TOPIC)
sentiment_glm_retweets_taxes = glm(retweets ~ positive*taxes + negative*taxes, 
                                trump_documents_sentiment,
                                family = 'quasipoisson')
summary(sentiment_glm_retweets_taxes)
interactions::interact_plot(sentiment_glm_retweets_taxes, 
                            pred = 'positive', modx = 'taxes')


####PREDICTING RETWEETS: SENTIMENTS*NATIONALISM(TOPIC)
sentiment_glm_retweets_nationalism = glm(retweets ~ positive*nationalism + negative*nationalism, 
                                      trump_documents_sentiment,
                                      family = 'quasipoisson')
summary(sentiment_glm_retweets_nationalism)
interactions::interact_plot(sentiment_glm_retweets_nationalism, 
                            pred = 'positive', modx = 'nationalism')



trump_approval<-read.csv("https://projects.fivethirtyeight.com/trump-approval-data/approval_topline.csv")

trump_approval = trump_approval %>%
  mutate(date = levels(modeldate)[as.numeric(modeldate)],
         date_lubridate = as.Date(date, format = "%m/%d/%Y"))
trump_approval %>%
  ggplot(aes(x = date_lubridate, y = approve_estimate, color = subgroup, fill = subgroup)) + 
  geom_line() + theme_minimal()

trump_approval %>%
  ggplot(aes(x = date_lubridate, y = disapprove_estimate, color = subgroup, fill = subgroup)) + 
  geom_line() + theme_minimal()

trump_documents_sentiment

trump_approval = trump_approval %>%
  filter(subgroup == 'All polls')

trump_documents_sentiment = trump_documents_sentiment %>%
  group_by(date) %>%
  mutate(approval = trump_approval$approve_estimate[trump_approval$date_lubridate == date],
         approval_hi = trump_approval$approve_hi[trump_approval$date_lubridate == date],
         approval_lo = trump_approval$approve_lo[trump_approval$date_lubridate == date],
         disapproval = trump_approval$disapprove_estimate[trump_approval$date_lubridate == date],
         disapproval_hi = trump_approval$disapprove_hi[trump_approval$date_lubridate == date],
         disapproval_lo = trump_approval$disapprove_lo[trump_approval$date_lubridate == date])

trump_approval %>%
  ggplot(aes(x = date_lubridate, y = disapprove_estimate)) + 
  geom_ribbon(aes(x = date_lubridate, ymin = disapprove_lo, ymax = disapprove_hi), 
              fill = 'red3', alpha = .3) +
  geom_line(color = 'red3') + 
  geom_ribbon(aes(x = date_lubridate, ymin = approve_lo, ymax = approve_hi), 
              fill = 'dodgerblue', alpha = .3) +
  geom_line(aes(x = date_lubridate, y = approve_estimate),color = 'dodgerblue') +
theme_minimal() + facet_wrap(.~subgroup)

trump_documents_sentiment %>%
  ggplot(.) +
  geom_ribbon(aes(x = date, ymin = disapproval_lo, ymax = disapproval_hi), 
              fill = 'red3', alpha = .3) +
    geom_line(aes(x = date, y = disapproval), color = 'red3') +
  #geom_line(aes(x = date, y = negative)) +
  geom_line(aes(x = date, y = immigration))

#####PREDICTING DISAPPROVAL ESTIMATES FROM SENTIMENTS
disapproval_sentiment_lm = lm(disapproval ~ negative + positive, trump_documents_sentiment)
summary(disapproval_sentiment_lm)

ggeffects::ggpredict(disapproval_sentiment_lm, c('positive')) %>%
  data.frame() %>%
  ggplot(.) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high),alpha = .5) +
  geom_line(aes(x = x, y = predicted)) + theme_minimal() + 
  labs(x = 'Number of positive words in daily tweets', y = 'Disapproval estimate')
  

approval_sentiment_lm = lm(approval ~ negative + positive, trump_documents_sentiment)
summary(approval_sentiment_lm)

ggeffects::ggpredict(approval_sentiment_lm, c('positive')) %>%
  data.frame() %>%
  ggplot(.) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high),alpha = .5) +
  geom_line(aes(x = x, y = predicted)) + theme_minimal() + 
  labs(x = 'Number of positive words in daily tweets', y = 'Approval estimate')


#####PREDICTING DISAPPROVAL ESTIMATES FROM TOPICS
disapproval_topics_lm = 
  lm(disapproval ~ immigration + taxes + election + 
       nationalism + foreign_affairs, trump_documents_sentiment)
summary(disapproval_topics_lm)

ggeffects::ggpredict(disapproval_topics_lm, c('nationalism')) %>%
  data.frame() %>%
  ggplot(.) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high),alpha = .5) +
  geom_line(aes(x = x, y = predicted)) + theme_minimal() + 
  labs(x = 'Increased coverage of Nationalism in tweets', y = 'Disapproval estimate')

ggeffects::ggpredict(disapproval_topics_lm, c('immigration')) %>%
  data.frame() %>%
  ggplot(.) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high),alpha = .5) +
  geom_line(aes(x = x, y = predicted)) + theme_minimal() + 
  labs(x = 'Increased coverage of Immigration in tweets', y = 'Disapproval estimate')



#####PREDICTING approval ESTIMATES FROM TOPICS
approval_topics_lm = 
  lm(approval ~ immigration + taxes + election + 
       nationalism + foreign_affairs, trump_documents_sentiment)
summary(approval_topics_lm)

ggeffects::ggpredict(approval_topics_lm, c('nationalism')) %>%
  data.frame() %>%
  ggplot(.) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high),alpha = .5) +
  geom_line(aes(x = x, y = predicted)) + theme_minimal() + 
  labs(x = 'Increased coverage of Nationalism in tweets', y = 'approval estimate')

ggeffects::ggpredict(approval_topics_lm, c('immigration')) %>%
  data.frame() %>%
  ggplot(.) + 
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high),alpha = .5) +
  geom_line(aes(x = x, y = predicted)) + theme_minimal() + 
  labs(x = 'Increased coverage of Immigration in tweets', y = 'approval estimate')



######################## POSITIVE COVERAGE OF NATIONALISM LEADS TO WORSE APPROVAL RATINGS #############################
####PREDICTING APPROVAL ESTIMATES FROM TOPICS*SENTIMENTS
approval_topics_sentiment_lm = 
  lm(approval ~ nationalism*positive + nationalism*negative + immigration + taxes + election + 
      foreign_affairs, trump_documents_sentiment)
summary(approval_topics_sentiment_lm)

interactions::interact_plot(approval_topics_sentiment_lm,
                            pred = 'positive', modx = 'nationalism', interval = T)


disapproval_topics_sentiment_lm = 
  lm(disapproval ~ nationalism*positive + nationalism*negative +
       immigration + taxes + election + 
       foreign_affairs, trump_documents_sentiment)
summary(disapproval_topics_sentiment_lm)

interactions::interact_plot(disapproval_topics_sentiment_lm,
                            pred = 'positive', modx = 'nationalism', interval = T)

disapproval_topics_sentiment_lm %>%
  ggeffects::ggpredict(., c('positive', 'nationalism [.1, .5, .9]')) %>%
    data.frame() %>%
    ggplot(.) +
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = .3) +
    geom_line(aes(x = x, y = predicted, group = group, linetype = group)) +
    theme_minimal() + 
    guides(fill = guide_legend('Nationalism'),linetype = guide_legend('Nationalism')) + 
    labs(x = 'Positive word count', y = 'Disapproval Rating')



# 
# trump_documents_sentiment %>%
#   ggplot(.) +
#   geom_line(aes(x = date, y = nationalism*100), alpha = .5)+
#   geom_ribbon(aes(x = date, ymin = disapproval_lo, ymax = disapproval_hi), 
#               fill = 'red3', alpha = .3) +
#   geom_line(aes(x = date, y = disapproval), color = 'red3') + 
#   labs(x = 'Date in day', y = 'Disapproval Percent') + 
#   scale_y_continuous(sec.axis = sec_axis(~.*.01, name = "Tweet loadings on Nationalism"))
#   



  #geom_line(aes(x = date, y = negative)) 
