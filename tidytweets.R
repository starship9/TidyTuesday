tweets <- readr::read_rds('tidytuesday_tweets.rds')
#str(tweets)
names(tweets)

library(tidyverse)
library(tidytext)

tweets %>%
  select(text) %>%
  head()

tweetText <- tweets %>%
  select(user_id,
         status_id,
         screen_name,
         text,
         source,
         verified,
         created_at) %>%
  mutate(tweet_id = row_number())

topUsers <- tweetText %>%
  unnest_tokens(word, text, token = "tweets") %>%
  anti_join(stop_words) %>%
  count(screen_name, sort = TRUE) %>%
  top_n(10)

topUsers %>%
  count(screen_name, sort = TRUE)

library(forcats)

tweetText %>%
  unnest_tokens(word, text, token = "tweets") %>%
  anti_join(stop_words) %>%
  inner_join(topUsers) %>%
  group_by(screen_name) %>%
  summarize(tweetNum = n(), avgLen = mean(length(text))) %>%
  mutate(screen_name = as.factor(screen_name)) %>%
  mutate(screen_name = fct_reorder(screen_name, tweetNum)) %>%
  ggplot(aes(screen_name, tweetNum)) + geom_col(aes(fill = screen_name)) + coord_flip() + ggtitle("Top users", subtitle = "According to the total number of words") + xlab("Twitter handle") + ylab("Number of words")


#tweetText %>%
#  mutate(textLen = length(text), screen_name = as.factor(screen_name)) %>%
#  purrr::map(~t.test(.~length$screen_name)$p.value)

tweetText %>%
  inner_join(topUsers) %>%
  select(screen_name, text)

#tweetText %>%
#  mutate(tweetLen = length(text), screen_name = as.factor(screen_name)) %>%
#  split(.$screen_name) %>%
#  map(~lm(tweetLen~screen_name, data = .)) %>%
#  map(summary) %>%
#  map_dbl("r.squared")


tidyTweets <- tweetText %>%
  unnest_tokens(word, text, token = "tweets") %>%
  anti_join(stop_words)

tidyTweets %>%
  mutate(word = as.factor(word)) %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col(aes(fill = word)) + coord_flip() + ggtitle("Most frequently occurring words") + ylab("Frequency")

tidyTweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(screen_name, sentiment, sort = TRUE) %>%
  mutate(screen_name = as.factor(screen_name),
         screen_name = fct_reorder(screen_name, n)) %>%
  top_n(20) %>%
  mutate(n = ifelse(sentiment == "negative",-1 * n, n)) %>%
  mutate(sentiment = as.factor(sentiment)) %>%
  ggplot(aes(screen_name, n, fill = sentiment)) + geom_col(position = "identity") + coord_flip() + ggtitle("Top users visualized according to sentiment") + xlab("Twitter handle")


library(lubridate)

tidyTweets %>%
  select(created_at) %>%
  mutate(hour = hour(created_at)) %>%
  ggplot(aes(hour)) + geom_histogram()

tidyTweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  mutate(sentiment = as.factor(sentiment), word = as.factor(word)) %>%
  top_n(30) %>%
  mutate(n = ifelse(sentiment == "negative",-1 * n, n),
         word = fct_reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + geom_col() + coord_flip() + ggtitle("Frequest words divided across sentiments", subtitle = "Plot should not be treated as negative") + ylab("Word count")

library(stringr)
library(ggrepel)

tweetText %>%
  unnest_tokens(word, text, token = "tweets") %>%
  anti_join(stop_words) %>%
  inner_join(topUsers) %>%
  count(screen_name, word, sort = TRUE) %>%
  bind_tf_idf(word, screen_name, nn) %>%
  filter(
    !str_detect(word, "https"),
    !str_detect(word, ".U0001*"),
    !str_detect(word, "t.co"),
    !str_detect(word, "tidytuesday"),
    !str_detect(word, "data"),
    !str_detect(word, "rstats")
  ) %>%
  arrange(desc(nn), desc(tf_idf)) %>%
  top_n(20) %>%
  ggplot(aes(screen_name, tf_idf, color = screen_name)) + geom_point() + geom_label_repel(aes(label = word)) +  coord_flip() + ggtitle("Top words for the top 10 users", subtitle = "The tf-idf metric is used") + xlab("Twitter handle")

tweetText %>%
  unnest_tokens(word, text, token = "tweets") %>%
  anti_join(stop_words) %>%
  inner_join(topUsers) %>%
  count(screen_name, word, sort = TRUE) %>%
  mutate(screen_name = as.factor(screen_name), word = as.factor(word)) %>%
  mutate(word = fct_reorder(word, nn)) %>%
  group_by(screen_name) %>%
  top_n(5, wt = nn) %>%
  ungroup %>%
  ggplot(aes(word, nn, fill = screen_name)) + geom_col() + facet_wrap( ~ screen_name, scales = "free", ncol = 2) + coord_flip() + ggtitle("Frequently occurring words per user", subtitle = "Some plots have more rows due to ties") + ylab("Word count")



tweetText %>%
  unnest_tokens(word, text, token = "tweets") %>%
  anti_join(stop_words) %>%
  inner_join(topUsers) %>%
  count(screen_name, word, sort = TRUE) %>%
  bind_tf_idf(word, screen_name, nn) %>%
  mutate(word = factor(word, levels = unique(word))) %>%
  mutate(word = fct_reorder(word, tf_idf)) %>% group_by(screen_name) %>%
  top_n(5, wt = tf_idf) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = screen_name)) + geom_col()  + facet_wrap( ~ screen_name, scales = "free", ncol = 2) + coord_flip() + ggtitle("Words more specific to each user", subtitle = "Some plots have more rows due to ties")

library(stm)

tweetSparse <- tweetText %>%
  unnest_tokens(word, text, token = "tweets") %>%
  anti_join(stop_words) %>%
  inner_join(topUsers) %>%
  count(screen_name, word, sort = TRUE) %>%
  cast_sparse(screen_name, word, nn)


#Probably does not make a lot of sense since most of these tweets are going to be tidytuesday related anyway
topicModel <-
  stm(tweetSparse,
      K = 3,
      verbose = FALSE,
      init.type = "Spectral")
class(topicModel)
class(tweetSparse)

library(broom)

tdBeta <- tidy(topicModel)

tdBeta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = fct_reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap( ~ topic, scales = "free") +
  coord_flip() +
  labs(
    x = NULL,
    y = expression(beta),
    title = "Highest word probabilities for each topic",
    subtitle = "Different words are associated with different topics"
  )

library(widyr)
library(igraph)
library(ggraph)
library(scales)


topCors <- tweetText %>%
  unnest_tokens(word, text, token = "tweets") %>%
  anti_join(stop_words) %>%
  inner_join(topUsers) %>%
  select(word, tweet_id, screen_name) %>%
  filter(!str_detect(word, "http")) %>%
  pairwise_cor(word, tweet_id, sort = TRUE) %>%
  head(300)

head(topCors)

vertices <- tweetText %>%
  unnest_tokens(word, text, token = "tweets") %>%
  anti_join(stop_words) %>%
  inner_join(topUsers) %>%
  filter(word %in% topCors$item1 | word %in% topCors$item2) %>%
  count(word) %>%
  filter(!str_detect(word, "http"))

head(vertices)
tail(vertices)

set.seed(100)
topCors %>%
  graph_from_data_frame(vertices = vertices) %>%
  ggraph() +
  geom_edge_link() +
  geom_node_point(aes(color = nn)) +
  geom_node_text(aes(label = name), repel = TRUE)
