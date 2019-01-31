imdbMovies <-
  read.csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv",
    stringsAsFactors = FALSE
  )
str(imdbMovies)

library(tidyverse)

imdbMovies %>%
  head()

library(lubridate)

imdbMovies <- imdbMovies %>%
  mutate(date = date(date)) %>%
  mutate(year = year(date), month = month(date))

imdbMovies %>%
  ggplot() +
  geom_histogram(aes(av_rating)) +
  facet_wrap( ~ year)

imdbMovies %>%
  ggplot(aes(date, av_rating)) +
  geom_point(aes(color = as.factor(month), alpha = 0.5)) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ month)

library(tidytext)

imdbMovieTidy <- imdbMovies %>%
  unnest_tokens(genre, genres)

View(imdbMovieTidy)

imdbMovieTidy %>%
  count(genre, sort = TRUE) %>%
  mutate(genre = as.factor(genre), genre = fct_reorder(genre, n)) %>%
  ggplot(aes(genre, n + 1)) +
  geom_col(aes(fill = genre)) +
  scale_y_log10() +
  coord_flip()

imdbMovieTidy %>%
  mutate(genre = as.factor(genre)) %>%
  group_by(genre) %>%
  summarise(count = n(), medianRating = median(av_rating)) %>%
  mutate(genre = fct_reorder(genre, medianRating)) %>%
  ggplot(aes(genre, medianRating)) +
  geom_col(aes(fill = genre, alpha = 0.8)) +
  coord_flip() +
  guides(alpha = FALSE)


names(imdbMovieTidy)

imdbMovies %>%
  mutate(month = as.factor(month)) %>%
  group_by(month) %>%
  summarise(count = n(), medianRating = median(av_rating)) %>%
  mutate(month = fct_reorder(month, count)) %>%
  ggplot(aes(month, count)) +
  geom_col(aes(fill = month)) +
  coord_flip()

imdbMovieTidy %>%
  mutate(month = as.factor(month)) %>%
  ggplot(aes(month, av_rating)) +
  geom_boxplot()

summary(lm(av_rating ~ . - titleId, data = imdbMovieTidy))

names(imdbMovieTidy)


movieRatingLM <- function(df) {
  lm(av_rating ~ seasonNumber + year + month + share -1, data = df)
}


imdbMovieNested <- imdbMovieTidy %>%
  group_by(titleId) %>%
  nest() %>%
  mutate(model = map(data, movieRatingLM))

imdbMovieNested %>% 
  mutate(tidyLM = model %>% map(tidy), glanceLM = model %>% map(glance), rsq = glanceLM %>%  map_dbl("r.squared")) %>% 
  unnest(tidyLM) %>% 
  ggplot(aes(term, rsq)) + 
  geom_jitter(aes(color = p.value, alpha = 0.5)) + 
  coord_flip() + 
  guides(alpha = FALSE)

imdbMovieTidy %>% 
  mutate(title = as.factor(title)) %>% 
  group_by(title) %>% 
  summarise(count = n(), medianRating = median(av_rating)) %>% 
  arrange(desc(medianRating)) %>% 
  top_n(20) %>% 
  mutate(title = fct_reorder(title, medianRating)) %>% 
  ggplot(aes(title, medianRating)) + 
  geom_col(aes(fill = title)) + 
  coord_flip() +
  guides(fill = FALSE)

imdbMovieTidy %>% 
  group_by(genre) %>% 
  summarise(count = n(), medianRating = median(av_rating)) %>% 
  ggplot(aes(genre, medianRating)) + 
  geom_point(aes(size = count, color = genre)) +
  geom_line(group = 1, aes(size = 1)) + 
  coord_flip()


