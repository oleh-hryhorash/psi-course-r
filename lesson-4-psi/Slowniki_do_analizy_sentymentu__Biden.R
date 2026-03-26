# Analiza sentymentu

library(tm)
library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)
library(ggthemes)

# Wczytanie danych tekstowych
text <- readLines("Trump - State of the Union_25.02.2026.txt", encoding="UTF-8", warn = FALSE)
docs <- VCorpus(VectorSource(text))
tdm <- TermDocumentMatrix(docs)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing=TRUE)

tokeny <- data.frame(Review = names(v), freq = v, stringsAsFactors = FALSE)
tokeny_data <- as_tibble(tokeny)

# Tokenizacja
tidy_tokeny <- tokeny_data %>%
  unnest_tokens(word, Review)

# Usuwanie stopwords
tidy_tokeny2 <- tokeny_data %>%
  unnest_tokens(word, Review) %>%
  anti_join(stop_words)

# ================= Loughran =================

sentiment_review <- tidy_tokeny %>%
  inner_join(get_sentiments("loughran"), relationship = "many-to-many")

sentiment_review2 <- sentiment_review %>%
  filter(sentiment %in% c("positive", "negative"))

word_counts <- sentiment_review2 %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word2 = fct_reorder(word, n))

ggplot(word_counts, aes(x=word2, y=n, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Loughran")

# ================= NRC =================

sentiment_review_nrc <- tidy_tokeny %>%
  inner_join(get_sentiments("nrc"), relationship = "many-to-many")

sentiment_review_nrc2 <- sentiment_review_nrc %>%
  filter(sentiment %in% c("positive", "negative"))

word_counts_nrc2 <- sentiment_review_nrc2 %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word2 = fct_reorder(word, n))

ggplot(word_counts_nrc2, aes(x=word2, y=n, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("NRC")

# ================= Bing =================

sentiment_review_bing <- tidy_tokeny %>%
  inner_join(get_sentiments("bing"))

sentiment_review_bing2 <- sentiment_review_bing %>%
  filter(sentiment %in% c("positive", "negative"))

word_counts_bing2 <- sentiment_review_bing2 %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word2 = fct_reorder(word, n))

ggplot(word_counts_bing2, aes(x=word2, y=n, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Bing")

# ================= AFINN =================

sentiment_review_afinn <- tidy_tokeny %>%
  inner_join(get_sentiments("afinn"))

sentiment_review_afinn3 <- sentiment_review_afinn %>%
  filter(value %in% c(-5, -4, -3, 3, 4, 5))

word_counts_afinn3 <- sentiment_review_afinn3 %>%
  count(word, value) %>%
  group_by(value) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word2 = fct_reorder(word, n))

ggplot(word_counts_afinn3, aes(x=word2, y=n, fill=value)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~value, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("AFINN")