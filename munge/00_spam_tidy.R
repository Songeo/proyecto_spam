# library(ProjectTemplate)
# reload.project()
library(ProjectTemplate)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(tm)

df.spam <- read_csv("data/Emails.Spam.csv") %>% 
  dplyr::select(spam, text) %>% 
  rownames_to_column("document")

df.spam$text[[1]]
df.spam %>% head
df.spam %>% dim # 5728
df.spam$spam %>% table %>% prop.table() # desbalance (24%)

cache("df.spam")



# tidy form data
df.spam.tidy <- df.spam %>% 
  # tokenizing per word
  unnest_tokens(word, text, token = "words") %>% 
  # remove stopwords from snowball
  anti_join(stop_words %>%
              filter(lexicon == "snowball"),
            by = "word") %>% 
  # remove numbers
  mutate(numbers = parse_number(word)) %>% 
  filter(is.na(numbers)) %>% 
  dplyr::select(-numbers)
df.spam.tidy # 819,333
df.spam.tidy %>% dim
df.spam.tidy$document %>% n_distinct() # 5728
cache("df.spam.tidy")


# tf-idf transformation
tab.spam.idf <- df.spam.tidy  %>% 
  # frequency per word per document
  count(document, word, sort = TRUE) %>%
  ungroup %>% 
  group_by(document) %>% 
  # frequency of words per document
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  # tf and idf calculation
  bind_tf_idf(word, document, n)
cache("tab.spam.idf")


# remove some sparse words
tab.spam.rsparse <- tab.spam.idf %>% 
  ungroup %>% 
  cast_dtm(document = document, term = word, value = n) %>% 
  removeSparseTerms(x = ., sparse = 0.95) %>% 
  tidy() %>% 
  bind_tf_idf(term, document, count) %>% 
  group_by(document) %>% 
  # frequency of words per document
  mutate(total = sum(count)) %>% 
  ungroup() %>% 
  left_join( df.spam.tidy %>% 
               dplyr::select(document, spam) %>% 
               unique,
             by = "document")
tab.spam.rsparse
tab.spam.rsparse$document %>% n_distinct() # 5728
tab.spam.rsparse$term %>% n_distinct()
tab.spam.rsparse$total %>% mean # 62.34

cache("tab.spam.rsparse")
tab.spam.rsparse



