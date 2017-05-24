library(ProjectTemplate)
load.project()

source("munge/00_spam_tidy.R")

# Wordclouds total
tab <- df.spam.tidy %>% 
  group_by(word) %>% 
  tally %>% 
  ungroup() 
wordcloud(words = tab$word, freq = tab$n, max.words = 90)


# Wordclouds by spam
tab <- df.spam.tidy %>% 
  group_by(word, spam) %>% 
  tally %>% 
  ungroup()

tab.spam <- tab %>% filter(spam == 1)
wordcloud(words = tab.spam$word, freq = tab.spam$n, max.words =50)

tab.nspam <- tab %>% filter(spam == 0)
wordcloud(words = tab.nspam$word, freq = tab.nspam$n, max.words =50)

# Wordclouds preprocessed total
tab <- tab.spam.rsparse %>% 
  group_by(term) %>% 
  summarise(n = sum(count))
wordcloud(words = tab$term, freq = tab$n, max.words = 90)

# Wordclouds preprocessed spam
tab <- tab.spam.rsparse %>% 
  group_by(term, spam) %>% 
  summarise(n = sum(count)) %>% 
  ungroup

tab.spam <- tab %>% filter(spam == 1)
wordcloud(words = tab.spam$term, freq = tab.spam$n, max.words =60)

tab.nspam <- tab %>% filter(spam == 0)
wordcloud(words = tab.nspam$term, freq = tab.nspam$n, max.words = 60)


