
library(ProjectTemplate)
reload.project()

load("cache/df.spam.RData")
load("cache/tab.spam.rsparse.RData")


# ............................................................................ # 

# Document-Term Matrix
tab.spam.rsparse %>% head

dtm.df <- tab.spam.rsparse %>%
  filter(tf_idf > 0) %>%
  dplyr::select(document, term, tf_idf) %>% 
  spread(term, tf_idf, fill = 0) %>% 
  left_join(
    tab.spam.rsparse %>% 
      group_by(document, spam) %>% 
      dplyr::summarise(n_words = sum(count)), 
    by = "document"
  ) %>% 
  dplyr::select(document, spam, n_words, able:years)
dtm.df  %>% data.frame() %>% head


# ............................................................................ # 

# Train - Test data frame
nrow(dtm.df)

indice1 <- which(dtm.df$spam==1)
length(indice1)

indice0 <- which(dtm.df$spam==0)
length(indice0)


set.seed(105720)
# cerca del 80% es una muestra de entrenamiento
indice1train <- sample(indice1, 1200, replace=F) 
indice0train <- sample(indice0, 1200, replace=F) 
indicetrain <- c(indice1train,indice0train)


y.train <- dtm.df$spam[indicetrain]
y.test <- dtm.df$spam[-indicetrain]

x.train <- dtm.df[indicetrain, ] %>% 
  dplyr::select(-(document:spam))
x.test <- dtm.df[-indicetrain, ] %>% 
  dplyr::select(-(document:spam))

dim(x.train) + dim(x.test)
nrow(dtm.df)

save(y.train, 
     y.test,
     x.train, 
     x.test, file = "cache/df.train.test.RData")

save(indicetrain, 
     dtm.df, file = "cache/dtm_train.RData")

