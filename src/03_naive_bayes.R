library(ProjectTemplate)
reload.project()

load("cache/dtm_train.RData")

library(e1071)


# ............................................................................ # 

# df train test
df.train <- dtm.df[indicetrain, ] %>% 
  dplyr::select(-document, -n_words) %>% 
  mutate(spam = factor(spam))
df.test <- dtm.df[-indicetrain, ] %>% 
  dplyr::select(-document, -n_words) %>% 
  mutate(spam = factor(spam))


# ............................................................................ # 

# Model
nb.mod <- naiveBayes(formula = spam~., data = df.train)

preds.train <- predict(object = nb.mod, newdata = df.train)
preds.train %>% head


(tab.acc.train <- table(df.train$spam ==  preds.train))
(tab.conf.train <- table(df.train$spam, preds.train))

# roc train
ggroc.train <- CRoc_GG(obs = as.numeric(as.character(df.train$spam)),
        pred = as.numeric(as.character(preds.train)),
        mod.tit = "Bayes Ingenuo (Entrenamiento)")
ggroc.train


# ............................................................................ # 

# Test results
preds.test <- predict(object = nb.mod, newdata = df.test)
preds.test %>% head

(tab.acc.test <- table(df.test$spam ==  preds.test))
(tab.conf.test <- table(df.test$spam, preds.test))

ggroc.test <- CRoc_GG(obs = as.numeric(as.character(df.test$spam)),
        pred = as.numeric(as.character(preds.test)),
        mod.tit = "Bayes Ingenuo (Prueba)")
ggroc.test



# ............................................................................ # 

# Saving parameters

results.nb <- list(tab.acc.train, tab.conf.train, ggroc.train,
     tab.acc.test, tab.conf.test, ggroc.test)

save(results.nb, file = "cache/results_models/results_nb.Rdata")
