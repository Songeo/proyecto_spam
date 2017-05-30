
library(ProjectTemplate)
reload.project()

load("cache/dtm_train.RData")

library(gbm)


# ............................................................................ # 

# df train test
df.train <- dtm.df[indicetrain, ] %>% 
  dplyr::select(-document) #%>% #, -n_words) 
  # mutate(spam = factor(spam))

df.test <- dtm.df[-indicetrain, ] %>% 
  dplyr::select(-document) #%>% #, -n_words) 
  # mutate(spam = factor(spam))

# ............................................................................ # 

# boosted tree
trees.num <- 4000

set.seed(105720)
boost.mod <- gbm(spam ~ ., 
                 data = df.train,
                 # cv.folds = 3,
                 distribution='bernoulli',
                 n.trees = trees.num,
                 shrinkage = 0.01,
                 train.fraction = 1.)   
gbm.perf(boost.mod)



# entrenamiento
preds.train <- predict.gbm(boost.mod, 
                           newdata = df.train, 
                           n.trees = trees.num, 
                           type="response") %>% 
  round
preds.train %>% head

(tab.acc.train <- table(df.train$spam ==  preds.train))
(tab.conf.train <- table(df.train$spam, preds.train))

# roc train
ggroc.train <- CRoc_GG(obs = df.train$spam,
                       pred = preds.train,
                       mod.tit = "Gradient Boosting (Entrenamiento)")
ggroc.train


# ............................................................................ # 

# Test results
preds.test <- predict.gbm(boost.mod, 
                          newdata = df.test, 
                          n.trees = trees.num, 
                          type="response") %>% 
  round
preds.test %>% head

(tab.acc.test <- table(df.test$spam ==  preds.test))
(tab.conf.test <- table(df.test$spam, preds.test))

ggroc.test <- CRoc_GG(obs  = df.test$spam,
                      pred = preds.test,
                      mod.tit = "Gradient Boosting (Prueba)")
ggroc.test



# ............................................................................ # 

# Saving parameters

results.gb <- list(tab.acc.train, tab.conf.train, ggroc.train,
                   tab.acc.test, tab.conf.test, ggroc.test)

save(results.gb, file = "cache/results_models/results_gboost.Rdata")
