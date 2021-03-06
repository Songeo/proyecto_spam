
library(ProjectTemplate)
reload.project()

load("cache/dtm_train.RData")

library(randomForest)


# ............................................................................ # 

# df train test
df.train <- dtm.df[indicetrain, ] %>% 
  dplyr::select(-document) %>% #, -n_words) 
  mutate(spam = factor(spam))

df.test <- dtm.df[-indicetrain, ] %>% 
  dplyr::select(-document) %>% #, -n_words) 
  mutate(spam = factor(spam))

names(df.train)[1:281] <- paste0("x", 1:281)
names(df.test)[1:281] <- paste0("x", 1:281)


# ............................................................................ # 

# Random Forest Classification

rf.mod <- randomForest(formula = spam ~ ., 
                       data = df.train, #type = "classification",
                       ntree = 4000,
                       nodesize = 25,
                       importance = TRUE,
                       keep.inbag = TRUE)
cache("rf.mod")

# entrenamiento
preds.train <- predict(rf.mod)
preds.train %>% head

(tab.acc.train <- table(df.train$spam ==  preds.train))
(tab.conf.train <- table(df.train$spam, preds.train))

# roc train
ggroc.train <- CRoc_GG(obs = as.numeric(as.character(df.train$spam)),
                       pred = as.numeric(as.character(preds.train)),
                       mod.tit = "Bosque Aleatorio (Entrenamiento)")
ggroc.train


# ............................................................................ # 

# Test results
preds.test <- predict(rf.mod, newdata = df.test)
preds.test %>% head

(tab.acc.test <- table(df.test$spam ==  preds.test))
(tab.conf.test <- table(df.test$spam, preds.test))

ggroc.test <- CRoc_GG(obs = as.numeric(as.character(df.test$spam)),
                      pred = as.numeric(as.character(preds.test)),
                      mod.tit = "Bosque Aleatorio (Prueba)")
ggroc.test



# ............................................................................ # 

# Saving parameters

results.rf <- list(tab.acc.train, tab.conf.train, ggroc.train,
                    tab.acc.test, tab.conf.test, ggroc.test)

save(results.rf, file = "cache/results_models/results_rforest.Rdata")




# Para devianza
preds.prob.test <- predict(rf.mod, newdata = df.test, type = "prob")[, 2]
preds.resp.test <- predict(rf.mod, newdata = df.test, type = "response") 

test.results.rf <- list(prob = preds.prob.test, 
     resp = preds.resp.test,
     obs = df.test$spam)
save(test.results.rf, file = "cache/results_models/test_results_rforest.Rdata")
