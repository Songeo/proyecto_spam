library(ProjectTemplate)
reload.project()

load("cache/dtm_train.RData")

library(e1071)


# ............................................................................ # 

# df train test
df.train <- dtm.df[indicetrain, ] %>% 
  dplyr::select(-document) %>% 
  mutate(spam = factor(spam))
df.test <- dtm.df[-indicetrain, ] %>% 
  dplyr::select(-document) %>% 
  mutate(spam = factor(spam))


# ............................................................................ # 

# Model
formu <- paste("spam ~", paste0(names(df.train)[-length(names(df.train))], 
                                collapse = " + ")) %>% 
  as.formula()

svm.mod <- svm(formula = spam ~., data = df.train)

preds.train <- predict(object = svm.mod, newdata = df.train)
preds.train %>% head

(tab.acc.train <- table(df.train$spam ==  preds.train))
(tab.conf.train <- table(df.train$spam, preds.train))

# roc train
ggroc.train <- CRoc_GG(obs = as.numeric(as.character(df.train$spam)),
                       pred = as.numeric(as.character(preds.train)),
                       mod.tit = "SVM (Entrenamiento)")
ggroc.train



# ............................................................................ # 

# Test results
preds.test <- predict(object = svm.mod, newdata = df.test)
preds.test %>% head

(tab.acc.test <- table(df.test$spam ==  preds.test))
(tab.conf.test <- table(df.test$spam, preds.test))

ggroc.test <- CRoc_GG(obs = as.numeric(as.character(df.test$spam)),
                      pred = as.numeric(as.character(preds.test)),
                      mod.tit = "SVM (Prueba)")
ggroc.test



# ............................................................................ # 

# Saving parameters

results.svm <- list(tab.acc.train, tab.conf.train, ggroc.train,
                   tab.acc.test, tab.conf.test, ggroc.test)

save(results.svm, file = "cache/results_models/results_svm.Rdata")
