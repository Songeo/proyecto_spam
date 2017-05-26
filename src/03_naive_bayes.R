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

table(df.train$spam, preds.train)
table(df.train$spam, preds.train) %>% prop.table(2)
verification::roc.plot(x = df.train$spam,
                       pred = preds.train,
                       plot = 'both', binormal =T, CI = T, n.boot = 500)



# Test results
preds.test <- predict(object = nb.mod, newdata = df.test)
preds.test %>% head

table(df.test$spam, preds.test)
table(df.test$spam, preds.test) %>% prop.table(2)

verification::roc.plot(x = df.test$spam,
                       pred = preds.test)
