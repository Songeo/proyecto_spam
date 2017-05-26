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

table(df.train$spam, preds.train)
table(df.train$spam, preds.train) %>% prop.table(2)
verification::roc.plot(x = as.numeric(as.character(df.train$spam)),
                       pred = as.numeric(as.character(preds.train)) )


# Test results
preds.test <- predict(object = svm.mod, newdata = df.test)
preds.test %>% head

table(df.test$spam, preds.test)
table(df.test$spam, preds.test) %>% prop.table(2)

verification::roc.plot(x = as.numeric(as.character(df.test$spam)),
                       pred = as.numeric(as.character(preds.test)) )


# Perform a grid search
tuneResult <- tune(svm, spam~., data = df.train,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult) # best performance: MSE = 8.371412, RMSE = 2.89 epsilon 1e-04 cost 4
# Draw the tuning graph
plot(tuneResult)