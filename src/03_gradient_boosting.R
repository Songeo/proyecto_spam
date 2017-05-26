
library(ProjectTemplate)
load.project()

library(randomForest)
library(gbm)


# ............................................................................ # 

# df train test
df.train <- dtm.df[indicetrain, ] %>% 
  dplyr::select(-document) %>% #, -n_words) 
  mutate(spam = factor(spam))

df.test <- dtm.df[-indicetrain, ] %>% 
  dplyr::select(-document) %>% #, -n_words) 
  mutate(spam = factor(spam))

# ............................................................................ # 

num.trees <- 3000

# boosted tree
gbm(formula = Response ~ Var1 + Var2 + Cat5 + Cat6,
    distribution = "bernoulli",
    data = train,
    n.trees = 2500,
    shrinkage = .01,
    n.minobsinnode = 20)



boost.mod <- gbm(spam ~ ., 
                 data = df.train,
                 distribution='bernoulli',
                 n.trees=B,
                 shrinkage=0.01,
                 train.fraction=1.)   


table(predict(boost.mod, type = "response"), 
      df.train$spam)

# entrenamiento
tab <- table(predict(boost.mod, newdata = df.test), 
             df.test$spam) 
tab
prop.table(tab, 1) 
prop.table(tab, 2) 
