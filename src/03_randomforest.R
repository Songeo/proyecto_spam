
library(ProjectTemplate)
load.project()

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
formu <- paste("spam ~", paste0(names(df.train)[-length(names(df.train))], 
                                collapse = " + ")) %>% 
  as.formula()

rf.mod <- randomForest(formula = spam ~ ., 
                       data = df.train, #type = "classification",
                       ntree=3000,
                       nodesize=25,
                       importance=TRUE,
                       keep.inbag=TRUE)


table(predict(rf.mod, type = "response"), 
      df.train$spam)

# entrenamiento
tab <- table(predict(rf.mod, newdata = df.test), 
             df.test$spam) 
tab
prop.table(tab, 1) 
prop.table(tab, 2) 
