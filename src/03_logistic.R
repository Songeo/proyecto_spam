library(ProjectTemplate)
reload.project()

load("cache/dtm_train.RData")
load("cache/df.train.test.RData")

library(glmnet)
library(arm)


# ............................................................................ # 

# df train test
df.train <- dtm.df[indicetrain, ] %>% 
  dplyr::select(-document, -n_words)
df.test <- dtm.df[-indicetrain, ] %>% 
  dplyr::select(-document, -n_words)

names(df.train)



# ............................................................................ # 

# Logistic Regression
formu <- paste("spam ~", paste0(names(df.train)[-length(names(df.train))], 
                                collapse = " + ")) %>% 
  as.formula()

logit.mod <- glm(formula = spam~., data = df.train, family = "binomial")

preds.train <- predict(logit.mod, type = "response") %>% round()
resid.train <- resid(logit.mod, "response")
binnedplot(preds.train, resid.train)

table(df.train$spam, preds.train)
table(df.train$spam, preds.train) %>% prop.table(2)

verification::roc.plot(x = df.train$spam,
                       pred = preds.train)#, 
                       # plot = 'both', binormal =T, CI = T, n.boot = 500)


# Test results
preds.test <- predict(logit.mod, newdata = df.test, type = "response") %>% round()

table(df.test$spam, preds.test)
table(df.test$spam, preds.test) %>% prop.table(2)

verification::roc.plot(x = df.test$spam,
                       pred = preds.test)

# ............................................................................ # 

# glmnet
glmnet.bin <- glmnet(x = x.train, y = y.train, family = "binomial", alpha = .5)


# ............................................................................ # 

# PCA
pca.train <- prcomp(df.train %>% 
                      dplyr::select(-spam))
summary(pca.train)

# pca train
pca.fit.train <- df.train %>% 
  cbind(pca.train$x %>% scale()) %>% 
  as_tibble() 

# pca test
pca.fit.test <- predict(pca.train, df.test)
df.pca.test <- df.test %>% 
  cbind(pca.fit.test) %>% 
  as_tibble() 


# ............................................................................ # 

# logistic PCA
names(df.train)
formu <- paste("spam ~", paste0("PC", 1:50, collapse = " + ")) %>% 
  as.formula()

mod.pca <- glm(formula = formu, 
               data = pca.fit.train, 
               family = binomial(link = 'logit'))

table(predict(mod.pca, type = "response") %>% round, 
      pca.fit.train$spam)

# entrenamiento
tab <- table(df.pca.test$spam,
             round(predict(mod.pca, df.pca.test, type = "response")))
tab
prop.table(tab) 





