library(ProjectTemplate)
reload.project()

load("cache/dtm_train.RData")
#load("cache/df.train.test.RData")

library(glmnet)
library(arm)


# ............................................................................ # 

# df train test
df.train <- dtm.df[indicetrain, ] %>% 
  dplyr::select(-document) %>% 
  mutate(spam = factor(spam))
df.test <- dtm.df[-indicetrain, ] %>% 
  dplyr::select(-document) %>% 
  mutate(spam = factor(spam))



# ............................................................................ # 

# Logistic Regression

logit.mod <- glm(formula = spam~., data = df.train, 
                 family = binomial(link = "probit"),
                 maxit = 500)

# logit.mod <- bayesglm(formula = spam~., data = df.train, 
#                  family = binomial(link = "logit"))


gg.dens <- data.frame(preds = predict(logit.mod, type='link'),
           spam =df.train$spam) %>% 
  ggplot(aes(x = preds, color = spam)) + 
  geom_density() + 
  xlab("Prediccion Link") + 
  ggtitle("Distribución de Predicción Link",
          "Regresión Logística")




# entrenamiento
preds.train <- predict(logit.mod, type = "response") %>% round()
preds.train %>% head

(tab.acc.train <- table(df.train$spam ==  preds.train))
(tab.conf.train <- table(df.train$spam, preds.train))

# roc train
ggroc.train <- CRoc_GG(obs = as.numeric(as.character(df.train$spam)),
                       pred = as.numeric(as.character(preds.train)),
                       mod.tit = "Regresión Logit (Entrenamiento)")
ggroc.train
# ............................................................................ # 

# Test results
preds.test <- predict(logit.mod, newdata = df.test, type = "response") %>% round()
preds.test %>% head

(tab.acc.test <- table(df.test$spam ==  preds.test))
(tab.conf.test <- table(df.test$spam, preds.test))

ggroc.test <- CRoc_GG(obs  = as.numeric(as.character(df.test$spam)),
                      pred = as.numeric(as.character(preds.test)),
                      mod.tit = "Regresión Logit (Prueba)")
ggroc.test



# ............................................................................ # 

# Saving parameters

results.logit <- list(tab.acc.train, tab.conf.train, ggroc.train,
                   tab.acc.test, tab.conf.test, ggroc.test, 
                   gg.dens)

save(results.logit, file = "cache/results_models/results_logit.Rdata")







# ............................................................................ # 
# Otros intentos
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





