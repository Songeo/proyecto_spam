library(ProjectTemplate)
reload.project()

load("cache/df.train.test.RData")
library(arm)


# ............................................................................ # 

# df train test
df.train <- dtm.df[indicetrain, ] %>% 
  dplyr::select(-document, -n_words)
df.test <- dtm.df[-indicetrain, ] %>% 
  dplyr::select(-document, -n_words)



# ............................................................................ # 

# Logistic Regression
logit.mod <- glm(formula = spam~., data = df.train, family = "binomial")

preds.train <- predict(logit.mod, type = "response") %>% round()
resid.train <- resid(logit.mod, "response")
binnedplot(preds.train, resid.train)

verification::roc.plot(x = df.train$spam,
                       pred = preds.train)#, 
                       # plot = 'both', binormal =T, CI = T, n.boot = 500)
table(df.train$spam, preds.train)
table(df.train$spam, preds.train) %>% prop.table(2)


# Test results
preds.test <- predict(logit.mod, newdata = df.test, type = "response") %>% round()

table(df.test$spam, preds.test)
table(df.test$spam, preds.test) %>% prop.table(2)

verification::roc.plot(x = df.test$spam,
                       pred = preds.test)

