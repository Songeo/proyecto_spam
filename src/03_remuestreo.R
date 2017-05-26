
library(ProjectTemplate)
reload.project()

load("cache/dtm_train.RData")
library(broom)


# ............................................................................ # 

# df train test
df.train <- dtm.df[indicetrain, ] %>% 
  dplyr::select(-document, -n_words)
df.test <- dtm.df[-indicetrain, ] %>% 
  dplyr::select(-document, -n_words)

names(df.train)


# ............................................................................ # 

# logit regression
boot.sample <- 100
formu <- paste("spam ~", paste0(names(df.train)[-length(names(df.train))], 
                                collapse = " + ")) %>% 
  as.formula()

logit.boot <- df.train %>% 
  bootstrap(m = 2) %>% 
  do( tidy(glm(formu, data = ., family = binomial(link = "logit"))) )
head(logit.boot)

