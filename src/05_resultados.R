tab <- df.eda %>% 
  group_by(document, spam) %>% 
  dplyr::summarise(n_words = sum(count)) %>% 
  ungroup %>% 
  group_by(spam) %>% 
  dplyr::summarise(q1 = quantile(n_words, .25),
                   `promedio` = mean(n_words),
                   `mediana` = median(n_words),
                   q3 = quantile(n_words, .75)) %>% 
  ungroup() %>% 
  t() %>% 
  round %>% 
  data.frame() 
colnames(tab) <- c("no spam", 'spam')

tab %>% 
  xtable(digits = 0) %>% 
  print(include.rownames = F)



load("cache/results_models/results_logit.Rdata")
cbind(results.logit[[2]], results.logit[[5]])

results.logit[[2]] 
results.logit[[5]] 
