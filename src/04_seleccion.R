
library(ProjectTemplate)
reload.project()

library(arm)
library(e1071)
library(randomForest)
library(gbm)
library(xtable)


# Loading results
load(file = "cache/results_models/results_logit.Rdata")
load(file = "cache/results_models/results_svm.Rdata")
load(file = "cache/results_models/results_rforest.Rdata")
load(file = "cache/results_models/results_gboost.Rdata")


# 1. Matriz ed confusión 
# Muestra de Prueba
list.results <- list(`logística` = results.logit,
                     `SVM` = results.svm,
                     `bosque aleatorio` = results.rf,
                     `gradient boosting` = results.gb)

sapply(names(list.results), function(nom.elem){
  tab.test <- list.results[[nom.elem]][[5]]
  
  tp <- tab.test[2, 2]
  tn <- tab.test[1, 1]
  fp <- tab.test[1, 2]
  fn <- tab.test[2, 1]
  
  accuracy <- 100*(tp + tn)/sum(tab.test)
  tpr <- 100*(tp)/(tp + fn)
  tnr <- 100*(tn)/(tn + fp)
  precision <- 100*(tp)/(tp + fp)
  nvp <- 100*(tn)/(tn + fn) 
  fpr <- 100*(fp)/(fp + tn) 
  fnr <- 100*(fn)/(tp + fn) 
  fdr <- 100*(fp)/(tp + fp) 
  
  c(exactitud = accuracy, 
    `tasa verdadero positivo` =tpr,
    `tasa verdadero negativo` = tnr, 
    `tasa falso positivo` = fpr,
    `tasa falso negativo` = fnr,
    precisión = precision) %>% 
    round()
  }) %>% 
  data.frame(check.names = F) %>% 
  rownames_to_column("medición") %>% 
  xtable(., digits = 0, align = "rr|cccc") %>% 
  print(include.rownames = F)


# Muestra de Entrenamiento
list.results <- list(`logística` = results.logit,
                     `SVM` = results.svm,
                     `bosque aleatorio` = results.rf,
                     `gradient boosting` = results.gb)

sapply(names(list.results), function(nom.elem){
  tab.test <- list.results[[nom.elem]][[2]]
  
  tp <- tab.test[2, 2]
  tn <- tab.test[1, 1]
  fp <- tab.test[1, 2]
  fn <- tab.test[2, 1]
  
  accuracy <- 100*(tp + tn)/sum(tab.test)
  tpr <- 100*(tp)/(tp + fn)
  tnr <- 100*(tn)/(tn + fp)
  precision <- 100*(tp)/(tp + fp)
  nvp <- 100*(tn)/(tn + fn) 
  fpr <- 100*(fp)/(fp + tn) 
  fnr <- 100*(fn)/(tp + fn) 
  fdr <- 100*(fp)/(tp + fp) 
  
  c(exactitud = accuracy, 
    `tasa verdadero positivo` =tpr,
    `tasa verdadero negativo` = tnr, 
    `tasa falso positivo` = fpr,
    `tasa falso negativo` = fnr,
    precisión = precision) %>% 
    round()
  }) %>% 
  data.frame(check.names = F) %>% 
  rownames_to_column("medición") %>% 
  xtable(., digits = 0, align = "rr|cccc") %>% 
  print(include.rownames = F)




# 2. Devianza
# Únicamente comparable entre modelos de ensamble


load("cache/results_models/test_results_rforest.Rdata")
load("cache/results_models/test_results_gboost.Rdata")

test.results.rf %>% length
test.results.gb %>% length

test.results <- test.results.gb

list.test.results <- list( `bosque aleatorio` = test.results.rf,
                           `gradient boosting` = test.results.gb)

test.results$prob

sapply(names(list.test.results), function(nom.elem){
  test.results <- list.test.results[[nom.elem]]
  
  correctos <- test.results$resp  == test.results$obs
  prob <- test.results$prob[correctos]
  prob.mod <- ifelse(prob == 0, 0.0001, prob)
  cpk <- log(283)/length(prob.mod) 
  deviance <- -2*log(prob.mod)
  gf <- sum(deviance)/length(prob)
  
  c(`bondad de ajuste` = gf, 
    `penalización complejidad` = cpk,
    `DIC` =  gf + 0) %>% 
    round(4)
  }) %>% 
  data.frame(check.names = F) %>% 
  rownames_to_column("medición") %>% 
  xtable(., digits = 2, align = "rr|cc") %>% 
  print(include.rownames = F)


