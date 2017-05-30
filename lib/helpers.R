library(ggplot2)
theme_set(theme_light())


simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}

CRoc_GG <- function(obs, pred, mod.tit){
  simple_roc(obs, 
             pred) %>% 
    ggplot(aes(x = FPR, y = TPR)) +
    geom_abline(slope = 1, intercept = 0, 
                color = "salmon", linetype = 2) +
    geom_line(color = "gray60", size= 1) + 
    ggtitle(paste("Curva ROC", mod.tit))
}
