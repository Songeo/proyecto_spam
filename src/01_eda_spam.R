library(ProjectTemplate)
reload.project()

library(forcats)
library(ggrepel)
library(gridExtra)
library(ggbiplot)

load("cache/tab.spam.rsparse.RData")
load("cache/df.spam.tidy.RData")
load("cache/df.spam.RData")

df.eda <- tab.spam.rsparse

df.eda$document %>% n_distinct()
df.eda %>% 
  dplyr::select(document,spam) %>% 
  unique %>% 
  group_by(spam) %>% 
  tally %>% 
  ungroup %>% 
  mutate(prop = n/sum(n))

# ejemplo de texto por spam/nospam
df.spam.tidy %>% 
  dplyr::count(document, spam, sort = TRUE) %>% 
  filter(n == 20)

df.spam %>% 
  filter(document %in% c("1365")) %>% 
  .$text
df.spam.tidy %>% 
  filter(document == "1365")

df.spam %>% 
  filter(document %in% c("1386")) %>% 
  .$text
df.spam.tidy %>% 
  filter(document == "1386")

# Palabras por documentos preprocesamiento
df.eda %>% 
  group_by(document, spam) %>% 
  dplyr::summarise(n_words = sum(count)) %>% 
  ungroup %>% 
  group_by(spam) %>% 
  dplyr::summarise(`promedio filtro` = mean(n_words)) %>% 
  ungroup() 

df.eda %>% 
  group_by(document, spam) %>% 
  dplyr::summarise(n_words = sum(count)) %>% 
  ungroup %>% 
  .$n_words %>% 
  summary() %>% 
  tidy %>% t %>% 
  data.frame() %>% 
  rownames_to_column("Estadísticos") 


# ............................................................................ # 

# Palabras por documentos original
df.spam.tidy %>% 
  dplyr::count(document, spam, sort = TRUE) %>% 
  ungroup %>% 
  group_by(spam) %>% 
  dplyr::summarise(`promedio original` = mean(n)) %>% 
  ungroup() 

df.spam.tidy %>% 
  dplyr::count(document, sort = TRUE) %>% 
  .$n %>% 
  summary()

# ejemplo de mails con poco texto
selec.docs <- df.spam.tidy %>% 
  dplyr::count(document, sort = TRUE) %>% 
  filter(n < 5) %>% 
  .$document
df.spam %>% 
  filter(document %in% selec.docs)
df.spam.tidy %>% 
  filter(document == "5053")




# ............................................................................ # 

# Histograma: descripción de valores
gg <- df.eda %>% 
  gather(var.label, var.value, tf:tf_idf) %>% 
  group_by(var.label) %>% 
  mutate(median = median(var.value)) %>% 
  ungroup %>% 
  ggplot(aes(x = var.value)) +
  geom_histogram(bins = 25, alpha = .5, aes(fill = var.label)) + 
  geom_vline( aes(xintercept = median), 
              linetype = 2, color = "salmon") +
  facet_wrap(~var.label, scales = "free") + 
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("frecuencia\n(miles)") +
  ggtitle("Distribución de mediciones de términos/palabras ")  + 
  scale_y_continuous(labels = function(x)x/1000) 
ggsave(filename = "graphs/eda/01_tfidf_hist.png",plot = gg, width = 6, height = 2.5)


 # Boxplot: descripción de valores
gg <- df.eda %>% 
  gather(var.label, var.value, tf:tf_idf, count) %>% 
  ggplot(aes(y = var.value, x = var.label)) +
  geom_boxplot(aes(fill = var.label), alpha = .5) + 
  facet_wrap(~var.label, scales = "free", nrow = 1) + 
  theme(legend.position = "none") +
  xlab("mediciones") +
  ylab("value") +
  ggtitle("Distribución de mediciones de términos/palabras ")  
ggsave(filename = "graphs/eda/01_tfidf_boxplot.png", plot = gg, width = 7, height = 5)



# ............................................................................ # 

 # Wordclouds freq (n)
gg.1 <- df.eda %>% 
  group_by(term) %>% 
  summarise(n = sum(count)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  .[1:50, ] %>% 
  ggplot(aes(x = 1, y = 1, size = n, 
             label = term, color = n)) +
  geom_text_repel(segment.size = 0, force = 100, 
                  aes(alpha = n)) +
  scale_size(range = c(3, 13), guide = FALSE) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL)+
  labs(x = '', y = '')  +
  theme(legend.position = "none") + 
  scale_color_continuous(low = "#99c899", high = "#00aa00") +
  scale_alpha_continuous(range = c(.9, 1)) +
  ggtitle("Wordcloud freq",
          subtitle = "Documento: total")

# Wordcloud freq spam vs nospam
tab.spam <- df.eda %>% 
  group_by(spam, term) %>% 
  summarise(n = sum(count)) %>% 
  ungroup() %>% 
  arrange(desc(n)) 

gg.2 <- tab.spam %>% 
  filter(spam == 0) %>% 
  .[1:50, ] %>% 
  ggplot(aes(x = 1, y = 1, size = n, 
             label = term, color = n)) +
  geom_text_repel(segment.size = 0, force = 100, 
                  aes(alpha = n)) +
  scale_size(range = c(3, 13), guide = FALSE) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL)+
  labs(x = '', y = '')  +
  theme(legend.position = "none") + 
  scale_color_continuous(low = "#cce3e3", high = "#007777") +
  scale_alpha_continuous(range = c(.9, 1)) +
  ggtitle("Wordcloud freq",
          subtitle = "Documento: no spam")

gg.3 <- tab.spam %>% 
  filter(spam == 1) %>% 
  .[1:50, ] %>% 
  ggplot(aes(x = 1, y = 1, size = n, 
             label = term, color = n)) +
  geom_text_repel(segment.size = 0, force = 100, 
                  aes(alpha = n)) +
  scale_size(range = c(3, 13), guide = FALSE) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL)+
  labs(x = '', y = '')  +
  theme(legend.position = "none") + 
  scale_color_continuous(low = "#e3cce3", high = "#770077") +
  scale_alpha_continuous(range = c(.9, 1)) +
  ggtitle("Wordcloud freq",
          subtitle = "Documento: spam")

gg <- grid.arrange(gg.1, gg.2, gg.3, nrow = 1)
ggsave("graphs/eda/02_wc_freq.png", plot = gg, width = 10, height = 4)



# ............................................................................ # 

# Wordclouds tf-idf
gg.1 <- df.eda %>% 
  group_by(term) %>% 
  summarise(n = mean(tf_idf)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  .[1:50, ] %>% 
  ggplot(aes(x = 1, y = 1, size = n, 
             label = term, color = n)) +
  geom_text_repel(segment.size = 0, force = 100, 
                  aes(alpha = n)) +
  scale_size(range = c(3, 13), guide = FALSE) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL)+
  labs(x = '', y = '')  +
  theme(legend.position = "none") + 
  scale_color_continuous(low = "#99c899", high = "#00aa00") +
  scale_alpha_continuous(range = c(.9, 1)) +
  ggtitle("Wordcloud tf-idf",
          subtitle = "Documento: total")

# Wordcloud tf-idf spam vs nospam
tab.spam <- df.eda %>% 
  group_by(spam, term) %>% 
  summarise(n = sum(tf_idf)) %>% 
  ungroup() %>% 
  arrange(desc(n)) 

gg.2 <- tab.spam %>% 
  filter(spam == 0) %>% 
  .[1:50, ] %>% 
  ggplot(aes(x = 1, y = 1, size = n, 
             label = term, color = n)) +
  geom_text_repel(segment.size = 0, force = 100, 
                  aes(alpha = n)) +
  scale_size(range = c(3, 13), guide = FALSE) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL)+
  labs(x = '', y = '')  +
  theme(legend.position = "none") + 
  scale_color_continuous(low = "#cce3e3", high = "#007777") +
  scale_alpha_continuous(range = c(.9, 1)) +
  ggtitle("Wordcloud tf-idf",
          subtitle = "Documento: no spam")

gg.3 <- tab.spam %>% 
  filter(spam == 1) %>% 
  .[1:50, ] %>% 
  ggplot(aes(x = 1, y = 1, size = n, 
             label = term, color = n)) +
  geom_text_repel(segment.size = 0, force = 100, 
                  aes(alpha = n)) +
  scale_size(range = c(3, 13), guide = FALSE) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL)+
  labs(x = '', y = '')  +
  theme(legend.position = "none") + 
  scale_color_continuous(low = "#e3cce3", high = "#770077") +
  scale_alpha_continuous(range = c(.9, 1)) +
  ggtitle("Wordcloud tf-idf",
          subtitle = "Documento: spam")

gg <- grid.arrange(gg.1, gg.2, gg.3, nrow = 1)
ggsave("graphs/eda/02_wc_tfidf.png", plot = gg, width = 10, height = 4)



# ............................................................................ # 

# Asociación de palabras total
dtm.df <- df.eda %>%
  filter(tf_idf > 0) %>% 
  dplyr::select(document, term, tf_idf) %>% 
  spread(term, tf_idf, fill = 0)
dtm.df  %>% data.frame() %>% head

# pca de terminos con tf-idf
mat.pca <- as.matrix(dtm.df[,-1]) 
sum(apply(mat.pca, 2, sum) == 0 )
rownames(mat.pca) <- dtm.df$document
pca.docs <- princomp(mat.pca)
summary(pca.docs)

gg <- ggbiplot(pca.docs, circle = T, var.axes = T, #varname.adjust = T,
                 alpha = .2) + 
  geom_vline(xintercept = 0, color = "salmon", 
             linetype = 2, alpha = .3) + 
  geom_hline(yintercept = 0, color = "salmon", 
             linetype = 2, alpha = .3) 


# clustering de términos total # 60% de la varianza
summary(pca.docs)
hcl.mat <- pca.docs$loadings[, 1:79]
dim(hcl.mat)
hcl.pca <- hclust(dist(hcl.mat), method = "ward.D2")
plot(hcl.pca)
rect.hclust(hcl.pca, k = 15, border="#f69640")

png("graphs/eda/03_hclust_terms_total.png", width = 1700, height = 1000)
plot(hcl.pca, #labels=tab.spread$category,
     cex = 1.6,
     main = "Dendograma\nDocumento: total",
     col = "#00aa00", col.main = "#00aa00", col.lab = "#00aa00", 
     col.axis = "#F38630", sub = "", axes = FALSE)#, hang = -1)
dev.off()


# ............................................................................ # 

# Asociación de palabras no spam
dtm.df <- df.eda %>%
  filter(tf_idf > 0, 
         # NO SPAM
         spam == 0) %>% 
  dplyr::select(document, term, tf_idf) %>% 
  spread(term, tf_idf, fill = 0)
dtm.df  %>% data.frame() %>% head

# pca de terminos con tf-idf
mat.pca <- as.matrix(dtm.df[,-1]) 
sum(apply(mat.pca, 2, sum) == 0 )
rownames(mat.pca) <- dtm.df$document
pca.docs <- princomp(mat.pca)
summary(pca.docs)

gg <- ggbiplot(pca.docs, circle = T, var.axes = T, #varname.adjust = T,
         alpha = .2) + 
  geom_vline(xintercept = 0, color = "salmon", 
             linetype = 2, alpha = .3) + 
  geom_hline(yintercept = 0, color = "salmon", 
             linetype = 2, alpha = .3) 


# clustering de términos total # 60% de la varianza
summary(pca.docs)
hcl.mat <- pca.docs$loadings[, 1:75]
dim(hcl.mat)
hcl.pca <- hclust(dist(hcl.mat), method = "ward.D2")
plot(hcl.pca)
rect.hclust(hcl.pca, k = 15, border="#f69640")

png("graphs/eda/03_hclust_terms_nospam.png", width = 1700, height = 1000)
plot(hcl.pca, #labels=tab.spread$category,
     cex = 1.6,
     main = "Dendograma\nDocumento: no spam",
     col = "#007777", col.main = "#007777", col.lab = "#007777", 
     col.axis = "#F38630", sub = "", axes = FALSE)#, hang = -1)
dev.off()



# ............................................................................ # 

# Asociación de palabras spam
dtm.df <- df.eda %>%
  filter(tf_idf > 0, 
         # SPAM
         spam == 1) %>%  
  dplyr::select(document, term, tf_idf) %>% 
  spread(term, tf_idf, fill = 0)
dtm.df  %>% data.frame() %>% head

# pca de terminos con tf-idf
mat.pca <- as.matrix(dtm.df[,-1]) 
sum(apply(mat.pca, 2, sum) == 0 )
rownames(mat.pca) <- dtm.df$document
pca.docs <- princomp(mat.pca)
summary(pca.docs)

gg <- ggbiplot(pca.docs, circle = T, var.axes = T, #varname.adjust = T,
                 alpha = .2) + 
  geom_vline(xintercept = 0, color = "salmon", 
             linetype = 2, alpha = .3) + 
  geom_hline(yintercept = 0, color = "salmon", 
             linetype = 2, alpha = .3) 


# clustering de términos total # 60% de la varianza
summary(pca.docs)
hcl.mat <- pca.docs$loadings[, 1:31]
dim(hcl.mat)
hcl.pca <- hclust(dist(hcl.mat), method = "ward.D2")
plot(hcl.pca)
rect.hclust(hcl.pca, k = 15, border="#f69640")

png("graphs/eda/03_hclust_terms_spam.png", width = 1700, height = 1000)
plot(hcl.pca, #labels=tab.spread$category,
     cex = 1.6,
     main = "Dendograma\nDocumento: spam",
     col = "#770077", col.main = "#770077", col.lab = "#770077", 
     col.axis = "#F38630", sub = "", axes = FALSE)#, hang = -1)
dev.off()



