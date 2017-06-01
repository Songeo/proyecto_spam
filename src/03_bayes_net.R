library(ProjectTemplate)
reload.project()

load("cache/df.train.test.RData")

library(bnlearn)

# ............................................................................ # 

# df train test
df.train <- dtm.df[indicetrain, ] %>% 
  dplyr::select(-document, -n_words) %>% 
  mutate(spam = as.numeric(spam))
df.test <- dtm.df[-indicetrain, ] %>% 
  dplyr::select(-document, -n_words) %>% 
  mutate(spam = as.numeric(spam))


# ............................................................................ # 

# several algorithms
bn.gs <- gs(df.train)
# bn.iam <- iamb(aux.train)
# bn.fiam <- fast.iamb(aux.train)
# bn.iiam <- inter.iamb(aux.train)
# bn.hc <- hc(aux.train, score = 'bic')
# compare(bn.gs, bn.hc)

# par(mfrow = c(1, 2))
plot(bn.gs, main = "Constraint-based algorithms")
# plot(bn.hc, main = "Hill-Climbing")
# plot(bn.iiam, main = "Others")

# no blacklist


# Hill Climbing vs Grow-Shrink with Blacklist
# bn.hc.bckl <- hc(aux.train, score = 'bic', 
#                  blacklist = black.list)
# bn.gs.bckl <- cextend(gs(aux.train, blacklist = black.list))
# compare(bn.hc.bckl, bn.gs.bckl)
# all.equal(bn.hc.bckl, bn.gs.bckl)
# modelstring(bn.hc.bckl)
# undirected.arcs(bn.hc.bckl)

# par(mfrow = c(1,2))
# graphviz.plot(bn.hc.bckl,shape="ellipse")
graphviz.plot(bn.gs, shape="ellipse")

# Probabilistic Relationship Strength
# strength.hc.bckl <- arc.strength(bn.hc.bckl, aux.train, criterion = 'bic')
# strength.plot(bn.hc.bckl, strength.hc.bckl, shape="ellipse")
strength.gs.bckl <- arc.strength(bn.gs, df.train)
strength.plot(bn.gs.bckl, strength.gs.bckl, shape="ellipse")

# Fit
# fit.hc.bayes <- bn.fit(bn.hc.bckl, data = aux.train, method = "bayes")
fit.gs.bayes <- bn.fit(bn.gs, data = bn.gs, method = "bayes")
