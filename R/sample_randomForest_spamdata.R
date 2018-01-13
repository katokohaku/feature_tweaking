# feature tweaking: sample usage with spam data set -----------------------
for(LIB in c("randomForest", "tidyverse", "magrittr")){
  if(! require(LIB, character.only = TRUE)){
    install.packages(LIB, dependencies = TRUE)
    require(LIB, character.only = TRUE)
  }
}
if(! require(pforeach)){ 
  install.packages("foreach", dependencies = TRUE)
  devtools::install_github("hoxo-m/pforeach")
  require(pforeach)
}

rm(list=ls())
set.seed(777)
source("./R/utils.R")

# set up to run -------------------------------------------------------------
data(spam, package = "kernlab")
dataset <- sample_frac(spam)
n.test <- floor(NROW(dataset) *0.1)

dataset.train <- chop(dataset, n.test)
dataset.test  <- tail(dataset, n.test)

dim(dataset);dim(dataset.train);dim(dataset.test)

# classification with randomForest -----------------------------------------
X <- dataset.train[, 1:(ncol(dataset.train)-1)]
true.y <- dataset.train[, ncol(dataset.train)]

forest.all <- randomForest(X, true.y, ntree=500)
forest.all
par(mfrow=c(1,2))
varImpPlot(forest.all) # to view varImp, x3 & x4 should be removed.
plot(forest.all)
par(mfrow=c(1,1))

# model shrinlkage based on importance ------------------------------------
top.importance <- forest.all$importance %>% data.frame %>%
  tibble::rownames_to_column(var = "var") %>% 
  arrange(desc(MeanDecreaseGini)) %>% 
  head(12)

# scaling feature-selected data  ---------------------------
dataset.train.fs<- dataset.train %>% select(top.importance$var)
dataset.train.fs<- dataset.train %>% select(top.importance$var)

X.fs <- scale( dataset.train.fs )
head(X.fs)[, 1:6]
rescale( head(dataset.train.fs), X.fs )[, 1:6]

forest.rf <- randomForest(X.fs, true.y, ntree=100)
forest.all
forest.rf
plot(forest.rf)


# feature tweaking  ---------------------------------------------------------------
source("./R/parseRFrees.R")

# test sampling (for demo)
system.time(ep <- getRules.randomForest(forest.rf, k=2, label.to = NULL)); ep %>% str(2)

es.rf <- set.eSatisfactory.rf(forest.rf, ntree = 30, epsiron = 0.3)
es.rf %>% str(2)


# eval predicted instance -------------------------------------------------
source("./R/tweak_feature.R")

tweaked <- tweak(es.rf, newdata= X.fs[1:30, ], label.from = "spam", label.to = "nonspam",
                 .dopar = TRUE)
tweaked %>% str

dt <- descale.tweakedFeature(tweaked, X.fs)

# Plot population 
pp <- plot.tweakedPopulation(tweaked, "a")
pp <- plot.tweakedPopulation(tweaked, "d")
pp <- plot.tweakedPopulation(tweaked, "f")

which(tweaked$predict == "spam")
plot.suggest(tweaked, 3)
plot.suggest(tweaked, 6)
plot.suggest(tweaked, 7)
plot.suggest(tweaked, 7, .order = TRUE, .nonzero.only = TRUE)


# end ---------------------------------------------------------------------
