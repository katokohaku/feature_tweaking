# feature tweaking: sample usage with spam data set -----------------------
for(LIB in c("randomForest", "dplyr", "magrittr", "purrr", "tibble")){
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


# post analysis -----------------------------------------------------------
require(tidyr)
require(ggplot2)
tw.diff <- tweaked$diff
tw.diff %>% gather() %>% 
  mutate(var = as.array(key)) %>% 
  ggplot(aes(x=var, y=value)) +
  geom_hline(yintercept=0, colour = "red", size = 1.5) + 
  geom_boxplot() +
  coord_flip()
  
data.frame(var=colnames(tw.diff), nonZero=colMeans(tw.diff != 0)) %>% 
  ggplot(aes(x=reorder(var, nonZero), y=nonZero)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("") + ylab("non-zerp frequency")

data.frame(var=colnames(tw.diff), value=colMeans(abs(tw.diff))) %>% 
  ggplot(aes(x=reorder(var, value), y=value)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("mean absolute effort") +
  coord_flip()
  
which(rowSums(abs(tw.diff)) >0) %>% length
(ins.df <- tw.diff[6, ])
ins.df %>% gather() %>% 
  # filter(abs(value) > 0) %>% 
  # ggplot(aes(x=reorder(key, abs(value)), y=value)) +
  ggplot(aes(x=key, y=value)) +
  geom_bar(stat = "identity")  +
  xlab("") + ylab("amount of effort") +
  coord_flip()
  

# 
# 
# 
# 
# 
# dataset.train.fs[target.instance,]
# X.fs[target.instance,]
# 
# X.fs %>% str(1)
# descale(X.fs, X.fs)[target.instance, 1:6]
# dataset.train.fs[target.instance, 1:6]
# 

# end ---------------------------------------------------------------------
