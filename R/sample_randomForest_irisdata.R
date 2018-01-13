rm(list=ls())
# set.seed(777)
require(randomForest)
require(foreach)
require(dplyr)
require(magrittr)
require(purrr)
require(pforeach)

source("./R/utils.R")

# set up to run -------------------------------------------------------------

# get dataset

# dataset <- iris; label.from <- "virginica"; label.to = "versicolor"; target.instance=140

data(spam, package = "kernlab")
dataset <- spam; label.from <- "spam"   ; label.to = "nonspam"

dataset %>% str
head(dataset)

# classification with randomForest -----------------------------------------
X <- dataset[, 1:(ncol(dataset)-1)]
true.y <- dataset[, ncol(dataset)]

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
dataset.fs<- dataset %>% select(top.importance$var)

X.fs <- scale( dataset.fs )
head(X.fs)[, 1:6]
rescale( head(dataset.fs), X.fs )[, 1:6]

descale(head(X.fs), X.fs)[,1:6]
head(dataset.fs)[,1:6]

X.fs <- X %>% data.frame %>% select(top.importance$var)
forest.rf <- randomForest(X.fs, true.y, ntree=100)
forest.all
forest.rf
plot(forest.rf)

pred.y <- predict(forest.rf, newdata=X.fs, predict.all=TRUE)
plot(forest.rf)


# functions ---------------------------------------------------------------
source("./R/parseRFrees.R")
NTREE=30
gr <- set.eSatisfactory.rf(forest.rf, ntree = NTREE, epsiron = 0.3)
gr %>% str(1)
gr[[20]]$nonspam[[167]]

# eval each path ----------------------------------------------------------
label.from = "spam"
label.to = "nonspam"
forest = forest.rf

pred.y$individual %>% dim

tree.predict <- pred.y$individual[, 1:NTREE]
which(! map_lgl(tree.predict, function(x) all(x == "spam"))) %>% head

target.instance = 21
this.instance <- X.fs[target.instance, ]

true.y[target.instance]
pred.y$aggregate[target.instance]

tree.predict[target.instance, ]

tree.pred.from <- which(tree.predict[target.instance, ]  == label.from )
catf("instance[%i]: %i of %i trees predicted: \"%s\" (wants -> \"%s\")", target.instance,
     length(tree.pred.from),  length(pred.y$individual[target.instance,  1:NTREE]), 
     label.from, label.to)

i.tree = tree.pred.from[1]
cand.eSatisfy <- npforeach(i.tree = tree.pred.from)(gr[[i.tree]][[label.to]] )
str(cand.eSatisfy, 0)

catf("evalutate %i candidates", length(cand.eSatisfy))
tweaked.instance <- NA
delta.min <- 1e+99

for(this.path in cand.eSatisfy){
  
  this.tweak <- this.instance
  for(ip in 1:NROW(this.path)){
    feature <- as.character(this.path[ip, ]$split.var)
    this.tweak[feature] <- this.path[ip, ]$e.satisfy
  }
  delta <- dist(rbind(this.instance, this.tweak))
  
  if(delta < delta.min){
    if(predict(forest, newdata=this.tweak) == label.to){
      
      tweaked.instance <- rbind(
        instance = this.instance,
        tweaked  = this.tweak, 
        diff     = this.instance - this.tweak) %>% t
      
      delta.min <- delta
    }
  }
}
tweaked.instance; delta.min


tweaked.unscaled <- tweaked.instance * scaled.scale[1:2] + scaled.center[1:2]



# plot shift --------------------------------------------------------------
original.instance - tweaked.unscaled
original.instance %>% str
points(x1~x2, col="purple", pch=16, data=original.instance)
points(x1~x2, col="green", pch=16, data=tweaked.unscaled %>% t %>% data.frame)

segments(x0 = original.instance$x2, y0 = original.instance$x1,
         x1 = tweaked.unscaled[2], y1 =tweaked.unscaled[1])
}
legend("topright", legend = c("original", "teaked"),
       pch = 16,     col = c("purple", "green"))

# iris[1,]
# iris.unscaled <- t(X[target.instance,] * scaled.scale + scaled.center) %>% data.frame
# iris.tweaked  <- t(tweaked.unscaled) %>% data.frame
# 
# points(x=iris.unscaled$Petal.Width, y=iris.unscaled$Petal.Length, col="purple", pch=16)
# points(x=iris.tweaked$Petal.Width, y=iris.tweaked$Petal.Length, col="purple", pch=16)
# 

