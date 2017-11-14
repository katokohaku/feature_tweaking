# 

rm(list=ls())
# set.seed(777)
require(randomForest)
require(foreach)
require(dplyr)
require(magrittr)

# need to supply an explicit variable to use to compute the row number.
filter(mtcars, row_number() == 1L)
filter(mtcars, row_number() == n())
filter(mtcars, between(row_number(), 5, n()))

source("./R/utils.R")
# functions ---------------------------------------------------------------


# parse a decision tree in rtandomForest into list of path as data.frame
enumratePath.randomForest <- function(rf, k=1) {
  stopifnot(class(rf) == "randomForest")
  
  suppressWarnings(
    tree.df <- getTree(rfobj = rf, k=k, labelVar = as.factor(rf$classes))
  )
  colnames(tree.df) <- c("left.daughter", "right.daughter", "split.var", 
                         "split.point", "status", "prediction")
  
  tree.df.tar <- which(!is.na(tree.df$prediction))
  this.leaf <- NULL
  this.path <- NULL
  for(i in 1:NROW(tree.df.tar)){
    end.of.path <- tree.df.tar[i]
    prediction <- tree.df[end.of.path, ]$prediction
    this.path[[i]] <- data.frame(
      node       = end.of.path, 
      path.to    = NA, 
      split.var  = NA,
      lr         = prediction, 
      point      = NA, 
      prediction = prediction,
      stringsAsFactors = FALSE)
    
    this.leaf <- c(this.leaf, prediction)
    this <- end.of.path
    while(TRUE){
      pos.in.left  <- which(tree.df$left.daughter  == this)
      pos.in.right <- which(tree.df$right.daughter == this)
      
      pos <- pos.in.left
      sym <- "<"
      
      if(length(pos.in.left) == 0){
        pos <- pos.in.right
        sym <- ">="
      }
      tree.df[pos, ]
      this.path[[i]] <- rbind(
        data.frame(node=pos, path.to = this, split.var=tree.df[pos, ]$split.var, 
                   lr=sym, point = tree.df[pos, ]$split.point, prediction = prediction,
                   stringsAsFactors = FALSE),
        this.path[[i]])
      this <- pos
      
      if(this == 1){ break() }
    }
  }
  
  obj <- list(leaf = this.leaf, path = this.path)
  class(obj) <- c(class(obj), "enumratePath")
  
  return(obj)
}


# set up to run -------------------------------------------------------------

data(spam, package = "kernlab")

dataset <- iris; label.from <- "virginica"; label.to = "versicolor"; target.instance=140
# dataset <- spam; label.from <- "spam"   ; label.to = "nonspam"
dataset %>% str

# "classification"
X <- scale( dataset[, 1:(ncol(dataset)-1)] )
scaled.center <- attr(X, "scaled:center")
scaled.scale  <- attr(X, "scaled:scale")

iris[1, ]; X[1,] * scaled.scale + scaled.center

true.y <- dataset[, ncol(dataset)]

ntree=100
rf <- randomForest(X, true.y, ntree=ntree, nodesize=5, maxnodes=16)
# plot(rf)
pred.y <- predict(rf, newdata=X, predict.all=TRUE)


# target samples are: pred.y == true.y ------------------------------------
x <- X[target.instance, ]
label.from = true.y[target.instance]
label.to
epsiron = 0.1



# get e-satisfactory instance of aim-leaf from all tree ---------------------
try.trees <- which(label.from ==  pred.y$individual[target.instance, ])

cand.eSatisfy <- list()
for(i.tree in 1:NROW(try.trees)){ # trees to be re-evaluated
  
  enum.paths <- enumratePath.randomForest(rf, i.tree)
  cand.paths <- enum.paths$path[which(enum.paths$leaf == label.to)]
  
  this.cands <- lapply(
    cand.paths, 
    function(obj) {
      mutate(obj, 
             eps = ifelse(lr=="<", -epsiron, +epsiron),
             e.satisfy = ifelse(lr=="<", point-epsiron, point+epsiron))
    }
  )  
  
  cand.eSatisfy <- c(cand.eSatisfy, this.cands)
}

cand.eSatisfy


# eval each path ----------------------------------------------------------


e.satisfy.instance <- NA
delta.min = 1e+99

for(path in cand.eSatisfy){
  this.tweak <- x
  this.path  <- chop(path)
  
  for(ip in 1:NROW(this.path)){
    feature <- as.character(this.path[ip, ]$split.var)
    this.tweak[feature] <-this.path[ip, ]$e.satisfy
  }
  
  delta <- 1/dist(rbind(x, this.tweak))
  print(c(this.tweak, effort=delta))
  if(delta < delta.min){
    tweaked.instance <- this.tweak 
    delta.min <- delta
  }
}
pred.newpredict(rf, newdata=tweaked.instance)

tweaked.unscaled <- tweaked.instance * scaled.scale + scaled.center





# plot shift --------------------------------------------------------------
original.instance - tweaked.instance
plot(Petal.Length~Petal.Width, data=X, col=true.y)

points(Petal.Length~Petal.Width, col="purple", pch=16, data=original.instance %>% t %>% data.frame)
points(Petal.Length~Petal.Width, col="purple", pch=16, data=tweaked.instance %>% t %>% data.frame)

arrows(x0 = original.instance[4], y0 = original.instance[3],
       x1 = tweaked.instance[4], y1 =tweaked.instance[3])


# iris[1,]
# iris.unscaled <- t(X[target.instance,] * scaled.scale + scaled.center) %>% data.frame
# iris.tweaked  <- t(tweaked.unscaled) %>% data.frame
# 
# points(x=iris.unscaled$Petal.Width, y=iris.unscaled$Petal.Length, col="purple", pch=16)
# points(x=iris.tweaked$Petal.Width, y=iris.tweaked$Petal.Length, col="purple", pch=16)
# 

