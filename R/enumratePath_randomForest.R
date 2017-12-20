rm(list=ls())
# set.seed(777)
require(randomForest)
require(foreach)
require(dplyr)
require(magrittr)
require(purrr)

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

# get dataset
source("./R/generate_artificial_data.R")
label.from <- "in"   ; label.to = "out"

# dataset <- iris; label.from <- "virginica"; label.to = "versicolor"; target.instance=140

# data(spam, package = "kernlab")
# dataset <- spam; label.from <- "spam"   ; label.to = "nonspam"
dataset %>% str
head(dataset)

# "classification"
X <- scale( dataset[, 1:(ncol(dataset)-1)] )
scaled.center <- attr(X, "scaled:center")
scaled.scale  <- attr(X, "scaled:scale")

iris[1, ]; X[1,] * scaled.scale + scaled.center

true.y <- dataset[, ncol(dataset)]

ntree=200
rf.all <- randomForest(X, true.y, ntree=ntree, nodesize=5, maxnodes=16)
varImpPlot(rf.all) # to view varImp, x3 & x4 should be removed.

X.fs <- X[,-c(3,4)]
rf <- randomForest(X.fs, true.y, ntree=ntree, nodesize=5, maxnodes=16)

# plot(rf)
pred.y <- predict(rf, newdata=X.fs, predict.all=TRUE)
varImpPlot(rf)


plot(x1~x2, data=dataset, col=true.y)

# target samples are: pred.y == true.y ------------------------------------
epsiron = 0.3

for(target.instance in 1:60){
  
  dataset[target.instance,]
  
  x <- X.fs[target.instance, ]
  label.from = true.y[target.instance]
  label.to
  
  str(rf)
  
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
  
  cand.eSatisfy %>% str(0)
  
  
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
    
    delta <- dist(rbind(x, this.tweak)) # if euclidean distance
    # delta <- 
    # print(c(this.tweak, effort=delta))
    if(delta < delta.min){
      if(predict(rf, newdata=this.tweak) == label.to){
        
        tweaked.instance <- this.tweak 
        delta.min <- delta
      }
    }
  }
  
  pred.new <- predict(rf, newdata=tweaked.instance)
  pred.new == label.to
  original.instance <- dataset[target.instance,1:2]
  
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

