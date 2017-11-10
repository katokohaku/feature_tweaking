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
  s
  uppressWarnings(
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
  return(list(
    leaf = this.leaf,
    path = this.path))
}


# example run -------------------------------------------------------------

data(spam, package = "kernlab")

dataset <- iris; label.from <- "virginica"; label.to = "versicolor"; target.instance=140
# dataset <- spam; label.from <- "spam"   ; label.to = "nonspam"
dataset %>% str

# "classification"
X <- scale( dataset[, 1:(ncol(dataset)-1)] )
true.y <- dataset[, ncol(dataset)]

ntree=100
rf <- randomForest(X, true.y, ntree=ntree, nodesize=5, maxnodes=16)
# plot(rf)
pred.y <- predict(rf, newdata=X, predict.all=TRUE)


# target samples are: pred.y == true.y ------------------------------------
i =1#NROW(true.y) #  the number of instance
true.y[i] ==  pred.y$aggregate[i]
evaluated.trees <- which(true.y[i] ==  pred.y$individual[i, ])



# get all Path of a decision tree in randomForest -------------------------

  paths.of.tree <- enumratePath.randomForest(rf,1)
)
paths.of.tree




