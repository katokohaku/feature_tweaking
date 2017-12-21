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
source("./R/generate_artificial_data.R")
label.from <- "in"   ; label.to = "out"

# dataset <- iris; label.from <- "virginica"; label.to = "versicolor"; target.instance=140

data(spam, package = "kernlab")
dataset <- spam; label.from <- "spam"   ; label.to = "nonspam"
dataset %>% str
head(dataset)

# "classification"
X <- scale( dataset[, 1:(ncol(dataset)-1)] )
rescale(head(X), X)[,1:6]
head(dataset)[,1:6]

true.y <- dataset[, ncol(dataset)]

ntree=500
forest.all <- randomForest(X, true.y, ntree=ntree)
forest.all
varImpPlot(forest.all) # to view varImp, x3 & x4 should be removed.
plot(forest.all)
top.importance <- forest.all$importance %>% data.frame %>%
  tibble::rownames_to_column(var = "var") %>% 
  arrange(desc(MeanDecreaseGini)) %>% 
  head(12)

X.fs <- X %>% data.frame %>% select(top.importance$var)
forest <- randomForest(X.fs, true.y, ntree=100)
forest
forest.all
plot(forest)

# plot(forest)
pred.y <- predict(forest, newdata=X.fs, predict.all=TRUE)
plot(forest)


# plot(x1~x2, data=dataset, col=true.y)

# functions ---------------------------------------------------------------


# parse a decision tree in randomForest into list of path as data.frame
getRules.randomForest <- function(forest, k=1, label.to=NULL) {
  stopifnot(class(forest) == "randomForest")
  class.label <- if(is.null(label.to)){
    as.character(forest$classes)
  } else {
    as.character(label.to)
  }
  
  suppressWarnings(
    tree.df <- getTree(rfobj = forest, k=k, labelVar = as.factor(forest$classes))
  )
  colnames(tree.df) <- c("left.daughter", "right.daughter", "split.var", 
                         "split.point", "status", "leaf")
  
  leaf.in.tree.df <- which(tree.df$leaf %in% class.label)
  catf("%i of path will be traced", length(leaf.in.tree.df))
  
  all.leaf <- npforeach(end.of.path = leaf.in.tree.df, .c=list)({
    this.path <- NULL
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
      # tree.df[pos, ]
      this.path <- rbind(
        data.frame(node=pos, path.to = this, 
                   split.var=tree.df[pos, ]$split.var, 
                   lr=sym, 
                   point = round(tree.df[pos, ]$split.point, 4),
                   stringsAsFactors = FALSE),
        this.path)
      this <- pos
      
      if(this == 1){ break() }
    }
    list(leaf = tree.df[end.of.path, ]$leaf, path = this.path)
  })
  
  path.by.class <- list()
  for(i.label in class.label){
    path.by.class[[i.label]] <-list(leaf = i.label, path = NULL)
  } 
  
  for(i.path in all.leaf){
    path.by.class[[ i.path$leaf ]]$path[[
      length(path.by.class[[ i.path$leaf ]]$path) + 1
      ]] <- i.path$path
  }
  
  class(path.by.class) <- c(class(path.by.class), "enumratePath")
  return(path.by.class)
}
system.time(ep <- getRules.randomForest(forest, k=2, label.to = NULL)); ep %>% str(2)
# system.time(ep <- getRules(forest, k=2, label.to = "spam")); ep %>% str(2)
ep$spam$path[[1]]


# extract all rules a decision tree in rtandomForest into list of path as data.frame
enumrateRules <- function(forest, ntree=NULL, label.to=NULL) {
  stopifnot(!missing(forest))
  all.trees <- NULL
  
  if(class(forest) != "randomForest"){ stop() }
  
  maxk <- if(is.null(ntree)){ forest$ntree } else { ntree }
  class.label <- if(is.null(label.to)){
    as.character(forest$classes)
  } else {
    if(label.to %in% as.character(forest$classes)){
      label.to
    } else {
      stop("unexpected input in label.to")
    }
  }
  if(is.null(ntree)){
    catf("extract all trees", maxk, forest$ntree)
  } else {
    catf("extract head(%i) of %i trees", maxk, forest$ntree)
  }
  start.time <- Sys.time()
  
  all.trees <- pforeach(k = 1:maxk, .c=list)({
    getRules.randomForest(forest, k=k, label.to = label.to)
  })

  catf("organize path of %s class", 
       ifelse(is.null(label.to), "all", as.character(length(class.label))))
  all.leaf <- list()
  
  for(i.label in class.label){
    all.leaf[[i.label]] <-list(leaf = i.label, path = NULL)
  } 
  for(i.label in class.label){
    all.leaf[[i.label]]$path <- 
      pforeach(i.tree = all.trees)( i.tree[[i.label]]$path )
    catf(" > class= %s (%i rules)",i.label, length(all.leaf[[i.label]]$path))
  }
  print(Sys.time() - start.time)
  extract.path <- list(trees = all.trees, leaf = all.leaf)
  class(extract.path) <- c(class(extract.path), "extractPath")
  invisible(extract.path)
}


epath.forest <- enumrateRules(forest)
str(epath.forest, 1)


# get e-satisfactory instance of aim-leaf from all tree ---------------------

epsiron = 0.3
label.to = "nonspam"
cand.paths <- epath.forest$leaf[[label.to]]$path

cand.paths %>% str(0)
cand.paths[[1]]
# target samples are: pred.y == true.y
which(pred.y$aggregate == label.from) %>% head

target.instance =1

try.trees <- which(label.from ==  pred.y$individual[target.instance, ])
i.tree=1
start.time <- Sys.time()
cand.eSatisfy <- list()
cand.paths <- NULL
# for(i.tree in 1:2){#NROW(try.trees)){ # trees to be re-evaluated
#   cat("=")
#   enum.paths <- enumratePath(forest, i.tree)
#   cand.paths <- c(cand.paths,
#                   map(enum.paths$path[which(enum.paths$leaf == label.to)],
#                       function(obj){ select(obj, -c(1:2)) }))
#   
# }

start.time <- Sys.time()
enum.paths <- pforeach::pforeach(i.tree = 1:forest$ntree, .c=list)({
  enumratePath2(forest, i.tree)
})
(end.time <- Sys.time() - start.time)

enum.paths %>% str(0) 
enum.paths[[1]][[1]] %>% str(1) 
i.path = 1
if(enum.paths[[i.path]]$leaf == label.to){
  map(enum.paths$path,         
      function(obj){ 
        obj %>% 
          select(-c(1:2)) %>% 
          mutate(eps = ifelse(lr=="<", -epsiron, +epsiron),
                 e.satisfy = ifelse(lr=="<", point-epsiron, point+epsiron))
      })
} else{NULL}
})
(end.time <- Sys.time() - start.time)

cand.paths[[2]] 
cand.paths %>% str(0)
cand.paths %>% unique %>% str(0)

this.cands <- lapply(
  cand.paths, 
  function(obj) {
    mutate(obj, 
           eps = ifelse(lr=="<", -epsiron, +epsiron),
           e.satisfy = ifelse(lr=="<", point-epsiron, point+epsiron))
  }
)  
# cand.eSatisfy <- c(cand.eSatisfy, this.cands)


end.time <- Sys.time() - start.time

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
    if(predict(forest, newdata=this.tweak) == label.to){
      
      tweaked.instance <- this.tweak 
      delta.min <- delta
    }
  }
}

pred.new <- predict(forest, newdata=tweaked.instance)
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

