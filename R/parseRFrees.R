
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
  
  class(path.by.class) <- c(class(path.by.class), "decisionTree.rulePath")
  return(path.by.class)
}
# system.time(ep <- getRules.randomForest(forest, k=2, label.to = NULL)); ep %>% str(2)
# system.time(ep <- getRules(forest, k=2, label.to = "spam")); ep %>% str(2)



# get e-satisfactory instance of aim-leaf from all tree ---------------------


# extract all rules a decision tree in rtandomForest into list of path as data.frame

set.eSatisfactory.rf <- function(forest, ntree=NULL, epsiron = 0.1) {
  stopifnot(!missing(forest), epsiron > 0)
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
    catf("extracting all (%i of %i trees)", maxk, forest$ntree)
  } else {
    catf("extracting head(%i) of %i trees", maxk, forest$ntree)
  }

  start.time <- Sys.time()
  all.trees <- pforeach(k = 1:maxk, .c=list)({
    getRules.randomForest(forest, k=k)
  })
  print(Sys.time() - start.time)

  catf("set e-satisfactory instance (%i trees)", length(all.trees))
  start.time <- Sys.time()
  all.eTrees <- pforeach(tree.rules = all.trees, .c=list)({
    tree.eRules <- NULL
    for(cn in names(tree.rules)){
      tree.eRules[[cn]] <- map(
        tree.rules[[cn]]$path,
        function(obj){ 
          mutate(obj,
                 eps = ifelse(lr=="<", -epsiron, +epsiron),
                 e.satisfy = point + eps) %>% 
            select(-node, -path.to)
        }
      )
    }
    return(tree.eRules)
  })
  print(Sys.time() - start.time)
  
  class(all.eTrees) <- c(class(all.eTrees), "decisionTree.eSatisfactoryRules")
  invisible(all.eTrees)
}

# gr <- getRules.eSatisfy.rf(forest, ntree = 20, epsiron = 0.3)
# gr %>% str(2)





