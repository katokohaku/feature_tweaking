source("./R/utils.R")
source("./R/parseRFrees.R")

getRules <- function(forest, ntree=NULL, resample = FALSE){
  
  all.trees <- NULL
  i.tree <- NULL
  if(class(forest) == "randomForest"){
    
    if(is.null(ntree)){
      i.tree <- 1:forest$ntree
      catf("extracting all (%i of %i trees)", length(i.tree), forest$ntree)
    } else {
      maxk <- ifelse(ntree > forest$ntree, forest$ntree, ntree)
      if(resample == FALSE){
        i.tree <- 1:maxk
        catf("extracting head(%i) of %i trees", length(i.tree), forest$ntree)
      } else {
        i.tree <- sample(forest$ntree, maxk, replace = FALSE)
        catf("extracting sampled %i of %i trees", length(i.tree), forest$ntree)
      }
    }
    
    start.time <- Sys.time()
    all.trees <- pforeach(k = i.tree, .c=list)({
      getRules.randomForest(forest, k=k)
    })
    print(Sys.time() - start.time)
  } 
  if(class(forest) == "xgb.Booster"){
    catf("Currently not compatible with xgboost")
  }
  
  return(all.trees)
}

# get e-satisfactory instance of aim-leaf from all tree
set.eSatisfactory <- function(forest, ntree=NULL, resample = FALSE, epsiron = 0.1) {
  stopifnot(epsiron > 0)
  
  all.trees <- getRules(forest, ntree=ntree, resample=resample)
  stopifnot(! is.null(all.trees))
  
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
  
  esforest <- list(forest = forest, trees = all.eTrees)
  class(esforest) <- "forest.eSatisfactoryRules"
  invisible(esforest)
}

# gr <- getRules.eSatisfy.rf(forest, ntree = 20, epsiron = 0.3)
# gr %>% str(2)


# calcurate e-satisfactory instance from predicted results ----------------
tweak <- function(
  trees.eSatisfactory, newdata, label.from, label.to, .dopar = TRUE)
{
  stopifnot(class(trees.eSatisfactory) == "forest.eSatisfactoryRules",
            !missing(newdata), !missing(label.from), !missing(label.to) )
  
  forest <- trees.eSatisfactory$forest
  estrees <- trees.eSatisfactory$trees
  nestree <- length(trees.eSatisfactory$trees)
  catf("%i instances were predicted by %i trees: ", NROW(newdata), nestree)
  
  pred.y <- predict(forest, newdata=newdata, predict.all=TRUE)
  pred.Freq <- table(pred.y$aggregate)
  print(pred.Freq)
  
  .loop <- ifelse(.dopar, pforeach, npforeach)
  start.time <- Sys.time()
  tweak <- .loop(target.instance = 1:length(pred.y$aggregate), .combine = rbind)(
    {
      this.instance  <- newdata[target.instance, ]
      this.aggregate <- pred.y$aggregate[target.instance]
      tree.predict   <- pred.y$individual[target.instance, 1:nestree]
      tree.agreed    <- which(tree.predict == this.aggregate )
      
      catf("instance[%i]: predicted \"%s\" (wants \"%s\"->\"%s\")", 
           target.instance, this.aggregate, label.from, label.to)
      
      tweaked.instance <- this.instance
      delta.min <- 0
      
      if(this.aggregate == label.to){
        catf("- SKIP")
      } else {
        cand.eSatisfy <- npforeach(i.tree = tree.agreed)( 
          estrees[[i.tree]][[label.to]] 
        )
        catf("- evalutate %i candidate of rules", length(estrees))
        
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
              tweaked.instance <- this.tweak
              delta.min <- delta
            }
          }
        }
      }
      return(tweaked.instance)
    }
  )
  print(Sys.time() - start.time)
  all.tweak <- list(predict = pred.y$aggregate, original = newdata, suggest = tweak)
  class(all.tweak) <- "tweaked.suggestion"
  return(all.tweak)
}


# Restore tweaked instances to the original scale

descale.tweakedFeature <- function(tweaked.X, scaled.X){
  stopifnot(class(tweaked.X) == "tweaked.suggestion", !missing(scaled.X))
  original <- descale(tweaked.X$original, scaled.X)
  suggest <- descale(tweaked.X$suggest, scaled.X)
  
  return(list(original = original, suggest = suggest,
              diff = as.tibble(suggest - original)) )
}

# Plot statistics of tweaked instances (population)
plot.tweakedPopulation <- function(
  tweaked.X, type = c( "absoluteSum", "direction", "frequency")) {
  stopifnot(class(tweaked.X) == "tweaked.suggestion")
  type <- match.arg(type)
  
  tw.diff <- data.frame(tweaked.X$suggest - tweaked.X$original)
  pos <- which(rowSums(abs(tw.diff)) > 0)
  if(length(pos) > 0){
    tw.diff <-   tw.diff[pos, ]
  }
  
  stats <- data.frame(var=colnames(tw.diff), value=colMeans(abs(tw.diff))) 
  p <- stats %>% 
    ggplot(aes(x=reorder(var, value), y=value)) +
    geom_bar(stat = "identity") +
    xlab("") + 
    ylab("mean absolute effort") +
    coord_flip()
  
  if(type == "direction"){
    stats <- tw.diff  
    p <- tw.diff %>% gather() %>% 
      mutate(var = as.factor(key)) %>% 
      ggplot(aes(x=var, y=value)) +
      geom_hline(yintercept=0, colour = "red", size = 1.5) + 
      geom_boxplot() +
      xlab("") + 
      ylab("All direction of tweak") +
      coord_flip()
  }
  
  if(type == "frequency"){
    stats <- data.frame(var=colnames(tw.diff), nonZero=colMeans(tw.diff != 0)) 
    p <- stats %>% 
      ggplot(aes(x=reorder(var, nonZero), y=nonZero)) +
      geom_bar(stat = "identity") +
      xlab("") + 
      ylab("non-zerp frequency of feature tweaking") + 
      coord_flip()
  }
  
  print(p)
  rownames(stats) <- NULL
  invisible(list(stats = stats, plot = p))
}

# Plot individual suggestion (direction to modify)
plot.suggest <- function(tweaked.X, k = 1, .ordered = FALSE, .nonzero.only = FALSE){

  stopifnot(class(tweaked.X) == "tweaked.suggestion")
  tw.diff <- data.frame(tweaked.X$suggest - tweaked.X$original)
  
  instance <- tw.diff[k, ] %>% gather
  if(.nonzero.only){
    instance %<>% filter(abs(value) > 0)
  }
  p <- ggplot(instance, aes(x=key, y=value))
  if(.order) {
    p <- ggplot(instance, aes(x=reorder(key, abs(value)), y=value))
  }
  p <- p +
    geom_hline(yintercept=0, colour = "red", size = 1.5) + 
    geom_bar(stat = "identity")  +
    ggtitle(catf("instance #%i", k)) +
    xlab("") + 
    ylab("directions of tweak") +
    coord_flip()
  
  print(p)
  invisible(p)
}

# end ---------------------------------------------------------------------
