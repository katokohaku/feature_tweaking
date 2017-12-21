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
