# devtools::install_github("AppliedDataSciencePartners/xgboostExplainer")
library(xgboost)
library(xgboostExplainer)
library(tidyverse)
  
set.seed(123)

data(agaricus.train, package='xgboost')

X = as.matrix(agaricus.train$data)
y = agaricus.train$label

train_idx = 1:5000

train.data = X[train_idx,]
test.data = X[-train_idx,]

xgb.train.data <- xgb.DMatrix(train.data, label = y[train_idx])
xgb.test.data <- xgb.DMatrix(test.data)

param <- list(objective = "binary:logistic")
xgb.model <- xgboost(param =param,  data = xgb.train.data, nrounds=3)

col_names = colnames(X)

pred.train = predict(xgb.model,X)
nodes.train = predict(xgb.model,X,predleaf =TRUE)
trees = xgb.model.dt.tree(col_names, model = xgb.model)

#### The XGBoost Explainer
# explainer = buildExplainer(xgb.model,xgb.train.data, type="binary", 
#                            base_score = 0.5, n_first_tree = xgb.model$best_ntreelimit - 1)
trainingData = xgb.train.data
xgb.model = xgb.model
type="binary"
base_score = 0.5
n_first_tree = xgb.model$best_ntreelimit - 1

col_names = attr(trainingData, ".Dimnames")[[2]]
cat("\nCreating the trees of the xgboost model...")
trees = xgb.model.dt.tree(col_names, model = xgb.model, 
                          n_first_tree = n_first_tree)
cat("\nGetting the leaf nodes for the training set observations...")
nodes.train = predict(xgb.model, trainingData, predleaf = TRUE)
cat("\nBuilding the Explainer...")
cat("\nSTEP 1 of 2")
tree_list = xgboostExplainer:::getStatsForTrees(trees, nodes.train, type = type, 
                             base_score = base_score)

tree_list = data.table::copy(trees)
tree_list[, `:=`(leaf, Feature == "Leaf")]

tree_list[, `:=`(H, Cover)]
non.leaves = which(tree_list[, leaf] == F)
cat("\n\nRecalculating the cover for each non-leaf... \n")
pb <- txtProgressBar(style = 3)
j = 0
for (i in rev(non.leaves)) {
  left = tree_list[i, Yes]
  right = tree_list[i, No]
  tree_list[i, `:=`(H, tree_list[ID == left, H] + tree_list[ID ==
                                                              right, H])]
  j = j + 1
  setTxtProgressBar(pb, j/length(non.leaves))
}
tree_list

if (type == "regression") {
  base_weight = base_score
} else {
  base_weight = log(base_score/(1 - base_score))
}

tree_list[leaf == T, `:=`(weight, base_weight + Quality)]
tree_list[, `:=`(previous_weight, base_weight)]
tree_list[1, `:=`(previous_weight, 0)]
tree_list[leaf == T, `:=`(G, -weight * H)]

tree_list = split(tree_list, as.factor(tree_list$Tree))
num_tree_list = length(tree_list)
treenums = as.character(0:(num_tree_list - 1))
t = 0
cat("\n\nFinding the stats for the xgboost trees...\n")
pb <- txtProgressBar(style = 3)
for (tree in tree_list) {
  t = t + 1
  num_nodes = nrow(tree)
  non_leaf_rows = rev(which(tree[, leaf] == F))
  for (r in non_leaf_rows) {
    left = tree[r, Yes]
    right = tree[r, No]
    leftG = tree[ID == left, G]
    rightG = tree[ID == right, G]
    tree[r, `:=`(G, leftG + rightG)]
    w = tree[r, -G/H]
    tree[r, `:=`(weight, w)]
    tree[ID == left, `:=`(previous_weight, w)]
    tree[ID == right, `:=`(previous_weight, w)]
  }
  tree[, `:=`(uplift_weight, weight - previous_weight)]
  setTxtProgressBar(pb, t/num_tree_list)
}
tree_list

# return(tree_list)
# cat("\n\nSTEP 2 of 2")
# explainer = xgboostExplainer:::buildExplainerFromTreeList(tree_list, col_names)
tree_list_breakdown <- vector("list", length(col_names) + 
                                3)
names(tree_list_breakdown) = c(col_names, "intercept", "leaf", 
                               "tree")
num_trees = length(tree_list)
cat("\n\nGetting breakdown for each leaf of each tree...\n")
pb <- txtProgressBar(style = 3)
for (x in 1:num_trees) {
  tree = tree_list[[x]]
  tree_breakdown = xgboostExplainer:::getTreeBreakdown(tree, col_names)
  tree_breakdown$tree = x - 1
  tree_list_breakdown = data.table::rbindlist(append(list(tree_list_breakdown), 
                                         list(tree_breakdown)))
  setTxtProgressBar(pb, x/num_trees)
}
cat("\n\nDONE!\n")
tree_list_breakdown %>% dim

pred.breakdown = explainPredictions(xgb.model, explainer, xgb.test.data)
pred.breakdown %>% rowSums() %>% table

showWaterfall(xgb.model, explainer, xgb.test.data, test.data,  1, type = "binary")
showWaterfall(xgb.model, explainer, xgb.test.data, test.data,  2, type = "binary")
showWaterfall(xgb.model, explainer, xgb.test.data, test.data,  3, type = "binary")
showWaterfall(xgb.model, explainer, xgb.test.data, test.data,  4, type = "binary")
showWaterfall(xgb.model, explainer, xgb.test.data, test.data,  5, type = "binary")
showWaterfall(xgb.model, explainer, xgb.test.data, test.data,  6, type = "binary")
showWaterfall(xgb.model, explainer, xgb.test.data, test.data,  7, type = "binary")
showWaterfall(xgb.model, explainer, xgb.test.data, test.data,  8, type = "binary")
