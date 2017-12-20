require(xgboost)
require(tidyverse)

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')

dtrain <- xgb.DMatrix(agaricus.train$data, label = agaricus.train$label)
dtest <- xgb.DMatrix(agaricus.test$data, label = agaricus.test$label)
watchlist <- list(eval = dtest, train = dtrain)

## A simple xgb.train example:
param <- list(max_depth = 2, eta = 1, silent = 1, nthread = 2, 
              objective = "binary:logistic", eval_metric = "auc")
bst <- xgb.train(param, dtrain, nrounds = 2, watchlist)

## An 'xgboost' interface example:
bst <- xgboost(data = agaricus.train$data, label = agaricus.train$label, 
               max_depth = 2, eta = 1, nthread = 2, nrounds = 2, 
               objective = "binary:logistic")

xgb.plot.tree(feature_names = colnames(agaricus.train$data), model = bst)

bst %>% str
(dt <- xgb.model.dt.tree(colnames(agaricus.train$data), bst))
dt %>% str

print(xgb.dump(bst, with_stats = TRUE))

