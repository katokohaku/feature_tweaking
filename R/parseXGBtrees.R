require(xgboost)
require(tidyverse)

source("./R/utils.R")

# set up to run -------------------------------------------------------------

# get dataset
data(spam, package = "kernlab")
dataset <- spam; label.from <- "spam"   ; label.to = "nonspam"
dataset %>% str; head(dataset)

X <- scale( dataset[, 1:(ncol(dataset)-1)] )
rescale(head(X), X)[,1:6]
head(dataset)[,1:6]

true.y <- dataset[, ncol(dataset)] %>% as.factor() %>% as.integer() -1

## A simple xgb.train example: "classification"
param <- list(max_depth = 3, eta = 1, silent = 1, nthread = 2,
              objective = "binary:logistic", eval_metric = "auc")

# k<-round(1+log2(nrow(X)))
# cv.nround <- 100 #search
# bst.cv <- xgb.cv(param=param, data = X, label = true.y,  nfold = k, nrounds=cv.nround)

nround = 15
bst <- xgboost(param=param, data = X, label = true.y, nrounds=nround)
bst %>% str

pred <- xgboost:::predict.xgb.Booster(bst,X, predleaf = TRUE) 
pred %>% str()
plot(pred, true.y, col=2+true.y)

data.frame(Feature=1:NCOL(X),Name=colnames(X)) %>% 
  right_join(
    xgb.importance(model = bst) %>% 
      mutate_all(as.numeric) %>% round(3) %>% 
      arrange(desc(Gain)))

# xgb.plot.tree(model = bst)

bst %>% str
(dt <- xgb.model.dt.tree(colnames(X), bst))

dmp <- data.frame(x = xgb.dump(bst, with_stats = FALSE), booster=NA) 
cn =NULL
for(i in 1:NROW(dmp)){
  if(str_detect(dmp[i, ]$x,"booster")){
    cn <- as.character(dmp[i, ]$x)
  }
  dmp[i, ]$booster <- cn
}


dmp <- dmp %>% group_by(booster) %>% nest()
ldmp <-  map(dmp$data, function(x) chop(data.frame(x), -1))
NNN=2

z <-ldmp[[NNN + 1]] %>%
  mutate(x = as.character(x))%>% 
  map(., function(x) str_split(x, ":")) %>% 
  flatten() %>% 
  map(~ data_frame(Node=as.numeric(.x[1]), rule=.x[2])) %>% 
  bind_rows() %>% 
  arrange(Node) %>% 
  left_join(filter(dt, Tree == NNN), by ="Node") %>% 
  mutate(Leaf = str_split_fixed(rule,"leaf=", n=2)[,2],
         left = str_split_fixed(.$Yes, "-", n=2)[,2],
         right= str_split_fixed(.$No,  "-", n=2)[,2]) %>%
  mutate_all(.funs=function(x) ifelse(x=="", NA,x)) %>% 
  select(Node, Feature, Split, left, right, Leaf, rule, everything()) %>% 
  rename(left.daughter = left, right.daughter=right, split.var=Feature, 
         split.point=Split, leaf=Leaf)
z



ldmp[[NNN + 1]][1,] %>% str

