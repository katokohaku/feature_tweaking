require(xgboost)
require(tidyverse)

source("./R/utils.R")

# set up to run -------------------------------------------------------------

# get dataset
data(spam, package = "kernlab")
dataset <- spam; label.from <- "spam"   ; label.to = "nonspam"
dataset %>% str
head(dataset)

# "classification"
X <- scale( dataset[, 1:(ncol(dataset)-1)] )
rescale(head(X), X)[,1:6]
head(dataset)[,1:6]

true.y <- dataset[, ncol(dataset)] %>% as.factor() %>% as.integer() -1

## A simple xgb.train example:
param <- list(max_depth = 2, eta = 1, silent = 1, nthread = 2, 
              objective = "binary:logistic", eval_metric = "auc")

k<-round(1+log2(nrow(X)))
cv.nround <- 100 #search
bst.cv <- xgb.cv(param=param, data = X, label = true.y,  nfold = k, nrounds=cv.nround)

nround = 27
bst <- xgboost(param=param, data = X, label = true.y, nrounds=nround)
pred <- predict(bst,X) 
pred %>% str
plot(pred, true.y, col=2+true.y)
xgb.importance(model = bst)

# xgb.plot.tree(model = bst)

bst %>% str
(dt <- xgb.model.dt.tree(colnames(X), bst, ))
dtm <- merge(dt, dt[, .(ID, Y.Feature=Feature)], 
             by.x='Yes', by.y='ID', all.x=TRUE)[order(Tree,Node)]
  
for(i in 0:2){
  dtm %>%
    filter(Tree == i) %>% 
    mutate(left = str_split_fixed(.$Yes, "-", n=2)[,2],
           right= str_split_fixed(.$No,  "-", n=2)[,2]) %>% 
    select(-Yes, -No) %>% 
    print 
}
dt$left <- 1#
  c(str_split_fixed(dt$Yes, "-",n=2)[, 2])

dmp <- data.frame(x = xgb.dump(bst, with_stats = FALSE))

dmp$y <- str_detect(dmp$x,"booster")
dmp
nt=0
v <- NULL
for(i in dmp$y){ 
  if(i){ nt <- nt+1 }
  v <- c(v, nt)
}
dmp$nt <- v

dmp <- dmp %>% group_by(nt) %>% nest()
ldmp <-
  map(dmp$data, 
      function(x, y){
        chop(data.frame(x), -1) %>% select(-y)
      })
NNN=26
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
  select(Node, Feature, Split, left, right, Leaf, rule, everything())
z
replace_na(z, replace=list(""))

