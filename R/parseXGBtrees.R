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

# k<-round(1+log2(nrow(X)))
# cv.nround <- 100 #search
# bst.cv <- xgb.cv(param=param, data = X, label = true.y,  nfold = k, nrounds=cv.nround)

nround = 15
bst <- xgboost(param=param, data = X, label = true.y, nrounds=nround)
pred <- predict(bst,X) 
pred %>% str
plot(pred, true.y, col=2+true.y)

data.frame(Feature=1:NCOL(X),Name=colnames(X)) %>% 
  right_join(
    xgb.importance(model = bst) %>% 
      mutate_all(as.numeric) %>% round(3) %>% 
      arrange(desc(Gain)))

# xgb.plot.tree(model = bst)

bst %>% str
(dt <- xgb.model.dt.tree(colnames(X), bst))
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
colnames(tree.df) <- c("left.daughter", "right.daughter", "split.var", 
                       "split.point", "status", "leaf")


ldmp[[NNN + 1]][1,]

df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
df %>% replace_na(list(x = 0, y = "unknown"))

## last expression will have to be:
# node path.to    split.var lr   point
# 1    1       2   charDollar  < -0.0826
# 2    2       5         free >= -0.0652
# 3    5      11          our >= -0.3899
# 4   11      23 capitalTotal >= -0.0186
# 5   23      44           hp  <  0.2456
