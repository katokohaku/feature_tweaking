dim(iris) # �s��:150, ��:5
odd.n<-2*(1:75)-1
iris.train<-iris[odd.n,] # ����P���f�[�^
iris.test<-iris[-odd.n,] # ���������؃f�[�^


library(xgboost)
y <- iris.train[,5] # �ړI�ϐ�
y <- as.integer(y)-1 #xgboost �Ŋ��肳�ꂢ��N���X�� 0 base

train.x<-iris.train[,1:4]
x <- rbind(train.x,iris.test[,-5]) # xgboost ���g���Ƃ��̂���
x <- as.matrix(x)

trind <- 1:length(y) # �����`����x �̒��̌P���f�[�^���w���̂Ɏg��
teind <- (nrow(train.x)+1):nrow(x) # �����`����x �̒��̌��ؗp�f�[�^���w���̂Ɏg��

set.seed(131) # �Œ�V�[�h�Ŏ���
param <- list("objective" = "multi:softprob", # ���N���X�̕��ނŊe�N���X�ɏ�������m�������߂�
              "eval_metric" = "mlogloss", # �����֐��̐ݒ�
              "num_class" = 3 # class ���������݂���̂�
)

k<-round(1+log2(nrow(train.x)))
cv.nround <- 100 #search
bst.cv <- xgb.cv(param=param, data = x[trind,], label = y,  nfold = k, nrounds=cv.nround)

bst.cv %>% str
plot(test_mlogloss_mean~iter, bst.cv$evaluation_log, ylim=c(0.12,0.14), type = "l", col ="blue")
plot(train_mlogloss_mean~iter, bst.cv$evaluation_log[-c(1:9),], type = "b", col ="red")
abline(v=c(17,26))
nround <- which.min(bst.cv$evaluation_log$test_mlogloss_mean)

# ���f���̍\�z
bst <- xgboost(param=param, data = x[trind,], label = y, nrounds=nround)
pred <- predict(bst,x[teind,]) # ���f�����g���ė\���l���Z�o
pred <- matrix(pred,3,length(pred)/3) # �����3�N���X����̂�
pred <- t(pred)
colnames(pred)<-c("setosa","versicolor","virginica")
head(pred,3)

param <- list("objective" = "multi:softmax", # multi:softmax �ɕύX�I
              "eval_metric" = "mlogloss", 
              "num_class" = 3 
)
nround <- which.min(bst.cv$evaluation_log$test_mlogloss_mean)
bst <- xgboost(param=param, data = x[trind,], label = y, nrounds=nround)
pred <- predict(bst,x[teind,])

table(iris.test[,5],pred)

(dt <- xgb.model.dt.tree(feature_names = colnames(X), bst))
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
str(z)
head(data.frame(z) ,10)

p <- sort(as.numeric(z$leaf))
round(p,3)

iris %>% filter(Species == )