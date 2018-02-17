# XGBoost method by JSW

# This file contains the XGBoost training code using ONE HOT ENCODING
rm(list =ls())

# load necessary packages
require(xgboost)  # xgboost algorithm
require(Matrix)   # to create sparse matrix
require(data.table)  # to create data.table format
require(mlr)  # to create dummy variables of one hot
require(ROCR)  # to plot ROC curve
require(dplyr)  # data manipulation
require(ggplot2)

ThisMetrics <- function(x, y) {
  
  TP <- sum(x & y)
  FP <- sum(x & !y)
  FN <- sum(!x & y)
  TN <- sum(!x & !y)
  RECALL = TP/(TP+FN)
  PRECISION = TP/(TP+FP)
  Fscore = 2*PRECISION*RECALL/(PRECISION+RECALL)
  #print(paste("RECALL:",RECALL, "PRECISION:", PRECISION))
  c(round(RECALL,3),round(PRECISION,3),round(Fscore,3))
}

# load the training data
load('rawData/2stepDiscreteData/train.discrete.Rdata')
load('rawData/2stepDiscreteData/test.discrete.Rdata')
load("rawData/testData/labels.RData")  # provide all the true labels of test data

test.label <- train.label
class(test.label$客户号)

# convert kehuhao to character
test.label <- test.label %>% mutate_at('客户号', as.character )
# initiate all labels to be 0
all.true.labels <- test.discrete %>% select(客户号) %>% mutate(label = 0)
# set the true label as 1, 
all.true.labels[which(all.true.labels$客户号 %in% test.label$客户号),]$label <- 1
table(all.true.labels$label)
true.label <- all.true.labels$label

# use one hot encoding to dummify the factor type data
train.discrete$客户号 <- NULL
test.discrete$客户号 <- NULL
data.type <- sapply(train.discrete, class)
factor.var <- names(train.discrete)[which(data.type == 'factor' | data.type == 'character')]
train.discrete <- train.discrete %>% mutate_at(factor.var, as.factor)
test.discrete <- test.discrete %>% mutate_at(factor.var, as.factor)

# row bind train.discrete and test.discrete together to make the same one-hot encoding
rbind.train.test <- rbind(train.discrete[,-ncol(train.discrete)], test.discrete)
rbind.features <- createDummyFeatures(rbind.train.test, cols = factor.var)
# the train feature and label
train.feature <- rbind.features[1:nrow(train.discrete), ]
train.label <- train.discrete[,ncol(train.discrete)]
# save to file
#save(train.feature, file = 'train.feature.RData')
#save(train.label, file = 'train.label.RData')

# the test feature and label
test.feature <- rbind.features[(nrow(train.discrete)+1):nrow(rbind.features), ]
rm(rbind.features)
rm(rbind.train.test)
#test.label <- test.data[,1]
# save to file
#save(test.feature, file = 'test.feature.RData')
#save(test.label, file = 'test.label.RData')

# the sparse matrix of training features
#train.sparse <- sparse.model.matrix(default ~ ., data = train.data)[,-1]
# the -1 remove the intercept column which is added automatically by the covnersion
train.feature <- train.feature %>% mutate_all(as.numeric)
test.feature <- test.feature %>% mutate_all(as.numeric)

xgmat <- xgb.DMatrix(data = Matrix(as.matrix(train.feature), sparse = TRUE), label = as.matrix(train.label), missing = NA)
xgmat1 <- xgb.DMatrix(data = Matrix(as.matrix(train.feature1), sparse = TRUE), label = as.matrix(train.label), missing = NA)
# write the DMatrix to file
# xgb.DMatrix.save(xgmat, 'xgb.DMatrix.data')
# load DMatrix from file
# xgmat <- xgb.DMatrix('xgb.DMatrix.data')

##########################################################################################
# cross-validation
# use the cv brought by xgboost itself

# parameters for model training
my_params1 <- list(objective = "binary:logistic",
                   eta = 0.08, # learning rate
                   max_depth = 4,  # depth of trees
                   lambda = 0.02,  # l2 penalty
                   alpha = 0.00,  # l1 penalty
                   nthread = 24)   # number of threads


#nrounds <- 100
set.seed(2014)
cv.model <- xgb.cv(params = my_params1, data =xgmat,
                   nfold = 10, nrounds =300, verbose = TRUE, prediction = TRUE, metrics = 'logloss')

# plot the training and test error
p1 <- ggplot(cv.model$evaluation_log,aes(c(1:length(cv.model$evaluation_log$train_logloss_mean)))) + 
  geom_point(aes(y=cv.model$evaluation_log$train_logloss_mean,colour="Train")) + 
  geom_line(aes(y=cv.model$evaluation_log$train_logloss_mean,colour="Train")) + 
  geom_point(aes(y=cv.model$evaluation_log$test_logloss_mean,colour="Test")) + 
  geom_line(aes(y=cv.model$evaluation_log$test_logloss_mean,colour="Test")) + 
  xlab("nrounds") + ylab("error") +
  scale_colour_manual("", breaks = c("Train", "Test"), values = c("red", "black")) +
  labs(title="error vs nrounds") +
  theme(plot.title = element_text(hjust = 0.5))
p1

# p1 <- ggplot(cv.model$dt,aes(c(1:length(cv.model$dt$train.logloss.mean)))) + 
#   geom_point(aes(y=cv.model$dt$train.logloss.mean,colour="Train")) + 
#   geom_line(aes(y=cv.model$dt$train.logloss.mean,colour="Train")) + 
#   geom_point(aes(y=cv.model$dt$test.logloss.mean,colour="Test")) + 
#   geom_line(aes(y=cv.model$dt$test.logloss.mean,colour="Test")) + 
#   xlab("nrounds") + ylab("error") +
#   scale_colour_manual("", breaks = c("Train", "Test"), values = c("red", "black")) +
#   labs(title="error vs nrounds") +
#   theme(plot.title = element_text(hjust = 0.5))

print(cv.model)


#########################################################################################
#   train the model and inspect the feature importance
positive.count <- train.label %>% sum()
negative.count <- nrow(train.label) - positive.count
scale.weight <- negative.count / positive.count
my_params2 <- list(objective = "binary:logistic",
                   eta = 0.08, # learning rate
                   max_depth = 4, # depth of tree
                   lambda = 0.02,  # l2 penalty
                   alpha = 0.00,  # l1 penalty
                   nthread = 20,
                   scale_pos_weight = scale.weight )  # number of threads

watchlist <- list('train' = xgmat)
dim(xgmat)
model <- xgb.train(params = my_params2, data =xgmat,
                   nrounds =75, verbose = 1, prediction = TRUE, 
                   eval_metric = "logloss", watchlist = watchlist)
xgb.save(model, 'XGBjsw.model')
pred <- predict(model, Matrix(as.matrix(test.feature), sparse = TRUE))
head(pred)
save(pred, file = 'XGboostPred.csv')

# set pred threshold to convert pred to 1 or 0
thre <- 0.54
prediction <- as.numeric(pred > thre)
# visualize the confusion matrix
table(prediction,true.label)

res <- ThisMetrics(prediction,true.label)
print(paste("RECALL:",res[1], "PRECISION:", res[2], "F-Score:", res[3]))

this.auc <- measureAUC(pred, true.label, 0, 1)
# or use this.auc = performance(pred1, measure = "auc")@y.values[[1]], same result
print(paste("AUC is",this.auc))

########################### plot ROC curve and AUC
pred1 <- prediction(pred,true.label)
perf1 <- performance(pred1,"tpr","fpr")
plot(perf1, main="JSB POC - XGBoost ROC Curves Iteration 75, threshold 0.54",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="blue")
abline(0,1,col="grey")
text(x=0.8,y=0.4,labels=c(paste("AUC       = ",round(this.auc,3))))
text(x=0.8,y=0.3,labels=c(paste("RECALL    = ",round(res[1],3))))
text(x=0.8,y=0.2,labels=c(paste("PRECISION = ",round(res[2],3))))
text(x=0.8,y=0.1,labels=c(paste("F-Score   = ",round(res[3],3))))
##########################



######################### visualze the training error
p2 <- ggplot(model$evaluation_log,aes(model$evaluation_log$iter)) + 
  geom_point(aes(y=model$evaluation_log$train_error,colour="Train")) + 
  geom_line(aes(y=model$evaluation_log$train_error,colour="Train")) + 
  xlab("nrounds") + ylab("error")  +
  scale_colour_manual("", breaks = "Train", values = "red") +
  labs(title="train error vs nrounds") +
  theme(plot.title = element_text(hjust = 0.5))
p2

####################### visualize the importance matrix
importance.matrix <- xgb.importance(feature_names = colnames(train.feature), model = model)
xgb.plot.importance(importance.matrix)
#xgb.ggplot.importance(importance.matrix)
# save the importance file
write.csv(importance.matrix,file = 'importance.csv')
# plot the deepness of the tree
#xgb.plot.deepness(model = model)
xgb.ggplot.deepness(model = model)

# plot the tree
xgb.plot.multi.trees(model = model,feature_names = colnames(train.feature))


# end of this file
