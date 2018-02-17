
train_test.disperse <- rbind(train.feature, test.feature)
train_test.disperse$客户号 <- NULL

vars.class <- sapply(train_test.disperse, class)
vars.need_dummy <- names(train_test.disperse)[vars.class == 'factor' | vars.class == 'character']
vars.need_dummy

train_test.disperse <- train_test.disperse %>% mutate_at(vars.need_dummy, as.factor)
train_test.disperse <- createDummyFeatures(train_test.disperse, cols = vars.need_dummy)

train.disperse <- train_test.disperse[0: nrow(train.feature), ]
test.disperse <- train_test.disperse[-(0: nrow(train.feature)), ]

rm(train_test.disperse)

summary(train.disperse)

# summary(train.disperse)

# train.disperse <- sparse.model.matrix(~ .-1, data = train.disperse )
sapply(train.disperse, class)

train.xgmat <- xgb.DMatrix(data = as.matrix(train.disperse), label = as.matrix(train.label$label), missing = NA)
test.xgmat <- xgb.DMatrix(data = as.matrix(test.disperse), label = as.matrix(test.label$label), missing = NA)

##################################################

train_test.label <- rbind(train.label, test.label)
train_test.xgmat <- xgb.DMatrix(data = as.matrix(train_test.disperse), label = as.matrix(train_test.label$label), missing = NA)

positive.count <- train.label$label %>% sum()
negative.count <- nrow(train.label) - positive.count
scale.weight <- negative.count / positive.count

my_params1 <- list(objective = "binary:logistic",
                   eta = 0.5, # learning rate
                   max_depth = 5,  # depth of trees
                   lambda = 0.02,  # l2 penalty
                   alpha = 0.02,  # l1 penalty
                   nthread = 24
                   , scale_pos_weight = scale.weight
)   

set.seed(2018)
model.cv <- xgb.cv(params = my_params1, data = train_test.xgmat,
                   nfold = 10, nrounds = 100, verbose = TRUE, metrics = list('error', 'auc') )


p <- ggplot(model.cv$evaluation_log, aes(x=1:length(model.cv$evaluation_log$iter))) +
  geom_line(aes(y=model.cv$evaluation_log$train_auc_mean, colour = 'train')) +
  geom_line(aes(y=model.cv$evaluation_log$test_auc_mean, colour = 'test')) +
  ggtitle('cross validation')
p

##################################################
# grid search

searchGridSubCol <- expand.grid(
  max_depth = c(3, 4, 5),
  lambda = c(0.2, 0.02, 0.002, 0.0002),
  alpha = c(0.2, 0.02, 0.002, 0.0002)
)

searchGrid.result <- apply(searchGridSubCol, 1, function(param) {
  max_depth <- param[['max_depth']]
  lambda <- param[['lambda']]
  alpha <- param[['alpha']]
  
  print(param)
  
  my_params1 <- list(objective = "binary:logistic",
                   eta = 0.5, # learning rate
                   max_depth = max_depth,  # depth of trees
                   lambda = lambda,  # l2 penalty
                   alpha = alpha,  # l1 penalty
                   nthread = 20
                   , scale_pos_weight = scale.weight
  )

  model.cv <- xgb.cv(params = my_params1, data = train_test.xgmat,
                   nfold = 2, nrounds = 10, verbose = 1, metrics = list('auc') )

  test_auc <- tail(model.cv$evaluation_log$test_auc_mean, 1)
  train_auc <- tail(model.cv$evaluation_log$train_auc_mean, 1)

  res <- list(test_auc, train_auc, max_depth, lambda, alpha)
  
  res
})

searchGrid.result


##################################################

watchlist <- list('train' = train.xgmat, 'test' = test.xgmat)

model <- xgb.train(params = my_params1, data = train.xgmat, watchlist = watchlist,
                   nrounds = 30, verbose = 1, metrics = list('error', 'auc'))

p <- ggplot(model$evaluation_log, aes(x=1:length(model$evaluation_log$iter))) +
  geom_line(aes(y=model$evaluation_log$train_error, colour = 'train')) +
  geom_line(aes(y=model$evaluation_log$test_error, colour = 'test')) +
  ggtitle('xgboost train')
p

pred <- predict(model, newdata = test.xgmat)

plot(pred)

pred.class <- as.numeric(pred > 0.9)
table(pred.class)

table(pred.class, test.label$label)
table((test.label$label))
table(as.factor(test.label$label))

pred.roc <- roc(pred, as.factor(test.label$label))

plot(pred.roc)
auc(pred.roc)

model_iter_1000 <- model
save(file = 'models/model_iter_1000_roc_0.915.RData',model_iter_1000)
