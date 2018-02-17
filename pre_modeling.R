
################## combine feature ###########################
train.feature <- train.feature.cust_info 
test.feature <- test.feature.cust_info 
rm(train.feature.cust_info)
rm(test.feature.cust_info)

train.feature <- train.feature.adshq
test.feature <- test.feature.adshq 
rm(train.feature.adshq)
rm(test.feature.adshq)

train.feature <- train.feature.asdfh
test.feature <- test.feature.asdfh 
rm(train.feature.asdfh)
rm(test.feature.asdfh)

train.feature <- train.feature.vyktd
test.feature <- test.feature.vyktd
rm(train.feature.vyktd)
rm(test.feature.vyktd)


train.feature <- train.feature.cust_info %>% 
  left_join(train.feature.adshq, by = '客户号') %>% 
  left_join(train.feature.asdfh, by = '客户号') %>%
  left_join(train.feature.vyktd, by = '客户号') %>%
  left_join(train.feature.tranflow, by = '客户号') %>%
  left_join(train.feature.bckzm, by = '客户号') 

test.feature <- test.feature.cust_info %>% 
  left_join(test.feature.adshq, by = '客户号') %>%
  left_join(test.feature.asdfh, by = '客户号') %>%
  left_join(test.feature.vyktd, by = '客户号') %>%
  left_join(test.feature.tranflow, by = '客户号') %>%
  left_join(test.feature.bckzm, by = '客户号') 
  

################## generate label ###########################
load('rawData/trainData/labels.RData')
labels$label <- as.integer(labels$label)
labels$客户号 <- as.character(labels$客户号)

train.label <- train.feature %>% left_join(labels, '客户号') %>% select(客户号, label)
head(train.label)
head(train.feature)

load('rawData/testData/labels.RData')
labels$label <- as.integer(labels$label)
labels$客户号 <- as.character(labels$客户号)

# head(labels)

test.label <- test.feature %>% select(客户号) %>% mutate(label = 0)

true_idx <- which( test.label$客户号 %in% labels$客户号 )
test.label$label[true_idx] <- 1

head(test.label)
head(test.feature)

rm(labels)
rm(true_idx)

save(train.feature, file = 'train.feature.RData')
save(test.feature, file = 'test.feature.RData')
save(train.label, file = 'train.label.RData')
save(test.label, file = 'test.label.RData')
