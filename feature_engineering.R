

load(paste(rdata_root, 'vyktd.RData', sep = ''))

total_count <- nrow(vyktd)
sample_index <- sample(1:total_count, sample_rate * total_count)
vyktd <- vyktd[sample_index, ]

vyktd <- vyktd %>% select(
  客户号
)

vyktd <- vyktd %>% group_by(客户号) %>% summarise(
  kashu = n()
) 
vyktd$客户号 <- as.character(vyktd$客户号)


load(paste(rdata_root, 'tranflow.RData', sep = ''))
total_count <- nrow(tranflow)
sample_index <- sample(1:total_count, sample_rate * total_count)
tranflow <- tranflow[sample_index, ]

tranflow <- tranflow %>% select(
  网银客户号 , 交易金额 
)

tranflow <- tranflow %>% group_by(网银客户号) %>%
  summarise(
    jine.mean = mean(交易金额, na.rm = T),
    jine.max = max(交易金额, na.rm = T),
    jine.sd = sd(交易金额, na.rm = T)
  ) %>% mutate_at('网银客户号', as.character)
names(tranflow)[1] <- '客户号'

load(paste(rdata_root, 'asdfh.RData', sep = ''))
total_count <- nrow(asdfh)
sample_index <- sample(1:total_count, sample_rate * total_count)
asdfh <- asdfh[sample_index, ]

asdfh <- asdfh %>% select(
  客户号 , 开户金额 , 上日帐户余额 , 前日余额 , 帐户余额
)

asdfh.split_by_kehuhao <- split(asdfh, asdfh$客户号)

asdfh.list <- mclapply(asdfh.split_by_kehuhao, function(x){
  sel <- x %>% mutate( balance_open = (开户金额), balance_up_day = 上日帐户余额, 
                       balance_previous_day = 前日余额, balance = 帐户余额 )
  sel %>% group_by(客户号) %>% summarise(
    dingqi.balance_open.max = max(balance_open, na.rm = T),
    dingqi.balance_open.mean = mean(balance_open, na.rm = T),
    dingqi.balance_open.sd = sd(balance_open, na.rm = T),
    dingqi.balance_up_day.max = max(balance_up_day, na.rm = T),
    dingqi.balance_up_day.mean = mean(balance_up_day, na.rm = T),
    dingqi.balance_up_day.sd = sd(balance_up_day, na.rm = T),
    dingqi.balance_previous_day.max = max(balance_previous_day, na.rm = T),
    dingqi.balance_previous_day.mean = mean(balance_previous_day, na.rm = T),
    dingqi.balance_previous_day.sd = sd(balance_previous_day, na.rm = T),
    dingqi.balance.sd = max(balance, na.rm = T),
    dingqi.balance.mean = mean(balance, na.rm = T),
    dingqi.balance.sd = sd(balance, na.rm = T)
  )
}, mc.cores = 20)

asdfh <- data.table::rbindlist(asdfh.list)
rm(asdfh.list)
asdfh$客户号 <- as.character(asdfh$客户号)

load(paste(rdata_root, 'adshq.RData', sep = ''))
total_count <- nrow(adshq)
sample_index <- sample(1:total_count, sample_rate * total_count)
adshq <- adshq[sample_index, ]

adshq <- adshq %>% select(
  客户号, 前日余额 , 积数 , 相关贷款户数 , 欠款户数 , 上日帐户余额 , 欠款户数 , 帐户余额
)

adshq.split_by_kehuhao <- split(adshq, adshq$客户号)

adshq.list <- mclapply(adshq.split_by_kehuhao, function(x){
  sel <- x %>% mutate( balance_up_day = 上日帐户余额, balance_previous_day = 前日余额, 
                       balance = 帐户余额 , jishu = 积数 , relative_loan_count = 相关贷款户数 ,
                       debt_count = 欠款户数 )
  sel %>% group_by(客户号) %>% summarise(
    huoqi.balance_up_day.max = max(balance_up_day, na.rm = T),
    huoqi.balance_up_day.mean = mean(balance_up_day, na.rm = T),
    huoqi.balance_up_day.sd = sd(balance_up_day, na.rm = T),
    huoqi.balance_previous_day.max = max(balance_previous_day, na.rm = T),
    huoqi.balance_previous_day.mean = mean(balance_previous_day, na.rm = T),
    huoqi.balance_previous_day.sd = sd(balance_previous_day, na.rm = T),
    huoqi.balance.sd = max(balance, na.rm = T),
    huoqi.balance.mean = mean(balance, na.rm = T),
    huoqi.balance.sd = sd(balance, na.rm = T),
    huoqi.jishu.max = max(jishu, na.rm = T),
    huoqi.jishu.mean = mean(jishu, na.rm = T),
    huoqi.jishu.sd = sd(jishu, na.rm = T),
    huoqi.relative_loan_count.max = max(relative_loan_count, na.rm = T),
    huoqi.relative_loan_count.mean = mean(relative_loan_count, na.rm = T),
    huoqi.relative_loan_count.sd = sd(relative_loan_count, na.rm = T),
    huoqi.debt_count.max = max(debt_count, na.rm = T)
  )
}, mc.cores = 20)

adshq$客户号 <- as.character(adshq$客户号)
adshq <- data.table::rbindlist(adshq.list)
rm(adshq.list)

load(paste(rdata_root, 'bckzm.RData', sep = ''))
total_count <- nrow(bckzm)
sample_index <- sample(1:total_count, sample_rate * total_count)
bckzm <- bckzm[sample_index, ]

bckzm <- bckzm %>% select(
  客户号 , 金额
)

bckzm <- bckzm %>% mutate(money = 金额) %>% group_by(客户号) %>% summarise(
  zhengming.money.max = max(money, na.rm = T),
  zhengming.money.mean = mean(money, na.rm = T),
  zhengming.money.sd = sd(money, na.rm = T)
) %>% mutate_at(客户号, as.character)

feature <- cust_info %>% 
  left_join(tranflow, by = '客户号') %>% 
  left_join(vyktd, by = '客户号') %>% 
  left_join(bckzm, by = '客户号') %>% 
  left_join(asdfh, by = '客户号') %>% 
  left_join(adshq, by = '客户号')

