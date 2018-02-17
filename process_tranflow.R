
isTrain <- F

rdata_root <- ifelse(isTrain, 'rawData/trainData/', 'rawData/testData/') 

load(paste(rdata_root, 'tranflow.RData', sep = ''))

names(tranflow) <- c("网银交易流水号", "批次号", "客户号", "网银交易代码", "付款方账号", "付款方账号类型1", "付款方账号子账号", "付款方账号开户行", 
                     "付款方户名", "收款方账号", "付款方账号类型", "收款方户名", "收款方开户行", "转账类型", "收款方开户行联行号", "币种", "钞汇标识", 
                     "用户提交时间", "交易金额", "邮电费", "手续费", "工本费", "付款用途", "用户备注", "银行备注", "加急标志", "认证方式", "预约时间", 
                     "预约类型", "核心流水号", "交易发送主机时间", "主机返回错误代码", "主机返回业务编号", "主机返回柜员流水", "指令状态", "渠道标识")

# cols.select <- feature.get_useful_cols_name(tranflow)
# cat(cols.select)

tranflow <- tranflow %>% select(
  客户号, 交易金额, 渠道标识
)

total_count <- nrow(tranflow)
tranflow <- tranflow[sample(1:total_count, sample_rate * total_count), ]

# for (i in 1:length(names(tranflow))) {
#   n <- names(tranflow)[i]
#   cat(n)
#   cat('\n')
#   if (n == '客户号') next
#   
#   t <- (table(tranflow[, n]))
#   cat(names(t))
#   cat('\n\n')
#   cat(t)
#   cat('\n\n')
#   barplot(t)
#   title(n)
# }


for (i in 1:length(names(tranflow))) {
  n <- names(tranflow)[i]
  val <- tranflow[, n]
  idx <- (val == '\\N' | val == ' ')
  tranflow[, n][idx] <- NA
}

tranflow <- tranflow %>% 
  mutate_at('交易金额', as.numeric) %>% 
  mutate_at('客户号', as.character) %>% 
  mutate_at('渠道标识', as.factor)

summary(tranflow)

tranflow <- tranflow %>% group_by(客户号) %>% summarise(
  transaction_money.max = max(交易金额, na.rm = T),
  transaction_money.mean = mean(交易金额, na.rm = T),
  transaction_money.sd = sd(交易金额, na.rm = T),
  channel_idenification.mode = Mode(渠道标识, na.rm = T)
)

# head(tranflow)
# summary(tranflow)

if (isTrain) {
  train.feature.tranflow <- tranflow
} else {
  test.feature.tranflow <- tranflow
}

rm(tranflow)
