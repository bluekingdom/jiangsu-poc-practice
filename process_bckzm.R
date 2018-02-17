
isTrain <- F

rdata_root <- ifelse(isTrain, 'rawData/trainData/', 'rawData/testData/') 

load(paste(rdata_root, 'bckzm.RData', sep = ''))

names(bckzm) <- c("存款证明登记号", "机构号", "客户号", "客户中文名", "证件号", "客户账号类型", "客户账号", "顺序号", "账号", "冻结编号", "冻结日期", "解冻日期", 
                  "凭证号", "货币代号", "金额", "起息日", "到期日", "操作柜员", "复核柜员", "审评柜员", "交易日期", "备注", "时间戳", "记录状态")

cols.select <- feature.get_useful_cols_name(bckzm)
cat(cols.select)

bckzm <- bckzm %>% select(
  客户号, 客户账号类型, 金额
)

total_count <- nrow(bckzm)
bckzm <- bckzm[sample(1:total_count, sample_rate * total_count), ]

# for (i in 1:length(names(bckzm))) {
  # n <- names(bckzm)[i]

# for (i in 1:length(cols.select)) {
#   n <- cols.select[i]
#   cat(n)
#   cat('\n')
#   if (n == '客户号') next
# 
#   t <- (table(bckzm[, n]))
#   cat(names(t))
#   cat('\n\n')
#   cat(t)
#   cat('\n\n')
#   barplot(t)
#   title(n)
# }


# for (i in 1:length(names(bckzm))) {
#   n <- names(bckzm)[i]
#   val <- bckzm[, n]
#   idx <- (val == '\\N' | val == ' ')
#   bckzm[, n][idx] <- NA
# }

bckzm <- bckzm %>% 
  mutate_at('金额', as.numeric) %>% 
  mutate_at('客户号', as.character) %>% 
  mutate_at('客户账号类型', as.factor)

summary(bckzm)

bckzm <- bckzm %>% group_by(客户号) %>% summarise(
  bckzm.money.max = max(金额, na.rm = T),
  bckzm.money.mean = mean(金额, na.rm = T),
  bckzm.money.sd = sd(金额, na.rm = T),
  custom_account_type.mode = Mode(客户账号类型, na.rm = T)
)

# head(bckzm)
# summary(bckzm)

if (isTrain) {
  train.feature.bckzm <- bckzm
} else {
  test.feature.bckzm <- bckzm
}

rm(bckzm)
