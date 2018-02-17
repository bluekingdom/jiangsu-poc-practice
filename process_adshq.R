
isTrain <- T

rdata_root <- ifelse(isTrain, 'rawData/trainData/', 'rawData/testData/') 

load(paste(rdata_root, 'adshq.RData', sep = ''))

names(adshq) <- c("主账号", "账号", "营业机构号", "账务机构号", "货币代号", "业务代号", "账号序号", "钞汇标志", "客户号", "个人中文名", "信息代码", "科目号", "余额性质", 
                  "上日账户余额", "上次计息日", "上日余额方向", "保留余额", "冻结余额", "控制余额", "前日余额", "账户余额", "余额方向", "计息方法", "过息标志", "利率编号", 
                  "执行利率", "浮动金额方式", "浮动有效日期", "浮动类型", "浮动值", "累计利息", "积数", "应加减利息", "应加减积数", "透支标志", "脱机透支额度", 
                  "联机透支额度", "透支积数", "透支利率", "透支日期", "透支优惠天数", "通存通兑标识", "当期最低余额", "起点金额", "存款联系人", "卡折账户种类", 
                  "个人结算账户标志", "相关贷款户数", "欠款户数", "开户日期", "开户柜员", "维护日期", "维护柜员", "销户日期", "销户柜员", "最后财务交易日", "客户交易日期", 
                  "时间戳", "记录状态")

# cols.select <- feature.get_useful_cols_name(adshq)
# cat(cols.select)

adshq <- adshq %>% select(
  客户号, 上日账户余额, 前日余额, 账户余额, 积数, 相关贷款户数
)

total_count <- nrow(adshq)
adshq <- adshq[sample(1:total_count, sample_rate * total_count), ]

# for (i in 2:length(names(adshq))) {
#   n <- names(adshq[i])
#   cat(n)
#   cat('\n')
#   t <- (table(adshq[, n]))
#   cat(names(t))
#   cat('\n')
#   cat(t)
#   cat('\n')
# }

adshq <- adshq %>% mutate_all(as.numeric) %>% 
  mutate_at(c("客户号"), as.character)

adshq <- adshq %>% group_by(客户号) %>% summarise(
  huoqi.above_day_balance.max = max(上日账户余额),
  huoqi.above_day_balance.mean = mean(上日账户余额),
  huoqi.above_day_balance.var = var(上日账户余额),
  huoqi.previous_day_balance.max = max(前日余额),
  huoqi.previous_day_balance.mean = mean(前日余额),
  huoqi.previous_day_balance.var = var(前日余额),
  huoqi.balance.max = max(账户余额),
  huoqi.balance.mean = mean(账户余额),
  huoqi.balance.var = var(账户余额),
  huoqi.jishu.max = max(积数),
  huoqi.jishu.mean = mean(积数),
  huoqi.jishu.var = var(积数),
  huoqi.debt_count.mean = mean(相关贷款户数)
)

# head(adshq)
# summary(adshq)

if (isTrain) {
  train.feature.adshq <- adshq
} else {
  test.feature.adshq <- adshq
}

rm(adshq)
