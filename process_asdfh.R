
isTrain <- T

rdata_root <- ifelse(isTrain, 'rawData/trainData/', 'rawData/testData/') 

load(paste(rdata_root, 'asdfh.RData', sep = ''))

names(asdfh) <- c("账号", "主账号", "营业机构号", "账务机构号", "货币代号", "业务代号", "账号序号", "钞汇标志", "客户号", "客户中文名", "信息代码", "科目号", 
                  "余额性质", "开户金额", "存款种类", "起息日", "到期日", "存期", "上次计息日", "上日账户余额", "上日余额方向", "保留余额", "控制余额", "冻结余额", 
                  "前日余额", "账户余额", "余额方向", "计息方法", "利率编号", "浮动金额方式", "浮动有效日期", "浮动类型", "浮动值", "利率", "年月利率", "累计利息", 
                  "日积数", "月积数", "应加减利息", "应加减积数", "每次支取金额", "累计支取金额", "取息间隔续存间隔", "支取次数", "续存次数", "漏存次数", "违约日期", 
                  "起点金额", "违约定期积数", "续存标记取息方式", "转存期", "自动转存账户", "活期计息金额", "不计息金额", "通知种类", "通存通兑标识", "存款联系人", 
                  "开户日期", "开户柜员", "销户日期", "销户柜员", "维护日期", "维护柜员", "最后财务交易日", "时间戳", "记录状态")

# cols.select <- feature.get_useful_cols_name(asdfh)
# cat(cols.select)

asdfh <- asdfh %>% select(
  客户号, 开户金额, 上日账户余额, 前日余额, 账户余额, 累计利息, 日积数
)

total_count <- nrow(asdfh)
asdfh <- asdfh[sample(1:total_count, sample_rate * total_count), ]

# for (i in 2:length(names(asdfh))) {
#   n <- names(asdfh[i])
#   cat(n)
#   cat('\n')
#   t <- (table(asdfh[, n]))
#   cat(names(t))
#   cat('\n')
#   cat(t)
#   cat('\n')
# }

asdfh <- asdfh %>% mutate_all(as.numeric) %>% 
  mutate_at(c("客户号"), as.character)

asdfh <- asdfh %>% group_by(客户号) %>% summarise(
  dingqi.open_balance.max = max(开户金额),
  dingqi.open_balance.mean = mean(开户金额),
  dingqi.open_balance.sd = sd(开户金额),
  dingqi.above_day_balance.max = max(上日账户余额),
  dingqi.above_day_balance.mean = mean(上日账户余额),
  dingqi.above_day_balance.sd = sd(上日账户余额),
  dingqi.previous_day_balance.max = max(前日余额),
  dingqi.previous_day_balance.mean = mean(前日余额),
  dingqi.previous_day_balance.sd = sd(前日余额),
  dingqi.balance.max = max(账户余额),
  dingqi.balance.mean = mean(账户余额),
  dingqi.balance.sd = sd(账户余额),
  dingqi.accumulated_interest.max = max(累计利息),
  dingqi.accumulated_interest.mean = mean(累计利息),
  dingqi.accumulated_interest.sd = sd(累计利息),
  dingqi.rijishu.max = max(日积数),
  dingqi.rijishu.mean = mean(日积数),
  dingqi.rijishu.sd = sd(日积数)
)

# head(asdfh)
# summary(asdfh)

if (isTrain) {
  train.feature.asdfh <- asdfh
} else {
  test.feature.asdfh <- asdfh
}

rm(asdfh)
