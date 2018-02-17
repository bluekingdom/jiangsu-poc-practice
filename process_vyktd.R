
isTrain <- F

rdata_root <- ifelse(isTrain, 'rawData/trainData/', 'rawData/testData/') 

load(paste(rdata_root, 'vyktd.RData', sep = ''))

names(vyktd) <- c("客户号", "卡号", "活期一本通号", "定期一本通号", "最大顺序号", "有无卡折标志", "账务机构", "营业机构", "制卡机构代码", "发卡机构代码", 
                  "尾箱号", "卡性质", "卡种类", "卡类型", "卡对象", "账号", "姓名拼音", "卡等级", "主附卡标志", "主卡号", "发卡柜员", "发卡日期", "发卡方式",
                  "核销柜员", "核销日期", "交易密码", "查询密码", "交易密码重复错误次数", "新老密码标志", "换卡次数", "有效日期", "凭证使用状态", "是否需要密码封", 
                  "发卡渠道", "发卡联系人", "消费积分", "尾箱账务机构", "是否检查CVV标志", "是否有功能控制", "日志子序号", "逻辑发卡日期", "卡大额消费序号", "卡大额圈提序号", 
                  "预消户日期", "是否自动扣年费", "是否自动扣手续费", "是否自动续期", "是否自动续卡", "是否产生对账单", "国际卡标志", "一卡通标志", "新老卡标志", "项目编号", 
                  "卡类型编号", "卡小额消费序号", "卡小额圈提序号", "定期质押总额控制", "外汇宝总额控制", "卡的月消费透支累计金额", "当期跨行取款次数", "紧急援救使用次数", 
                  "预留标志", "受理编号", "备注", "时间戳", "记录状态", "社保编号", "个人编号", "CVV号")

cols.select <- feature.get_useful_cols_name(vyktd)
cat(cols.select)

vyktd <- vyktd %>% select(
  客户号, 卡种类, 卡类型, 一卡通标志, 国际卡标志, 卡类型编号
)

total_count <- nrow(vyktd)
vyktd <- vyktd[sample(1:total_count, sample_rate * total_count), ]

for (i in 1:length(names(vyktd))) {
  n <- names(vyktd)[i]
  cat(n)
  cat('\n')
  if (n == '客户号') next
  
  t <- (table(vyktd[, n]))
  cat(names(t))
  cat('\n\n')
  cat(t)
  cat('\n\n')
  barplot(t)
  title(n)
}

for (i in 1:length(names(vyktd))) {
  n <- names(vyktd)[i]
  val <- vyktd[, n]
  idx <- (val == '\\N' | val == ' ')
  vyktd[, n][idx] <- NA
}

summary(vyktd)

vyktd <- vyktd %>% mutate_all(as.factor) %>% 
  mutate_at(c("客户号"), as.character)

vyktd <- vyktd %>% group_by(客户号) %>% summarise(
  card_category.mode = Mode(卡种类, na.rm = T),
  card_type.mode = Mode(卡类型, na.rm = T),
  one_card.mode = Mode(一卡通标志, na.rm = T),
  internetional_card.mode = Mode(国际卡标志, na.rm = T),
  card_type_number = Mode(卡类型编号, na.rm = T)
)

# head(vyktd)
# summary(vyktd)

if (isTrain) {
  train.feature.vyktd <- vyktd
} else {
  test.feature.vyktd <- vyktd
}

rm(vyktd)
