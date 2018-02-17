
isTrain <- T

rdata_root <- ifelse(isTrain, 'rawData/trainData/', 'rawData/testData/') 

load(paste(rdata_root, 'cust_info.RData', sep = ''))

names(cust_info) <- c("ECIF客户编号", "行别", "客户名称", "证件类型", "证件号码", "客户称谓", "客户拼音姓英文姓", "客户拼音名英文名", "性别", "出生日期", "婚姻状态", 
                      "户籍", "民族代码", "国别代码", "子女情况", "籍贯", "教育程度", "毕业院校", "所学专业", "毕业时间", "政治面貌", "抚养人数", "所属地区代码", "职务", 
                      "职称", "职业", "行业类别", "是否为本行员工", "行内员工号", "工作单位", "单位性质", "单位规模", "任职部门", "发证机关签发机关", "发证机关国家", 
                      "发证机关地区", "证件发放日期", "证件到期日期", "是否境内居民", "是否本行股东", "客户综合评估级别", "身份核实结果", "无法核实原因", "处置方法", 
                      "个贷经办行", "个贷经办人", "创建柜员", "创建机构", "创建日期", "代理人名称", "代理人证件类型", "代理人证件号码", "备用0", "备用1", "备用2", "备用3", 
                      "备用4", "更新柜员", "更新机构号", "进入ECIF的时间", "在ECIF中更新的时间", "创建渠道", "源系统创建时间", "最新更新渠道", "最新更新时间" )

# cols.select <- feature.get_useful_cols_name(cust_info)
# cat(cols.select)

cust_info <- cust_info %>% select(
  ECIF客户编号, 性别, 婚姻状态, 子女情况, 教育程度, 
  职业, 单位性质, 客户综合评估级别
)

total_count <- nrow(cust_info)
cust_info <- cust_info[sample(1:total_count, sample_rate * total_count), ]

# for (i in 2:length(names(cust_info))) {
#   n <- names(cust_info[i])
#   cat(n)
#   cat('\n')
#   t <- (table(cust_info[, n]))
#   cat(names(t))
#   cat('\n')
#   cat(t)
#   cat('\n')
# }

cust_info$性别[cust_info$性别 == 9] <- NA
for (i in 2:length(names(cust_info))) {
  n <- names(cust_info)[i]
  idx <- cust_info[, n] == '\\N'
  cust_info[, n][idx] <- NA
}

cust_info <- cust_info %>% mutate_all(as.factor) %>% 
  mutate_at(c("ECIF客户编号"), as.character)
names(cust_info)[1] <- "客户号"

# head(cust_info)
# summary(cust_info)

if (isTrain) {
  train.feature.cust_info <- cust_info
} else {
  test.feature.cust_info <- cust_info
}
rm(cust_info)
