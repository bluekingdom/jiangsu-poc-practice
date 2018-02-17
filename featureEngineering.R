if (!require(stringr)){
  install.packages("stringr")
}
library(stringr)

if (!require(parallel)){
  install.packages("parallel")
}
library(parallel)

featureEngineering <- function(adshq, asdfh, bckzm, cust.info, tranflow, vyktd) {
  # adshq <- tbl_df(adshq)
  # asdfh <- tbl_df(asdfh)
  # bckzm <- tbl_df(bckzm)
  # cust.info <- tbl_df(cust.info)
  # tranflow <- tbl_df(tranflow)
  # vyktd <- tbl_df(vyktd)
  
  # feature engineering
  
  # fields rename
  names(vyktd) <- c("客户号", "卡号", "活期一本通号", "定期一本通号", "最大顺序号", "有无卡折标志", "账务机构", "营业机构", "制卡机构代码", "发卡机构代码", 
                    "尾箱号", "卡性质", "卡种类", "卡类型", "卡对象", "账号", "姓名拼音", "卡等级", "主附卡标志", "主卡号", "发卡柜员", "发卡日期", "发卡方式",
                    "核销柜员", "核销日期", "交易密码", "查询密码", "交易密码重复错误次数", "新老密码标志", "换卡次数", "有效日期", "凭证使用状态", "是否需要密码封", 
                    "发卡渠道", "发卡联系人", "消费积分", "尾箱账务机构", "是否检查CVV标志", "是否有功能控制", "日志子序号", "逻辑发卡日期", "卡大额消费序号", "卡大额圈提序号", 
                    "预消户日期", "是否自动扣年费", "是否自动扣手续费", "是否自动续期", "是否自动续卡", "是否产生对账单", "国际卡标志", "一卡通标志", "新老卡标志", "项目编号", 
                    "卡类型编号", "卡小额消费序号", "卡小额圈提序号", "定期质押总额控制", "外汇宝总额控制", "卡的月消费透支累计金额", "当期跨行取款次数", "紧急援救使用次数", 
                    "预留标志", "受理编号", "备注", "时间戳", "记录状态", "社保编号", "个人编号", "CVV号")
  
  names(tranflow) <- c("网银交易流水号", "批次号", "网银客户号", "网银交易代码", "付款方账号", "付款方账号类型1", "付款方账号子账号", "付款方账号开户行", 
                       "付款方户名", "收款方账号", "付款方账号类型", "收款方户名", "收款方开户行", "转账类型", "收款方开户行联行号", "币种", "钞汇标识", 
                       "用户提交时间", "交易金额", "邮电费", "手续费", "工本费", "付款用途", "用户备注", "银行备注", "加急标志", "认证方式", "预约时间", 
                       "预约类型", "核心流水号", "交易发送主机时间", "主机返回错误代码", "主机返回业务编号", "主机返回柜员流水", "指令状态", "渠道标识")
  
  names(cust.info) <- c("ECIF客户编号", "行别", "客户名称", "证件类型", "证件号码", "客户称谓", "客户拼音姓英文姓", "客户拼音名英文名", "性别", "出生日期", "婚姻状态", 
                        "户籍", "民族代码", "国别代码", "子女情况", "籍贯", "教育程度", "毕业院校", "所学专业", "毕业时间", "政治面貌", "抚养人数", "所属地区代码", "职务", 
                        "职称", "职业", "行业类别", "是否为本行员工", "行内员工号", "工作单位", "单位性质", "单位规模", "任职部门", "发证机关签发机关", "发证机关国家", 
                        "发证机关地区", "证件发放日期", "证件到期日期", "是否境内居民", "是否本行股东", "客户综合评估级别", "身份核实结果", "无法核实原因", "处置方法", 
                        "个贷经办行", "个贷经办人", "创建柜员", "创建机构", "创建日期", "代理人名称", "代理人证件类型", "代理人证件号码", "备用0", "备用1", "备用2", "备用3", 
                        "备用4", "更新柜员", "更新机构号", "进入ECIF的时间", "在ECIF中更新的时间", "创建渠道", "源系统创建时间", "最新更新渠道", "最新更新时间" )
  
  names(bckzm) <- c("存款证明登记号", "机构号", "客户号", "客户中文名", "证件号", "客户账号类型", "客户账号", "顺序号", "账号", "冻结编号", "冻结日期", "解冻日期", 
                    "凭证号", "货币代号", "金额", "起息日", "到期日", "操作柜员", "复核柜员", "审评柜员", "交易日期", "备注", "时间戳", "记录状态")
  
  names(asdfh) <- c("账号", "主账号", "营业机构号", "账务机构号", "货币代号", "业务代号", "账号序号", "钞汇标志", "客户号", "客户中文名", "信息代码", "科目号", 
                    "余额性质", "开户金额", "存款种类", "起息日", "到期日", "存期", "上次计息日", "上日账户余额", "上日余额方向", "保留余额", "控制余额", "冻结余额", 
                    "前日余额", "账户余额", "余额方向", "计息方法", "利率编号", "浮动金额方式", "浮动有效日期", "浮动类型", "浮动值", "利率", "年月利率", "累计利息", 
                    "日积数", "月积数", "应加减利息", "应加减积数", "每次支取金额", "累计支取金额", "取息间隔续存间隔", "支取次数", "续存次数", "漏存次数", "违约日期", 
                    "起点金额", "违约定期积数", "续存标记取息方式", "转存期", "自动转存账户", "活期计息金额", "不计息金额", "通知种类", "通存通兑标识", "存款联系人", 
                    "开户日期", "开户柜员", "销户日期", "销户柜员", "维护日期", "维护柜员", "最后财务交易日", "时间戳", "记录状态")
  
  names(adshq) <- c("主账号", "账号", "营业机构号", "账务机构号", "货币代号", "业务代号", "账号序号", "钞汇标志", "客户号", "个人中文名", "信息代码", "科目号", "余额性质", 
                    "上日账户余额", "上次计息日", "上日余额方向", "保留余额", "冻结余额", "控制余额", "前日余额", "账户余额", "余额方向", "计息方法", "过息标志", "利率编号", 
                    "执行利率", "浮动金额方式", "浮动有效日期", "浮动类型", "浮动值", "累计利息", "积数", "应加减利息", "应加减积数", "透支标志", "脱机透支额度", 
                    "联机透支额度", "透支积数", "透支利率", "透支日期", "透支优惠天数", "通存通兑标识", "当期最低余额", "起点金额", "存款联系人", "卡折账户种类", 
                    "个人结算账户标志", "相关贷款户数", "欠款户数", "开户日期", "开户柜员", "维护日期", "维护柜员", "销户日期", "销户柜员", "最后财务交易日", "客户交易日期", 
                    "时间戳", "记录状态")
  
  #####################################################################################################################################################################
  ###   part 1. feature select
  ###  每个表选择的第一个字段作为join字段
  
  # select the features entropy greter than 0.4, Variance greater than 0.1
  adshq.select <- adshq %>% select(客户号, 上次计息日, 销户日期, 卡折账户种类, 积数, 账号序号,
                                      前日余额, 上日账户余额, 账户余额, 最后财务交易日, 维护日期, 营业机构号, 账务机构号, 
                                      货币代号) %>% distinct()
  adshq.select$客户号 <- as.character(adshq.select$客户号)
  
  # select the features entropy greter than 0.4, Variance greater than 0.1
  asdfh.select <- asdfh %>% select(客户号, 销户日期, 续存标记取息方式, 上日账户余额, 利率编号, 续存次数,
                                      开户金额, 前日余额, 账户余额, 上次计息日, 最后财务交易日, 维护日期,
                                      营业机构号, 货币代号, 账号序号) %>% distinct()
  asdfh.select$客户号 <- as.character(asdfh.select$客户号)
  
  # select the features entropy greter than 0.4, Variance greater than 0.1
  vyktd.select <- vyktd %>% select(客户号, 发卡方式, 卡类型, 一卡通标志, 记录状态, 发卡机构代码) %>% distinct()
  vyktd.select$客户号 <- as.character(vyktd.select$客户号)
  
  # select the features entropy greter than 0.4, Variance greater than 0.1
  tranflow.select <- tranflow %>% select(网银客户号, 交易金额, 转账类型) %>% distinct()
  names(tranflow.select)[1] <- "客户号"
  tranflow.select$客户号 <- as.character(tranflow.select$客户号)
  
  # select the features entropy greter than 0.4, Variance greater than 0.1
  cust.info.select <- cust.info %>% select(ECIF客户编号, 最新更新时间, 发证机关签发机关, 是否为本行员工,
                                           个贷经办行, 更新机构号, 单位性质, 最新更新渠道, 职称, 
                                           职务, 创建渠道, 子女情况, 客户综合评估级别, 进入ECIF的时间, 源系统创建时间) %>% distinct()
  
  names(cust.info.select)[1] <- "客户号"
  cust.info.select <- cust.info.select %>% 
    mutate_all(as.factor) %>%
    mutate_at(c("最新更新时间", "进入ECIF的时间", "源系统创建时间"), function(x) {
      t <- str_split(x, " ")
      sapply(t, function(y) {as.numeric(as.Date(y[1]))})
    }) %>%
    mutate_at("客户号", as.character)
  
  
  # select the features entropy greter than 0.4, Variance greater than 0.1
  bckzm.select <- bckzm %>% select(客户号, 客户账号类型, 金额) %>% distinct()
  bckzm.select$客户号 <- as.character(bckzm.select$客户号)
  
  #######################################################################
  ### 2 feature transform
  
  # 0726 select  adshq features
  # only data after 2017.01.01
  adshq.select.list <- split(adshq.select, adshq.select$客户号)
  adshq.feature.list <- mclapply(adshq.select.list, function(x) {
    x %>% group_by(客户号) %>% 
      summarise(
        Max.jishu = max(积数,na.rm = TRUE),
        Mean.jishu = mean(积数,na.rm = TRUE),
        Var.jishu = var(积数,na.rm = TRUE),
        huoqi.cishu = length(最后财务交易日),
        huoqi.lastday = max(最后财务交易日,na.rm = TRUE),
        huoqi.yu11 = mean(上日账户余额,na.rm = TRUE),
        huoqi.yu12 = max(上日账户余额,na.rm = TRUE),
        huoqi.yu13 = var(上日账户余额,na.rm = TRUE),
        huoqi.yu21 = mean(前日余额,na.rm = TRUE),
        huoqi.yu22 = max(前日余额,na.rm = TRUE),
        huoqi.yu23 = var(前日余额,na.rm = TRUE),
        huoqi.yu31 = mean(账户余额,na.rm = TRUE),
        huoqi.yu32 = max(账户余额,na.rm = TRUE),
        huoqi.yu33 = var(账户余额,na.rm = TRUE),
        ka.type = paste(sort(unique(卡折账户种类)), collapse = "+"),
        huoqi.weihu1 = mean(维护日期,na.rm = TRUE),
        huoqi.weihu2 = max(维护日期,na.rm = TRUE),
        huoqi.weihu3 = var(维护日期,na.rm = TRUE),
        huoqi.zhanghao = length(unique(账号序号)),
        huoqi.xiaohu1 = mean(as.numeric(销户日期),na.rm = TRUE),
        huoqi.xiaohu2 = max(as.numeric(销户日期),na.rm = TRUE),
        huoqi.xiaohu3 = var(as.numeric(销户日期),na.rm = TRUE),
        huoqi.amt1705 = mean(账户余额[which(最后财务交易日 >= 20170501 & 最后财务交易日 < 20170601)], na.rm = TRUE),
        huoqi.amt1704 = mean(账户余额[which(最后财务交易日 >= 20170401 & 最后财务交易日 < 20170501)], na.rm = TRUE),
        huoqi.amt1703 = mean(账户余额[which(最后财务交易日 >= 20170301 & 最后财务交易日 < 20170401)], na.rm = TRUE),
        huoqi.amt1702 = mean(账户余额[which(最后财务交易日 >= 20170201 & 最后财务交易日 < 20170301)], na.rm = TRUE),
        huoqi.amt1701 = mean(账户余额[which(最后财务交易日 >= 20170101 & 最后财务交易日 < 20170201)], na.rm = TRUE),
        huoqi.amt1612 = mean(账户余额[which(最后财务交易日 >= 20161201 & 最后财务交易日 < 20170101)], na.rm = TRUE),
        huoqi.amt1611 = mean(账户余额[which(最后财务交易日 >= 20161101 & 最后财务交易日 < 20161201)], na.rm = TRUE),
        huoqi.amt1610 = mean(账户余额[which(最后财务交易日 >= 20161001 & 最后财务交易日 < 20161101)], na.rm = TRUE),
        huoqi.amt1609 = mean(账户余额[which(最后财务交易日 >= 20160901 & 最后财务交易日 < 20161001)], na.rm = TRUE),
        huoqi.amt1608 = mean(账户余额[which(最后财务交易日 >= 20160801 & 最后财务交易日 < 20160901)], na.rm = TRUE),
        huoqi.amt1607 = mean(账户余额[which(最后财务交易日 >= 20160701 & 最后财务交易日 < 20160801)], na.rm = TRUE),
        huoqi.amt1606 = mean(账户余额[which(最后财务交易日 >= 20160601 & 最后财务交易日 < 20160701)], na.rm = TRUE),
        huoqi.amt1605 = mean(账户余额[which(最后财务交易日 >= 20160501 & 最后财务交易日 < 20160601)], na.rm = TRUE),
        huoqi.amt1604 = mean(账户余额[which(最后财务交易日 >= 20160401 & 最后财务交易日 < 20160501)], na.rm = TRUE),
        huoqi.amt1603 = mean(账户余额[which(最后财务交易日 >= 20160301 & 最后财务交易日 < 20160401)], na.rm = TRUE),
        huoqi.amt1602 = mean(账户余额[which(最后财务交易日 >= 20160201 & 最后财务交易日 < 20160301)], na.rm = TRUE),
        huoqi.amt1601 = mean(账户余额[which(最后财务交易日 >= 20160101 & 最后财务交易日 < 20160201)], na.rm = TRUE)
      )  
  }, mc.cores = 20)
  
  adshq.feature <- data.table::rbindlist(adshq.feature.list)
  
  # 2.3 transform asdfh.select
  asdfh.select.list <- split(asdfh.select, asdfh.select$客户号)
  asdfh.feature.list <- mclapply(asdfh.select.list, function(x) {
    asdfh.filter <- x %>% mutate(lilv = transform.lilv(利率编号), huobi = transform.huobi(货币代号))
    asdfh.filter %>% group_by(客户号) %>% 
      summarise(jigou = Mode(as.character(营业机构号)),
                Mean.kaihu = mean(开户金额,na.rm = TRUE),
                Max.kaihu = max(开户金额,na.rm = TRUE),
                Var.kaihu = var(开户金额,na.rm = TRUE),
                xucun = mean(续存次数,na.rm = TRUE),
                lilv = paste(sort(unique(lilv)), collapse = "+"),
                huobi = paste(sort(unique(huobi)), collapse = "+"),
                dingqi.cishu = length(最后财务交易日),
                dingqi.lastday = max(最后财务交易日,na.rm = TRUE),
                dingqi.yu11 = mean(上日账户余额,na.rm = TRUE),
                dingqi.yu12 = max(上日账户余额,na.rm = TRUE),
                dingqi.yu13 = var(上日账户余额,na.rm = TRUE),
                dingqi.yu21 = mean(前日余额,na.rm = TRUE),
                dingqi.yu22 = max(前日余额,na.rm = TRUE),
                dingqi.yu23 = var(前日余额,na.rm = TRUE),
                dingqi.yu31 = mean(账户余额,na.rm = TRUE),
                dignqi.yu32 = max(账户余额,na.rm = TRUE),
                dingqi.yu33 = var(账户余额,na.rm = TRUE),
                dingqi.jixi1 = mean(上次计息日,na.rm = TRUE),
                dingqi.jixi2 = max(上次计息日,na.rm = TRUE),
                dingqi.jixi3 = var(上次计息日,na.rm = TRUE),
                dingqi.weihu1 = mean(维护日期,na.rm = TRUE),
                dingqi.weihu2 = max(维护日期,na.rm = TRUE),
                dingqi.weihu3 = var(维护日期,na.rm = TRUE),
                dingqi.zhanghu = length(unique(账号序号)),
                dingqi.xiaohu1 = mean(as.numeric(销户日期),na.rm = TRUE),
                dingqi.xiaohu2 = max(as.numeric(销户日期),na.rm = TRUE),
                dingqi.xiaohu3 = var(as.numeric(销户日期),na.rm = TRUE),
                dingqi.amt1705 = mean(账户余额[which(最后财务交易日 >= 20170501 & 最后财务交易日 < 20170601)], na.rm = TRUE),
                dingqi.amt1704 = mean(账户余额[which(最后财务交易日 >= 20170401 & 最后财务交易日 < 20170501)], na.rm = TRUE),
                dingqi.amt1703 = mean(账户余额[which(最后财务交易日 >= 20170301 & 最后财务交易日 < 20170401)], na.rm = TRUE),
                dingqi.amt1702 = mean(账户余额[which(最后财务交易日 >= 20170201 & 最后财务交易日 < 20170301)], na.rm = TRUE),
                dingqi.amt1701 = mean(账户余额[which(最后财务交易日 >= 20170101 & 最后财务交易日 < 20170201)], na.rm = TRUE),
                dingqi.amt1612 = mean(账户余额[which(最后财务交易日 >= 20161201 & 最后财务交易日 < 20170101)], na.rm = TRUE),
                dingqi.amt1611 = mean(账户余额[which(最后财务交易日 >= 20161101 & 最后财务交易日 < 20161201)], na.rm = TRUE),
                dingqi.amt1610 = mean(账户余额[which(最后财务交易日 >= 20161001 & 最后财务交易日 < 20161101)], na.rm = TRUE),
                dingqi.amt1609 = mean(账户余额[which(最后财务交易日 >= 20160901 & 最后财务交易日 < 20161001)], na.rm = TRUE),
                dingqi.amt1608 = mean(账户余额[which(最后财务交易日 >= 20160801 & 最后财务交易日 < 20160901)], na.rm = TRUE),
                dingqi.amt1607 = mean(账户余额[which(最后财务交易日 >= 20160701 & 最后财务交易日 < 20160801)], na.rm = TRUE),
                dingqi.amt1606 = mean(账户余额[which(最后财务交易日 >= 20160601 & 最后财务交易日 < 20160701)], na.rm = TRUE),
                dingqi.amt1605 = mean(账户余额[which(最后财务交易日 >= 20160501 & 最后财务交易日 < 20160601)], na.rm = TRUE),
                dingqi.amt1604 = mean(账户余额[which(最后财务交易日 >= 20160401 & 最后财务交易日 < 20160501)], na.rm = TRUE),
                dingqi.amt1603 = mean(账户余额[which(最后财务交易日 >= 20160301 & 最后财务交易日 < 20160401)], na.rm = TRUE),
                dingqi.amt1602 = mean(账户余额[which(最后财务交易日 >= 20160201 & 最后财务交易日 < 20160301)], na.rm = TRUE),
                dingqi.amt1601 = mean(账户余额[which(最后财务交易日 >= 20160101 & 最后财务交易日 < 20160201)], na.rm = TRUE)
      )
  }, mc.cores = 20)
  asdfh.feature <- data.table::rbindlist(asdfh.feature.list)
  
  # 2.4 transform vyktd.select
  # vyktd data selection 0727
  # no filter 
  vyktd.select.list <- split(vyktd.select, vyktd.select$客户号)
  vyktd.feature.list <- mclapply(vyktd.select.list, function(x) {
    x %>% group_by(客户号) %>%
      summarise(faka.method = Mode(as.character(发卡方式)),
                ka.type = Mode(as.character(卡类型)),
                yikatong.sign = Mode(as.character(一卡通标志)),
                jilu.status = Mode(as.character(记录状态)),
                faka.jigou = Mode(as.character(发卡机构代码))
      )
  }, mc.cores = 20)
  vyktd.feature <- data.table::rbindlist(vyktd.feature.list)
  
  # 2.5 transform tranflow.select
  tranflow.factor <- tranflow.select %>% group_by(客户号) %>%
    summarise(faka.method = Mode(as.character(转账类型)),
              zhuanzhang.amt = Mean(交易金额)
    )
  tranflow.feature <- tranflow.factor
  
  
  # 2.6 transform cust.info.select
  # cust.info.feature <- cust.info.count %>% inner_join(cust.info.sum, by = "客户号") %>% inner_join(cust.info.mod, by = "客户号")
  cust.info.feature <- cust.info.select
  
  # 2.7 transform bckzm.select
  # bckzm data selection 0727
  bckzm.factor <- bckzm.select %>% group_by(客户号) %>%
    summarise(acc.type = Mode(as.character(客户账号类型)),
              jine = Mean(金额)
    )
  bckzm.feature <- bckzm.factor
  
  # feature join
  feature.join <- cust.info.feature %>%
    left_join(adshq.feature, by = "客户号") %>%
    left_join(asdfh.feature, by = "客户号") %>%
    left_join(vyktd.feature, by = "客户号") %>%
    left_join(tranflow.feature, by = "客户号") %>%
    left_join(bckzm.feature, by = "客户号")
  
  return(feature.join)
}


