# this file read in the raw TRAIN data, add column names and save them as R friendly format RData for later use.
# pay attention to the file path when you run this code

generateRData <- function(root_path)
{
  root <- root_path
    
  file_path <- paste(root, "wy_his_pb_tranflow.csv", sep='')
  
  tranflow <- read.csv(file_path, sep = "|", header = FALSE, quote = "", stringsAsFactors = FALSE)
  tranflow <- tranflow[,-35]
  tranflow.fields <- c('网银交易流水号', '批次号', '网银客户号', '网银交易代码', '付款方账号', '付款方账号类型1', '付款方账号子账号', 
                       '付款方账号开户行', '付款方户名', '收款方账号', '收款方账号类型（1：借记卡；2：活期一本通；3：定期一本通；C：对公活期）', 
                       '收款方户名', '收款方开户行', '转账类型（0：行内；1：他行；2：网银互联）', '收款方开户行联行号（他行使用）', '币种', 
                       '钞汇标识', '用户提交时间', '交易金额', '邮电费', '手续费', '工本费', '付款用途', '用户备注', '银行备注', 
                       '加急标志（0：普通；1：加急；）', '认证方式（1.电子银行密码；2.USBKey；3.手机动态密码；4.TOKEN；）', 
                       '预约时间', '预约类型（0：系统转预约；1：指定日期转账；2:其它转账；）', '核心流水号', '交易发送主机时间', 
                       '主机返回错误代码', '主机返回业务编号', '主机返回柜员流水', 
                       '指令状态（01：落地等待处理；02：落地时被拒绝03：落地处理完成，等待发送主机10：主机处理中；11：等待发送；12：等待预约处理；20：交易成功；30：交易失败；31：被银行取消；32：预约终止）', 
                       '渠道标识（EB：网银；MB：手机；）')
  names(tranflow) <- tranflow.fields
  
  file_path <- paste(root, "tranflow.RData", sep='')
  save(tranflow, file = file_path)
  print('process tranflow done!')
  
  file_path <- paste(root, "ecif_mir_cust_info.csv", sep='')
  cust_info <- read.csv(file_path, sep = "|", header = FALSE, quote = "", stringsAsFactors = FALSE)
  cust_info.fields <- c('ECIF客户编号', '行别', '客户名称', '证件类型', '证件号码', '客户称谓', '客户拼音姓/英文姓', '客户拼音名/英文名', 
                        '性别', '出生日期', '婚姻状态', '户籍', '民族代码', '国别代码', '子女情况', '籍贯', '教育程度', '毕业院校', '所学专业', 
                        '毕业时间', '政治面貌', '抚养人数', '所属地区代码', '职务', '职称', '职业', '行业类别', '是否为本行员工', '行内员工号', 
                        '工作单位', '单位性质', '单位规模', '任职部门', '发证机关(签发机关)', '发证机关国家', '发证机关地区', '证件发放日期', 
                        '证件到期日期', '是否境内居民', '是否本行股东', '客户综合评估级别', '身份核实结果', '无法核实原因', '处置方法', 
                        '个贷经办行', '个贷经办人', '创建柜员', '创建机构', '创建日期', '代理人名称', '代理人证件类型', '代理人证件号码', 
                        '备用0', '备用1', '备用2', '备用3', '备用4', '更新柜员', '更新机构号', '进入ECIF的时间', '在ECIF中更新的时间', '创建渠道', 
                        '源系统创建时间', '最新更新渠道', '最新更新时间')
  names(cust_info) <- cust_info.fields
  file_path <- paste(root, "cust_info.RData", sep='')
  save(cust_info, file = file_path)
  print('process cust info done!')
  
  file_path <- paste(root, "sop_mir_vyktd.csv", sep='')
  vyktd <- read.csv(file_path, sep = "|", header = FALSE, quote = "", stringsAsFactors = FALSE)
  vyktd.fields <- c('客户号', '卡号', '活期一本通号', '定期一本通号', '最大顺序号', '有无卡折标志', '帐务机构', '营业机构', '制卡机构代码', 
                    '发卡机构代码', '尾箱号', '卡性质', '卡种类', '卡类型', '卡对象', '帐号', '姓名拼音', '卡等级', '主附卡标志', '主卡号', 
                    '发卡柜员', '发卡日期', '发卡方式', '核销柜员', '核销日期', '交易密码', '查询密码', '交易密码重复错误次数', '新老密码标志', 
                    '换卡次数', '有效日期', '凭证使用状态', '是否需要密码封', '发卡渠道', '发卡联系人', '消费积分', '尾箱帐务机构', 
                    '是否检查CVV标志', '是否有功能控制', '日志子序号', '逻辑发卡日期', '卡大额消费序号', '卡大额圈提序号', '预消户日期', 
                    '是否自动扣年费', '是否自动扣手续费', '是否自动续期', '是否自动续卡(国际卡)', '是否产生对帐单', '国际卡标志', '一卡通标志', 
                    '"新老卡标志(原描述有误：应为卡类型2标志）"', '项目编号', '卡类型编号', '卡小额消费序号', '卡小额圈提序号　', '定期质押总额控制', 
                    '外汇宝总额控制', '卡的月消费透支累计金额(自然月)', '当期跨行取款次数(自然月)', '紧急援救使用次数', '预留标志', '受理编号', 
                    '备注', '时间戳', '记录状态', '社保编号', '个人编号', 'CVV号')
  names(vyktd) <- vyktd.fields
  file_path <- paste(root, "vyktd.RData", sep='')
  save(vyktd, file = file_path)
  print('process vyktd done!')
  
  file_path <- paste(root, "sop_mir_adshq.csv", sep='')
  adshq <- read.csv(file_path, sep = "|", header = FALSE, quote = "", stringsAsFactors = FALSE)
  adshq.fields <- c('主帐号', '帐号', '营业机构号', '帐务机构号', '货币代号', '业务代号', '帐号序号', '钞汇标志', '客户号', '个人中文名', 
                    '信息代码', '科目号', '余额性质', '上日帐户余额', '上次计息日', '上日余额方向', '保留余额', '冻结余额', '控制余额', 
                    '前日余额', '帐户余额', '余额方向', '计息方法', '过息标志', '利率编号', '执行利率', '浮动金额方式', '浮动有效日期', '浮动类型', 
                    '浮动值', '累计利息', '积数', '应加/减利息', '应加/减积数', '透支标志', '脱机透支额度', '联机透支额度', '透支积数', '透支利率', 
                    '透支日期', '透支优惠天数', '通存通兑标识', '当期最低余额', '起点金额', '存款联系人', '卡折账户种类', '个人结算账户标志', '相关贷款户数', 
                    '欠款户数', '开户日期', '开户柜员', '维护日期', '维护柜员', '销户日期', '销户柜员', '最后财务交易日', '客户交易日期', '时间戳', '记录状态')
  names(adshq) <- adshq.fields
  file_path <- paste(root, "adshq.RData", sep='')
  save(adshq, file = file_path)
  print('process mir adshq done!')
  
  file_path <- paste(root, "sop_mir_asdfh.csv", sep='')
  asdfh <- read.csv(file_path,  sep = "|", header = FALSE, quote = "", stringsAsFactors = FALSE)
  asdfh.fields <- c('帐号', '主帐号', '营业机构号', '帐务机构号', '货币代号', '业务代号', '帐号序号', '钞汇标志', '客户号', '客户中文名', '信息代码', 
                    '科目号', '余额性质', '开户金额', '存款种类', '起息日', '到期日', '存期', '上次计息日', '上日帐户余额', '上日余额方向', '保留余额', 
                    '控制余额', '冻结余额', '前日余额', '帐户余额', '余额方向', '计息方法', '利率编号', '浮动金额方式', '浮动有效日期', '浮动类型', '浮动值', 
                    '利率', '年/月利率', '累计利息', '日积数', '月积数', '应加/减利息', '应加/减积数', '每次支取金额', '累计支取金额', '取息间隔/续存间隔', 
                    '支取次数', '续存次数', '漏存次数', '违约日期', '起点金额', '违约定期积数', '续存标记/取息方式', '转存期', '自动转存帐户', 
                    '活期计息金额（通知存款用）', '不计息金额（通知存款用）', '通知种类（通知存款用，一天或七天）', '通存通兑标识', '存款联系人', '开户日期', 
                    '开户柜员', '销户日期', '销户柜员', '维护日期', '维护柜员', '最后财务交易日', '时间戳', '记录状态')
  names(asdfh) <- asdfh.fields
  file_path <- paste(root, "asdfh.RData", sep='')
  save(asdfh, file = file_path)
  print('process mir asdfh done!')
  
  file_path <- paste(root, "sop_his_bckzm.csv", sep='')
  bckzm <- read.csv(file_path, sep = "|", header = FALSE, quote = "", stringsAsFactors = FALSE)
  bckzm.fields <- c('存款证明登记号', '机构号', '客户号', '客户中文名', '证件号', '客户账号类型', '客户账号', '顺序号', '账号', '冻结编号', '冻结日期', 
                    '解冻日期', '凭证号', '货币代号', '金额', '起息日', '到期日', '操作柜员', '复核柜员', '审评柜员', '交易日期', '备注', '时间戳', '记录状态')
  names(bckzm) <- bckzm.fields
  file_path <- paste(root, "bckzm.RData", sep='')
  save(bckzm, file = file_path)
  print('process his bckzm done!')
  
  file_path <- paste(root, "labels.csv", sep='')
  labels <- read.csv(file_path, header = FALSE, quote = "", stringsAsFactors = FALSE)
  names(labels) <- c("客户号", "label")
  file_path <- paste(root, "labels.RData", sep='')
  save(labels, file = file_path)
  print('process labels done!')
}
