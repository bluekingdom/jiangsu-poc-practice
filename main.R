library(discoverR)


# load csv data and generate RData for rapidly access
isGenerateRData <- T

if (isGenerateRData)
{
  source('generate_RData.R')
  generateRData('./rawData/trainData/')
  generateRData('./rawData/testData/')
}

# conf 
featureRData <- c('adshq', 'asdfh', 'bckzm', 'cust.info', 'tranflow', 'vyktd')
labelRData <- c('labels')
trainDataRoot <- './rawData/trainData/'
testDataRoot <- './rawData/testData/'

min_entropy_value <- 0.4
min_sd_value <- 0.1

# data preprocess, assuming all data were stored in .RData format

source('feature_analysis.R')
source('featureEngineering.R')

# train data
for (rdata_file in featureRData) {
  rdata_path <- paste(trainDataRoot, rdata_file, sep = '')
  rdata_path <- paste(rdata_path, '.RData', sep = '')
  load(rdata_path)
  print(sprintf('load var %s done!', rdata_path))
}

# 
isFilterFeature <- T
if (isFilterFeature)
{
  adshq <- filterFeatures(adshq, min_entropy_value, min_sd_value)
  asdfh <- filterFeatures(asdfh, min_entropy_value, min_sd_value)
  bckzm <- filterFeatures(bckzm, min_entropy_value, min_sd_value)
  cust.info <- filterFeatures(cust.info, min_entropy_value, min_sd_value)
  tranflow <- filterFeatures(tranflow, min_entropy_value, min_sd_value)
  vyktd <- filterFeatures(vyktd, min_entropy_value, min_sd_value) 
}

train.feature <- featureEngineering(adshq, asdfh, bckzm, cust.info, tranflow, vyktd)

# model train
