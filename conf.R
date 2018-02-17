
require(dplyr)
require(stringr)
require(parallel)
require(ggplot2)
require(Matrix)
require(mlr)
require(xgboost)
require(AUC)

isGenericRData <- F

if (isGenericRData)
{
  source('generate_RData.R')
  generateRData('rawData/trainData/')
  generateRData('rawData/testData/')
  isGenericRData <- F
}

sample_rate <- 1.0

source('feature_analysis.R')
