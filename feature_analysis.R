
require(entropy)

feature.get_useful_cols_name <- function(tdata)
{
  min_sd_value <- 0.1
  min_entropy_value <- 0.4
  
  var.colsClass <- sapply(tdata, class)
  var.colsClass
  
  var.digitColsName <- names(tdata)[var.colsClass == 'integer' | var.colsClass == 'numeric']
  
  var.categoryColsName <- names(tdata)[var.colsClass == 'character']
  
  assertthat::are_equal(length(var.digitColsName), length(var.categoryColsName))
  
  var.reserve_cols <- c()
  
  for (v in var.digitColsName) 
  {
    cat(v)
    sd_value <- sd(tdata[, v])
    cat('\t')
    cat(sd_value)
    if (sd_value > min_sd_value)
    {
      var.reserve_cols <- c(var.reserve_cols, v)
      cat('\t')
      cat('reserve')
    }
    cat('\n')
  }
  
  for (v in var.categoryColsName)
  {
    cat(v)
    ent_value <- entropy(table(tdata[, v]), method = 'ML')
    cat('\t')
    cat(ent_value)
    if (ent_value > min_entropy_value)
    {
      cat('\t')
      var.reserve_cols <- c(var.reserve_cols, v)
      cat('\t')
      cat('reserve')
    }
    cat('\n')
  }
  
  return(var.reserve_cols)
}

Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
