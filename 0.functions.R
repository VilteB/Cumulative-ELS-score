
# These functions are taken from SereDefe GitHub '0-Setup_and_functions.R'

# For several risk factors, we have repeated measurements. To keep the score as 
# comprehensive as possible, we design a function that could apply two different 
# strategies to combine measurements over time:
# . once a risk, always a risk strategy: when at least one of the repeated 
#   measurements reported the adversity, the risk factor is present. Default.
# . chronic risk strategy: when the adversities were always present, the risk 
#   factor is present.
repmeas <- function(items, strategy = 'oncealways'){
  x <- data.frame(items)
  # making sure all repeated measures are binary variables
  for (i in 1:ncol(x)){
    if (range(x[,i], na.rm = T)[1] == 1 & range(x[,i], na.rm = T)[2] == 2){
      x[,i] <- x[,i] - 1}
    else {
      if ( (range(x[,i], na.rm = T)[1] != 0 | range(x[,i], na.rm = T)[2] != 1) & ( range(x[,i], na.rm = T)[1] != range(x[,i], na.rm = T)[2] ) ) { 
        stop('Items are not dichotomized')}
    } 
  }
  # combine and dichotomize the repeated measures
  if (strategy == 'oncealways') { 
    temp <- rowMeans(x, na.rm=T) 
    temp[temp > 0] <- 1 
    return(temp)}
  if (strategy == 'chronic') { 
    temp <- rowMeans(x, na.rm=T) 
    temp[temp < 1] <- 0 
    return(temp)}
}



# Domain scores measure how many adversities are reported. This function returns 
# the domain risk score and the percentage of missing values within the domain. 
# Default score_type is the mean nr of adversities (ranging from 0 to 1), that is
# NOT adjusted for number of missing values, so whenever a domain is not complete, 
# the domain score is NA. This missing value needs to be accounted for by multiple 
# imputation. However, the function can also provide a cumulative number of adversities 
# (set score_type = 'sum_simple') or a weighted sum score that allows for 25% missing
# like the one used in Rijlaarsdam et al. (2016) and Cecil et al. (2014) (set 
# score_type = 'sum_weighted')

# calculate the domain scores
domainscore <- function(df, score_type = 'mean_simple'){
  # dataframe with all the included items
  df <- data.frame(df)
  # check if all variables in df are dichotomized 
  for (i in 1:ncol(df)){
    if (range(df[,i], na.rm = T)[1] == 1 & range(df[,i], na.rm = T)[2] == 2){ df[,i] <- df[,i] - 1}
    else {
      if (range(df[,i], na.rm = T)[1] != 0 | range(df[,i], na.rm = T)[2] != 1 ){
        stop('Items are not dichotomized') } 
    }
  }
  # calculate number of missing items per participant
  domain_miss = apply(df, 1, percent_missing)
  # calculate the domain score
  if (score_type == 'mean_simple') { 
    score <- rowMeans(df, na.rm = F) 
  } else if (score_type == 'sum_simple') {
    score <- rowSums(df, na.rm = F)
  } else if (score_type == 'sum_weighted') { 
    score <- ifelse(domain_miss >= 25, NA, 
                    rowSums(df, na.rm = T) * length(df)/(length(df) - rowSums(is.na(df)))) 
  }
  return(c(domain_miss, score))
}

# NOTE: repmeas and domainscore functions require dichotomized variables 
# (0 and 1, or 1 and 2). They assume that the highest value indicates the risk!