
# Load the necessary libraries
library(foreign)
library(tidyverse)

# Chose the folder where the input file is stored and all results will be saved
if (exists("alspac_file") == F) { alspac_file <- file.choose() }
alspac_folder <- dirname(alspac_file)

# Read in the data
alspac.table <- foreign::read.spss(alspac_file, use.value.label=TRUE, to.data.frame=TRUE) 

# Specify some funcitons:
# These functions are taken from SereDefe GitHub '0-Setup_and_functions.R'

#-------------------------------------------------------------------------------
# To avoid repetitions, lets define a function that takes a data.frame and a list of
# values representing risk (i.e. "yes") and no risk (i.e., "no") answers. 
dichotomize <- function(vars, 
                        yes = c("affected a lot","fairly affected","mildly affected","N effect at all"), 
                        no = c("didnt happen"), 
                        already_dich = c(), 
                        check_transf = F) {
  
  dset <- alspac.table[, vars]
  answ = c(yes, no)
  
  # check if required levels are present / unexpected levels are not present
  for (i in vars) {
    lvls = levels(dset[,i])
    
    if (length(setdiff(lvls, answ)) > 0) { 
      message("Column ", i, " has these unexpected values: ", setdiff(lvls, answ), 
              ". These will be recoded as NA.")
      for (h in setdiff(lvls, answ)) {
        lvls[lvls == h] <- NA }
      if (length(setdiff(answ, lvls)) > 0) {
        message("Column ", i, " has no registered '", setdiff(answ, lvls), "' answers.") }
    } else { final <- "All variables behave as expected, mazel tov :)" }
  }
  if (exists("final")) { cat(final)}
  
  # recode
  for(i in vars) {
    # some var names already have an "a" at the end
    if (substr(i, nchar(i), nchar(i)) == 'a') { appendix = "_rec" } else { appendix = "a_rec" }
    var.out = paste0(i, appendix)
    dset[,var.out] <- ifelse(dset[,i] %in% no, 0, ifelse(dset[,i] %in% yes, 1, NA))
    # Checking table of continuous vs. binary columns 
    if (check_transf == T) { 
      mat <- table(dset[,i], dset[,var.out])
      message("\n", i); print(mat) }
  }
  
  # add variables that were already dichotomous
  if (length(already_dich) > 0) {
    for (v in already_dich) {
      if (levels(alspac.table[, v]) > 2) { message("Are you sure variable ", v, " is already dichotomous?") }
      }
  } else { dset <- cbind(dset, alspac.table[, already_dich]) }
  
  return(dset)
}

#-------------------------------------------------------------------------------
ccei_score <- function(set1, set2, set3, check_transf = T) {
  vars <- c(set1, set2, set3)
  dset <- alspac.table[, vars]
  
  for (v in set1) {
    if (substr(v, nchar(v), nchar(v)) == 'a') { appendix = "_rec" } else { appendix = "a_rec" }
    var.out = paste0(v, appendix)
    dset[,var.out] <- ifelse(dset[, v] %in% c('V Often', 'Often'), 2, 
                             ifelse(dset[, v] %in% c('Not V often', 'Never'), 0, NA)) 
    if (check_transf == T) { 
      mat <- table(dset[,v], dset[,var.out])
      message("\n", v); print(mat) }
  }
  for (v in set2) {
    if (substr(v, nchar(v), nchar(v)) == 'a') { appendix = "_rec" } else { appendix = "a_rec" }
    var.out = paste0(v, appendix)
    dset[,var.out] <- ifelse(dset[, v] %in% c('V Often', 'Often'), 2,
                             ifelse(dset[, v] == 'Not V often', 1,
                                    ifelse(dset[, v] == 'Never', 0, NA)))
    if (check_transf == T) {
      mat <- table(dset[,v], dset[,var.out])
      message("\n", v); print(mat) }
  }
  for (v in set3) {
    if (substr(v, nchar(v), nchar(v)) == 'a') { appendix = "_rec" } else { appendix = "a_rec" }
    var.out = paste0(v, appendix)
    dset[,var.out] <- ifelse(dset[, v] %in% c('V Often', 'Often', 'Not V often'), 2,
                             ifelse(dset[, v] == 'Never', 0, NA))
    if (check_transf == T) {
      mat <- table(dset[,v], dset[,var.out])
      message("\n", v); print(mat) }
  }
  
  dset[, "sumscore"] <- rowSums(dset[,grep("a_rec",names(dset), value=T)], na.rm = F)
  
  return(dset)
}

#-------------------------------------------------------------------------------
epds_score <- function(set, revset, check_transf = T) {
  vars <- c(set, revset)
  # save as integer, so that labels would become numerical values
  alspac.table[, vars] <- sapply(alspac.table[, vars], as.integer) 
  #as.numeric(levels(f))[f]
  dset <- alspac.table[, vars]
  
  for (v in vars) {
    if (substr(v, nchar(v), nchar(v)) == 'a') { appendix = "_rec" } else { appendix = "a_rec" }
    var.out = paste0(v, appendix)
    if (v %in% set) { tr = 1 } else if (v %in% revset) { tr = 4 }
    dset[,var.out] <- abs(dset[, v] - tr)
    
    if (check_transf == T) { 
      mat <- table(dset[,v], dset[,var.out])
      message("\n", v); print(mat) }
  }
  
  dset[, "sumscore"] <- rowSums(dset[,grep("a_rec",names(dset), value=T)], na.rm = F)
  
  return(dset)
}

#-------------------------------------------------------------------------------
# This function facilitates the dichotomization of pyschopathology variables 
dich_psychopath <- function(var, yes, no, yes_label = "", no_label = "", check_transf = T) {
  dset <- data.frame(as.character(alspac.table[, var]))
  names(dset) <- var
  if (substr(var, nchar(var), nchar(var)) == 'a') { appendix = "_rec" } else { appendix = "a_rec" }
  var.out = paste0(var, appendix)
  dset[,var.out] <- ifelse(dset[,var] %in% yes | dset[,var] == yes_label , 1,
                           ifelse(dset[,var] %in% no | dset[,var] == no_label, 0, NA))
  
  # Checking continuous and binary columns 
  if (check_transf == T) { 
    mat <- table(dset[,var], dset[,var.out])
    message("\n", var); print(mat) }
  
  return(dset)
}

#-------------------------------------------------------------------------------
# For several risk factors, we have repeated measurements. To keep the score as 
# comprehensive as possible, we design a function that could apply two different 
# strategies to combine measurements over time:
# . once a risk, always a risk strategy: when at least one of the repeated 
#   measurements reported the adversity, the risk factor is present. Default.
# . chronic risk strategy: when the adversities were always present, the risk 
#   factor is present.
repmeas <- function(items, strategy = 'oncealways', check_corrs = T){
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
  if (check_corrs == T) {
    mat <- cor(x, method = "spearman", use = "complete.obs") 
    print(mat)
    }
  
  # combine and dichotomize the repeated measures
  if (strategy == 'oncealways') { 
    temp <- rowMeans(x, na.rm=T) 
    temp[temp > 0] <- 1 
    return(temp) }
  if (strategy == 'chronic') { 
    temp <- rowMeans(x, na.rm=T) 
    temp[temp < 1] <- 0 
    return(temp) }
}

#-------------------------------------------------------------------------------
# Calculate the percentage of missing data 
percent_missing <- function(var) { sum(is.na(var)) / length(var) * 100 }

#-------------------------------------------------------------------------------
# Domain scores measure how many adversities are reported. This function returns 
# the domain risk score and the percentage of missing values within the domain. 
# Default score_type is the mean nr of adversities (ranging from 0 to 1), that is 
# calculated whenever a domain is at least 75% complete, otherwise the domain score 
# is NA. This missing value needs to be accounted for by multiple imputation. 
# However, the function can also provide a cumulative number of adversities 
# (set score_type = 'sum_simple') or a weighted sum score like the one used in 
# Rijlaarsdam et al. (2016) and Cecil et al. (2014) (set score_type = 'sum_weighted')
# that both also allow for 25% missing. 

# calculate the domain scores
domainscore <- function(df, score_type = 'mean_simple', postnatal = F){
  if (postnatal == T) {
    item_scores <- c()
    for (item in df) {
      items <- as.data.frame(postnatal_stress[, grepl(item , names(postnatal_stress))])
      if (ncol(items) > 1) {
        # calculate number of missing across timepoints per participant
        items_miss <- apply(items, 1, percent_missing)
        item_score <- ifelse(items_miss >= 25, NA, rowSums(items, na.rm = T) )
      } else { item_score <- items }
      
      item_scores <- cbind(item_scores, item_score)
    }
    # calculate number of missing items per participant
    domain_miss = apply(item_scores, 1, percent_missing)
    score <- ifelse(domain_miss >= 25, NA, rowMeans(item_scores, na.rm = T) )
    
  } else {
    # dataframe with all the included items
    df <- data.frame(df)
    # calculate number of missing items per participant
    domain_miss = apply(df, 1, percent_missing)
    # calculate the domain score
    if (score_type == 'mean_simple') { 
      score <- ifelse(domain_miss >= 25, NA, rowMeans(df, na.rm = T) )
    } else if (score_type == 'sum_simple') {
      score <- ifelse(domain_miss >= 25, NA, rowSums(df, na.rm = T) )
    } else if (score_type == 'sum_weighted') { 
      score <- ifelse(domain_miss >= 25, NA, 
                      rowSums(df, na.rm = T) * length(df)/(length(df) - rowSums(is.na(df)))) 
    }
  }
  return(c(domain_miss, score))
}

# NOTE: repmeas and domainscore functions require dichotomized variables 
# (0 and 1, or 1 and 2). They assume that the highest value indicates the risk!