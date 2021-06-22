####################################################################################################################################################

# PRENATAL

####################################################################################################################################################

# The following script builds a dataset with all variables and domain scores used in
# the creation of prenatal cumulative risk score by Cecil et al. (2014). 
# It adapts the scripts developed by S. Defina where she created a similar score for Generation R. 

# The script contains the following functions: readquick, percent_missing and domainscore
# You can find them at SereDef GitHub account (https://github.com/SereDef/cumulative-ELS-score) in 'Setup_and_functions.R'.

####################################################################################################################################################

library(tidyverse)

# first, exclude duplicated values (based on first 4 column names; keep first and remove second occurence)
alspac.table = alspac.table %>% distinct(cidB2957,qlet,kz021,ff1ms100, .keep_all = T)

# remote siblings (all qlet=B)
alspac.table=alspac.table[alspac.table$qlet=="A",]

# check if any duplicated cidB2957s left
table(duplicated(alspac.table$cidB2957))

# 1. LIFE EVENTS

# Steps 1-4 assign variables to the pertinent ELS domains (LE/CR/PR/IR) 
# depending on the format of the data, you may want to write "attach(nameofdataframe)" and then use the code below. 
# This will permit the variables to be accessed directly without having to use the dollar sign, as here: 'nameofdata$variablename'


Prenatal_LifeEvents <- data.frame(alspac.table[,c("cidB2957", # add mothers ID here 
                                                  "qlet", 
                                                  'partner_died_pre',
                                                  'smbd_important_died_pre',
                                                  'smbd_important_ill_pre',
                                                  'sick_or_accident_pre',
                                                  'moved_pre',
                                                  'blood_loss',
                                                  'examination',
                                                  'pregnancy_worried',
                                                  'baby_worried',
                                                  'burglary_or_car_theft_pre',
                                                  'work_problems_pre',
                                                  'abortion_pre',
                                                  'married_pre',
                                                  'twins_pre')])


####################################################################################################################################################

# 2. CONTEXTUAL RISKS


Prenatal_ContextualRisks  <- data.frame(alspac.table[,c("cidB2957", # add mothers ID here,
                                                        "qlet",
                                                        'income_reduced_pre',
                                                        'homeless_pregnancy',
                                                        'major_financial_problems_pre',
                                                        'housing_adequacy_pre',
                                                        'housing_basic_living_pre',
                                                        'housing_defects_pre',
                                                        'm_education_pre',
                                                        'unemployed_pre')])


####################################################################################################################################################

# 3. PARENTAL RISKS



Prenatal_ParentalRisks  <- data.frame(alspac.table[,c("cidB2957", # add mothers ID here,
                                                      "qlet",
                                                      'criminal_record_parent_pre',
                                                      'm_attempted_suicide_pre',
                                                      'early_pregnancy',
                                                      'm_depression_pre',
                                                      'm_anxiety_pre',
                                                      'm_interpersonal_sensitivity_pre')]) 

 
####################################################################################################################################################


# 4. INTERPERSONAL RISKS


Prenatal_InterpersonalRisks <- data.frame(alspac.table[,c("cidB2957", # add mothers ID here,
                                                          "qlet",
                                                          'divorce_pre',
                                                          'p_rejected_child_pre',
                                                          'p_went_away_pre',
                                                          'separated_pre',
                                                          'conflict_in_family_pre',
                                                          'argued_fam_friends_pre',
                                                          'conflict_family_violence_pre',
                                                          'marital_status_pregnancy',
                                                          'family_affection',
                                                          'family_size_pregnancy',
                                                          'family_problems',
                                                          'family_support',
                                                          'social_network_emotional',
                                                          'social_network_practical')]) 



####################################################################################################################################################


# 5. CREATE PRENATAL CUMULATIVE RISK SCORE 

# This function merges together all prenatal ELS domains according to the 'cidB2957' column 
# results in a dataframe with all relevant items for prenatal stress.
# tech-tip from Serena: use Reduce because merge can only take two dataframes at a time 

prenatal_stress <- Reduce(function(x,y) merge(x = x, y = y, by = c('cidB2957','qlet'),  all.x = TRUE), 
                          list(Prenatal_LifeEvents,
                               Prenatal_ContextualRisks,
                               Prenatal_ParentalRisks,
                               Prenatal_InterpersonalRisks))

# ####################################################################################################################################################
# 
# # 6. LINK IDM to IDC 
# 
# # In order to later merge the prenatal dataset with postnatal variables, the code below links
# # pregnancy ID (IDM) to the child ID (IDC) (according to Esther this will most likely not be required for ALSPAC data)
# 
# child_general <- readquick("nameofchilddataset.sav") # read in the data containing mother and child IDs 
# # if you use the readquick function you need to obtain it from SereDef 'Setup_and_functions.R' script at https://github.com/SereDef/cumulative-ELS-score 
# 
# child_id <- data.frame(child_general$IDM, child_general$IDC) # extract mother and child IDs
# colnames(child_id) <- c("IDM","IDC") # rename the columns to 'IDM' and 'IDC'
# 
# 
# # ATTENTION! Only need to run the line below if you are using the prenatal score together with postnatal outcomes/scores. 
# # Note: this will probably change the number of observations
# 
# prenatal_stress <- merge(child_id, prenatal_stress, by = 'IDM', all.x = T) 
# 
# ####################################################################################################################################################

# 7. SUMMARY STATISTICS 

# Let's have a look at risk distribution and missing data per indicator (as per Serena's script) 

prenatal_summary = data.frame(row.names=c("no risk","risk","NA","%risk","%miss"))

for (i in 3:ncol(prenatal_stress)) { # ATTENTION, if not merged with child_id, count from 2.
  s = summary(as.factor(prenatal_stress[,i]))
  c = colnames(prenatal_stress)[i]
  prenatal_summary[1:3,c] <- s
  prenatal_summary[4,c] <- round((prenatal_summary[2,c] / 15442)*100, 2)
  prenatal_summary[5,c] <- round((prenatal_summary[3,c] / 15442)*100, 2)
}

####################################################################################################################################################

# 8. MISSINGNESS

# Calculate the percentage missing data (obtained from SereDef 'Setup_and_functions.R' script at https://github.com/SereDef/cumulative-ELS-score  
percent_missing <- function(var) { sum(is.na(var)) / length(var) * 100 }


# Apply the percent_missing function to the rows (1) of the entire dataset 

prenatal_stress$pre_percent_missing = apply(prenatal_stress[,3:ncol(prenatal_stress)], # Same as above, if not merged with child_id, count from 2.
                                            1, percent_missing)


####################################################################################################################################################

# 9. CREATE UN-WEIGHTED DOMAIN SCORES 

# ATTENTION! Here we use the default argument of domainscore function: calculating  a 
# *mean domain score* (range = 0 to 1) that is NOT adjusted for 25% missingness as in 
# e.g. Rijlaarsdam et al. (2016). If you prefer working with the actual number of risks
# (i.e. sum score) or the weighted version of it, you can set the argument score_type
# to 'sum_simple' or 'sum_weighted' respectively (see 'Setup_and_functions.R' script
# for calculation details at https://github.com/SereDef/cumulative-ELS-score). 


# BEFORE RUNNING THE SCRIPT BELOW MAKE SURE YOU OBTAINED THE 'DOMAINSCORE' FUNCTION FROM SEREDEF
# 'Setup_and_functions.R' script at https://github.com/SereDef/cumulative-ELS-score 
# Running the domainscore function will add two extra columns to prenatal_stress dataframe: 
# one containing the % of missingnes per participant and the other containing the domain score 

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
# LIFE EVENTS


prenatal_stress[,c('pre_LE_percent_missing','pre_life_events')] <- domainscore(prenatal_stress[,c(
  'partner_died_pre',
  'smbd_important_died_pre',
  'smbd_important_ill_pre',
  'sick_or_accident_pre',
  'moved_pre',
  'blood_loss',
  'examination',
  'pregnancy_worried',
  'baby_worried',
  'burglary_or_car_theft_pre',
  'work_problems_pre',
  'abortion_pre',
  'married_pre',
  'twins_pre')])



# CONTEXTUAL RISKS


prenatal_stress[,c('pre_CR_percent_missing','pre_contextual_risk')] <- domainscore(prenatal_stress[,c(
  'income_reduced_pre',
  'homeless_pregnancy',
  'major_financial_problems_pre',
  'housing_adequacy_pre',
  'housing_basic_living_pre',
  'housing_defects_pre',
  'm_education_pre',
  'unemployed_pre')]) 



# PARENTAL RISKS


prenatal_stress[,c('pre_PR_percent_missing','pre_parental_risks')] <- domainscore(prenatal_stress[,c(
  'criminal_record_parent_pre',
  'm_attempted_suicide_pre',
  'early_pregnancy',
  'm_depression_pre',
  'm_anxiety_pre',
  'm_interpersonal_sensitivity_pre')]) 



# INTERPERSONAL RISKS


prenatal_stress[,c('pre_IS_percent_missing','pre_interpersonal_risks')] <- domainscore(prenatal_stress[,c(
  'divorce_pre',
  'p_rejected_child_pre',
  'p_went_away_pre',
  'separated_pre',
  'conflict_in_family_pre',
  'argued_fam_friends_pre',
  'conflict_family_violence_pre',
  'marital_status_pregnancy',
  'family_affection',
  'family_size_pregnancy',
  'family_problems',
  'family_support',
  'social_network_emotional',
  'social_network_practical')]) 



####################################################################################################################################################

# 10. SAVE DATA

# Save the dataset in an .RData file, in the directory where you have the raw data
save(prenatal_stress, file = 'prenatal_stress.RData')
save(prenatal_summary, file  = 'prenatal_summary.RData')

# Or save the dataset in a .csv format, in the directory where you have the raw data
write.csv(prenatal_stress, file = "prenatal_stress.csv", row.names = FALSE, quote = FALSE)
write.csv(prenatal_summary, file = "prenatal_summary.csv", row.names = T, quote = FALSE)

# if you used attach(nameofdatafrandata) makes sure to detach the data by typing: detach(nameofdataframe).

####################################################################################################################################################
