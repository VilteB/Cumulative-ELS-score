####################################################################################################################################################
#ELS score 
# PRENATAL

####################################################################################################################################################

# The following script builds a dataset with all variables and domain scores used in
# the creation of prenatal cumulative risk score by Cecil et al. (2014). 
# It adapts the scripts developed by S. Defina where she created a similar score for Generation R. 

# The script contains the following functions: readquick, percent_missing and domainscore
# You can find them at SereDef GitHub account (https://github.com/SereDef/cumulative-ELS-score) in 'Setup_and_functions.R'.

####################################################################################################################################################

library(tidyverse)
#load('alspac.table.collapsed.Rdata')

# first, exclude duplicated values (based on first 4 column names; keep first and remove second occurence)
alspac.table = alspac.table %>% distinct(cidB2957,qlet,kz021,ff1ms100, .keep_all = T)
# cidB2957 = unique pregnancy identifier (shared by mothers, children, partners) 
# qlet = children can be distinguished from others via 'qlet', which is birth order (within pregnancy)
# kz021 = sex 
# ff1ms100 = Height (mm) from focus on fathers (FOF1) - why are we including this?

# check if any duplicated cidB2957s left
table(duplicated(alspac.table$cidB2957)) # yes because siblings haven't yet been removed (we will select a sibling based on data availability in the 'Dataset_construction.R' script)

####################################################################################################################################################

# 1. CREATE PRENATAL CUMULATIVE ELS RISK SCORE 

# This section subsets all prenatal stress variables (LE/CR/PR/IR) into 'prenatal_stress'
prenatal_stress <- data.frame(alspac.table[,c("cidB2957", # pregnancy ID 
                                                  "qlet", # sibling identifier
                                                  'partner_died_pre',
                                                  'smbd_important_died_pre',
                                                  'smbd_important_ill_pre',
                                                  'sick_or_accident_pre',
                                                  'moved_pre',
                                                  'blood_loss',
                                                  'pregnancy_worried',
                                                  'baby_worried',
                                                  'burglary_or_car_theft_pre',
                                                  'work_problems_pre',
                                                  'abortion_pre',
                                                  'married_pre',
                                                  'unemployed_pre', # LE
                                                  'income_reduced_pre',
                                                  'homeless_pregnancy',
                                                  'major_financial_problems_pre',
                                                  'housing_adequacy_pre',
                                                  'housing_basic_living_pre',
                                                  'housing_defects_pre',
                                                  'm_education_pre', # CR
                                                  'criminal_record_parent_pre',
                                                  'm_attempted_suicide_pre',
                                                  'early_pregnancy',
                                                  'm_depression_pre',
                                                  'm_anxiety_pre',
                                                  'm_interpersonal_sensitivity_pre',
                                                  'p_depression_pre',
                                                  'p_anxiety_pre',
                                                  'p_interpersonal_sensitivity_pre', # PR 
                                                  'divorce_pre',
                                                  'p_rejected_child_pre',
                                                  'p_went_away_pre',
                                                  'conflict_in_family_pre',
                                                  'argued_fam_friends_pre',
                                                  'conflict_family_violence_pre',
                                                  'marital_status_pregnancy',
                                                  'family_affection',
                                                  'family_size_pregnancy',
                                                  'family_problems',
                                                  'family_support',
                                                  'social_network_emotional',
                                                  'social_network_practical')]) # IR



# ####################################################################################################################################################

# 2. SUMMARY STATISTICS 

# Let's have a look at risk distribution and missing data per indicator

prenatal_summary = data.frame(row.names=c("no risk","risk","NA","%risk","%miss"))

for (i in 3:ncol(prenatal_stress)) { # ATTENTION, if not merged with child_id, count from 2.
  s = summary(as.factor(prenatal_stress[,i]))
  c = colnames(prenatal_stress)[i]
  prenatal_summary[1:3,c] <- s
  prenatal_summary[4,c] <- round((prenatal_summary[2,c] / 15442)*100, 2)
  prenatal_summary[5,c] <- round((prenatal_summary[3,c] / 15442)*100, 2)
}

####################################################################################################################################################

# 3. MISSINGNESS

# Calculate the percentage missing data (obtained from SereDef 'Setup_and_functions.R' script at https://github.com/SereDef/cumulative-ELS-score  
percent_missing <- function(var) { sum(is.na(var)) / length(var) * 100 }


# Apply the percent_missing function to the rows (1) of the entire dataset 

prenatal_stress$pre_percent_missing = apply(prenatal_stress[,3:ncol(prenatal_stress)], # Same as above, if not merged with child_id, count from 2.
                                            1, percent_missing)


####################################################################################################################################################

# 4. CREATE UN-WEIGHTED DOMAIN SCORES 

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

# source the domainscore function
#source("0.functions.R")

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
  'pregnancy_worried',
  'baby_worried',
  'burglary_or_car_theft_pre',
  'work_problems_pre',
  'abortion_pre',
  'married_pre',
  'unemployed_pre')])



# CONTEXTUAL RISKS


prenatal_stress[,c('pre_CR_percent_missing','pre_contextual_risk')] <- domainscore(prenatal_stress[,c(
  'income_reduced_pre',
  'homeless_pregnancy',
  'major_financial_problems_pre',
  'housing_adequacy_pre',
  'housing_basic_living_pre',
  'housing_defects_pre',
  'm_education_pre')]) 



# PARENTAL RISKS


prenatal_stress[,c('pre_PR_percent_missing','pre_parental_risks')] <- domainscore(prenatal_stress[,c(
  'criminal_record_parent_pre',
  'm_attempted_suicide_pre',
  'early_pregnancy',
  'm_depression_pre',
  'm_anxiety_pre',
  'm_interpersonal_sensitivity_pre',
  'p_depression_pre',
  'p_anxiety_pre',
  'p_interpersonal_sensitivity_pre')]) 



# INTERPERSONAL RISKS


prenatal_stress[,c('pre_IS_percent_missing','pre_interpersonal_risks')] <- domainscore(prenatal_stress[,c(
  'divorce_pre',
  'p_rejected_child_pre',
  'p_went_away_pre',
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

# 5. SAVE DATA

# Save the dataset in an .RData file, in the directory where you have the raw data
save(prenatal_stress, file = 'prenatal_stress.RData')
save(prenatal_summary, file  = 'prenatal_summary.RData')

# Or save the dataset in a .csv format, in the directory where you have the raw data
write.csv(prenatal_stress, file = "prenatal_stress.csv", row.names = FALSE, quote = FALSE)
write.csv(prenatal_summary, file = "prenatal_summary.csv", row.names = T, quote = FALSE)

# if you used attach(nameofdatafrandata) makes sure to detach the data by typing: detach(nameofdataframe).

####################################################################################################################################################
