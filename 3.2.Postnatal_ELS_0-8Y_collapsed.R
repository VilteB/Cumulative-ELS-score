####################################################################################################################################################

# POSTNATAL ELS SCORE 0-8 YEARS

####################################################################################################################################################

# The following script builds a dataset with all variables and domain scores used in
# the creation of postnatal cumulative risk score by Cecil et al. (2014). 
# It adapts the scripts developed by S. Defina where she created a similar score for Generation R. 

# The script contains the following functions: readquick, percent_missing and domainscore
# You can find them at SereDef GitHub account (https://github.com/SereDef/cumulative-ELS-score) in 'Setup_and_functions.R'.

####################################################################################################################################################
# TERMS 

# LE = Life Events, CR = Contextual Risk, PR = Parental risk, IR = Interpersonal risk
# DV = Direct Victimization
####################################################################################################################################################

library(tidyverse)

# first, exclude duplicated values (based on first 4 column names; keep first and remove second occurence)
alspac.table = alspac.table %>% distinct(cidB2957,qlet,kz021,ff1ms100, .keep_all = T)

# remove all siblings (all qlet=B/C/D)
alspac.table=alspac.table[alspac.table$qlet=="A",]

# check if any duplicated cidB2957s left
table(duplicated(alspac.table$cidB2957))


####################################################################################################################################################

# 1. LIFE EVENTS

# Steps 1-5 assign variables to the pertinent ELS domains (LE/CR/PR/IR/DV) 

####################################################################################################################################################


# Construct LE8weeks 
# items used LE at 8 weeks in ALSPAC mothers and children

# 8wk = 8wk
# 18m = 8m, 18m 
# 30m = 21m, 30m
# 3y = 3y
# 4y = 4y
# 5y = 5y
# 6y = 6y
# 9y = 8y, 9y

#ASLPAC variables are not YES/NO, here, 1-4 = YES, 5 = NO, all others = NA

LE8weeks <- data.frame(alspac.table[,c('cidB2957', #add ALSPAC family ID here!
                                       'qlet',
                                       'sick_or_accident_8wk',
                                       'family_member_ill_8wk',
                                       'smbd_important_ill_8wk',
                                       'partner_died_8wk',
                                       'smbd_important_died_8wk',
                                       'moved_8wk',
                                       'burglary_or_car_theft_8wk',
                                       'work_problems_8wk')])
                                              
                                              

#LE 18 month (includes 8m) variables are dichotomised, 1=Y, 0=N, 
LE18months <- data.frame(alspac.table[,c('cidB2957', #family ID
                                        'qlet',
                                        'sick_or_accident_18m',
                                        'family_member_ill_8m',
                                        'smbd_important_ill_8m',
                                        'partner_died_8m',
                                        'smbd_important_died_8m',
                                        'pet_died_18m',
                                        'separated_from_parent_18m',
                                        'started_nursery_18m',
                                        'acquired_new_parent_18m',
                                        'change_carer_18m',
                                        'moved_18m',
                                        'm_pregnant_8m',
                                        'new_sibling_18m',
                                        'separated_from_smbd_18m',
                                        'ch_had_fright_18m',
                                        'work_problems_8m')])


#LE 30 month (includes 21m) variables are dichotomised, 1=Y, 0=N, 
LE30months <- data.frame(alspac.table[,c("cidB2957", # family ID 
                                         "qlet",
                                         'sick_or_accident_30m',
                                         'family_member_ill_21m',
                                         'smbd_important_ill_21m',
                                         'partner_died_21m',
                                         'smbd_important_died_21m',
                                         'pet_died_30m',
                                         'separated_from_parent_30m',
                                         'started_nursery_30m',
                                         'acquired_new_parent_30m',
                                         'change_carer_30m',
                                         'moved_30m',
                                         'm_pregnant_21m',
                                         'new_sibling_30m',
                                         'separated_from_smbd_30m',
                                         'burglary_or_car_theft_21m',
                                         'ch_had_fright_30m',
                                         'work_problems_21m')])

#LE 3 years is a dichotomised variable, 1=Y, 0=N 
LE3years <- data.frame(alspac.table[,c("cidB2957", #family ID
                                       "qlet",
                                       'sick_or_accident_3y',
                                       'family_member_ill_3y',
                                       'smbd_important_ill_3y',
                                       'partner_died_3y',
                                       'smbd_important_died_3y',
                                       'pet_died_3y',
                                       'separated_from_parent_3y',
                                       'started_nursery_3y',
                                       'acquired_new_parent_3y',
                                       'change_carer_3y',
                                       'moved_3y',
                                       'm_pregnant_3y',
                                       'new_sibling_3y',
                                       'separated_from_smbd_3y',
                                       'burglary_or_car_theft_3y',
                                       'ch_had_fright_3y',
                                       'work_problems_3y')])


#LE 4 years is a binary variable, Y=1, N=0
LE4years <- data.frame(alspac.table[,c("cidB2957", # add ALPSAC mother ID here
                                       "qlet",
                                       'sick_or_accident_4y',
                                       'family_member_ill_4y',
                                       'smbd_important_ill_4y',
                                       'partner_died_4y',
                                       'smbd_important_died_4y',
                                       'pet_died_4y',
                                       'separated_from_parent_4y',
                                       'started_nursery_4y',
                                       'acquired_new_parent_4y',
                                       'change_carer_4y',
                                       'moved_4y',
                                       'm_pregnant_4y',
                                       'new_sibling_4y',
                                       'separated_from_smbd_4y',
                                       'burglary_or_car_theft_4y',
                                       'ch_had_fright_4y',
                                       'work_problems_4y')])

#LE 5 years is a binary variable, Y=1, N=0
LE5years <- data.frame(alspac.table[,c("cidB2957", # add ALPSAC mother ID here
                                       "qlet",
                                       'sick_or_accident_5y',
                                       'family_member_ill_5y',
                                       'smbd_important_ill_5y',
                                       'partner_died_5y',
                                       'smbd_important_died_5y',
                                       'pet_died_5y',
                                       'separated_from_parent_5y',
                                       'started_nursery_5y',
                                       'acquired_new_parent_5y',
                                       'change_carer_5y',
                                       'moved_5y',
                                       'm_pregnant_5y',
                                       'new_sibling_5y',
                                       'separated_from_smbd_5y',
                                       'burglary_or_car_theft_5y',
                                       'ch_had_fright_5y',
                                       'work_problems_5y')])


#LE 6 years is a binary variable, Y=1, N=0
LE6years <- data.frame(alspac.table[,c("cidB2957", # add ALPSAC mother ID here
                                       "qlet",
                                       'sick_or_accident_6y',
                                       'family_member_ill_6y',
                                       'smbd_important_ill_6y',
                                       'partner_died_6y',
                                       'smbd_important_died_6y',
                                       'pet_died_6y',
                                       'separated_from_parent_6y',
                                       'acquired_new_parent_6y',
                                       'change_carer_6y',
                                       'moved_6y',
                                       'm_pregnant_6y',
                                       'new_sibling_6y',
                                       'separated_from_smbd_6y',
                                       'burglary_or_car_theft_6y',
                                       'ch_had_fright_6y',
                                       'work_problems_6y')])

#LE 9 years is a binary variable, Y=1, N=0
LE9years <- data.frame(alspac.table[,c("cidB2957", # add ALPSAC mother ID here
                                       "qlet",
                                       'sick_or_accident_9y',
                                       'family_member_ill_9y',
                                       'smbd_important_ill_9y',
                                       'partner_died_9y',
                                       'smbd_important_died_9y',
                                       'pet_died_9y',
                                       'separated_from_parent_8y',
                                       'acquired_new_parent_8y',
                                       'change_carer_8y',
                                       'moved_9y',
                                       'm_pregnant_9y',
                                       'new_sibling_8y',
                                       'separated_from_smbd_8y',
                                       'burglary_or_car_theft_9y',
                                       'ch_had_fright_8y',
                                       'work_problems_9y')])



####################################################################################################################################################


# Combining all life event variables with pregnancy ID

LE_all <- Reduce(function(x,y) merge(x = x, y = y, by = c('cidB2957','qlet'),  all.x = TRUE), 
                list(LE8weeks, LE18months, LE30months, LE3years, LE4years, 
                     LE5years, LE6years, LE9years))
                        
####################################################################################################################################################

# 2. CONTEXTUAL RISKS

####################################################################################################################################################


CR_all <- data.frame(alspac.table[,c("cidB2957", 
                                     "qlet",
                                     'housing_adequacy_2y',
                                     'housing_adequacy_4y',
                                     'housing_basic_living_2y',
                                     'housing_basic_living_4y',
                                     'housing_defects_2y',
                                     'housing_defects_4y',
                                     'unemployed_8wk',
                                     'unemployed_8m',
                                     'unemployed_21m',
                                     'unemployed_3y',
                                     'unemployed_4y',
                                     'unemployed_5y',
                                     'unemployed_6y',
                                     'unemployed_9y',
                                     'income_reduced_8wk',
                                     'income_reduced_8m',
                                     'income_reduced_21m',
                                     'income_reduced_3y',
                                     'income_reduced_4y',
                                     'income_reduced_5y',
                                     'income_reduced_6y',
                                     'income_reduced_9y',
                                     'homeless_childhood_8wk',
                                     'homeless_childhood_8m',
                                     'homeless_childhood_21m',
                                     'homeless_childhood_3y',
                                     'homeless_childhood_4y',
                                     'homeless_childhood_5y',
                                     'homeless_childhood_6y',
                                     'homeless_childhood_9y',
                                     'major_financial_problems_8wk',
                                     'major_financial_problems_8m',
                                     'major_financial_problems_21m',
                                     'major_financial_problems_3y',
                                     'major_financial_problems_4y',
                                     'major_financial_problems_5y',
                                     'major_financial_problems_6y',
                                     'major_financial_problems_9y',
                                     'm_education',
                                     'p_education',
                                     'neighbourhood_problems_21m',
                                     'neighbourhood_problems_3y')])

####################################################################################################################################################

# 3. PARENTAL RISKS


####################################################################################################################################################

PR_all <- data.frame(alspac.table[,c("cidB2957", 
                                     "qlet",
                                     'criminal_record_parent_8wk',
                                     'criminal_record_parent_8m',
                                     'criminal_record_parent_21m',
                                     'criminal_record_parent_3y',
                                     'criminal_record_parent_4y',
                                     'criminal_record_parent_5y',
                                     'criminal_record_parent_6y',
                                     'criminal_record_parent_9y',
                                     'miscarriage_or_abortion_8wk',
                                     'miscarriage_or_abortion_8m',
                                     'miscarriage_or_abortion_21m',
                                     'miscarriage_or_abortion_3y',
                                     'miscarriage_or_abortion_4y',
                                     'miscarriage_or_abortion_5y',
                                     'miscarriage_or_abortion_6y',
                                     'miscarriage_or_abortion_9y',
                                     'm_attempted_suicide_8wk',
                                     'm_attempted_suicide_8m',
                                     'm_attempted_suicide_21m',
                                     'm_attempted_suicide_3y',
                                     'm_attempted_suicide_4y',
                                     'm_attempted_suicide_5y',
                                     'm_attempted_suicide_6y',
                                     'm_attempted_suicide_9y',
                                     'm_age',
                                     'p_age',
                                     'm_depression_8m',
                                     'm_depression_21m',
                                     'm_depression_3y',
                                     'm_depression_4y',
                                     'm_depression_5y',
                                     'm_depression_6y',
                                     'm_depression_9y',
                                     'p_depression_8m',
                                     'p_depression_21m',
                                     'p_depression_3y',
                                     'p_depression_4y',
                                     'p_depression_5y',
                                     'p_depression_6y',
                                     'p_depression_8y',
                                     'p_depression_9y',
                                     'm_anxiety_8wk',
                                     'm_anxiety_8m',
                                     'm_anxiety_21m',
                                     'm_anxiety_3y',
                                     'm_anxiety_5y',
                                     'm_anxiety_6y',
                                     'p_anxiety_8m',
                                     'p_anxiety_21m',
                                     'p_anxiety_3y',
                                     'p_anxiety_4y',
                                     'p_anxiety_5y',
                                     'p_anxiety_6y',
                                     'p_anxiety_9y',
                                     'm_interpersonal_sensitivity',
                                     'p_interpersonal_sensitivity')])
                                          
                                           
                                           
                                           
####################################################################################################################################################

# 4. INTERPERSONAL RISKS

####################################################################################################################################################


IR_all <- data.frame(alspac.table[,c("cidB2957", 
                                     "qlet",  
                                     'divorce_8wk',
                                     'divorce_8m',
                                     'divorce_21m',
                                     'divorce_3y',
                                     'divorce_4y',
                                     'divorce_5y',
                                     'divorce_6y',
                                     'divorce_9y',
                                     'p_rejected_child_8wk',
                                     'p_rejected_child_8m',
                                     'p_rejected_child_21m',
                                     'p_rejected_child_3y',
                                     'p_rejected_child_4y',
                                     'p_rejected_child_5y',
                                     'p_rejected_child_6y',
                                     'p_rejected_child_9y',
                                     'p_went_away_8wk',
                                     'p_went_away_8m',
                                     'p_went_away_21m',
                                     'p_went_away_3y',
                                     'p_went_away_4y',
                                     'p_went_away_5y',
                                     'p_went_away_6y',
                                     'p_went_away_9y',
                                     'conflict_in_family_8wk',
                                     'conflict_in_family_8m',
                                     'conflict_in_family_21m',
                                     'conflict_in_family_3y',
                                     'conflict_in_family_4y',
                                     'conflict_in_family_5y',
                                     'conflict_in_family_6y',
                                     'conflict_in_family_9y',
                                     'conflict_family_violence_8wk',
                                     'conflict_family_violence_8m',
                                     'conflict_family_violence_21m',
                                     'conflict_family_violence_3y',
                                     'conflict_family_violence_4y',
                                     'conflict_family_violence_5y',
                                     'conflict_family_violence_6y',
                                     'conflict_family_violence_9y',
                                     'm_new_partner_8wk',
                                     'm_new_partner_8m',
                                     'm_new_partner_21m',
                                     'm_new_partner_3y',
                                     'm_new_partner_4y',
                                     'm_new_partner_5y',
                                     'm_new_partner_6y',
                                     'm_new_partner_9y',
                                     'argued_fam_friends_8wk',
                                     'argued_fam_friends_8m',
                                     'argued_fam_friends_21m',
                                     'argued_fam_friends_3y',
                                     'argued_fam_friends_4y',
                                     'argued_fam_friends_5y',
                                     'argued_fam_friends_6y',
                                     'argued_fam_friends_9y',
                                     'separated_8wk',
                                     'separated_8m',
                                     'separated_21m',
                                     'separated_3y',
                                     'separated_4y',
                                     'separated_5y',
                                     'separated_6y',
                                     'separated_9y')])

####################################################################################################################################################

# 5. DIRECT VICTIMISATION

DV_all <- data.frame(alspac.table[,c("cidB2957",
                                     "qlet",
                                     'bullying_8y',
                                     'physical_violence_18m',
                                     'physical_violence_30m',
                                     'physical_violence_3y',
                                     'physical_violence_4y',
                                     'physical_violence_5y',
                                     'physical_violence_6y',
                                     'physical_violence_9y',
                                     'sexual_abuse_18m',
                                     'sexual_abuse_30m',
                                     'sexual_abuse_3y',
                                     'sexual_abuse_4y',
                                     'sexual_abuse_5y',
                                     'sexual_abuse_6y',
                                     'sexual_abuse_8y',
                                     'p_physical_violence_8wk',
                                     'p_physical_violence_8m',
                                     'p_physical_violence_21m',
                                     'p_physical_violence_3y',
                                     'p_physical_violence_4y',
                                     'p_physical_violence_5y',
                                     'p_physical_violence_6y',
                                     'p_physical_violence_9y',
                                     'm_physical_violence_8m',
                                     'm_physical_violence_21m',
                                     'm_physical_violence_3y',
                                     'm_physical_violence_4y',
                                     'm_physical_violence_5y',
                                     'm_physical_violence_6y',
                                     'm_physical_violence_9y',
                                     'p_cruelty_emotional_8wk',
                                     'p_cruelty_emotional_8m',
                                     'p_cruelty_emotional_21m',
                                     'p_cruelty_emotional_3y',
                                     'p_cruelty_emotional_4y',
                                     'p_cruelty_emotional_5y',
                                     'p_cruelty_emotional_6y',
                                     'p_cruelty_emotional_9y',
                                     'm_cruelty_emotional_21m',
                                     'm_cruelty_emotional_3y',
                                     'm_cruelty_emotional_4y',
                                     'm_cruelty_emotional_5y',
                                     'm_cruelty_emotional_6y',
                                     'm_cruelty_emotional_9y')])
                                           
                                  
####################################################################################################################################################

# 6. CREATE POSTNATAL CUMULATIVE RISK SCORE 

# This function merges together all separate data.frames according to the 'cidB2957' column with all relevant items for postnatal stress.
# tech-tip from Serena: use Reduce because merge can only take two dataframes at a time 

postnatal_stress <- Reduce(function(x,y) merge(x = x, y = y, by = c('cidB2957','qlet'),  all = TRUE),
                           list(LE_all, CR_all, PR_all, IR_all, DV_all))
                            


####################################################################################################################################################


# 7. SUMMARY STATISTICS 


# Let's have a look at risk distribution and missing data per indicator (as per Serena's script) 

postnatal_summary = data.frame(row.names=c("no risk","risk","NA","%risk","%miss"))

for (i in 3:ncol(postnatal_stress)) { # ATTENTION, if not merged with child_id, count from 2.
  s = summary(as.factor(postnatal_stress[,i]))
  c = colnames(postnatal_stress)[i]
  postnatal_summary[1:3,c] <- s
  postnatal_summary[4,c] <- round((postnatal_summary[2,c] / 15442)*100, 2)
  postnatal_summary[5,c] <- round((postnatal_summary[3,c] / 15442)*100, 2)
}

####################################################################################################################################################


# 8. MISSINGNESS

# Calculate the percentage missing data (obtained from SereDef 'Setup_and_functions.R' script at https://github.com/SereDef/cumulative-ELS-score  
percent_missing <- function(var) { sum(is.na(var)) / length(var) * 100 }


# Apply the percent_missing function to the rows (1) of the entire dataset 

postnatal_stress$post_percent_missing = apply(postnatal_stress[,3:ncol(postnatal_stress)], # Same as above, if not merged with child_id, count from 2.
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
# Running the domainscore function will add two extra columns to postnatal_stress dataframe: 
# one containing the % of missingnes per participant and the other containing the domain score 

# Source the domainscore function
#source("0.functions.R")

# LE
postnatal_stress[,c('post_LE_percent_missing','post_life_events')] <- domainscore(postnatal_stress[,c(
                                         'sick_or_accident_8wk',
                                         'family_member_ill_8wk',
                                         'smbd_important_ill_8wk',
                                         'partner_died_8wk',
                                         'smbd_important_died_8wk',
                                         'moved_8wk',
                                         'burglary_or_car_theft_8wk',
                                         'work_problems_8wk',
                                           'sick_or_accident_18m',
                                           'family_member_ill_8m',
                                           'smbd_important_ill_8m',
                                           'partner_died_8m',
                                           'smbd_important_died_8m',
                                           'pet_died_18m',
                                           'separated_from_parent_18m',
                                           'started_nursery_18m',
                                           'acquired_new_parent_18m',
                                           'change_carer_18m',
                                           'moved_18m',
                                           'm_pregnant_8m',
                                           'new_sibling_18m',
                                           'separated_from_smbd_18m',
                                           'ch_had_fright_18m',
                                           'work_problems_8m',
                                           'sick_or_accident_30m',
                                           'family_member_ill_21m',
                                           'smbd_important_ill_21m',
                                           'partner_died_21m',
                                           'smbd_important_died_21m',
                                           'pet_died_30m',
                                           'separated_from_parent_30m',
                                           'started_nursery_30m',
                                           'acquired_new_parent_30m',
                                           'change_carer_30m',
                                           'moved_30m',
                                           'm_pregnant_21m',
                                           'new_sibling_30m',
                                           'separated_from_smbd_30m',
                                           'burglary_or_car_theft_21m',
                                           'ch_had_fright_30m',
                                         'work_problems_21m',
                                         'sick_or_accident_3y',
                                         'family_member_ill_3y',
                                         'smbd_important_ill_3y',
                                         'partner_died_3y',
                                         'smbd_important_died_3y',
                                         'pet_died_3y',
                                         'separated_from_parent_3y',
                                         'started_nursery_3y',
                                         'acquired_new_parent_3y',
                                         'change_carer_3y',
                                         'moved_3y',
                                         'm_pregnant_3y',
                                         'new_sibling_3y',
                                         'separated_from_smbd_3y',
                                         'burglary_or_car_theft_3y',
                                         'ch_had_fright_3y',
                                         'work_problems_3y',
                                         'sick_or_accident_4y',
                                         'family_member_ill_4y',
                                         'smbd_important_ill_4y',
                                         'partner_died_4y',
                                         'smbd_important_died_4y',
                                         'pet_died_4y',
                                         'separated_from_parent_4y',
                                         'started_nursery_4y',
                                         'acquired_new_parent_4y',
                                         'change_carer_4y',
                                         'moved_4y',
                                         'm_pregnant_4y',
                                         'new_sibling_4y',
                                         'separated_from_smbd_4y',
                                         'burglary_or_car_theft_4y',
                                         'ch_had_fright_4y',
                                         'work_problems_4y',
                                         'sick_or_accident_5y',
                                         'family_member_ill_5y',
                                         'smbd_important_ill_5y',
                                         'partner_died_5y',
                                         'smbd_important_died_5y',
                                         'pet_died_5y',
                                         'separated_from_parent_5y',
                                         'started_nursery_5y',
                                         'acquired_new_parent_5y',
                                         'change_carer_5y',
                                         'moved_5y',
                                         'm_pregnant_5y',
                                         'new_sibling_5y',
                                         'separated_from_smbd_5y',
                                         'burglary_or_car_theft_5y',
                                         'ch_had_fright_5y',
                                         'work_problems_5y',
                                         'sick_or_accident_6y',
                                         'family_member_ill_6y',
                                         'smbd_important_ill_6y',
                                         'partner_died_6y',
                                         'smbd_important_died_6y',
                                         'pet_died_6y',
                                         'separated_from_parent_6y',
                                         'acquired_new_parent_6y',
                                         'change_carer_6y',
                                         'moved_6y',
                                         'm_pregnant_6y',
                                         'new_sibling_6y',
                                         'separated_from_smbd_6y',
                                         'burglary_or_car_theft_6y',
                                         'ch_had_fright_6y',
                                         'work_problems_6y',
                                         'sick_or_accident_9y',
                                         'family_member_ill_9y',
                                         'smbd_important_ill_9y',
                                         'partner_died_9y',
                                         'smbd_important_died_9y',
                                         'pet_died_9y',
                                         'separated_from_parent_8y',
                                         'acquired_new_parent_8y',
                                         'change_carer_8y',
                                         'moved_9y',
                                         'm_pregnant_9y',
                                         'new_sibling_8y',
                                         'separated_from_smbd_8y',
                                         'burglary_or_car_theft_9y',
                                         'ch_had_fright_8y',
                                         'work_problems_9y')])



# CR
postnatal_stress[,c('post_CR_percent_missing','post_contextual_risk')] <- domainscore(postnatal_stress[,c(
  'housing_adequacy_2y',
  'housing_adequacy_4y',
  'housing_basic_living_2y',
  'housing_basic_living_4y',
  'housing_defects_2y',
  'housing_defects_4y',
  'unemployed_8wk',
  'unemployed_8m',
  'unemployed_21m',
  'unemployed_3y',
  'unemployed_4y',
  'unemployed_5y',
  'unemployed_6y',
  'unemployed_9y',
  'income_reduced_8wk',
  'income_reduced_8m',
  'income_reduced_21m',
  'income_reduced_3y',
  'income_reduced_4y',
  'income_reduced_5y',
  'income_reduced_6y',
  'income_reduced_9y',
  'homeless_childhood_8wk',
  'homeless_childhood_8m',
  'homeless_childhood_21m',
  'homeless_childhood_3y',
  'homeless_childhood_4y',
  'homeless_childhood_5y',
  'homeless_childhood_6y',
  'homeless_childhood_9y',
  'major_financial_problems_8wk',
  'major_financial_problems_8m',
  'major_financial_problems_21m',
  'major_financial_problems_3y',
  'major_financial_problems_4y',
  'major_financial_problems_5y',
  'major_financial_problems_6y',
  'major_financial_problems_9y',
  'm_education',
  'p_education',
  'neighbourhood_problems_21m',
  'neighbourhood_problems_3y')])


# PR
postnatal_stress[,c('post_PR_percent_missing','post_parental_risk')] <- domainscore(postnatal_stress[,c(
  'criminal_record_parent_8wk',
  'criminal_record_parent_8m',
  'criminal_record_parent_21m',
  'criminal_record_parent_3y',
  'criminal_record_parent_4y',
  'criminal_record_parent_5y',
  'criminal_record_parent_6y',
  'criminal_record_parent_9y',
  'miscarriage_or_abortion_8wk',
  'miscarriage_or_abortion_8m',
  'miscarriage_or_abortion_21m',
  'miscarriage_or_abortion_3y',
  'miscarriage_or_abortion_4y',
  'miscarriage_or_abortion_5y',
  'miscarriage_or_abortion_6y',
  'miscarriage_or_abortion_9y',
  'm_attempted_suicide_8wk',
  'm_attempted_suicide_8m',
  'm_attempted_suicide_21m',
  'm_attempted_suicide_3y',
  'm_attempted_suicide_4y',
  'm_attempted_suicide_5y',
  'm_attempted_suicide_6y',
  'm_attempted_suicide_9y',
  'm_age',
  'p_age',
  'm_depression_8m',
  'm_depression_21m',
  'm_depression_3y',
  'm_depression_4y',
  'm_depression_5y',
  'm_depression_6y',
  'm_depression_9y',
  'p_depression_8m',
  'p_depression_21m',
  'p_depression_3y',
  'p_depression_4y',
  'p_depression_5y',
  'p_depression_6y',
  'p_depression_8y',
  'p_depression_9y',
  'm_anxiety_8wk',
  'm_anxiety_8m',
  'm_anxiety_21m',
  'm_anxiety_3y',
  'm_anxiety_5y',
  'm_anxiety_6y',
  'p_anxiety_8m',
  'p_anxiety_21m',
  'p_anxiety_3y',
  'p_anxiety_4y',
  'p_anxiety_5y',
  'p_anxiety_6y',
  'p_anxiety_9y',
  'm_interpersonal_sensitivity',
  'p_interpersonal_sensitivity')])


# IR
postnatal_stress[,c('post_IR_percent_missing','post_interpersonal_risk')] <- domainscore(postnatal_stress[,c(
  'divorce_8wk',
  'divorce_8m',
  'divorce_21m',
  'divorce_3y',
  'divorce_4y',
  'divorce_5y',
  'divorce_6y',
  'divorce_9y',
  'p_rejected_child_8wk',
  'p_rejected_child_8m',
  'p_rejected_child_21m',
  'p_rejected_child_3y',
  'p_rejected_child_4y',
  'p_rejected_child_5y',
  'p_rejected_child_6y',
  'p_rejected_child_9y',
  'p_went_away_8wk',
  'p_went_away_8m',
  'p_went_away_21m',
  'p_went_away_3y',
  'p_went_away_4y',
  'p_went_away_5y',
  'p_went_away_6y',
  'p_went_away_9y',
  'conflict_in_family_8wk',
  'conflict_in_family_8m',
  'conflict_in_family_21m',
  'conflict_in_family_3y',
  'conflict_in_family_4y',
  'conflict_in_family_5y',
  'conflict_in_family_6y',
  'conflict_in_family_9y',
  'conflict_family_violence_8wk',
  'conflict_family_violence_8m',
  'conflict_family_violence_21m',
  'conflict_family_violence_3y',
  'conflict_family_violence_4y',
  'conflict_family_violence_5y',
  'conflict_family_violence_6y',
  'conflict_family_violence_9y',
  'm_new_partner_8wk',
  'm_new_partner_8m',
  'm_new_partner_21m',
  'm_new_partner_3y',
  'm_new_partner_4y',
  'm_new_partner_5y',
  'm_new_partner_6y',
  'm_new_partner_9y',
  'argued_fam_friends_8wk',
  'argued_fam_friends_8m',
  'argued_fam_friends_21m',
  'argued_fam_friends_3y',
  'argued_fam_friends_4y',
  'argued_fam_friends_5y',
  'argued_fam_friends_6y',
  'argued_fam_friends_9y',
  'separated_8wk',
  'separated_8m',
  'separated_21m',
  'separated_3y',
  'separated_4y',
  'separated_5y',
  'separated_6y',
  'separated_9y')])

# DV
postnatal_stress[,c('post_DV_percent_missing','post_direct_victimization')] <- domainscore(postnatal_stress[,c(
  'bullying_8y',
  'physical_violence_18m',
  'physical_violence_30m',
  'physical_violence_3y',
  'physical_violence_4y',
  'physical_violence_5y',
  'physical_violence_6y',
  'physical_violence_9y',
  'sexual_abuse_18m',
  'sexual_abuse_30m',
  'sexual_abuse_3y',
  'sexual_abuse_4y',
  'sexual_abuse_5y',
  'sexual_abuse_6y',
  'sexual_abuse_8y',
  'p_physical_violence_8wk',
  'p_physical_violence_8m',
  'p_physical_violence_21m',
  'p_physical_violence_3y',
  'p_physical_violence_4y',
  'p_physical_violence_5y',
  'p_physical_violence_6y',
  'p_physical_violence_9y',
  'm_physical_violence_8m',
  'm_physical_violence_21m',
  'm_physical_violence_3y',
  'm_physical_violence_4y',
  'm_physical_violence_5y',
  'm_physical_violence_6y',
  'm_physical_violence_9y',
  'p_cruelty_emotional_8wk',
  'p_cruelty_emotional_8m',
  'p_cruelty_emotional_21m',
  'p_cruelty_emotional_3y',
  'p_cruelty_emotional_4y',
  'p_cruelty_emotional_5y',
  'p_cruelty_emotional_6y',
  'p_cruelty_emotional_9y',
  'm_cruelty_emotional_21m',
  'm_cruelty_emotional_3y',
  'm_cruelty_emotional_4y',
  'm_cruelty_emotional_5y',
  'm_cruelty_emotional_6y',
  'm_cruelty_emotional_9y')])



####################################################################################################################################################

# 10. SAVE DATA

# Save the dataset in an .rds file, in the directory where you have the raw data
save(postnatal_stress, file = 'postnatal_stress.RData')
save(postnatal_summary, file  = 'postnatal_summary.RData')

# Or save the dataset in a .csv format, in the directory where you have the raw data
write.csv(postnatal_stress, file = "postnatal_stress.csv", row.names = FALSE, quote = FALSE)
write.csv(postnatal_summary, file = "postnatal_summary.csv", row.names = T, quote = FALSE)

####################################################################################################################################################
