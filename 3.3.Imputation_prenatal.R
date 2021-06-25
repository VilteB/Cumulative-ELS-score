
# Hi again, 
# the following code runs the imputation of missing data, necessary for 
# the analysis of the association between ELS and psycho-cardio-metabolic multi-morbidity 
# in children. 

# All you need it the file we created in the 'Dataset_contruction.R' script: the 
# "ELSPCM_dataset.rds" 
# Ok, let's get started!

# Load libraries
library(mice);
library(miceadds)
library(tidyverse)

################################################################################
# Load datasets
load('ELSPCM_dataset.RData')

#------------------------------------------------------------------------------#
##---------------------------- Imputation model ----------------------------- ##
#------------------------------------------------------------------------------#

#convert to a data frame
ELSPCM <- as.data.frame(ELS_PCM_essentials)

#drop all postnatal vars (keeping postnatal m_depression and p_depression as these are used as auxiliary vars)
# Also drop:  'post_life_events', 'post_contextual_risk', 'post_parental_risk', 'post_interpersonal_risk', 'post_direct_victimization', "postnatal_stress"
# intern_score_z?
ELSPCM_pre <- ELSPCM %>% select(-c('sick_or_accident_8wk',	'family_member_ill_8wk',	'smbd_important_ill_8wk',	'partner_died_8wk',	'smbd_important_died_8wk',	'moved_8wk',	'burglary_or_car_theft_8wk',	'work_problems_8wk',	'sick_or_accident_18m',	'family_member_ill_8m',	'smbd_important_ill_8m',	'partner_died_8m',	'smbd_important_died_8m',	'pet_died_18m',	
                                   'separated_from_parent_18m',	'started_nursery_18m',	'acquired_new_parent_18m',	'change_carer_18m',	'moved_18m',	'm_pregnant_8m',	'new_sibling_18m',	'separated_from_smbd_18m',	'ch_had_fright_18m',	'work_problems_8m',	'sick_or_accident_30m',	'family_member_ill_21m',	'smbd_important_ill_21m',	'partner_died_21m',	
                                   'smbd_important_died_21m',	'pet_died_30m',	'separated_from_parent_30m',	'started_nursery_30m',	'acquired_new_parent_30m',	'change_carer_30m',	'moved_30m',	'm_pregnant_21m',	'new_sibling_30m',	'separated_from_smbd_30m',	'burglary_or_car_theft_21m',	'ch_had_fright_30m',	'work_problems_21m',	'sick_or_accident_3y',	
                                   'family_member_ill_3y',	'smbd_important_ill_3y',	'partner_died_3y',	'smbd_important_died_3y',	'pet_died_3y',	'separated_from_parent_3y',	'started_nursery_3y',	'acquired_new_parent_3y',	'change_carer_3y',	'moved_3y',	'm_pregnant_3y',	'new_sibling_3y',	'separated_from_smbd_3y',	'burglary_or_car_theft_3y',	'ch_had_fright_3y',	
                                   'work_problems_3y',	'sick_or_accident_4y',	'family_member_ill_4y',	'smbd_important_ill_4y',	'partner_died_4y',	'smbd_important_died_4y',	'pet_died_4y',	'separated_from_parent_4y',	'started_nursery_4y',	'acquired_new_parent_4y',	'change_carer_4y',	'moved_4y',	'm_pregnant_4y',	'new_sibling_4y',	'separated_from_smbd_4y',	
                                   'burglary_or_car_theft_4y',	'ch_had_fright_4y',	'work_problems_4y',	'sick_or_accident_5y',	'family_member_ill_5y',	'smbd_important_ill_5y',	'partner_died_5y',	'smbd_important_died_5y',	'pet_died_5y',	'separated_from_parent_5y',	'started_nursery_5y',	'acquired_new_parent_5y',	'change_carer_5y',	'moved_5y',	'm_pregnant_5y',
                                   'new_sibling_5y',	'separated_from_smbd_5y',	'burglary_or_car_theft_5y',	'ch_had_fright_5y',	'work_problems_5y',	'sick_or_accident_6y',	'family_member_ill_6y',	'smbd_important_ill_6y',	'partner_died_6y',	'smbd_important_died_6y',	'pet_died_6y',	'separated_from_parent_6y',	'acquired_new_parent_6y',	'change_carer_6y',	'moved_6y',	
                                   'm_pregnant_6y',	'new_sibling_6y',	'separated_from_smbd_6y',	'burglary_or_car_theft_6y',	'ch_had_fright_6y',	'work_problems_6y',	'sick_or_accident_9y',	'family_member_ill_9y',	'smbd_important_ill_9y',	'partner_died_9y',	'smbd_important_died_9y',	'pet_died_9y',	'separated_from_parent_8y',	'acquired_new_parent_8y',	'change_carer_8y',	
                                   'moved_9y',	'm_pregnant_9y',	'new_sibling_8y',	'separated_from_smbd_8y',	'burglary_or_car_theft_9y',	'ch_had_fright_8y',	'work_problems_9y', # LE
                                   'housing_adequacy_2y',	'housing_adequacy_4y',	'housing_basic_living_2y',	'housing_basic_living_4y',	'housing_defects_2y',	'housing_defects_4y',	'unemployed_8wk',	'unemployed_8m',	'unemployed_21m',	'unemployed_3y',	'unemployed_4y',	'unemployed_5y',	'unemployed_6y',	'unemployed_9y',	'income_reduced_8wk',	'income_reduced_8m',	
                                   'income_reduced_21m',	'income_reduced_3y',	'income_reduced_4y',	'income_reduced_5y',	'income_reduced_6y',	'income_reduced_9y',	'homeless_childhood_8wk',	'homeless_childhood_8m',	'homeless_childhood_21m',	'homeless_childhood_3y',	'homeless_childhood_4y',	'homeless_childhood_5y',	'homeless_childhood_6y',	'homeless_childhood_9y',	
                                   'major_financial_problems_8wk',	'major_financial_problems_8m',	'major_financial_problems_21m',	'major_financial_problems_3y',	'major_financial_problems_4y',	'major_financial_problems_5y',	'major_financial_problems_6y',	'major_financial_problems_9y',	'm_education',	'p_education',	'neighbourhood_problems_21m',	'neighbourhood_problems_3y', # CR
                                   'criminal_record_parent_8wk',	'criminal_record_parent_8m',	'criminal_record_parent_21m',	'criminal_record_parent_3y',	'criminal_record_parent_4y',	'criminal_record_parent_5y',	'criminal_record_parent_6y',	'criminal_record_parent_9y',	'miscarriage_or_abortion_8wk',	'miscarriage_or_abortion_8m',	'miscarriage_or_abortion_21m',	
                                   'miscarriage_or_abortion_3y',	'miscarriage_or_abortion_4y',	'miscarriage_or_abortion_5y',	'miscarriage_or_abortion_6y',	'miscarriage_or_abortion_9y',	'm_attempted_suicide_8wk',	'm_attempted_suicide_8m',	'm_attempted_suicide_21m',	'm_attempted_suicide_3y',	'm_attempted_suicide_4y',	'm_attempted_suicide_5y',	'm_attempted_suicide_6y',	
                                   'm_attempted_suicide_9y',	'm_age',	'p_age',		
                                   'm_anxiety_8wk',	'm_anxiety_8m',	'm_anxiety_21m',	'm_anxiety_3y',	'm_anxiety_5y',	'm_anxiety_6y',	'p_anxiety_8m',	'p_anxiety_21m',	'p_anxiety_3y',	'p_anxiety_4y',	'p_anxiety_5y',	'p_anxiety_6y',	'p_anxiety_9y',	'm_interpersonal_sensitivity',	'p_interpersonal_sensitivity', # PR
                                   'divorce_8wk',	'divorce_8m',	'divorce_21m',	'divorce_3y',	'divorce_4y',	'divorce_5y',	'divorce_6y',	'divorce_9y',	'p_rejected_child_8wk',	'p_rejected_child_8m',	'p_rejected_child_21m',	'p_rejected_child_3y',	'p_rejected_child_4y',	'p_rejected_child_5y',	'p_rejected_child_6y',	'p_rejected_child_9y',	'p_went_away_8wk',	'p_went_away_8m',	
                                   'p_went_away_21m',	'p_went_away_3y',	'p_went_away_4y',	'p_went_away_5y',	'p_went_away_6y',	'p_went_away_9y',	'conflict_in_family_8wk',	'conflict_in_family_8m',	'conflict_in_family_21m',	'conflict_in_family_3y',	'conflict_in_family_4y',	'conflict_in_family_5y',	'conflict_in_family_6y',	'conflict_in_family_9y',	'conflict_family_violence_8wk',	
                                   'conflict_family_violence_8m',	'conflict_family_violence_21m',	'conflict_family_violence_3y',	'conflict_family_violence_4y',	'conflict_family_violence_5y',	'conflict_family_violence_6y',	'conflict_family_violence_9y',	'm_new_partner_8wk',	'm_new_partner_8m',	'm_new_partner_21m',	'm_new_partner_3y',	'm_new_partner_4y',	'm_new_partner_5y',	
                                   'm_new_partner_6y',	'm_new_partner_9y',	'argued_fam_friends_8wk',	'argued_fam_friends_8m',	'argued_fam_friends_21m',	'argued_fam_friends_3y',	'argued_fam_friends_4y',	'argued_fam_friends_5y',	'argued_fam_friends_6y',	'argued_fam_friends_9y',	'separated_8wk',	'separated_8m',	'separated_21m',	'separated_3y',	'separated_4y',	'separated_5y',	
                                   'separated_6y',	'separated_9y', # IR
                                   'bullying_8y',	'physical_violence_18m',	'physical_violence_30m',	'physical_violence_3y',	'physical_violence_4y',	'physical_violence_5y',	'physical_violence_6y',	'physical_violence_9y',	'sexual_abuse_18m',	'sexual_abuse_30m',	'sexual_abuse_3y',	'sexual_abuse_4y',	'sexual_abuse_5y',	'sexual_abuse_6y',	'sexual_abuse_8y',	'p_physical_violence_8wk',	
                                   'p_physical_violence_8m',	'p_physical_violence_21m',	'p_physical_violence_3y',	'p_physical_violence_4y',	'p_physical_violence_5y',	'p_physical_violence_6y',	'p_physical_violence_9y',	'm_physical_violence_8m',	'm_physical_violence_21m',	'm_physical_violence_3y',	'm_physical_violence_4y',	'm_physical_violence_5y',	'm_physical_violence_6y',	
                                   'm_physical_violence_9y',	'p_cruelty_emotional_8wk',	'p_cruelty_emotional_8m',	'p_cruelty_emotional_21m',	'p_cruelty_emotional_3y',	'p_cruelty_emotional_4y',	'p_cruelty_emotional_5y',	'p_cruelty_emotional_6y',	'p_cruelty_emotional_9y',	'm_cruelty_emotional_21m',	'm_cruelty_emotional_3y',	'm_cruelty_emotional_4y',	'm_cruelty_emotional_5y',	
                                   'm_cruelty_emotional_6y',	'm_cruelty_emotional_9y',
                                   # postnatal domain scores
                                   'post_life_events', 'post_contextual_risk', 'post_parental_risk', 'post_interpersonal_risk', 'post_direct_victimization', "postnatal_stress"))

# We started with a dry run to specify the default arguments.
imp0 <- mice(ELSPCM_pre, maxit = 0, 
             defaultMethod = rep('pmm',4)) # set the imputation method to predictive mean matching (PMM)* 
#remove.collinear = F) # Because maternal age is measured twice in prenatal and postnatal 

# * PMM imputes a value randomly from a set of observed values whose predicted values 
#   are closest to the predicted value of the specified regression model. PMM has been 
#   said to perform quite well under circumstance where the categorical data is sparse 
#   (Van Buuren, 2018).

# 17 logged  events: all m and p depression items are collinear - should I drop them? m_attempted_suicide_pre is constant - why?

meth <- make.method(ELSPCM_pre)
# We use passive imputation for the domain scores. This means that the indicator items  
# are imputed first, and then, using these complete items, mean domain scores are 
# derived by the formula specified below.
meth['pre_life_events'] <- "~I( (partner_died_pre  + smbd_important_died_pre  + smbd_important_ill_pre + sick_or_accident_pre + moved_pre +
blood_loss +examination + pregnancy_worried + baby_worried + burglary_or_car_theft_pre + work_problems_pre +
abortion_pre + married_pre + twins_pre) / 14)" 

meth['pre_contextual_risk'] <- "~I( (income_reduced_pre +  homeless_pregnancy +  major_financial_problems_pre + 
housing_adequacy_pre + housing_basic_living_pre + housing_defects_pre + m_education_pre + unemployed_pre) / 8)"

meth['pre_parental_risks'] <- "~I( (criminal_record_parent_pre +
                                                      m_attempted_suicide_pre + 
                                                      early_pregnancy +
                                                      m_depression_pre +
                                                      m_anxiety_pre +
                                                      m_interpersonal_sensitivity_pre) / 6)"

meth['pre_interpersonal_risks'] <- "~I( (divorce_pre +
                                                          p_rejected_child_pre +
                                                          p_went_away_pre +
                                                          separated_pre +
                                                          conflict_in_family_pre +
                                                          argued_fam_friends_pre +
                                                          conflict_family_violence_pre +
                                                          marital_status_pregnancy +
                                                          family_affection +
                                                          family_size_pregnancy +
                                                          family_problems +
                                                          family_support +
                                                          social_network_emotional +
                                                          social_network_practical)/ 14)"


# below hasn't been changed to ALSPAC yet
#meth['post_life_events'] <- "~I( (sick_or_accident + family_member_ill + smbd_important_ill + parent_died + smbd_important_died + pet_died + school_workload + repeated_grade + lost_smth_important + moved + changed_school + friend_moved + fire_or_burglary) / 13)"
#meth['post_contextual_risk'] <- "~I( (material_deprivation + financial_difficulties + neiborhood_problems + trouble_pay_childhood + income_once + income_chronic + unemployed_once + unemployed_chronic + m_education + p_education) / 10)"
#meth['post_parental_risk'] <- "~I( (tension_at_work + m_age + p_age + m_interpersonal_sensitivity + m_anxiety + m_depression + p_interpersonal_sensitivity + p_depression + p_anxiety) / 9)"
#meth['post_interpersonal_risk'] <- "~I( (conflict_family_member + conflict_smbd_else + conflict_in_family + divorce_childhood + argument_friend + marital_problems + marital_status + family_size + m_fad_5yrs + m_fad_9yrs + p_fad_9yrs) / 11)"
#meth['post_direct_victimization'] <- "~I( (physical_violence + physical_threats + sexual_harrasment + sexual_behavior + rumors_or_gossip + m_harsh_parent + p_harsh_parent + bullying) / 8)"



# We also use passive imputation for the period specific cumulative ELS scores.
meth['prenatal_stress'] <- "~I( pre_life_events + pre_contextual_risk + pre_parental_risks + pre_interpersonal_risks )"
#meth['postnatal_stress'] <- "~I( post_life_events + post_contextual_risk + post_parental_risk + post_interpersonal_risk + post_direct_victimization )"


# We are going to need a different set of predictors for the different variables we impute 
# so let's define them using the predictormatrix, that gives these instructions to 
# mice
predictormatrix <- imp0$predictorMatrix

# Do not use cidB2957 as predictor:
predictormatrix[, "cidB2957"] <- predictormatrix["cidB2957",] <- 0
# Do not use age_child as a predictor (no reason to believe it is associated with missingness)
predictormatrix[, c("age_child.10y","age_child.13y","age_child.15y","age_child.17y", "age_child.23y")] <- predictormatrix[c("age_child.10y","age_child.13y","age_child.15y","age_child.17y", "age_child.23y"),] <- 0
# Do not use outcome groups as a predictor
predictormatrix[, "risk_groups"] <- predictormatrix["risk_groups",] <- 0


### Impute auxiliary variables and covariates ###

# To prevent multicollinearity, we adjust the predictor matrix such that the 
# auxiliary variables would be imputed given the domain scores and the outcomes, 
# but not by the single items.


predictormatrix[c('sex', 'age_child.10y', 'age_child.13y', 'age_child.15y', 'age_child.17y', 'age_child.23y',
                  'm_bmi_before_pregnancy', 'm_smoking', 'm_drinking',
                  'ethnicity', 'parity', 'gest_age_birth', 'gest_weight', # 'm_bmi_pregnancy', 'm_bmi_5yrs'
                  'm_depression_pre', 'm_depression_8m', 'm_depression_21m', 'm_depression_3y', 'm_depression_4y',
                  'm_depression_5y', 'm_depression_6y', 'm_depression_9y', 
                  'p_depression_8m', 'p_depression_21m', 'p_depression_3y', 
                  'm_depression_4y','m_depression_5y', 'p_depression_6y', 
                  'p_depression_8y', 'p_depression_9y',
                  'm_age_cont'),
                # all variables for prenatal risk
                c('partner_died_pre',	'smbd_important_died_pre',	'smbd_important_ill_pre',	'sick_or_accident_pre',	'moved_pre',	'blood_loss',	'examination',	'pregnancy_worried',	'baby_worried',	'burglary_or_car_theft_pre',	'work_problems_pre', 'abortion_pre',	'married_pre',	'twins_pre', #LE
                  'income_reduced_pre',	'homeless_pregnancy',	'major_financial_problems_pre',	'housing_adequacy_pre',	'housing_basic_living_pre',	'housing_defects_pre',	'm_education_pre',	'unemployed_pre',	#CR
                  'criminal_record_parent_pre',	'm_attempted_suicide_pre',	'early_pregnancy',	'm_depression_pre',	'm_anxiety_pre',	'm_interpersonal_sensitivity_pre',	#PR
                  'divorce_pre',	'p_rejected_child_pre',	'p_went_away_pre',	'separated_pre',	'conflict_in_family_pre',	'argued_fam_friends_pre',	'conflict_family_violence_pre',	'marital_status_pregnancy',	'family_affection',	'family_size_pregnancy',	'family_problems',	'family_support',	'social_network_emotional',	'social_network_practical', # IR
                  'prenatal_stress', # 'postnatal_stress',
                  'sex', 'age_child.10y', 'age_child.13y', 'age_child.15y', 'age_child.17y', 'age_child.23y',
                  'm_bmi_before_pregnancy', 'm_smoking', 'm_drinking',
                  'ethnicity', 'parity', 'gest_age_birth', 'gest_weight', # 'm_bmi_pregnancy', 'm_bmi_5yrs'
                  'm_depression_pre', 'm_depression_8m', 'm_depression_21m', 'm_depression_3y', 'm_depression_4y',
                  'm_depression_5y', 'm_depression_6y', 'm_depression_9y', 
                  'p_depression_8m', 'p_depression_21m', 'p_depression_3y', 
                  'm_depression_4y','m_depression_5y', 'p_depression_6y', 
                  'p_depression_8y', 'p_depression_9y',
                  'm_age_cont' )] <- 0 



# All single items are then imputed on the item level. We adjust the predictormatrix 
# such that we impute the items within a domain score given the other items in that
# domain score, the remaining domain scores, the outcomes and the auxiliary variables.
# Auxiliary variables were selected because they are either related to missingness 
# or to the domain scores themselves. They are used in the prediction model for 
# every other variable. When information is available prenatally as well postnatally, 
# we use the different period to minimize bias (E.G., for the imputation of prenatal
# items we used BMI of the mother when the child was aged 5, and for imputation 
# of postnatal items we used BMI of the mother during pregnancy).
# This method is more efficient as it does not use all available items, hence the 
# computational load is lower. The multicollinearity issue is also resolved. 
# The technique has been found to reduce standard error substantially compared to 
# complete-case analysis (Plumpton et al., 2010), and it outperforms other existing 
# techniques (Eekhout et al., 2018). 

# So, let's adjust the predictormatrices such that 'redundant' items were not used as a predictor.

### PRENATAL ###

# LE domain 

predictormatrix[c('partner_died_pre',	'smbd_important_died_pre',	'smbd_important_ill_pre',	'sick_or_accident_pre',	'moved_pre',	'blood_loss',	'examination',	'pregnancy_worried',	'baby_worried',	'burglary_or_car_theft_pre',	'work_problems_pre', 'abortion_pre',	'married_pre',	'twins_pre'), #LE
                c('income_reduced_pre',	'homeless_pregnancy',	'major_financial_problems_pre',	'housing_adequacy_pre',	'housing_basic_living_pre',	'housing_defects_pre',	'm_education_pre',	'unemployed_pre',	#CR
                  'criminal_record_parent_pre',	'm_attempted_suicide_pre',	'early_pregnancy',	'm_depression_pre',	'm_anxiety_pre',	'm_interpersonal_sensitivity_pre',	#PR
                  'divorce_pre',	'p_rejected_child_pre',	'p_went_away_pre',	'separated_pre',	'conflict_in_family_pre',	'argued_fam_friends_pre',	'conflict_family_violence_pre',	'marital_status_pregnancy',	'family_affection',	'family_size_pregnancy',	'family_problems',	'family_support',	'social_network_emotional',	'social_network_practical', # IR
                  'pre_life_events',
                  'prenatal_stress', #  'postnatal_stress', 
                  'm_bmi_before_pregnancy',
                  'm_depression_pre', 'm_depression_8m', 'm_depression_21m', 'm_depression_3y', 'm_depression_4y',
                  'm_depression_5y', 'm_depression_6y', 'm_depression_9y', 
                  'p_depression_8m', 'p_depression_21m', 'p_depression_3y', 
                  'm_depression_4y','m_depression_5y', 'p_depression_6y', 
                  'p_depression_8y', 'p_depression_9y' )] <- 0




# CR domain 

predictormatrix[c('income_reduced_pre',	'homeless_pregnancy',	'major_financial_problems_pre',	'housing_adequacy_pre',	'housing_basic_living_pre',	'housing_defects_pre',	'm_education_pre',	'unemployed_pre'),	#CR
                c('partner_died_pre',	'smbd_important_died_pre',	'smbd_important_ill_pre',	'sick_or_accident_pre',	'moved_pre',	'blood_loss',	'examination',	'pregnancy_worried',	'baby_worried',	'burglary_or_car_theft_pre',	'work_problems_pre', 'abortion_pre',	'married_pre',	'twins_pre', #LE
                  'criminal_record_parent_pre',	'm_attempted_suicide_pre',	'early_pregnancy',	'm_depression_pre',	'm_anxiety_pre',	'm_interpersonal_sensitivity_pre',	#PR
                  'divorce_pre',	'p_rejected_child_pre',	'p_went_away_pre',	'separated_pre',	'conflict_in_family_pre',	'argued_fam_friends_pre',	'conflict_family_violence_pre',	'marital_status_pregnancy',	'family_affection',	'family_size_pregnancy',	'family_problems',	'family_support',	'social_network_emotional',	'social_network_practical', # IR
                  'pre_contextual_risk',
                  'prenatal_stress', # 'postnatal_stress', 
                  'm_bmi_before_pregnancy',
                  'm_depression_pre', 'm_depression_8m', 'm_depression_21m', 'm_depression_3y', 'm_depression_4y',
                  'm_depression_5y', 'm_depression_6y', 'm_depression_9y', 
                  'p_depression_8m', 'p_depression_21m', 'p_depression_3y', 
                  'm_depression_4y','m_depression_5y', 'p_depression_6y', 
                  'p_depression_8y', 'p_depression_9y' )] <- 0




# PR domain


predictormatrix[c('criminal_record_parent_pre',	'm_attempted_suicide_pre',	'early_pregnancy',	'm_depression_pre',	'm_anxiety_pre',	'm_interpersonal_sensitivity_pre'),	#PR
                c('partner_died_pre',	'smbd_important_died_pre',	'smbd_important_ill_pre',	'sick_or_accident_pre',	'moved_pre',	'blood_loss',	'examination',	'pregnancy_worried',	'baby_worried',	'burglary_or_car_theft_pre',	'work_problems_pre', 'abortion_pre',	'married_pre',	'twins_pre', #LE
                  'income_reduced_pre',	'homeless_pregnancy',	'major_financial_problems_pre',	'housing_adequacy_pre',	'housing_basic_living_pre',	'housing_defects_pre',	'm_education_pre',	'unemployed_pre',	#CR
                  'divorce_pre',	'p_rejected_child_pre',	'p_went_away_pre',	'separated_pre',	'conflict_in_family_pre',	'argued_fam_friends_pre',	'conflict_family_violence_pre',	'marital_status_pregnancy',	'family_affection',	'family_size_pregnancy',	'family_problems',	'family_support',	'social_network_emotional',	'social_network_practical', # IR
                  'pre_parental_risks',
                  'prenatal_stress', # 'postnatal_stress', 
                  'm_bmi_before_pregnancy',
                  'm_depression_pre', 'm_depression_8m', 'm_depression_21m', 'm_depression_3y', 'm_depression_4y',
                  'm_depression_5y', 'm_depression_6y', 'm_depression_9y', 
                  'p_depression_8m', 'p_depression_21m', 'p_depression_3y', 
                  'm_depression_4y','m_depression_5y', 'p_depression_6y', 
                  'p_depression_8y', 'p_depression_9y' )] <- 0


# IR domain 

predictormatrix[c('divorce_pre',	'p_rejected_child_pre',	'p_went_away_pre',	'separated_pre',	'conflict_in_family_pre',	'argued_fam_friends_pre',	'conflict_family_violence_pre',	'marital_status_pregnancy',	'family_affection',	'family_size_pregnancy',	'family_problems',	'family_support',	'social_network_emotional',	'social_network_practical'), # IR
                c('partner_died_pre',	'smbd_important_died_pre',	'smbd_important_ill_pre',	'sick_or_accident_pre',	'moved_pre',	'blood_loss',	'examination',	'pregnancy_worried',	'baby_worried',	'burglary_or_car_theft_pre',	'work_problems_pre', 'abortion_pre',	'married_pre',	'twins_pre', #LE
                  'income_reduced_pre',	'homeless_pregnancy',	'major_financial_problems_pre',	'housing_adequacy_pre',	'housing_basic_living_pre',	'housing_defects_pre',	'm_education_pre',	'unemployed_pre',	#CR
                  'criminal_record_parent_pre',	'm_attempted_suicide_pre',	'early_pregnancy',	'm_depression_pre',	'm_anxiety_pre',	'm_interpersonal_sensitivity_pre',	#PR
                  'pre_interpersonal_risks',
                  'prenatal_stress', # 'postnatal_stress', 
                  'm_bmi_before_pregnancy',
                  'm_depression_pre', 'm_depression_8m', 'm_depression_21m', 'm_depression_3y', 'm_depression_4y',
                  'm_depression_5y', 'm_depression_6y', 'm_depression_9y', 
                  'p_depression_8m', 'p_depression_21m', 'p_depression_3y', 
                  'm_depression_4y','m_depression_5y', 'p_depression_6y', 
                  'p_depression_8y', 'p_depression_9y' )] <- 0



# visit the sequence
VisSeq <- imp0$visitSequence


# Run the actual imputation. To ensure convergence among the variables but retain
# low computational load, we do 60 iterations using 30 imputed datasets (following 
# Isabel's approach)
imp_pre <- mice(ELSPCM_pre, m = 1, # nr of imputed datasets
                   maxit = 2, #nr of iteration taken to impute missing values
                   seed = 2021, # set a seed for the random number generation in case i need to generate the same dataset again
                   method = meth,
                   visitSequence = VisSeq, 
                   predictorMatrix = predictormatrix)

imp_pre$loggedEvents  # 35 logged events meth = pmm --> only issues with intern_score_z for all ages and fat_mass_z for all ages. p_depression 4y and 5y as well

# checking the imputed dataset
df <- complete(imp_pre,1)
sum(is.na(ELSPCM_pre)) #had 59397 NA in original dataset
sum(is.na(imp_pre)) #after imputation 0 NA

outlist <- as.character(imp_pre$loggedEvents[, "dep"])
length(outlist) # the outlist contains 35 variables
outlist2 <- unique(outlist)
length(outlist2) # 11 of them are unique

#There are 11 unique variables to be removed. Therefore, before attempting imputation, let's clean the data.
# The downsized data are
data <- ELSPCM_pre[, !names(ELSPCM_pre) %in% outlist2]
dim(data)

################################################################################################################################################

# Run imputation with the new dataset

# We started with a dry run to specify the default arguments.
imp0 <- mice(data, maxit = 0, 
             defaultMethod = rep('pmm',4)) # set the imputation method to predictive mean matching (PMM)* 
#remove.collinear = F) # Because maternal age is measured twice in prenatal and postnatal 

# * PMM imputes a value randomly from a set of observed values whose predicted values 
#   are closest to the predicted value of the specified regression model. PMM has been 
#   said to perform quite well under circumstance where the categorical data is sparse 
#   (Van Buuren, 2018).

# 16 logged  events: all m and p depression items are collinear - should I drop them or create a chronic depression item? 

meth <- make.method(data)
# We use passive imputation for the domain scores. This means that the indicator items  
# are imputed first, and then, using these complete items, mean domain scores are 
# derived by the formula specified below.
meth['pre_life_events'] <- "~I( (partner_died_pre  + smbd_important_died_pre  + smbd_important_ill_pre + sick_or_accident_pre + moved_pre +
blood_loss +examination + pregnancy_worried + baby_worried + burglary_or_car_theft_pre + work_problems_pre +
abortion_pre + married_pre + twins_pre) / 14)" 

meth['pre_contextual_risk'] <- "~I( (income_reduced_pre +  homeless_pregnancy +  major_financial_problems_pre + 
housing_adequacy_pre + housing_basic_living_pre + housing_defects_pre + m_education_pre + unemployed_pre) / 8)"

meth['pre_parental_risks'] <- "~I( (criminal_record_parent_pre +
                                                      m_attempted_suicide_pre + 
                                                      early_pregnancy +
                                                      m_depression_pre +
                                                      m_anxiety_pre +
                                                      m_interpersonal_sensitivity_pre) / 6)"

meth['pre_interpersonal_risks'] <- "~I( (divorce_pre +
                                                          p_rejected_child_pre +
                                                          p_went_away_pre +
                                                          separated_pre +
                                                          conflict_in_family_pre +
                                                          argued_fam_friends_pre +
                                                          conflict_family_violence_pre +
                                                          marital_status_pregnancy +
                                                          family_affection +
                                                          family_size_pregnancy +
                                                          family_problems +
                                                          family_support +
                                                          social_network_emotional +
                                                          social_network_practical)/ 14)"


# below hasn't been changed to ALSPAC yet
#meth['post_life_events'] <- "~I( (sick_or_accident + family_member_ill + smbd_important_ill + parent_died + smbd_important_died + pet_died + school_workload + repeated_grade + lost_smth_important + moved + changed_school + friend_moved + fire_or_burglary) / 13)"
#meth['post_contextual_risk'] <- "~I( (material_deprivation + financial_difficulties + neiborhood_problems + trouble_pay_childhood + income_once + income_chronic + unemployed_once + unemployed_chronic + m_education + p_education) / 10)"
#meth['post_parental_risk'] <- "~I( (tension_at_work + m_age + p_age + m_interpersonal_sensitivity + m_anxiety + m_depression + p_interpersonal_sensitivity + p_depression + p_anxiety) / 9)"
#meth['post_interpersonal_risk'] <- "~I( (conflict_family_member + conflict_smbd_else + conflict_in_family + divorce_childhood + argument_friend + marital_problems + marital_status + family_size + m_fad_5yrs + m_fad_9yrs + p_fad_9yrs) / 11)"
#meth['post_direct_victimization'] <- "~I( (physical_violence + physical_threats + sexual_harrasment + sexual_behavior + rumors_or_gossip + m_harsh_parent + p_harsh_parent + bullying) / 8)"



# We also use passive imputation for the period specific cumulative ELS scores.
meth['prenatal_stress'] <- "~I( pre_life_events + pre_contextual_risk + pre_parental_risks + pre_interpersonal_risks )"
#meth['postnatal_stress'] <- "~I( post_life_events + post_contextual_risk + post_parental_risk + post_interpersonal_risk + post_direct_victimization )"


# We are going to need a different set of predictors for the different variables we impute 
# so let's define them using the predictormatrix, that gives these instructions to 
# mice
predictormatrix <- imp0$predictorMatrix

# Do not use cidB2957 as predictor:
predictormatrix[, "cidB2957"] <- predictormatrix["cidB2957",] <- 0
# Do not use age_child as a predictor (no reason to believe it is associated with missingness)
predictormatrix[, c("age_child.10y","age_child.13y","age_child.15y","age_child.17y", "age_child.23y")] <- predictormatrix[c("age_child.10y","age_child.13y","age_child.15y","age_child.17y", "age_child.23y"),] <- 0
# Do not use outcome groups as a predictor
predictormatrix[, "risk_groups"] <- predictormatrix["risk_groups",] <- 0


### Impute auxiliary variables and covariates ###

# To prevent multicollinearity, we adjust the predictor matrix such that the 
# auxiliary variables would be imputed given the domain scores and the outcomes, 
# but not by the single items.


predictormatrix[c('sex', 'age_child.10y', 'age_child.13y', 'age_child.15y', 'age_child.17y', 'age_child.23y',
                  'm_bmi_before_pregnancy', 'm_smoking', 'm_drinking',
                  'ethnicity', 'parity', 'gest_age_birth', 'gest_weight', # 'm_bmi_pregnancy', 'm_bmi_5yrs'
                  'm_depression_pre', 'm_depression_8m', 'm_depression_21m', 'm_depression_3y', 'm_depression_4y',
                  'm_depression_5y', 'm_depression_6y', 'm_depression_9y', 
                  'p_depression_8m', 'p_depression_21m', 'p_depression_3y', 
                  'm_depression_4y','m_depression_5y', 'p_depression_6y', 
                  'p_depression_8y', 'p_depression_9y',
                  'm_age_cont'),
                # all variables for prenatal risk
                c('partner_died_pre',	'smbd_important_died_pre',	'smbd_important_ill_pre',	'sick_or_accident_pre',	'moved_pre',	'blood_loss',	'examination',	'pregnancy_worried',	'baby_worried',	'burglary_or_car_theft_pre',	'work_problems_pre', 'abortion_pre',	'married_pre',	'twins_pre', #LE
                  'income_reduced_pre',	'homeless_pregnancy',	'major_financial_problems_pre',	'housing_adequacy_pre',	'housing_basic_living_pre',	'housing_defects_pre',	'm_education_pre',	'unemployed_pre',	#CR
                  'criminal_record_parent_pre',	'm_attempted_suicide_pre',	'early_pregnancy',	'm_depression_pre',	'm_anxiety_pre',	'm_interpersonal_sensitivity_pre',	#PR
                  'divorce_pre',	'p_rejected_child_pre',	'p_went_away_pre',	'separated_pre',	'conflict_in_family_pre',	'argued_fam_friends_pre',	'conflict_family_violence_pre',	'marital_status_pregnancy',	'family_affection',	'family_size_pregnancy',	'family_problems',	'family_support',	'social_network_emotional',	'social_network_practical', # IR
                  'prenatal_stress', # 'postnatal_stress',
                  'sex', 'age_child.10y', 'age_child.13y', 'age_child.15y', 'age_child.17y', 'age_child.23y',
                  'm_bmi_before_pregnancy', 'm_smoking', 'm_drinking',
                  'ethnicity', 'parity', 'gest_age_birth', 'gest_weight', # 'm_bmi_pregnancy', 'm_bmi_5yrs'
                  'm_depression_pre', 'm_depression_8m', 'm_depression_21m', 'm_depression_3y', 'm_depression_4y',
                  'm_depression_5y', 'm_depression_6y', 'm_depression_9y', 
                  'p_depression_8m', 'p_depression_21m', 'p_depression_3y', 
                  'm_depression_4y','m_depression_5y', 'p_depression_6y', 
                  'p_depression_8y', 'p_depression_9y',
                  'm_age_cont' )] <- 0 



# All single items are then imputed on the item level. We adjust the predictormatrix 
# such that we impute the items within a domain score given the other items in that
# domain score, the remaining domain scores, the outcomes and the auxiliary variables.
# Auxiliary variables were selected because they are either related to missingness 
# or to the domain scores themselves. They are used in the prediction model for 
# every other variable. When information is available prenatally as well postnatally, 
# we use the different period to minimize bias (E.G., for the imputation of prenatal
# items we used BMI of the mother when the child was aged 5, and for imputation 
# of postnatal items we used BMI of the mother during pregnancy).
# This method is more efficient as it does not use all available items, hence the 
# computational load is lower. The multicollinearity issue is also resolved. 
# The technique has been found to reduce standard error substantially compared to 
# complete-case analysis (Plumpton et al., 2010), and it outperforms other existing 
# techniques (Eekhout et al., 2018). 

# So, let's adjust the predictormatrices such that 'redundant' items were not used as a predictor.

### PRENATAL ###

# LE domain 

predictormatrix[c('partner_died_pre',	'smbd_important_died_pre',	'smbd_important_ill_pre',	'sick_or_accident_pre',	'moved_pre',	'blood_loss',	'examination',	'pregnancy_worried',	'baby_worried',	'burglary_or_car_theft_pre',	'work_problems_pre', 'abortion_pre',	'married_pre',	'twins_pre'), #LE
                c('income_reduced_pre',	'homeless_pregnancy',	'major_financial_problems_pre',	'housing_adequacy_pre',	'housing_basic_living_pre',	'housing_defects_pre',	'm_education_pre',	'unemployed_pre',	#CR
                  'criminal_record_parent_pre',	'm_attempted_suicide_pre',	'early_pregnancy',	'm_depression_pre',	'm_anxiety_pre',	'm_interpersonal_sensitivity_pre',	#PR
                  'divorce_pre',	'p_rejected_child_pre',	'p_went_away_pre',	'separated_pre',	'conflict_in_family_pre',	'argued_fam_friends_pre',	'conflict_family_violence_pre',	'marital_status_pregnancy',	'family_affection',	'family_size_pregnancy',	'family_problems',	'family_support',	'social_network_emotional',	'social_network_practical', # IR
                  'pre_life_events',
                  'prenatal_stress', #  'postnatal_stress', 
                  'm_bmi_before_pregnancy',
                  'm_depression_pre', 'm_depression_8m', 'm_depression_21m', 'm_depression_3y', 'm_depression_4y',
                  'm_depression_5y', 'm_depression_6y', 'm_depression_9y', 
                  'p_depression_8m', 'p_depression_21m', 'p_depression_3y', 
                  'm_depression_4y','m_depression_5y', 'p_depression_6y', 
                  'p_depression_8y', 'p_depression_9y' )] <- 0




# CR domain 

predictormatrix[c('income_reduced_pre',	'homeless_pregnancy',	'major_financial_problems_pre',	'housing_adequacy_pre',	'housing_basic_living_pre',	'housing_defects_pre',	'm_education_pre',	'unemployed_pre'),	#CR
                c('partner_died_pre',	'smbd_important_died_pre',	'smbd_important_ill_pre',	'sick_or_accident_pre',	'moved_pre',	'blood_loss',	'examination',	'pregnancy_worried',	'baby_worried',	'burglary_or_car_theft_pre',	'work_problems_pre', 'abortion_pre',	'married_pre',	'twins_pre', #LE
                  'criminal_record_parent_pre',	'm_attempted_suicide_pre',	'early_pregnancy',	'm_depression_pre',	'm_anxiety_pre',	'm_interpersonal_sensitivity_pre',	#PR
                  'divorce_pre',	'p_rejected_child_pre',	'p_went_away_pre',	'separated_pre',	'conflict_in_family_pre',	'argued_fam_friends_pre',	'conflict_family_violence_pre',	'marital_status_pregnancy',	'family_affection',	'family_size_pregnancy',	'family_problems',	'family_support',	'social_network_emotional',	'social_network_practical', # IR
                  'pre_contextual_risk',
                  'prenatal_stress', # 'postnatal_stress', 
                  'm_bmi_before_pregnancy',
                  'm_depression_pre', 'm_depression_8m', 'm_depression_21m', 'm_depression_3y', 'm_depression_4y',
                  'm_depression_5y', 'm_depression_6y', 'm_depression_9y', 
                  'p_depression_8m', 'p_depression_21m', 'p_depression_3y', 
                  'm_depression_4y','m_depression_5y', 'p_depression_6y', 
                  'p_depression_8y', 'p_depression_9y' )] <- 0




# PR domain


predictormatrix[c('criminal_record_parent_pre',	'm_attempted_suicide_pre',	'early_pregnancy',	'm_depression_pre',	'm_anxiety_pre',	'm_interpersonal_sensitivity_pre'),	#PR
                c('partner_died_pre',	'smbd_important_died_pre',	'smbd_important_ill_pre',	'sick_or_accident_pre',	'moved_pre',	'blood_loss',	'examination',	'pregnancy_worried',	'baby_worried',	'burglary_or_car_theft_pre',	'work_problems_pre', 'abortion_pre',	'married_pre',	'twins_pre', #LE
                  'income_reduced_pre',	'homeless_pregnancy',	'major_financial_problems_pre',	'housing_adequacy_pre',	'housing_basic_living_pre',	'housing_defects_pre',	'm_education_pre',	'unemployed_pre',	#CR
                  'divorce_pre',	'p_rejected_child_pre',	'p_went_away_pre',	'separated_pre',	'conflict_in_family_pre',	'argued_fam_friends_pre',	'conflict_family_violence_pre',	'marital_status_pregnancy',	'family_affection',	'family_size_pregnancy',	'family_problems',	'family_support',	'social_network_emotional',	'social_network_practical', # IR
                  'pre_parental_risks',
                  'prenatal_stress', # 'postnatal_stress', 
                  'm_bmi_before_pregnancy',
                  'm_depression_pre', 'm_depression_8m', 'm_depression_21m', 'm_depression_3y', 'm_depression_4y',
                  'm_depression_5y', 'm_depression_6y', 'm_depression_9y', 
                  'p_depression_8m', 'p_depression_21m', 'p_depression_3y', 
                  'm_depression_4y','m_depression_5y', 'p_depression_6y', 
                  'p_depression_8y', 'p_depression_9y' )] <- 0


# IR domain 

predictormatrix[c('divorce_pre',	'p_rejected_child_pre',	'p_went_away_pre',	'separated_pre',	'conflict_in_family_pre',	'argued_fam_friends_pre',	'conflict_family_violence_pre',	'marital_status_pregnancy',	'family_affection',	'family_size_pregnancy',	'family_problems',	'family_support',	'social_network_emotional',	'social_network_practical'), # IR
                c('partner_died_pre',	'smbd_important_died_pre',	'smbd_important_ill_pre',	'sick_or_accident_pre',	'moved_pre',	'blood_loss',	'examination',	'pregnancy_worried',	'baby_worried',	'burglary_or_car_theft_pre',	'work_problems_pre', 'abortion_pre',	'married_pre',	'twins_pre', #LE
                  'income_reduced_pre',	'homeless_pregnancy',	'major_financial_problems_pre',	'housing_adequacy_pre',	'housing_basic_living_pre',	'housing_defects_pre',	'm_education_pre',	'unemployed_pre',	#CR
                  'criminal_record_parent_pre',	'm_attempted_suicide_pre',	'early_pregnancy',	'm_depression_pre',	'm_anxiety_pre',	'm_interpersonal_sensitivity_pre',	#PR
                  'pre_interpersonal_risks',
                  'prenatal_stress', # 'postnatal_stress', 
                  'm_bmi_before_pregnancy',
                  'm_depression_pre', 'm_depression_8m', 'm_depression_21m', 'm_depression_3y', 'm_depression_4y',
                  'm_depression_5y', 'm_depression_6y', 'm_depression_9y', 
                  'p_depression_8m', 'p_depression_21m', 'p_depression_3y', 
                  'm_depression_4y','m_depression_5y', 'p_depression_6y', 
                  'p_depression_8y', 'p_depression_9y' )] <- 0



# visit the sequence
VisSeq <- imp0$visitSequence



# Run the actual imputation. To ensure convergence among the variables but retain
# low computational load, we do 60 iterations using 30 imputed datasets (following 
# Isabel's approach)
imp_pre2 <- mice(data, m = 1, # nr of imputed datasets
                maxit = 2, #nr of iteration taken to impute missing values
                seed = 2021, # set a seed for the random number generation in case i need to generate the same dataset again
                method = meth,
                visitSequence = VisSeq, 
                predictorMatrix = predictormatrix)

imp_pre2$loggedEvents  # 1 logged event =  m_attempted_suicide_pre constant 

# checking the imputed dataset
df <- complete(imp_pre2,1)
sum(is.na(data)) #had 38865 NA in original dataset
sum(is.na(imp_pre2)) #after imputation 0 NA

#####
# Now that we defined the final imputation model, run the actual imputation (with  more datasets and iterations). 
# To ensure convergence among the variables but retain
# low computational load, we do 60 iterations using 30 imputed datasets (following Isabel's approach)
imp_final <- mice(data, m = 30, # nr of imputed datasets
                   maxit = 60, #nr of iteration taken to impute missing values
                   seed = 2021, # set a seed for the random number generation in case i need to generate the same dataset again
                   method = meth,
                   visitSequence = VisSeq, 
                   predictorMatrix = predictormatrix)

# completed with NULL logged events

################### OPTIONAL CHECKS (beware: it takes time) ####################
# # Inspecting the distribution of observed and imputed values
# stripplot(imputation, pch = 20, cex = 1.2) # red dots are imputed values
# # A scatter plot is also useful to spot unusual patterns in two vars
# xyplot(imputation, pre_life_events ~ post_life_events | .imp, pch = 20, cex = 1.4)


# Standardize prenatal and postnatal stress to obtain standard betas from regression
# First temporarily transform mids object into a datalist object
datlist <- miceadds::mids2datlist(imp_final)
# Scale those bad boyz
sdatlist <- miceadds::scale_datlist(datlist, orig_var=c('prenatal_stress'), 
                                    trafo_var=paste0(c('prenatal_stress'), "_z") )
# Reconvert to mids object
imp_final2 <- miceadds::datlist2mids(sdatlist)  # this resulted in 17 logged events, why?


################################################################################
#### ------------------------- complete and save -------------------------- ####
################################################################################

# I save the mids object (i.e. list of imputed datasets)
save(imp_final2, file='imputation_list_prenatal.RData')

# I also save the last imputed dataset for sanity checks
ELS_PCM_imputed <- complete(imp_final2, 30) 
save(ELS_PCM_imputed,  file='ELSPCM_imputed_prenatal.RData')

################################################################################

# compare NAs
sum(is.na(ELS_PCM_imputed)) # was 13270 NA
sum(is.na(data)) # now 0 NA

################################################################################













