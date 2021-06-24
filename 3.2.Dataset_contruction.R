# Hi again, 
# the following script is building the final dataset that will be used for the analysis 
# of the association between ELS and psycho-cardio-metabolic multi-morbidity in children. 
# I will first exclude participants based on predifined criteria & data availability, 
# then construct the main outcome variable (risk_group) and cumulative exposures. 
# Missing values will be then imputed (see Imputation.R)

# This time you will only need three files: the "PCM_allvars.rds" we build using the 
# "PCM_outcomes_covs_aux.R" script; "prenatal_stress.rds" and "postnatal_stress.rds", 
# for which, if you want to know more, check out https://github.com/SereDef/cumulative-ELS-score. 

# Ok, let's get started!

#### ---------------------------- Dependencies ---------------------------- ####

# Point to useful libraries
library(pheatmap) # optional 

# load data from previous scripts
#load("PCM_project.RData")
#load('postnatal_stress_collapsed.RData')
#load('prenatal_stress.RData')

# This will come in handy for exclusion
'%notin%' <- Negate('%in%')

# check if the path to the datasets is already in memory, otherwise ask for it. 
#if (exists("pathtodata") == F) { pathtodata = readline(prompt="Enter path to data: ") }

################################################################################
# Load datasets
# pre_risk <- readRDS(paste(pathtodata, 'prenatal_stress.rds', sep = ""))
# post_risk <- readRDS(paste(pathtodata, 'postnatal_stress.rds', sep = ""))
# outcome <- readRDS(paste(pathtodata,'PCM_allvars.rds', sep = ""))


pre_risk <- prenatal_stress
post_risk <- postnatal_stress
outcome <- PCM_project

# Renaming ID to be identical
outcome$cidB2957 <- outcome$cidb2957

# merge them
ELS_PCM <- Reduce(function(x,y) merge(x = x, y = y, by = c('cidB2957', 'qlet'), all.x = TRUE),
                  list(pre_risk, post_risk, outcome)) 

################################################################################
## -------------------- Exclude participants (flowchart) -------------------- ##
################################################################################

initial_sample <- nrow(ELS_PCM)

## First exclusion step: remove participants whose missing value frequency is too high. 
## (i.e > 50% missing) for each developmental period. 

# Exclude children with high missingness in the prenatal period
prenatal_miss50 <- ELS_PCM[ELS_PCM$pre_percent_missing < 50,]
after_prenatal <- nrow(prenatal_miss50)
sub1 <- after_prenatal - initial_sample # -3514

# Exclude children with high missingness in the postnatal period
postnatal_miss50 <- prenatal_miss50[prenatal_miss50$post_percent_missing < 50,]
after_postnatal <- nrow(postnatal_miss50)
sub2 <- after_postnatal - after_prenatal # -3008

## Second step: excluded children with missing internalizing or { CMR scores }. 

# Exclude children with missing internalizing score

################ for ALSPAC: do separately per time point ###############

postnatal_miss50$intern_score_z=postnatal_miss50$intern_score_z.10y

intern_miss <- postnatal_miss50[!is.na(postnatal_miss50$intern_score_z),] 
after_intern <- nrow(intern_miss)
sub3 <- after_intern - after_postnatal # -5832 

intern_miss$fat_mass_z=intern_miss$fat_mass_z.10y

# Exclude children with missing { CMR score } fat mass*
cmr_miss <- intern_miss[!is.na(intern_miss$fat_mass_z),] 
after_cmr <- nrow(cmr_miss)
sub4 <- after_cmr - after_intern # -991

## Third step: exclude all twins and select the sibling with better data 

# Exclude twins
no_twins <- cmr_miss[cmr_miss$twin == 0, ]
no_twins <- no_twins[!(is.na(no_twins$twin)), ]

after_twins <- nrow(no_twins)
sub5 <- after_twins - after_cmr # -78

# # below not done for ALSPAC
# 
# # Select only one sibling (based on data availability or randomly).
# # First, I determine a list of mothers that have more than one child in the set.
# # NOTE: duplicated() is the best option I could find to determine which mother IDs
# # recur more than once (very non-elegant, tbh, but using table() gets even uglier)
# siblings_id = data.frame(no_twins$mother[duplicated(no_twins$mother)])
# # duplicated() funtion does not allow to ignore NAs so I remove them manually.
# # I also transform the numeric vector into a dataframe because of indexing problems.
# siblings_id = siblings_id[!is.na(siblings_id),]; siblings_id = data.frame(siblings_id);
# # Second, I create an empty vector to fill with the IDC of the sibling(s) with more 
# # missing items or with a randomly picked sibling in case they have the same nr of missing. 
# worse_sibling = rep(NA, dim(siblings_id)[1] + 1) # I will need the "+1" for triplets! 
# # Loop through the mother IDs I previously identified and link them to IDCs
# for (i in 1:dim(siblings_id)[1]) {
#   siblings = no_twins[no_twins$mother == siblings_id[i,1], ] # identify the couples of siblings
#   # For some reason when I run the line above 2 rows of NAs are created too, go figure. 
#   # Let's get rid of them:
#   siblings = siblings[rowSums(is.na(siblings)) != ncol(siblings), ]
#   # There is one mother with 3 siblings, let's select the "best" one and get rid of the other two
#   if (dim(siblings)[1] > 2) {
#     nmiss = c( sum(is.na(siblings[1,])), sum(is.na(siblings[2,])), sum(is.na(siblings[3,])) )
#     if (which.min(nmiss) == 1) { worse_sibling[i] = siblings[2,'IDC']
#     worse_sibling[dim(siblings_id)[1] + 1] = siblings[3,'IDC']
#     } else if (which.min(nmiss) == 2) { worse_sibling[i] = siblings[1,'IDC']
#     worse_sibling[dim(siblings_id)[1] + 1] = siblings[3,'IDC']
#     } else { worse_sibling[i] = siblings[1,'IDC'] 
#     worse_sibling[dim(siblings_id)[1] + 1] = siblings[2,'IDC'] }
#   }
#   # otherwise, select the "worse" sibling (with more missing) and add to it the the black list
#   if ( sum(is.na(siblings[1,])) > sum(is.na(siblings[2,])) ) {
#     worse_sibling[i] = siblings[1,'IDC']
#   } else if ( sum(is.na(siblings[1,])) == sum(is.na(siblings[2,])) ) {
#     worse_sibling[i] = siblings[sample(1:2, 1),'IDC']
#   } else { worse_sibling[i] = siblings[2,'IDC'] }
# }

# Now we are finally ready to exclude siblings
#final <- no_twins[no_twins$IDC %notin% worse_sibling, ]
final <- no_twins
after_siblings <- nrow(final)
sub6 <- after_siblings - after_twins  # 0

# Flowchart
flowchart <- list(initial_sample, sub1, after_prenatal, sub2, after_postnatal, sub3, 
                  after_intern, sub4, after_cmr, sub5, after_twins, sub6, after_siblings)

# Rename final dataset:
ELS_PCM <- final
cat(paste("Well, congrats! Your final dataset includes", after_siblings ,"participants."))

################################################################################
#### ------------------- Construct RISK GROUPS variable ------------------- ####
################################################################################

# Compute groups 

ELS_PCM$int = ifelse(ELS_PCM$intern_score_z > quantile(ELS_PCM$intern_score_z, probs = 0.8), 1, 0) # 375 risk, 1644  no risk 
ELS_PCM$fat = ifelse(ELS_PCM$fat_mass_z > quantile(ELS_PCM$fat_mass_z, probs = 0.8), 1, 0) # 404 risk, 1615 no risk

ELS_PCM$risk_groups = rep(NA, after_siblings)
for (i in 1:after_siblings) {
  if (ELS_PCM$int[i] == 0 & ELS_PCM$fat[i] == 0) { ELS_PCM$risk_groups[i] = 0 # healthy
  } else if (ELS_PCM$int[i] == 1 & ELS_PCM$fat[i] == 0) { ELS_PCM$risk_groups[i] = 1 # High intenalizing  only
  } else if (ELS_PCM$int[i] == 0 & ELS_PCM$fat[i] == 1) { ELS_PCM$risk_groups[i] = 2 # High fat mass only
  } else { ELS_PCM$risk_groups[i] = 3 } # multimorbid
}

ELS_PCM$risk_groups = as.factor(ELS_PCM$risk_groups)

summary(ELS_PCM$risk_groups)  
# 0    1    2    3 
# 1340  275  304  100 

# # Let's first factor that bad boy
# imp$risk_groups = factor(groups$risk_groups,
#                          levels = c(0:3),
#                          labels = c("healthy", "internalizing_only", "cardiometabolic_only", "multimorbid"))

# #display groups
# attach(groups); plot(intern_score_z, fat_mass_z,
#                      col=c("cornflowerblue","darkgrey", "darkgoldenrod2","chartreuse4")[risk_groups]); detach(groups)
#
# #display groups in relation to stress
# attach(groups); plot(presum, postsum,
#                      col=c("cornflowerblue","darkgrey", "darkgoldenrod2","chartreuse4")[risk_groups]); detach(groups)

################################################################################
#### -------------- Construct CUMULATIVE STRESS variables ----------------- ####
################################################################################

# ask Serena: is pre_personal_stress meant to be pre_personal_stress
# and pre_interpersonal_stress = pre_interpersonal_risk?

# compute sum scores for prenatal and postnatal stress exposure
ELS_PCM$prenatal_stress <- rowSums(ELS_PCM[c("pre_life_events","pre_contextual_risk", 
                                             #"pre_personal_stress", 
                                             "pre_parental_risks",
                                             #"pre_interpersonal_stress",
                                             "pre_interpersonal_risks")], na.rm = F)

ELS_PCM$postnatal_stress <- rowSums(ELS_PCM[c("post_life_events","post_contextual_risk", 
                                              "post_parental_risk", "post_interpersonal_risk", 
                                              "post_direct_victimization")], na.rm = F)

#------------------------------------------------------------------------------#
## OPTIONAL : check out missing pattern per stress domain ##
#------------------------------------------------------------------------------#
# Let's have a look at the pattern of missing in our domains after exclusion process.

domainonly <- ELS_PCM[, c('pre_life_events', 'pre_contextual_risk',
                          #'pre_personal_stress', 'pre_interpersonal_stress',
                          'pre_parental_risks', 'pre_interpersonal_risks',
                          'post_life_events', 'post_contextual_risk', 'post_parental_risk', 'post_interpersonal_risk', 'post_direct_victimization')]
summary(domainonly)

# Heatmap of missing values together with table
missingpattern <- md.pairs(domainonly) # outputs four tables: ($rr) how many datapoints are observed
# ($rm) observed and missing, ($mr) missing versus observed and ($mm) is missing vs missing
pheatmap(as.matrix(missingpattern$mm), display_numbers = T, number_format = "%.0f")

#missingtable = md.pattern(domainonly)

#------------------------------------------------------------------------------#
# For the sake of time efficiency (and my mental health) let us reatain only those 
# variables that are useful for imputation in the final dataset. Once I am at it,
# I also order them by domain. This is important because, it turns out that mice 
# is sensitive to the order of the variables in the set (even though this may be 
# a version-specific issue)

### below still needs to be run for ALSPAC

ELS_PCM_essentials = ELS_PCM[, c('cidB2957', 
                                 # all variables for prenatal risk
                                 'partner_died_pre',	'smbd_important_died_pre',	'smbd_important_ill_pre',	'sick_or_accident_pre',	'moved_pre',	'blood_loss',	'examination',	'pregnancy_worried',	'baby_worried',	'burglary_or_car_theft_pre',	'work_problems_pre', 'abortion_pre',	'married_pre',	'twins_pre', #LE
                                 'income_reduced_pre',	'homeless_pregnancy',	'major_financial_problems_pre',	'housing_adequacy_pre',	'housing_basic_living_pre',	'housing_defects_pre',	'm_education_pre',	'unemployed_pre',	#CR
                                 'criminal_record_parent_pre',	'm_attempted_suicide_pre',	'early_pregnancy',	'm_depression_pre',	'm_anxiety_pre',	'm_interpersonal_sensitivity_pre',	#PR
                                 'divorce_pre',	'p_rejected_child_pre',	'p_went_away_pre',	'separated_pre',	'conflict_in_family_pre',	'argued_fam_friends_pre',	'conflict_family_violence_pre',	'marital_status_pregnancy',	'family_affection',	'family_size_pregnancy',	'family_problems',	'family_support',	'social_network_emotional',	'social_network_practical', # IR
                                 # all variables for postnatal risk
                                 'sick_or_accident_8wk',	'family_member_ill_8wk',	'smbd_important_ill_8wk',	'partner_died_8wk',	'smbd_important_died_8wk',	'moved_8wk',	'burglary_or_car_theft_8wk',	'work_problems_8wk',	'sick_or_accident_18m',	'family_member_ill_8m',	'smbd_important_ill_8m',	'partner_died_8m',	'smbd_important_died_8m',	'pet_died_18m',	
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
                                 'm_attempted_suicide_9y',	'm_age',	'p_age',	'm_depression_8m',	'm_depression_21m',	'm_depression_3y',	'm_depression_4y',	'm_depression_5y',	'm_depression_6y',	'm_depression_9y',	'p_depression_8m',	'p_depression_21m',	'p_depression_3y',	'p_depression_4y',	'p_depression_5y',	'p_depression_6y',	'p_depression_8y',	'p_depression_9y',	
                                 'm_anxiety_8wk',	'm_anxiety_8m',	'm_anxiety_21m',	'm_anxiety_3y',	'm_anxiety_5y',	'm_anxiety_6y',	'p_anxiety_8m',	'p_anxiety_21m',	'p_anxiety_3y',	'p_anxiety_4y',	'p_anxiety_5y',	'p_anxiety_6y',	'p_anxiety_9y',	'm_interpersonal_sensitivity',	'p_interpersonal_sensitivity', # PR
                                 'divorce_8wk',	'divorce_8m',	'divorce_21m',	'divorce_3y',	'divorce_4y',	'divorce_5y',	'divorce_6y',	'divorce_9y',	'p_rejected_child_8wk',	'p_rejected_child_8m',	'p_rejected_child_21m',	'p_rejected_child_3y',	'p_rejected_child_4y',	'p_rejected_child_5y',	'p_rejected_child_6y',	'p_rejected_child_9y',	'p_went_away_8wk',	'p_went_away_8m',	
                                 'p_went_away_21m',	'p_went_away_3y',	'p_went_away_4y',	'p_went_away_5y',	'p_went_away_6y',	'p_went_away_9y',	'conflict_in_family_8wk',	'conflict_in_family_8m',	'conflict_in_family_21m',	'conflict_in_family_3y',	'conflict_in_family_4y',	'conflict_in_family_5y',	'conflict_in_family_6y',	'conflict_in_family_9y',	'conflict_family_violence_8wk',	
                                 'conflict_family_violence_8m',	'conflict_family_violence_21m',	'conflict_family_violence_3y',	'conflict_family_violence_4y',	'conflict_family_violence_5y',	'conflict_family_violence_6y',	'conflict_family_violence_9y',	'm_new_partner_8wk',	'm_new_partner_8m',	'm_new_partner_21m',	'm_new_partner_3y',	'm_new_partner_4y',	'm_new_partner_5y',	
                                 'm_new_partner_6y',	'm_new_partner_9y',	'argued_fam_friends_8wk',	'argued_fam_friends_8m',	'argued_fam_friends_21m',	'argued_fam_friends_3y',	'argued_fam_friends_4y',	'argued_fam_friends_5y',	'argued_fam_friends_6y',	'argued_fam_friends_9y',	'separated_8wk',	'separated_8m',	'separated_21m',	'separated_3y',	'separated_4y',	'separated_5y',	
                                 'separated_6y',	'separated_9y', # IR
                                 'bullying_8y',	'physical_violence_18m',	'physical_violence_30m',	'physical_violence_3y',	'physical_violence_4y',	'physical_violence_5y',	'physical_violence_6y',	'physical_violence_9y',	'sexual_abuse_18m',	'sexual_abuse_30m',	'sexual_abuse_3y',	'sexual_abuse_4y',	'sexual_abuse_5y',	'sexual_abuse_6y',	'sexual_abuse_8y',	'p_physical_violence_8wk',	
                                 'p_physical_violence_8m',	'p_physical_violence_21m',	'p_physical_violence_3y',	'p_physical_violence_4y',	'p_physical_violence_5y',	'p_physical_violence_6y',	'p_physical_violence_9y',	'm_physical_violence_8m',	'm_physical_violence_21m',	'm_physical_violence_3y',	'm_physical_violence_4y',	'm_physical_violence_5y',	'm_physical_violence_6y',	
                                 'm_physical_violence_9y',	'p_cruelty_emotional_8wk',	'p_cruelty_emotional_8m',	'p_cruelty_emotional_21m',	'p_cruelty_emotional_3y',	'p_cruelty_emotional_4y',	'p_cruelty_emotional_5y',	'p_cruelty_emotional_6y',	'p_cruelty_emotional_9y',	'm_cruelty_emotional_21m',	'm_cruelty_emotional_3y',	'm_cruelty_emotional_4y',	'm_cruelty_emotional_5y',	
                                 'm_cruelty_emotional_6y',	'm_cruelty_emotional_9y', # DV
                                 # all domain scores
                                 'pre_life_events', 'pre_contextual_risk', 'pre_parental_risks', 'pre_interpersonal_risks', 
                                 'post_life_events', 'post_contextual_risk', 'post_parental_risk', 'post_interpersonal_risk', 'post_direct_victimization',
                                 # cumulative prenatal and postnatal stress exposure
                                 'prenatal_stress', 'postnatal_stress', 
                                 # outcome variables and covariates
                                 'intern_score_z.10y', 'intern_score_z.13y', 'intern_score_z.15y', 'intern_score_z.17y', 'intern_score_z.22y', 
                                 'fat_mass_z.10y', 'fat_mass_z.13y', 'fat_mass_z.15y', 'fat_mass_z.17y', 'fat_mass_z.24y', 
                                 'risk_groups',
                                 'sex', 'age_child.10y', 'age_child.13y', 'age_child.15y', 'age_child.17y', 'age_child.23y',
                                 'm_bmi_before_pregnancy', 'm_smoking', 'm_drinking',
                                 # additional auxiliary variables for imputation
                                 'ethnicity', 'parity', 'gest_age_birth', 'gest_weight', # 'm_bmi_pregnancy', 'm_bmi_5yrs'
                                 'm_depression_pre', 'm_depression_8m', 'm_depression_21m', 'm_depression_3y', 'm_depression_4y',
                                 'm_depression_5y', 'm_depression_6y', 'm_depression_9y', 
                                 'p_depression_8m', 'p_depression_21m', 'p_depression_3y', 
                                 'm_depression_4y','m_depression_5y', 'p_depression_6y', 
                                 'p_depression_8y', 'p_depression_9y',
                                 'm_age_cont')]

                              
#------------------------------------------------------------------------------#
## OPTIONAL : check out the dataset ##
#------------------------------------------------------------------------------#
# # Examine missing frequency per variable 
# percent_missing <- function(var) { sum(is.na(var)) / length(var) * 100 }
# missing_frequency = apply(ELS_PCM_essentials, 2, percent_missing)
# print(sort(missing_frequency, decreasing = T))
# 
# # Examine correlations 
# test_essentials = subset(ELS_PCM_essentials[, 2:113], select=-c(parent_died, p_age))
# correlations = round(cor(test_essentials, use = "complete.obs"),2)
# pheatmap(correlations, cluster_rows = F, cluster_cols = F)

################################################################################
#### --------------------------- save and run ----------------------------- ####
################################################################################

# Save the dataset in an .rds file, in the directory where the raw data are stored
save(ELS_PCM_essentials, file = 'ELSPCM_dataset.RData')

# Saving all (not just essentials)
# save(ELS_PCM, file ="ELSPCM_all.RData")



