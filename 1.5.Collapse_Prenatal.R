

####################################################################################################################################################


# COLLAPSING VARIABLES - PRENATAL 

# This script is used to collapse items of a similar type into a smaller number of variables (the collapsing is done across type, NOT time) 

####################################################################################################################################################

library(tidyverse)

#load('alspac.table.Rdata')

#make sure to load repmeas function from SereDef GitHub 'functions and setup'

# LIFE EVENTS

#partner_died_pre
alspac.table$partner_died_pre	<- alspac.table$b570a_rec	# PTNR died since PREG

# smbd_important_died_pre
alspac.table$smbd_important_died_pre <- repmeas(alspac.table[,c('b571a_rec','b572a_rec')]) # CH died since PREG, Friend or relative died since PREG 

# family_member_ill_pre
alspac.table$family_member_ill_pre <-	repmeas(alspac.table[,c('b573a_rec', 'b574a_rec')])	# CH was ill since PREG, PTNR was ill since PREG

# smbd_important_ill_pre
alspac.table$smbd_important_ill_pre	<- alspac.table$b575a_rec	# Friend or relative was ill since PREG


# sick_or_accident_pre
alspac.table$sick_or_accident_pre	<- repmeas(alspac.table[,c('b576a_rec', 'b580a_rec', 'b610a_rec')]) # Admitted to hospital since PREG, V ill since PREG, Had an accident since PREG

# moved_pre
alspac.table$moved_pre <-	alspac.table$b591a_rec	# Moved house since PREG

# blood_loss
alspac.table$blood_loss <- alspac.table$b599a_rec	# Bled & thought might miscarry

# examination
alspac.table$examination	<- alspac.table$b601a_rec	# Test to see if baby abnormal

# pregnancy_worried
alspac.table$pregnancy_worried <-	alspac.table$b602a_rec # Test result suggesting POSS abnormality

# baby_worried
alspac.table$baby_worried	<- alspac.table$b604a_rec	# POSS harm to baby

# burglary_or_car_theft_pre
alspac.table$burglary_or_car_theft_pre <-	alspac.table$b609a_rec	# House or car burgled since PREG

# work_problems_pre
alspac.table$work_problems_pre <- alspac.table$b583a_rec	# PROBS at work since PREG

# abortion_pre
alspac.table$abortion_pre	<- alspac.table$b605a_rec # Tried to have abortion

# married_pre
alspac.table$married_pre <-	alspac.table$b595a_rec	# Got married since PREG

# twins_pre
alspac.table$twins_pre <-	alspac.table$b603a_rec	# Told having twins


####################################################################################################################################################


# COLLAPSING VARIABLES 

# CONTEXTUAL RISKS

#income_reduced_pre
alspac.table$income_reduced_pre <-	repmeas(alspac.table[,c('b588a_rec', 'b581a_rec')]) #	Income reduced since PREG, PTNR lost job since PREG

#homeless_pregnancy
alspac.table$homeless_pregnancy	<- alspac.table$b593	# Became homeless since PREG

#major_financial_problems_pre
alspac.table$major_financial_problems_pre <-	alspac.table$b594a_rec # Major financial PROB since PREG

# housing_adequacy_pre
alspac.table$housing_adequacy_pre	<- alspac.table$p2 # Housing adequacy 

# housing_basic_living_pre
alspac.table$housing_basic_living_pre	<- alspac.table$p3	# Housing Basic Living 

# housing_defects_pre
alspac.table$housing_defects_pre	<- alspac.table$p4	# Housing Defects 

# m_education_pre
table(alspac.table$c645a, exclude = NULL)  # Mums highest ed qualification

alspac.table$m_education_pre <- ifelse(alspac.table$c645a %in% c("CSE", "Vocational", "O level", "A level"), 1, ifelse(alspac.table$c645a == "Degree", 0, NA))


# unemployed_pre
alspac.table$unemployed_pre	<- alspac.table$b584a_rec	# Lost job since PREG

####################################################################################################################################################


# COLLAPSING VARIABLES 

# PARENTAL RISKS

#criminal_record_parent_pre
alspac.table$criminal_record_parent_pre <- repmeas(alspac.table[,c('b577a_rec', 'b598', 'p14')]) #	In trouble with the law since PREG, Convicted of an offence since PREG, Crime trouble with police

#m_attempted_suicide_pre
alspac.table$m_attempted_suicide_pre <-	alspac.table$b597 # Attempted suicide since PREG

# early_pregnancy = m_age in postnatal script
# Because mz028b is a factor, we use as.character before as.numeric.
# Factors are stored internally as integers with a table to give the factor level labels.
alspac.table$mz028bn <- as.numeric(as.character(alspac.table$mz028b))
alspac.table$early_pregnancy <- ifelse(alspac.table$mz028bn < 19, yes = 1, no = 0) # early parenthood 
# mother age younger than 19 at baseline based on Cecil et al. (2014); Rijlaarsdam et al. (2016)

# m_depression_pregnancy
#alspac.table$m_depression_pre	<- alspac.table$p12 # maternal psychopathology
alspac.table$m_depression_pre <- alspac.table$b371a_rec # EPDS (>=13 risk, <13 no risk)
#In previous lit, a total score of 13 or more is considered a flag for the need for follow up of possible depressive symptoms.


# m_anxiety_pre
alspac.table$m_anxiety_pre	<- repmeas(alspac.table[,c('b351a_rec', 'c573a_rec')]) # CCEI anxiety subscale (complete) 18w gest, 32w gest 
# (this the only collapse across time exception due to it being the only variable from the prenatal score measured twice in pregnancy)
# cor(alspac.table$b351a_rec, alspac.table$c573a_rec, use='complete.obs') # 0.4625

# m_interpersonal_sensitivity, 80th percentile (Interpersonal awareness score)

# Higher scores = greater interpersonal sensitivity, based on items such as 'I avoid saying what I think for fear of being rejected'

# checking the distribution, since normally distributed, using 80th percentile
plot(alspac.table$b916) 

# changing a factor to numeric without changing values 
alspac.table$b916n <- as.numeric(levels(alspac.table$b916))[alspac.table$b916] 
quantile(alspac.table$b916n, .8, na.rm = T) # 80th percentile is 22

alspac.table$m_interpersonal_sensitivity_pre <- ifelse(alspac.table$b916n >= 22, 1, 
                                                   ifelse(alspac.table$b916n < 22, 0, NA)) 

# checking if recoding worked
data.frame(alspac.table$m_interpersonal_sensitivity_pre, alspac.table$b916) # looks good

####################################################################################################################################################

# COLLAPSING VARIABLES 

# INTERPERSONAL RISKS

#divorce_pregnancy
alspac.table$divorce_pre <-	alspac.table$b578 # Divorced since PREG

#p_rejected_child_pre
alspac.table$p_rejected_child_pre <- alspac.table$b579a_rec	# PTNR rejected PREG

#p_went_away_pre
alspac.table$p_went_away_pre	<-  alspac.table$b585a_rec #	PTNR went away since PREG

#separated_pre
alspac.table$separated_pre	<- alspac.table$b587	# Separated since PREG

#conflict_in_family_pre
alspac.table$conflict_in_family_pre	<- repmeas(alspac.table[,c('b589a_rec', 'b607a_rec', 'b608')])	# Argued with PTNR since PREG, PTNR was EMOT cruel to mum since PREG, PTNR was EMOT cruel to child since PREG

#argued_fam_friends_pre
alspac.table$argued_fam_friends_pre	<- alspac.table$b590a_rec	# Argued with family or friends since PREG

#conflict_family_violence_pre
alspac.table$conflict_family_violence_pre	<- repmeas(alspac.table[,c('b592a_rec', 'b596a_rec')])	# PTNR hurt mum since PREG, PTNR hurt CH since PREG

alspac.table$marital_status_pregnancy <-	alspac.table$p7	# Partner Status 

alspac.table$family_affection	<- alspac.table$p8	# Partner Affection 

alspac.table$family_size_pregnancy <-	alspac.table$p10	# Family Size 


alspac.table$family_problems	<- alspac.table$p11	 # Family Major problems 

alspac.table$family_support	<- alspac.table$p16	# Partner Support 

alspac.table$social_network_emotional	<- alspac.table$p17	# Social Network - Emotional 

alspac.table$social_network_practical	<- alspac.table$p18	 # Social Network - Practical 


####################################################################################################################################################

