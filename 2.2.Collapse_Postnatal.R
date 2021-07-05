
####################################################################################################################################################

# COLLAPSING VARIABLES

library(tidyverse)

#load('alspac.table.Rdata')
#make sure to load repmeas function from SereDef GitHub 'functions and setup'

####################################################################################################################################################

# LIFE EVENTS

# SECTION 1

# sick_or_accident_8wk
temp <- rowMeans(alspac.table[,c('e403a_rec','e431a_rec','e432a_rec')], na.rm=T) # Ch ill, Test for CH abnormality,Test suggested CH PROB.
temp[temp > 0] <- 1 
alspac.table$sick_or_accident_8wk <- temp 

#seems to work but why correlations are so low?

# viewing newly collapsed variable side to side with input variables
alspac.table[,c('e403a_rec','e431a_rec','e432a_rec', 'sick_or_accident_8wk')] 
cor(alspac.table$e432a_rec, alspac.table$sick_or_accident_8wk, use='complete.obs')# 0.240
cor(alspac.table$e403a_rec, alspac.table$sick_or_accident_8wk, use='complete.obs') # 0.687

# sick_or_accident_18m
alspac.table$sick_or_accident_18m <- repmeas(alspac.table[,c('f223a_rec', 'kd510a_rec')]) # Ch ill 8m, Ch admitted to hospital 18m

# sick_or_accident_30m
alspac.table$sick_or_accident_30m <- repmeas(alspac.table[,c('g303a_rec', 'kf460a_rec')])  # Ch ill 21m, Ch admitted to hospital 30m

# sick_or_accident_3y
alspac.table$sick_or_accident_3y <- repmeas(alspac.table[,c('h213a_rec', 'kj470a_rec')]) # Ch ill 3y, Ch admitted to hospital 3y

# sick_or_accident_4y
alspac.table$sick_or_accident_4y <- repmeas(alspac.table[,c('j303a_rec', 'kl480a_rec')]) # Ch ill, Ch admitted to hospital 

# sick_or_accident_5y
alspac.table$sick_or_accident_5y <- repmeas(alspac.table[,c('k4003a_rec', 'kn4010a_rec')]) # Ch ill, Ch admitted to hospital 

# sick_or_accident_6y
alspac.table$sick_or_accident_6y <- repmeas(alspac.table[,c('l4003a_rec', 'kq371a_rec')]) # Ch ill, Ch admitted to hospital 

# sick_or_accident_9y
alspac.table$sick_or_accident_9y <- repmeas(alspac.table[,c('p2003_rec', 'kt5011a_rec')]) # Ch ill, Ch admitted to hospital 

####################################################################################################################################################

# VARIABLE SECTION 2

# family_member_ill_8wk
alspac.table$family_member_ill_8wk <- repmeas(alspac.table[,c('e404a_rec', 'e406a_rec', 'e410a_rec', 'e440a_rec')]) #PTNR ill, Admitted to HOSP, Mum very ill, mum accident

# family_member_ill_8m
alspac.table$family_member_ill_8m <- repmeas(alspac.table[,c('f224a_rec', 'f226a_rec', 'f230a_rec')]) #PTNR ill, Admitted to HOSP, Mum very ill 

# family_member_ill_21m
alspac.table$family_member_ill_21m <- repmeas(alspac.table[,c('g304a_rec', 'g306a_rec', 'g310a_rec', 'g342a_rec')]) #PTNR ill, Admitted to HOSP, Mum very ill,mum accident 

# family_member_ill_3y
alspac.table$family_member_ill_3y <- repmeas(alspac.table[,c('h214a_rec', 'h216a_rec', 'h220a_rec', 'h252a_rec')]) #PTNR ill, Admitted to HOSP, Mum very ill, mum accident

# family_member_ill_4y
alspac.table$family_member_ill_4y <- repmeas(alspac.table[,c('j304a_rec', 'j306a_rec', 'j310a_rec', 'j342a_rec')]) #PTNR ill, Admitted to HOSP, Mum very ill, mum accident

# family_member_ill_5y
alspac.table$family_member_ill_5y <- repmeas(alspac.table[,c('k4004a_rec', 'k4006a_rec', 'k4010a_rec', 'k4042a_rec')]) #PTNR ill, Admitted to HOSP, Mum very ill, mum accident 

# family_member_ill_6y
alspac.table$family_member_ill_6y <- repmeas(alspac.table[,c('l4004a_rec', 'l4006a_rec', 'l4010a_rec', 'l4044a_rec')]) #PTNR ill, Admitted to HOSP, Mum very ill,  mum accident

# family_member_ill_9y
alspac.table$family_member_ill_9y <- repmeas(alspac.table[,c('p2004_rec', 'p2006_rec', 'p2010_rec', 'p2044_rec')]) #PTNR ill, Admitted to HOSP, Mum very ill,  mum accident

####################################################################################################################################################

# VARIABLE SECTION 3

# smbd_important_died_8wk
alspac.table$smbd_important_died_8wk <- repmeas(alspac.table[,c('e401a_rec', 'e402a_rec')]) # CH died, FRD or REL died

# smbd_important_died_8m
alspac.table$smbd_important_died_8m <- repmeas(alspac.table[,c('f221a_rec', 'f222a_rec')]) # CH died, FRD or REL died

# smbd_important_died_21m
alspac.table$smbd_important_died_21m <- repmeas(alspac.table[,c('g301a_rec', 'g302a_rec')]) # CH died, FRD or REL died

# smbd_important_died_3y
alspac.table$smbd_important_died_3y <- repmeas(alspac.table[,c('h211a_rec', 'h212a_rec')]) # CH died, FRD or REL died

# smbd_important_died_4y
alspac.table$smbd_important_died_4y <- repmeas(alspac.table[,c('j301a_rec', 'j302a_rec')]) # CH died, FRD or REL died

# smbd_important_died_5y
alspac.table$smbd_important_died_5y <- repmeas(alspac.table[,c('k4001a_rec', 'k4002a_rec')]) # CH died, FRD or REL died

# smbd_important_died_6y
alspac.table$smbd_important_died_6y <- repmeas(alspac.table[,c('l4001a_rec', 'l4002a_rec', 'kq366a_rec')]) # CH died, FRD or REL died, someone in the family died

# smbd_important_died_9y
alspac.table$smbd_important_died_9y <- repmeas(alspac.table[,c('p2001_rec', 'p2002_rec', 'kt5006a_rec')]) # CH died, FRD or REL died, someone in the family died (8y)

####################################################################################################################################################

# VARIABLE SECTION 4

alspac.table$separated_from_parent_18m <- repmeas(alspac.table[,c('kd500a_rec', 'kd506a_rec', 'kd507a_rec')]) 
# Ch taken into care, Ch separated from mum for > a wk, Ch separated from dad for > a wk

alspac.table$separated_from_parent_30m<- repmeas(alspac.table[,c('kf450a_rec', 'kf456a_rec', 'kf457a_rec')]) 
# Ch taken into care, Ch separated from mum for > a wk, Ch separated from dad for > a wk

alspac.table$separated_from_parent_3y<- repmeas(alspac.table[,c('kj460a_rec', 'kj466a_rec', 'kj467a_rec')]) 
# Ch taken into care, Ch separated from mum for > a wk, Ch separated from dad for > a wk

alspac.table$separated_from_parent_4y<- repmeas(alspac.table[,c('kl470a_rec', 'kl476a_rec', 'kl477a_rec')]) 
# Ch taken into care, Ch separated from mum for > a wk, Ch separated from dad for > a wk

alspac.table$separated_from_parent_5y<- repmeas(alspac.table[,c('kn4000a_rec', 'kn4006a_rec', 'kn4007a_rec')]) 
# Ch taken into care, Ch separated from mum for > a wk, Ch separated from dad for > a wk

alspac.table$separated_from_parent_6y<- repmeas(alspac.table[,c('kq360a_rec', 'kq367a_rec', 'kq368a_rec')]) 
# Ch taken into care, Ch separated from mum for > a wk, Ch separated from dad for > a wk 

alspac.table$separated_from_parent_8y <- repmeas(alspac.table[,c('kt5000a_rec', 'kt5007a_rec', 'kt5008a_rec')]) 
# Ch taken into care, Ch separated from mum for > a wk, Ch separated from dad for > a wk 


####################################################################################################################################################

# VARIABLE SECTION 5

# started_nursery

alspac.table$started_nursery_18m 	<- alspac.table$kd513a_rec	# CH started nursery
alspac.table$started_nursery_30m <- alspac.table$kf463a_rec	# CH started nursery
alspac.table$started_nursery_3y	<- alspac.table$kj473a_rec	# CH started nursery
alspac.table$started_nursery_4y <- alspac.table$kl483a_rec # CH started nursery
alspac.table$started_nursery_5y	<- alspac.table$kn4013a_rec	# CH started nursery


# acquired_new_parent

alspac.table$acquired_new_parent_18m	<- alspac.table$kd508a_rec # CH Acquired New Parent 
alspac.table$acquired_new_parent_30m	<- alspac.table$kf458a_rec # CH Acquired New Parent 
alspac.table$acquired_new_parent_3y <- alspac.table$kj468a_rec # CH Acquired New Parent 
alspac.table$acquired_new_parent_4y <- alspac.table$kl478a_rec # CH Acquired New Parent 
alspac.table$acquired_new_parent_5y	<- alspac.table$kn4008a_rec # CH Acquired New Parent 
alspac.table$acquired_new_parent_6y <- alspac.table$kq369a_rec # CH Acquired New Parent 
alspac.table$acquired_new_parent_8y <- alspac.table$kt5009a_rec # CH Acquired New Parent 

# change_carer

alspac.table$change_carer_18m <- alspac.table$kd511a_rec # Ch changed carer 
alspac.table$change_carer_30m <- alspac.table$kf461a_rec # Ch changed carer 
alspac.table$change_carer_3y	<- alspac.table$kj471a_rec # Ch changed carer 
alspac.table$change_carer_4y	<- alspac.table$kl481a_rec # Ch changed carer 
alspac.table$change_carer_5y	<- alspac.table$kn4011a_rec # Ch changed carer 
alspac.table$change_carer_6y	<- alspac.table$kq372a_rec # Ch changed carer 
alspac.table$change_carer_8y	<- alspac.table$kt5012a_rec # Ch changed carer 


####################################################################################################################################################

# VARIABLE SECTION 6

# moved

alspac.table$moved_8wk <- alspac.table$e421a_rec  # mum moved house 8wk

alspac.table$moved_18m <- repmeas(alspac.table[,c('f241a_rec', 'kd502a_rec')]) # mum moved house 8m, CH moved house 18m

alspac.table$moved_30m <- repmeas(alspac.table[,c('g321a_rec', 'kf452a_rec')]) # mum moved hous 21m, CH moved house 30m

alspac.table$moved_3y <- repmeas(alspac.table[,c('h231a_rec', 'kj462a_rec')]) # mum moved hous, CH moved house

alspac.table$moved_4y <- repmeas(alspac.table[,c('j321a_rec', 'kl472a_rec')]) # mum moved hous, CH moved house

alspac.table$moved_5y <- repmeas(alspac.table[,c('k4021a_rec', 'kn4002a_rec')]) # mum moved hous, CH moved house

alspac.table$moved_6y <- repmeas(alspac.table[,c('l4021a_rec', 'kq362a_rec')]) # mum moved hous, CH moved house

alspac.table$moved_9y <- repmeas(alspac.table[,c('p2021_rec', 'kt5002a_rec')]) # mum moved hous 9y, CH moved house 8y

####################################################################################################################################################

# VARIABLE SECTION 7

# pet_die

alspac.table$pet_died_18m <- repmeas(alspac.table[,c('f261a_rec', 'kd501a_rec')]) # mum pet died 8m, CH pet died 18m

alspac.table$pet_died_30m <- repmeas(alspac.table[,c('g341a_rec', 'kf451a_rec')]) # mum pet died, CH pet died 

alspac.table$pet_died_3y <- repmeas(alspac.table[,c('h251a_rec', 'kj461a_rec')]) # mum pet died, CH pet died 

alspac.table$pet_died_4y <- repmeas(alspac.table[,c('j341a_rec', 'kl471a_rec')]) # mum pet died, CH pet died 

alspac.table$pet_died_5y <- repmeas(alspac.table[,c('k4041a_rec', 'kn4001a_rec')]) # mum pet died, CH pet died 

alspac.table$pet_died_6y <- repmeas(alspac.table[,c('l4043a_rec', 'kq361a_rec')]) # mum pet died, CH pet died 

alspac.table$pet_died_9y<- repmeas(alspac.table[,c('p2043_rec', 'kt5001a_rec')]) # mum pet died, CH pet died 


####################################################################################################################################################

# VARIABLE SECTION 8 - renaming to match with GenR


# smbd_important_ill = Friend or REL ill
alspac.table$smbd_important_ill_8wk <- alspac.table$e405a_rec
alspac.table$smbd_important_ill_8m <- alspac.table$f225a_rec
alspac.table$smbd_important_ill_21m <- alspac.table$g305a_rec
alspac.table$smbd_important_ill_3y <- alspac.table$h215a_rec
alspac.table$smbd_important_ill_4y <- alspac.table$j305a_rec
alspac.table$smbd_important_ill_5y <- alspac.table$k4005a_rec
alspac.table$smbd_important_ill_6y <- alspac.table$l4005a_rec
alspac.table$smbd_important_ill_9y <- alspac.table$p2005_rec	


# partner_died (instead of 'parent_died' in GenR, we have no mother died item) 
alspac.table$partner_died_8wk <- alspac.table$e400a_rec
alspac.table$partner_died_8m <- alspac.table$f220a_rec
alspac.table$partner_died_21m <- alspac.table$g300a_rec
alspac.table$partner_died_3y <- alspac.table$h210a_rec 
alspac.table$partner_died_4y <- alspac.table$j300a_rec
alspac.table$partner_died_5y <- alspac.table$k4000a_rec
alspac.table$partner_died_6y <- alspac.table$l4000a_rec
alspac.table$partner_died_9y <- alspac.table$p2000_rec	


# burglary_or_car_theft = House burglary/car theft
alspac.table$burglary_or_car_theft_8wk <- alspac.table$e439a_rec
alspac.table$burglary_or_car_theft_21m <- alspac.table$g339a_rec
alspac.table$burglary_or_car_theft_3y <- alspac.table$h249a_rec
alspac.table$burglary_or_car_theft_4y <- alspac.table$j339a_rec
alspac.table$burglary_or_car_theft_5y <- alspac.table$k4039a_rec
alspac.table$burglary_or_car_theft_6y <- alspac.table$l4039a_rec
alspac.table$burglary_or_car_theft_9y <- alspac.table$p2039_rec	


# separated_from_smbd = Ch separated from someone else
alspac.table$separated_from_smbd_18m <- alspac.table$kd512a_rec
alspac.table$separated_from_smbd_30m <- alspac.table$kf462a_rec
alspac.table$separated_from_smbd_3y <- alspac.table$kj472a_rec
alspac.table$separated_from_smbd_4y <- alspac.table$kl482a_rec
alspac.table$separated_from_smbd_5y <- alspac.table$kn4012a_rec
alspac.table$separated_from_smbd_6y <- alspac.table$kq373a_rec
alspac.table$separated_from_smbd_8y <- alspac.table$kt5013a_rec	

# Child lost best friend (lost_smth_important in GenR)
alspac.table$lost_best_friend_8y <- alspac.table$kt5015a_rec	


# m_pregnant = Mum became pregnant
alspac.table$m_pregnant_8m <- alspac.table$f250a_rec
alspac.table$m_pregnant_21m <- alspac.table$g330a_rec
alspac.table$m_pregnant_3y <- alspac.table$h240a_rec
alspac.table$m_pregnant_4y <- alspac.table$j330a_rec
alspac.table$m_pregnant_5y <- alspac.table$k4030a_rec
alspac.table$m_pregnant_6y <- alspac.table$l4030a_rec
alspac.table$m_pregnant_9y <- alspac.table$p2030_rec	


# new_sibling  = Ch had a new sibling
alspac.table$new_sibling_18m <- alspac.table$kd509a_rec
alspac.table$new_sibling_30m <- alspac.table$kf459a_rec
alspac.table$new_sibling_3y <- alspac.table$kj469a_rec
alspac.table$new_sibling_4y <- alspac.table$kl479a_rec
alspac.table$new_sibling_5y <- alspac.table$kn4009a_rec
alspac.table$new_sibling_6y <- alspac.table$kq370a_rec
alspac.table$new_sibling_8y <- alspac.table$kt5010a_rec

#  ch_had_fright = Ch had fright 
alspac.table$ch_had_fright_18m <-	alspac.table$kd503a_rec
alspac.table$ch_had_fright_30m <-	alspac.table$kf453a_rec
alspac.table$ch_had_fright_3y <-	alspac.table$kj463a_rec
alspac.table$ch_had_fright_4y <-	alspac.table$kl473a_rec
alspac.table$ch_had_fright_5y <-	alspac.table$kn4003a_rec
alspac.table$ch_had_fright_6y <-	alspac.table$kq363a_rec
alspac.table$ch_had_fright_8y <-	alspac.table$kt5003a_rec

####################################################################################################################################################

# work_problems


alspac.table$work_problems_8wk <- repmeas(alspac.table[,c('e412a_rec', 'e413a_rec')]) # PTNR had PROBS at work, PROBS at work mum

alspac.table$work_problems_8m <- repmeas(alspac.table[,c('f232a_rec', 'f233a_rec')]) # PTNR had PROBS at work, PROBS at work mum

alspac.table$work_problems_21m <- repmeas(alspac.table[,c('g312a_rec', 'g313a_rec')]) # PTNR had PROBS at work, PROBS at work mum

alspac.table$work_problems_3y <- repmeas(alspac.table[,c('h222a_rec', 'h223a_rec')]) # PTNR had PROBS at work, PROBS at work mum

alspac.table$work_problems_4y <- repmeas(alspac.table[,c('j312a_rec', 'j313a_rec')]) # PTNR had PROBS at work, PROBS at work mum

alspac.table$work_problems_5y <- repmeas(alspac.table[,c('k4012a_rec', 'k4013a_rec')]) # PTNR had PROBS at work, PROBS at work mum

alspac.table$work_problems_6y <- repmeas(alspac.table[,c('l4012a_rec', 'l4013a_rec')]) # PTNR had PROBS at work, PROBS at work mum

alspac.table$work_problems_9y <- repmeas(alspac.table[,c('p2012_rec', 'p2013_rec')]) # PTNR had PROBS at work, PROBS at work mum

####################################################################################################################################################

# unemployed

alspac.table$unemployed_8wk <- repmeas(alspac.table[,c('e411a_rec', 'e414a_rec')]) # unemployed	PTNR lost job, unemployed	Lost job

alspac.table$unemployed_8m <- repmeas(alspac.table[,c('f231a_rec', 'f234a_rec')]) # unemployed	PTNR lost job, unemployed	Lost job

alspac.table$unemployed_21m <- repmeas(alspac.table[,c('g311a_rec', 'g314a_rec')]) # unemployed	PTNR lost job, unemployed	Lost job

alspac.table$unemployed_3y <- repmeas(alspac.table[,c('h221a_rec', 'h224a_rec')]) # unemployed	PTNR lost job, unemployed	Lost job

alspac.table$unemployed_4y <- repmeas(alspac.table[,c('j311a_rec', 'j314a_rec')]) # unemployed	PTNR lost job, unemployed	Lost job

alspac.table$unemployed_5y <- repmeas(alspac.table[,c('k4011a_rec', 'k4014a_rec')]) # unemployed	PTNR lost job, unemployed	Lost job

alspac.table$unemployed_6y <- repmeas(alspac.table[,c('l4011a_rec', 'l4014a_rec')]) # unemployed	PTNR lost job, unemployed	Lost job

alspac.table$unemployed_9y <- repmeas(alspac.table[,c('p2011_rec', 'p2014_rec')]) # unemployed	PTNR lost job, unemployed	Lost job

####################################################################################################################################################


####################################################################################################################################################

# COLLAPSING VARIABLES 

# CONTEXTUAL RISKS

# SECTION 1

# material_deprivation 0-2y composite, 2-4y composite

# I am separating the collapsed items below into separate variables to align with prenatal script
#alspac.table$material_deprivation_2y <- repmeas(alspac.table[,c("b2", "b3", "b4")]) # Housing adequacy, Housing Basic Living, Housing Defects
#alspac.table$material_deprivation_4y <- repmeas(alspac.table[,c("t2", "t3", "t4")]) # Housing adequacy, Housing Basic Living, Housing Defects

# Housing adequacy, Housing Basic Living, Housing Defects
alspac.table$housing_adequacy_2y <- ifelse(alspac.table$b2 == 1, 1,
                                           ifelse(alspac.table$b2 == 0, 0, NA)) #	Housing adequacy 0-2y composite

alspac.table$housing_adequacy_4y <-	ifelse(alspac.table$t2 == 1, 1,
                                           ifelse(alspac.table$t2 == 0, 0, NA)) # Housing adequacy 2-4y composite

alspac.table$housing_basic_living_2y <-	ifelse(alspac.table$b3 == 1, 1,
                                               ifelse(alspac.table$b3 == 0, 0, NA)) # Housing Basic Living 0-2y  composite

alspac.table$housing_basic_living_4y <-	ifelse(alspac.table$t3 == 1, 1,
                                               ifelse(alspac.table$t3 == 0, 0, NA)) #  Housing Basic Living 2-4y composite

alspac.table$housing_defects_2y	<- ifelse(alspac.table$b4 == 1, 1,
                                          ifelse(alspac.table$b4 == 0, 0, NA)) # Housing Defects 0-2y composite

alspac.table$housing_defects_4y	<-  ifelse(alspac.table$t4 == 1, 1,
                                           ifelse(alspac.table$t4 == 0, 0, NA)) # Housing Defects 2-4y composite

####################################################################################################################################################


# SECTION 2

# renaming to match GenR

# homeless_childhood = Became homeless
alspac.table$homeless_childhood_8wk <- alspac.table$e423a_rec
alspac.table$homeless_childhood_8m <- alspac.table$f243a_rec
alspac.table$homeless_childhood_21m <- alspac.table$g323a_rec 
alspac.table$homeless_childhood_3y <- alspac.table$h233a_rec 
alspac.table$homeless_childhood_4y <- alspac.table$j323a_rec
alspac.table$homeless_childhood_5y <- alspac.table$k4023a_rec
alspac.table$homeless_childhood_6y <- alspac.table$l4023a_rec
alspac.table$homeless_childhood_9y <- alspac.table$p2023_rec	

# major_financial_problems (trouble_pay_chidlhood in GenR)
alspac.table$major_financial_problems_8wk <- alspac.table$e424a_rec #  Major financial problems
alspac.table$major_financial_problems_8m <- alspac.table$f244a_rec #  Major financial problems
alspac.table$major_financial_problems_21m <- alspac.table$g324a_rec #  Major financial problems
alspac.table$major_financial_problems_3y <- alspac.table$h234a_rec #  Major financial problems
alspac.table$major_financial_problems_4y <- alspac.table$j324a_rec #  Major financial problems
alspac.table$major_financial_problems_5y <- alspac.table$k4024a_rec #  Major financial problems
alspac.table$major_financial_problems_6y <- alspac.table$l4024a_rec #  Major financial problems
alspac.table$major_financial_problems_9y <- alspac.table$p2024_rec #  Major financial problems

# income_reduced

alspac.table$income_reduced_8wk <- alspac.table$e418a_rec 
alspac.table$income_reduced_8m <- alspac.table$f238a_rec 
alspac.table$income_reduced_21m <- alspac.table$g318a_rec 
alspac.table$income_reduced_3y <- alspac.table$h228a_rec
alspac.table$income_reduced_4y <- alspac.table$j318a_rec 
alspac.table$income_reduced_5y <- alspac.table$k4018a_rec 
alspac.table$income_reduced_6y <- alspac.table$l4018a_rec 
alspac.table$income_reduced_9y <- alspac.table$p2018_rec 

####################################################################################################################################################

# SECTION 3

# m_education 

#alspac.table$m_education_2y <- alspac.table$b5 (using the c645a variable below instead)
#alspac.table$m_education_4y <- alspac.table$t5 

table(alspac.table$c645a, exclude = NULL)  # Mums highest ed qualification

alspac.table$m_education <- ifelse(alspac.table$c645a %in% c("CSE", "Vocational", "O level", "A level"), 1, ifelse(alspac.table$c645a == "Degree", 0, NA))

# p_education

table(alspac.table$c666, exclude = NULL) # Partner highest ed qualification

alspac.table$p_education <- ifelse(alspac.table$c666 %in% c("None", "CSE", "Vocational", "O level", "A level"), 1, ifelse(alspac.table$c666 == "Degree", 0, NA))

####################################################################################################################################################

# SECTION 4

# neighbourhood_problems

# higher score = worse neighbourhood problems


# g496
plot(alspac.table$g496)

# changing from factor to numeric
alspac.table$g496n <- as.numeric(levels(alspac.table$g496))[alspac.table$g496] 

# finding the 80th percentile
quantile(alspac.table$g496n, .8, na.rm = T) # 80th percentile is 6

alspac.table$neighbourhood_problems_21m <- ifelse(alspac.table$g496n >= 6, 1, 
                                              ifelse(alspac.table$g496n < 6, 0, NA)) 

# checking if recoding worked
data.frame(alspac.table$neighbourhood_problems_21m, alspac.table$g496) # looks good


# h366

plot(alspac.table$h366) # Neighbourhood stress score

# changing from factor to numeric
alspac.table$h366n <- as.numeric(levels(alspac.table$h366))[alspac.table$h366] 

# finding the 80th percentile
quantile(alspac.table$h366n, .8, na.rm = T) # 80th percentile is 6

alspac.table$neighbourhood_problems_3y <- ifelse(alspac.table$h366n >= 6, 1, 
                                                   ifelse(alspac.table$h366n < 6, 0, NA)) 

# checking if recoding worked
data.frame(alspac.table$neighbourhood_problems_3y, alspac.table$h366) # looks good


####################################################################################################################################################

# COLLAPSING VARIABLES 

# PARENTAL RISKS

# SECTION 1

# criminal_record_parent
alspac.table$criminal_record_parent_8wk <- repmeas(alspac.table[,c('e407a_rec', 'e416a_rec', 'e428a_rec')]) # Mum in trouble with law, PTNR in trouble with law, Court conviction

alspac.table$criminal_record_parent_8m <- repmeas(alspac.table[,c('f227a_rec', 'f236a_rec', 'f249a_rec')]) # Mum in trouble with law, PTNR in trouble with law, Court conviction

alspac.table$criminal_record_parent_21m <- repmeas(alspac.table[,c('g307a_rec', 'g316a_rec', 'g329a_rec')]) # Mum in trouble with law, PTNR in trouble with law, Court conviction

alspac.table$criminal_record_parent_3y <- repmeas(alspac.table[,c('h217a_rec', 'h226a_rec', 'h239a_rec')]) # Mum in trouble with law, PTNR in trouble with law, Court conviction

alspac.table$criminal_record_parent_4y <- repmeas(alspac.table[,c('j307a_rec', 'j316a_rec', 'j329a_rec')]) # Mum in trouble with law, PTNR in trouble with law, Court conviction

alspac.table$criminal_record_parent_5y <- repmeas(alspac.table[,c('k4007a_rec', 'k4016a_rec', 'k4029a_rec')]) # Mum in trouble with law, PTNR in trouble with law, Court conviction

alspac.table$criminal_record_parent_6y <- repmeas(alspac.table[,c('l4007a_rec', 'l4016a_rec', 'l4029a_rec')]) # Mum in trouble with law, PTNR in trouble with law, Court conviction

alspac.table$criminal_record_parent_9y <- repmeas(alspac.table[,c('p2007_rec', 'p2016_rec', 'p2029_rec')]) # Mum in trouble with law, PTNR in trouble with law, Court conviction


####################################################################################################################################################

# SECTION 2

# miscarriage_or_abortion

alspac.table$miscarriage_or_abortion_8wk <- alspac.table$e435a_rec # Attempted abortion

alspac.table$miscarriage_or_abortion_8m <- repmeas(alspac.table[,c('f254a_rec', 'f253a_rec')]) # Attempted abortion, Mum had miscarriage

alspac.table$miscarriage_or_abortion_21m <- repmeas(alspac.table[,c('g334a_rec', 'g333a_rec')]) # Attempted abortion, Mum had miscarriage

alspac.table$miscarriage_or_abortion_3y <- repmeas(alspac.table[,c('h244a_rec', 'h243a_rec')]) # Attempted abortion, Mum had miscarriage

alspac.table$miscarriage_or_abortion_4y <- repmeas(alspac.table[,c('j334a_rec', 'j333a_rec')]) # Attempted abortion, Mum had miscarriage

alspac.table$miscarriage_or_abortion_5y <- repmeas(alspac.table[,c('k4034a_rec', 'k4033a_rec')]) # Attempted abortion, Mum had miscarriage

alspac.table$miscarriage_or_abortion_6y <- repmeas(alspac.table[,c('l4034a_rec', 'l4033a_rec')]) # Attempted abortion, Mum had miscarriage

alspac.table$miscarriage_or_abortion_9y <- repmeas(alspac.table[,c('p2034_rec', 'p2033_rec')]) # Attempted abortion, Mum had miscarriage


#################################################################################################################################################
# SECTION 3

# m_attempted_suicide


alspac.table$m_attempted_suicide_8wk <- alspac.table$e427a_rec

alspac.table$m_attempted_suicide_8m <- alspac.table$f248a_rec

alspac.table$m_attempted_suicide_21m <- alspac.table$g328a_rec

alspac.table$m_attempted_suicide_3y <- alspac.table$h238a_rec

alspac.table$m_attempted_suicide_4y <- alspac.table$j328a_rec

alspac.table$m_attempted_suicide_5y <- alspac.table$k4028a_rec

alspac.table$m_attempted_suicide_6y <- alspac.table$l4028a_rec

alspac.table$m_attempted_suicide_9y <- alspac.table$p2028_rec


# m_age (Age of mother at completion of questionnaire = early_parent)
# Because mz028b is a factor, we use as.character before as.numeric.
# Factors are stored internally as integers with a table to give the factor level labels.

alspac.table$mz028bn <- as.numeric(as.character(alspac.table$mz028b)) # Error comes up that NAs introduced by coercion because 'consent withdrawn by mother' becomes NA (it is expected)
alspac.table$m_age <- ifelse(alspac.table$mz028bn < 19, yes = 1, no = 0)  # mother age younger than 19 at baseline based on Cecil et al. (2014); Rijlaarsdam et al. (2016)


# p_age (Age of PTNR at completion of questionnaire = early_parent)
alspac.table$pb910n <- as.numeric(as.character(alspac.table$pb910))
alspac.table$p_age <- ifelse(alspac.table$pb910n < 19, yes = 1, no = 0) # partner younger than 19 years at baseline based on Cecil et al. (2014); Rijlaarsdam et al. (2016)

####################################################################################################################################################

# SECTION 4

# m_depression

#alspac.table$m_depression_8m <- alspac.table$f021a_rec # had depression
#alspac.table$m_depression_21m <- alspac.table$g021a_rec # had depression
#alspac.table$m_depression_3y <- alspac.table$h013a_rec # had depression
#alspac.table$m_depression_4y <- alspac.table$j012a_rec # had depression
#alspac.table$m_depression_5y <- alspac.table$k1011a_rec # had depression
#alspac.table$m_depression_6y <- alspac.table$l3011a_rec # had depression
#alspac.table$m_depression_9y <- alspac.table$p1011a_rec # had depression

# m_depression
alspac.table$m_depression_8wk <- alspac.table$e391a_rec # EPDS total score
alspac.table$m_depression_8m <- repmeas(alspac.table[,c('f021a_rec', 'f201a_rec')])# had depression, EPDS total score
alspac.table$m_depression_21m <- repmeas(alspac.table[,c('g021a_rec', 'g291a_rec')]) # had depression, EPDS total score
alspac.table$m_depression_3y <- repmeas(alspac.table[,c('h013a_rec', 'h200ba_rec')]) # had depression, EPDS total score
alspac.table$m_depression_4y <- alspac.table$j012a_rec # had depression
alspac.table$m_depression_5y <- repmeas(alspac.table[,c('k1011a_rec', 'm_EPDS_total_5ya_rec')])  # had depression,  EPDS total score  5y
alspac.table$m_depression_6y <- alspac.table$l3011a_rec # had depression
alspac.table$m_depression_9y <- repmeas(alspac.table[,c('p1011a_rec', 'm_EPDS_total_8ya_rec')]) # had depression, EPDS total score 8y

# p_depression (lots of NAs)
alspac.table$p_depression_8wk <- alspac.table$pc103a_rec #  EPDS total score 
alspac.table$p_depression_8m <- repmeas(alspac.table[,c('pd021a_rec', 'pd201a_rec')]) # had depression, EPDS total score  
alspac.table$p_depression_21m <- repmeas(alspac.table[,c('pe021a_rec', 'pe291a_rec')]) # had depression, EPDS total score 
alspac.table$p_depression_3y <- repmeas(alspac.table[,c('pf1011a_rec', 'p_EPDS_total_3ya_rec')]) # had depression, EPDS total score
alspac.table$p_depression_4y <- alspac.table$pg1011a_rec # had depression
alspac.table$p_depression_5y <- repmeas(alspac.table[,c('ph1011a_rec', 'p_EPDS_total_5ya_rec')]) # had depression, EPDS total score
alspac.table$p_depression_6y <- repmeas(alspac.table[,c('pj3011a_rec', 'p_EPDS_total_6ya_rec')]) # had depression, EPDS total score
alspac.table$p_depression_9y <- repmeas(alspac.table[,c('pl1061a_rec', 'pm1011a_rec')]) # had depression 8y, had depression 9y

#################################################################################################################################################

# SECTION 5

# m_anxiety

alspac.table$m_anxiety_8wk <- alspac.table$e371a_rec # CCEI anxiety subscale (complete)
alspac.table$m_anxiety_8m <- alspac.table$f173a_rec # CCEI anxiety subscale (complete)
alspac.table$m_anxiety_21m <- alspac.table$g268a_rec # CCEI anxiety subscale (complete)
alspac.table$m_anxiety_3y <- alspac.table$h178a_rec # CCEI anxiety subscale (complete)
alspac.table$m_anxiety_5y <- alspac.table$CCEI_total_5ya_rec # CCEI anxiety subscale (complete)
alspac.table$m_anxiety_6y <- alspac.table$CCEI_total_6ya_rec # CCEI anxiety subscale (complete)

# p_anxiety

alspac.table$p_anxiety_8m <- alspac.table$pd020a_rec # had anxiety/nerves
alspac.table$p_anxiety_21m <- alspac.table$pe020a_rec # had anxiety/nerves
alspac.table$p_anxiety_3y <- alspac.table$pf1010a_rec # had anxiety/nerves
alspac.table$p_anxiety_4y <- alspac.table$pg1010a_rec # had anxiety/nerves
alspac.table$p_anxiety_5y <- alspac.table$ph1010a_rec # had anxiety/nerves
alspac.table$p_anxiety_6y <- alspac.table$pj3010a_rec # had anxiety/nerves
alspac.table$p_anxiety_9y <- alspac.table$pm1010a_rec # had anxiety/nerves


####################################################################################################################################################

# SECTION 6

# m_interpersonal_sensitivity, 80th percentile

# Higher scores = greater interpersonal sensitivity, based on items such as 'I avoid saying what I think for fear of being rejected'

# checking the distribution, since normally distributed, using 80th percentile
plot(alspac.table$b916) 

# changing a factor to numeric without changing values 
alspac.table$b916n <- as.numeric(levels(alspac.table$b916))[alspac.table$b916] 
quantile(alspac.table$b916n, .8, na.rm = T) # 80th percentile is 22

alspac.table$m_interpersonal_sensitivity <- ifelse(alspac.table$b916n >= 22, 1, 
                                                   ifelse(alspac.table$b916n < 22, 0, NA)) 

# checking if recoding worked
data.frame(alspac.table$m_interpersonal_sensitivity, alspac.table$b916) # looks good



# p_interpersonal_sensitivity, using 80th percentile

# Higher scores = greater interpersonal sensitivity, based on items such as 'feel insecure when saying goodbye'

alspac.table$p_interpersonal_sensitivity <- alspac.table$p_interpersonal_sensitivity_pre # should I remove this since we don't have it for postnatal? 



####################################################################################################################################################

# COLLAPSING VARIABLES 

# INTERPERSONAL RISKS

# SECTION 1

# Divorce
alspac.table$divorce_8wk <- alspac.table$e417a_rec # Respondent separated from partner
alspac.table$divorce_8m	<- repmeas(alspac.table[,c('f228a_rec', 'f237a_rec')]) # divorced, Respondent separated from partner
alspac.table$divorce_21m <- repmeas(alspac.table[,c('g308a_rec', 'g317a_rec')]) # divorced, Respondent separated from partner
alspac.table$divorce_3y	<- repmeas(alspac.table[,c('h218a_rec', 'h227a_rec')]) # divorced, Respondent separated from partner
alspac.table$divorce_4y <- repmeas(alspac.table[,c('j308a_rec', 'j317a_rec')]) # divorced, Respondent separated from partner
alspac.table$divorce_5y	<- repmeas(alspac.table[,c('k4008a_rec', 'k4017a_rec')]) # divorced, Respondent separated from partner
alspac.table$divorce_6y	<- repmeas(alspac.table[,c('l4008a_rec', 'l4017a_rec')]) # divorced, Respondent separated from partner
alspac.table$divorce_9y	<- repmeas(alspac.table[,c('p2008_rec', 'p2017_rec')]) # divorced, Respondent separated from partner


# Partner rejected child 
alspac.table$p_rejected_child_8wk	<- alspac.table$e409a_rec	 # Partner rejected child 
alspac.table$p_rejected_child_8m	<- alspac.table$f229a_rec	# Partner rejected child 
alspac.table$p_rejected_child_21m	<- alspac.table$g309a_rec	 # Partner rejected child 
alspac.table$p_rejected_child_3y	<- alspac.table$h219a_rec	# Partner rejected child 
alspac.table$p_rejected_child_4y	<- alspac.table$j309a_rec	# Partner rejected child 
alspac.table$p_rejected_child_5y	<- alspac.table$k4009a_rec	# Partner rejected child 
alspac.table$p_rejected_child_6y	<- alspac.table$l4009a_rec	# Partner rejected child 
alspac.table$p_rejected_child_9y	<- alspac.table$p2009_rec	 # Partner rejected child 

# Partner went away
alspac.table$p_went_away_8wk	<- alspac.table$e415a_rec	# Partner went away
alspac.table$p_went_away_8m	<- alspac.table$f235a_rec #	Partner went away
alspac.table$p_went_away_21m	<- alspac.table$g315a_rec	# Partner went away
alspac.table$p_went_away_3y	<- alspac.table$h225a_rec	# Partner went away
alspac.table$p_went_away_4y	<- alspac.table$j315a_rec	# Partner went away
alspac.table$p_went_away_5y	<- alspac.table$k4015a_rec # Partner went away
alspac.table$p_went_away_6y	<- alspac.table$l4015a_rec	# Partner went away
alspac.table$p_went_away_9y	<- alspac.table$p2015_rec	# Partner went away

####################################################################################################################################################

# SECTION 2

# conflict_in_family

alspac.table$conflict_in_family_8wk <- alspac.table$e437a_rec # Partner emotionally cruel to mum

alspac.table$conflict_in_family_8m <- alspac.table$f239a_rec # Partner emotionally cruel to mum

alspac.table$conflict_in_family_21m <- alspac.table$g336a_rec # Partner emotionally cruel to mum

alspac.table$conflict_in_family_3y <- alspac.table$h246a_rec # Partner emotionally cruel to mum

alspac.table$conflict_in_family_4y <- alspac.table$j336a_rec # Partner emotionally cruel to mum

alspac.table$conflict_in_family_5y <- alspac.table$k4036a_rec # Partner emotionally cruel to mum

alspac.table$conflict_in_family_6y <- alspac.table$l4036a_rec # Partner emotionally cruel to mum

alspac.table$conflict_in_family_9y <- alspac.table$p2036_rec #Partner emotionally cruel to mum


####################################################################################################################################################

# SECTION 3

# conflict_family_violence

alspac.table$conflict_family_violence_8wk <- alspac.table$e422a_rec # Partner hurt mum

alspac.table$conflict_family_violence_8m <- alspac.table$f242a_rec # Partner hurt mum

alspac.table$conflict_family_violence_21m <- repmeas(alspac.table[,c('g322a_rec', 'g712a_rec', 'g713a_rec')]) # Partner hurt mum, Mum/PTNR hit or slapped one another, Mum/PTNR threw something in anger

alspac.table$conflict_family_violence_3y <- repmeas(alspac.table[,c('h232a_rec', 'h581a_rec')]) # Partner hurt mum, Mum/PTNR hit or slapped one another

alspac.table$conflict_family_violence_4y <- alspac.table$j322a_rec # Partner hurt mum

alspac.table$conflict_family_violence_5y <- alspac.table$k4022a_rec # Partner hurt mum

alspac.table$conflict_family_violence_6y <- alspac.table$l4022a_rec # Partner hurt mum

alspac.table$conflict_family_violence_9y <- repmeas(alspac.table[,c('p2022_rec', 'p3154_rec', 'p3155_rec')]) # Partner hurt mum, Mum/PTNR hit or slapped one another, Mum/PTNR threw something in anger


####################################################################################################################################################

# SECTION 4

# m_new_partner

alspac.table$m_new_partner_8wk <- alspac.table$e425a_rec # Mum got married 
alspac.table$m_new_partner_8m <- alspac.table$f245a_rec # Mum got married 
alspac.table$m_new_partner_21m <- alspac.table$g325a_rec # Mum got married 
alspac.table$m_new_partner_3y <- alspac.table$h235a_rec # Mum got married 
alspac.table$m_new_partner_4y <- alspac.table$j325a_rec # Mum got married 
alspac.table$m_new_partner_5y <- alspac.table$k4025a_rec # Mum got married 

alspac.table$m_new_partner_6y <- repmeas(alspac.table[,c('l4025a_rec', 'l4040a_rec')]) # Mum got married, Mother found new partner
alspac.table$m_new_partner_9y <- repmeas(alspac.table[,c('p2025_rec', 'p2040_rec')]) # Mum got married, Mother found new partner



####################################################################################################################################################

# SECTION 5

# argued_fam_friends

alspac.table$argued_fam_friends_8wk <- alspac.table$e420a_rec # Argued with fam/friends
alspac.table$argued_fam_friends_8m <- alspac.table$f240a_rec # Argued with fam/friends
alspac.table$argued_fam_friends_21m <- alspac.table$g320a_rec # Argued with fam/friends
alspac.table$argued_fam_friends_3y <- alspac.table$h230a_rec # Argued with fam/friends
alspac.table$argued_fam_friends_4y <- alspac.table$j320a_rec # Argued with fam/friends
alspac.table$argued_fam_friends_5y <- alspac.table$k4020a_rec # Argued with fam/friends
alspac.table$argued_fam_friends_6y <- alspac.table$l4020a_rec # Argued with fam/friends
alspac.table$argued_fam_friends_9y <- alspac.table$p2020_rec # Argued with fam/friends



####################################################################################################################################################


# DIRECT VICTIMIZATION

# SECTION 1

# bullying

alspac.table$bullying_8y <- repmeas(alspac.table[,c('f8fp475a_rec', 'f8fp470a_rec')]) # Bullying, Child is relational victim; Bullying, Child is overt victim 

####################################################################################################################################################


# SECTION 2

# physical_violence	

alspac.table$physical_violence_18m <- alspac.table$kd504a_rec # Ch physically hurt by someone
alspac.table$physical_violence_30m <- alspac.table$kj464a_rec # Ch physically hurt by someone
alspac.table$physical_violence_3y <- alspac.table$kf454a_rec # Ch physically hurt by someone
alspac.table$physical_violence_4y <- alspac.table$kl474a_rec # Ch physically hurt by someone
alspac.table$physical_violence_5y <- alspac.table$kn4004a_rec # Ch physically hurt by someone
alspac.table$physical_violence_6y <- alspac.table$kq364a_rec # Ch physically hurt by someone

alspac.table$physical_violence_9y <- repmeas(alspac.table[,c('kt5004a_rec', 'ku298_rec')]) # Ch physically hurt by someone, Child is hit or slapped


####################################################################################################################################################

# SECTION 3

# sexual_abuse

alspac.table$sexual_abuse_18m <- alspac.table$kd505a_rec # Ch sexually abused
alspac.table$sexual_abuse_30m <- alspac.table$kj465a_rec # Ch sexually abused
alspac.table$sexual_abuse_3y <- alspac.table$kf455a_rec # Ch sexually abused
alspac.table$sexual_abuse_4y <- alspac.table$kl475a_rec # Ch sexually abused
alspac.table$sexual_abuse_5y <- alspac.table$kn4005a_rec # Ch sexually abused
alspac.table$sexual_abuse_6y <- alspac.table$kq365a_rec # Ch sexually abused
alspac.table$sexual_abuse_8y <- alspac.table$kt5005a_rec # Ch sexually abused

# p_physical_violence

alspac.table$p_physical_violence_8wk<- alspac.table$e426a_rec # Partner hurt child
alspac.table$p_physical_violence_8m <- alspac.table$f246a_rec # Partner hurt child
alspac.table$p_physical_violence_21m <- alspac.table$g336a_rec # Partner hurt child
alspac.table$p_physical_violence_3y <- alspac.table$h236a_rec # Partner hurt child
alspac.table$p_physical_violence_4y <- alspac.table$j326a_rec # Partner hurt child
alspac.table$p_physical_violence_5y<- alspac.table$k4026a_rec # Partner hurt child
alspac.table$p_physical_violence_6y <- alspac.table$l4026a_rec # Partner hurt child
alspac.table$p_physical_violence_9y <- alspac.table$p2026_rec # Partner hurt child


# m_physical_violence

alspac.table$m_physical_violence_8m <- alspac.table$f247a_rec # Mother physically cruel to child
alspac.table$m_physical_violence_21m <- alspac.table$g327a_rec # Mother physically cruel to child
alspac.table$m_physical_violence_3y <- alspac.table$h237a_rec # Mother physically cruel to child
alspac.table$m_physical_violence_4y <- alspac.table$j327a_rec # Mother physically cruel to child
alspac.table$m_physical_violence_5y <- alspac.table$k4027a_rec # Mother physically cruel to child
alspac.table$m_physical_violence_6y <- alspac.table$l4027a_rec # Mother physically cruel to child
alspac.table$m_physical_violence_9y <- alspac.table$p2027_rec # Mother physically cruel to child


# p_cruelty_emotional

alspac.table$p_cruelty_emotional_8wk <- alspac.table$e438a_rec # Partner emotionally cruel to child
alspac.table$p_cruelty_emotional_8m <- alspac.table$f257a_rec # Partner emotionally cruel to child
alspac.table$p_cruelty_emotional_21m <- alspac.table$g337a_rec # Partner emotionally cruel to child
alspac.table$p_cruelty_emotional_3y <- alspac.table$h247a_rec # Partner emotionally cruel to child
alspac.table$p_cruelty_emotional_4y <- alspac.table$j337a_rec # Partner emotionally cruel to child
alspac.table$p_cruelty_emotional_5y <- alspac.table$k4037a_rec # Partner emotionally cruel to child
alspac.table$p_cruelty_emotional_6y <- alspac.table$l4037a_rec # Partner emotionally cruel to child
alspac.table$p_cruelty_emotional_9y <- alspac.table$p2037_rec # Partner emotionally cruel to child

# m_cruelty_emotional

alspac.table$m_cruelty_emotional_21m <- alspac.table$g338a_rec # 	Mother emotionally cruel to child
alspac.table$m_cruelty_emotional_3y <- alspac.table$h248a_rec # 	Mother emotionally cruel to child
alspac.table$m_cruelty_emotional_4y <- alspac.table$j338a_rec # 	Mother emotionally cruel to child
alspac.table$m_cruelty_emotional_5y <- alspac.table$k4038a_rec # 	Mother emotionally cruel to child
alspac.table$m_cruelty_emotional_6y <- alspac.table$l4038a_rec # 	Mother emotionally cruel to child
alspac.table$m_cruelty_emotional_9y <- alspac.table$p2038_rec # 	Mother emotionally cruel to child

####################################################################################################################################################

# save to 'alspac.table' to .RData file

save(alspac.table, file = 'alspac.table.collapsed.Rdata')

####################################################################################################################################################



