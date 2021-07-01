####################################################################################################################################################

# POSTNATAL

####################################################################################################################################################

#install package 'lineup' which contains the 'corbetw2mat' function
library(lineup)

#Family Risk



#Mother feelings 

#h760 = MatEnjoyment_33M_Bottom10


# This script is used for dichotomising postnatal ELS variables into 1 = risk and 0 = no risk. 

# 1. LIFE EVENTS

# In most cases, variables have been coded as:

# 1 = affected a lot
# 2 = fairly affected
# 3 = mildly affected
# 4 = not effected at all
# 5 = didn't happen
# for total number of options inlcuded need to check the dataset 

#However, how this is phrased during post-natal coding is slightly different at each time point, so we have accounted for the changes in this script
# Below script recodes them into: 

# 1 = risk (for values between 1-4)
# 0 = no risk (for value of 5) 
# any other number = NA (missing)


#RECODE TO BINARY

# our R data file uses factor levels (not numeric) 
# define levels first
yes = c("Affected a lot","MOD affected","Mildly affected","No effect")
no = c("Did not happen")

# Factor levels missed out:
# Other, DK


#LE 8W
# now check if these levels are present and no other levels were missed out 
vars = c("e400", # PTNR died since MID PREG
         "e401", # CH died since MID PREG
         "e402", # FRD or REL died since MID PREG
         "e403", # CH ill since MID PREG
         "e404", # PTNR ill since MID PREG
         "e405", # Friend or REL ill since MID PREG
         "e406", # Admitted to HOSP since MID PREG
         "e410", # Ill since MID PREG
         "e411", # PTNR lost job since MID PREG
         "e412", # PTNR had PROBS at work since MID PREG
         "e413", # PROBS at work since MID PREG
         "e414", # Lost job since MID PREG
         "e421", # Moved house since PREG
         "e429", # MC scare since MID PREG
         "e430", # Started new job since MID PREG
         "e431", # Test for CH abnormality since MID PREG
         "e432", # Test suggested CH PROB since MID PREG
         "e433", # Discovered having twins since MID PREG
         "e434", # Heard event might harm CH since MID PREG
         "e436", # Took an exam since MID PREG
         "e439", # House burglary/car theft since MID PREG
         "e440") # Accident since MID PREG

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])



#LE 8M  

#This is an example where the phrasing of answers is slightly different to that of before, so we require new definitions for each timepoint
#our R data file uses factor levels (not numeric) 

# define levels first
yes = c("Y much affected","Y MOD affected","Y mildly affected","Y but N effect")
no = c("N did not happen")

# EW: another level: NK

# now check if these levels are present and no other levels were missed out 
vars = c(
  "f220", # Death of partner
  "f221", # Death of one of children
  "f222", # Death of friend or relative
  "f223", # Child ill
  "f224", # PTNR ill 
  "f225", # Friend or relative ill
  "f226", # Admitted to hospital
  "f230", # Mum ill
  "f231", # Parnter lost job
  "f232", # Work problems for partner
  "f233", # Work problems for Mum
  "f234", # Mum lost job
  "f241", # Moved house
  "f253", # Mum had miscarriage
  "f251", # New job for Mum
  "f250", # Mum became pregnant
  "f252", # Return to work
  "f255", # Exam taken
  "f260", # New job for partner
  "f261") # Pet died

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])        






#LE 21M          

# our R data file uses factor levels (not numeric) 
# define levels first
yes = c("Yes Big Effect","Yes Some Effect","Yes Mild Effect","Yes No Effect")
no = c("Did Not Happen")

# EW: other levels: Other, DK

# now check if these levels are present and no other levels were missed out 
vars = c(   
  "g300", # Partner died >CH8MTHs
  "g301", # One of Mums children died >CH8MTHs
  "g302", # Friend or relative died >CH8MTHs
  "g303", # One of Mums children ill >CH8MTHs
  "g304", # Partner ill >CH8MTHs
  "g305", # Friend or relative ill >CH8MTHs
  "g306", # Mum in hospital >CH8MTHs
  "g310", # Mum very ill >CH8MTHs
  "g311", # Partner lost job >CH8MTHs
  "g312", # Partner had problems with work >CH8MTHs
  "g313", # Mum had problems with work >CH8MTHs
  "g314", # Mum lost job >CH8MTHs
  "g321", # Mum moved house >CH8MTHs
  "g330", # Mum pregnant >CH8MTHs
  "g331", # Mum started new job >CH8MTHs
  "g332", # Mum returned to work >CH8MTHs
  "g333", # Mum had miscarraige >CH8MTHs
  "g335", # Mum took exam >CH8MTHs
  "g339", # Mums house or car burgled >CH 8MTHs
  "g340", # Partner started new job >CH8MTHs
  "g341", # A pet died >CH8MTHs
  "g342") # Mum had accident >CH8MTHs).)

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])        



#LE 3Y           

yes = c("yes had big effect","yes medium effect","yes mild effect","yes but no effect")
no = c("didnt happen")

# EW: other levels: other, dk (lower case!)

# now check if these levels are present and no other levels were missed out 
vars = c("h210", # Whether partner died since study child was 18 months old, Y/N
         "h211", # Whether one of mums children died since study child was 18 months old, Y/N
         "h212", # Whether a friend or relative died since study child was 18 months old, Y/N
         "h213", # Whether one of mums children was ill since study child was 18 months old, Y/N
         "h214", # Whether partner was ill since study child was 18 months old, Y/N
         "h215", # Whether a friend or relative was ill since study child was 18 months old, Y/N
         "h216", # Whether mum was admitted to hospital since study child was 18 months old, Y/N
         "h220", # Whether mum was very ill since study child was 18 months old, Y/N
         "h221", # Whether partner lost job since study child was 18 months old, Y/N
         "h222", # Whether partner had problems at work since study child was 18 months old, Y/N
         "h223", # Whether mum had problems at work since study child was 18 months old, Y/N
         "h224", # Whether mum lost job since study child was 18 months old, Y/N
         "h231", # Whether mum moved house since study child was 18 months old, Y/N
         "h240", # Whether mum became pregnant since study child was 18 months old, Y/N
         "h241", # Whether mum started a new job since study child was 18 months old, Y/N
         "h242", # Whether mum returned to work since study child was 18 months old, Y/N
         "h243", # Whether mum had a miscarriage since study child was 18 months old, Y/N
         "h245", # Whether mum took an exam since study child was 18 months old, Y/N
         "h249", # Whether house or car was burgled since study child was 18 months old, Y/N
         "h250", # Whether partner started a new job since study child was 18 months old, Y/N
         "h251", # Whether a pet died since study child was 18 months old, Y/N
         "h252")  # Whether mum had an accident since study child was 18 months old, Y/N)

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])         


#LE 4Y            


yes = c("Yes and affected a lot","Yes, moderately affected","Yes, mildly affected","Yes, not affect at all")
no = c("No")

# other level: "No/Missing" (set to missing)

vars = c("j300", # Partner Died > CH 30 MTHs y/n
         "j301", # 1 of MUMs Children Died> CH 30 MTHs y/n
         "j302", # MUMs FRD or Relative Died> CH 30 MTHs y/n
         "j303", # 1 of MUMs CDRN Ill> CH 30 MTHs y/n
         "j304", # Partner Ill> CH 30 MTHs y/n
         "j305", # MUM FRD or Relative Ill> CH 30 MTHs y/n
         "j306", # MUM in HOSP> CH 30 MTHs y/n
         "j310", # MUM was Very Ill> CH 30 MTHs y/n
         "j311", # Partner Lost Job> CH 30 MTHs y/n
         "j312", # Partner Had PROBs at Work> CH 30 MTHs y/n
         "j313", # MUM Had PROBs at Work> CH 30 MTHs y/n
         "j314", # MUM lost Job> CH 30 MTHs y/n
         "j321", # MUM Moved House> CH 30 MTHs y/n
         "j330", #MUM Became PREG> CH 30 MTHs y/n
         "j331", # MUM Began New Job> CH 30 MTHs y/n
         "j332", # MUM Returned to Work> CH 30 MTHs y/n
         "j333", # MUM Miscarried> CH 30 MTHs y/n
         "j335", # MUM Took An Exam> CH 30 MTHs y/n
         "j339", # MUMs House or Car Burgled> CH 30 MTHs y/n
         "j340", # Partner Began New Job> CH 30 MTHs y/n
         "j341", # MUMs Pet Died> CH 30 MTHs y/n
         "j342") # MUM Had Accident> CH 30 MTHs y/n) 

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])           


#LE 5Y          

yes = c("Yes, affected a lot","Yes, moderately affected","Yes, mildly affected","Yes, did not affect at all")
no = c("No, did not happen")

# EW: other levels: "Other text answer" (set to missing)

vars = c("k4000", # Mothers partner died in past year
         "k4001", # Mothers child died in past year
         "k4002", # D3: Mothers friend or relative died in past year
         "k4003", # D4: Mothers child was ill in past year
         "k4004", # D5: Mothers partner was ill in past year
         "k4005", # D6: Mothers friend or relative was ill in past year
         "k4006", # D7: Mother was admitted to hospital in past year
         "k4010", # D11: Mother was very ill in past year
         "k4011", # Mothers partner lost job in past year
         "k4012", # Mothers partner had problems at work in past year
         "k4013", # Mother had problems at work in past year
         "k4014", # Mother lost her job in past year
         "k4021", # Mother moved house in past year
         "k4030", # Mother became pregnant in past year
         "k4031", # Mother started a new job in past year
         "k4032", #  Mother returned to work in past year
         "k4033", # Mother had a miscarriage in past year
         "k4035", # Mother took an examination in past year
         "k4039", # Mothers house or car was burgled in past year
         "k4040", # Mothers partner started a new job in past year
         "k4041", # Mothers pet died in past year
         "k4042") # Mother had Accident in past year


for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])



#LE 6Y

yes = c("Yes & affected respondent a lot","Yes, moderately affected","Yes, mildly affected","Yes, did not affect respondent at all")
no = c("No, did not happen")

# EW: other levels: Other, DK (set to missing) 

# now check if these levels are present and no other levels were missed out 
vars = c("l4000", # Respondent's partner died since study child's 5th birthday
         "l4001", # One of respondent's children died since study child's 5th birthday
         "l4002", # Respondent's friend/relative died since study child's 5th birthday
         "l4003", # One of respondent's children was ill since study child's 5th birthday
         "l4004", # Respondent's partner was ill since study child's 5th birthday
         "l4005", # Respondent's friend/relative was ill since study child's 5th birthday
         "l4006", # Respondent was admitted to hospital since study child's 5th birthday
         "l4010", # Respondent was very ill since study child's 5th birthday
         "l4011", # Respondent's partner lost their job since study child's 5th birthday
         "l4012", # Respondent's partner had problems at work since study child's 5th birthday
         "l4013", # Respondent had problems at work since study child's 5th birthday
         "l4014", # Respondent lost their job since study child's 5th birthday
         "l4021", # Respondent moved house since study child's 5th birthday
         "l4030", # Respondent became pregnant since study child's 5th birthday
         "l4031", # Respondent started new job since study child's 5th birthday
         "l4032", # Respondent returned to work since study child's 5th birthday
         "l4033", # Respondent had miscarriage since study child's 5th birthday
         "l4035", # Respondent took an examination since study child's 5th birthday
         "l4039", # Respondent's house/car was burgled since study child's 5th birthday
         "l4042", # Respondent's partner started new job since study child's 5th birthday
         "l4043", # A pet of respondent died since study child's 5th birthday
         "l4044", # Respondent had an accident since study child's 5th birthday
         "l4041") # One of respondent's children started new school since study child's 5th birthday

#EW
for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}


# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)]) 

#LE child 18M         

yes = c("Yes & CH Very Upset", "Yes & CH Quite Upset", "Yes & CH Bit Upset", "Yes & CH Not Upset")
no = c("Did Not Happen")

# EW: other levels: Other, DK (set to missing)

vars = c("kd500a", # Ch taken into car
         "kd501a", # A pet died (adj)
         "kd502a", # Ch moved home (adj)
         "kd503a", # Ch had fright (adj)
         "kd506a", # Ch separated from mum for > a wk (adj)
         "kd507a", # Ch separated from dad for > a wk (adj)
         "kd508a", # CH Acquired New Parent > 6 MTHS
         "kd509a", # Ch had a new sibling (adj)
         "kd510a", # Ch admitted to hospital (adj)
         "kd511a", # Ch changed carer (adj)
         "kd512a", # Ch separated from someone else (adj)
         "kd513a") # Ch started nursery (adj))


for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("_rec",names(alspac.table), value=T)])

#LE child 30M

yes = c("yes child very upset", "yes quite upset", "yes bit upset", "yes not upset")
no = c("no didnt happen")

# EW other and dn set to missing ('other' up to n=7; unlike the other variables; still coded as missing)

vars = c("kf450", # Child taken into care > 18 months, Y/N
         "kf451", # A pet died > 18 months, Y/N
         "kf452", # Child moved home > 18 months, Y/N
         "kf453", # Child had fright > 18 months, Y/N
         "kf456", # Child sep.from mother >1wk >18 mths, Y/N
         "kf457", # Child sep.from father >1wk >18 mths, Y/N
         "kf458", # Child got new parent > 18 months, Y/N
         "kf459", # Child got new sibling > 18 months, Y/N
         "kf460", # Child admitted to hospital >18 mths, Y/N
         "kf461", # Child changed carer > 18 months, Y/N
         "kf462", # Child sep.from somebody > 18 months, Y/N
         "kf463") # Child started new creche >18 months, Y/N)

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])


#LE Child 3Y

yes = c("Yes CH Very Upset", "Yes Quite Upset", "Yes Bit Upset", "Yes Not Upset")
no = c("No didnt Happen")

# Ew: plus 'Other' and 'DK'

vars = c("kj460", # Child Taken Into Care
         "kj461", # Pet died 
         "kj462", # Child Moved Home 
         "kj463", # Child Had Shock 
         "kj466", # Child & Mum Separated 
         "kj467", # Child & Dad Separated 
         "kj468", # Child Got a New Parent 
         "kj469", # Child Got a New Sibling 
         "kj470", # Child Admitted To Hospital 
         "kj471", # Child Changed Carer 
         "kj472", # Child Separated From Someone Else
         "kj473") # Child Started New Creche 

for (i in vars){ 
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])


#LE child 4Y

yes = c("Yes, child was very upset","Yes, child was quite upset","Yes, child was a bit upset","Yes, child was not upset")
no = c("No, did not happen")

# EW: plus, 'Relevant text but no box ticked' set to missing

vars = c("kl470", # Child taken into care since age 3
         "kl471", # A pet died since child age 3
         "kl472", # Child moved home since age 3
         "kl473", # Child had shock or fright since age 3
         "kl476", # Child separated from mother since age 3
         "kl477", # Child separated from father since age 3
         "kl478", # Child acquired new mother or father since age 3
         "kl479", # Child had new brother or sister since age 3
         "kl480", # Child admitted to hospital since age 3
         "kl481", # Child changed care taker since age 3
         "kl482", # Child separated from someone else since age 3
         "kl483", # Child started new nursery/kindergarten since age 3
         "kl484") # Child started school since age 3)

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

#LE child 5Y        


yes = c("Yes And Was Very Upset","Yes And Was Quite Upset","Yes And Was A Bit Upset","Yes But Was Not Upset")
no = c("No Did Not Happen")

# EW: "Other Text Answer" "DK" set to missing

vars = c("kn4000", #  Child taken into care in past 15 months
         "kn4001", # Child's pet die in past 15 months
         "kn4002", # Child move home in past 15 months
         "kn4003", # Child have a fright or shock in past 15 months
         "kn4006", # Child separated from mother in past 15 months
         "kn4007", # Child separated from Father in past 15 months
         "kn4008", # Child acquire new parent in past 15 months
         "kn4009", # Child have a new brother or sister in past 15 months
         "kn4010", #  Child admitted to hospital in past 15 months
         "kn4011", # Child's main carer change in past 15 months
         "kn4012", # Child separated from another person in past 15 months
         "kn4013", # Child start a new nursery in past 15 months
         "kn4014") # Child start school in past 15 months)

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

#LE child 6y

yes = c("Yes, very upset", "Yes, quite upset", "Yes, a bit upset", "Yes, not upset")
no = c("No, did not happen")


vars = c("kq360", # Child was taken into care since his/her 5th birthday (Y/N)
         "kq361", # A pet died since child's 5th birthday (Y/N)
         "kq362", # Child moved home since his/her 5th birthday (Y/N)
         "kq363", # Child had a shock/fright since his/her 5th birthday (Y/N)
         "kq366", #  Somebody in the family died since child's 5th birthday (Y/N)
         "kq367", # Child was separated from his/her mother since his/her 5th birthday (Y/N)
         "kq368", # Child was separated from his/her father since his/her 5th birthday (Y/N)
         "kq369", # Child acquired a new mother/father since his/her 5th birthday (Y/N)
         "kq370", # Child had a new brother or sister since his/her 5th birthday (Y/N)
         "kq371", # Child was admitted to hospital since his/her 5th birthday (Y/N)
         "kq372", # Child changed care taker since his/her 5th birthday (Y/N)
         "kq373") # Child was separated from another close person since his/her 5th birthday (Y/N)

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

#EW
attach(alspac.table)


LE_postnatal_continuous <- data.frame(e400, # PTNR died since MID PREG
                                      e401, # CH died since MID PREG
                                      e402, # FRD or REL died since MID PREG
                                      e403, # CH ill since MID PREG
                                      e404, # PTNR ill since MID PREG
                                      e405, # Friend or REL ill since MID PREG
                                      e406, # Admitted to HOSP since MID PREG
                                      e410, # Ill since MID PREG
                                      e411, # PTNR lost job since MID PREG
                                      e412, # PTNR had PROBS at work since MID PREG
                                      e413, # PROBS at work since MID PREG
                                      e414, # Lost job since MID PREG
                                      e421, # Moved house since PREG
                                      e429, # MC scare since MID PREG
                                      e430, # Started new job since MID PREG
                                      e431, # Test for CH abnormality since MID PREG
                                      e432, # Test suggested CH PROB since MID PREG
                                      e433, # Discovered having twins since MID PREG
                                      e434, # Heard event might harm CH since MID PREG
                                      e436, # Took an exam since MID PREG
                                      e439, # House burglary/car theft since MID PREG
                                      e440, # Accident since MID PREG
                                      f220, # Death of partner
                                      f221, # Death of one of children
                                      f222, # Death of friend or relative
                                      f223, # Child ill
                                      f224, # PTNR ill 
                                      f225, # Friend or relative ill
                                      f226, # Admitted to hospital
                                      f230, # Mum ill
                                      f231, # Parnter lost job
                                      f232, # Work problems for partner
                                      f233, # Work problems for Mum
                                      f234, # Mum lost job
                                      f241, # Moved house
                                      f253, # Mum had miscarriage
                                      f251, # New job for Mum
                                      f250, # Mum became pregnant
                                      f252, # Return to work
                                      f255, # Exam taken
                                      f260, # New job for partner
                                      f261, # Pet died
                                      g300, # Partner died >CH8MTHs
                                      g301, # One of Mums children died >CH8MTHs
                                      g302, # Friend or relative died >CH8MTHs
                                      g303, # One of Mums children ill >CH8MTHs
                                      g304, # Partner ill >CH8MTHs
                                      g305, # Friend or relative ill >CH8MTHs
                                      g306, # Mum in hospital >CH8MTHs
                                      g310, # Mum very ill >CH8MTHs
                                      g311, # Partner lost job >CH8MTHs
                                      g312, # Partner had problems with work >CH8MTHs
                                      g313, # Mum had problems with work >CH8MTHs
                                      g314, # Mum lost job >CH8MTHs
                                      g321, # Mum moved house >CH8MTHs
                                      g330, # Mum pregnant >CH8MTHs
                                      g331, # Mum started new job >CH8MTHs
                                      g332, # Mum returned to work >CH8MTHs
                                      g333, # Mum had miscarraige >CH8MTHs
                                      g335, # Mum took exam >CH8MTHs
                                      g339, # Mums house or car burgled >CH 8MTHs
                                      g340, # Partner started new job >CH8MTHs
                                      g341, # A pet died >CH8MTHs
                                      g342, # Mum had accident >CH8MTHs).)
                                      h210, # Whether partner died since study child was 18 months old, Y/N
                                      h211, # Whether one of mums children died since study child was 18 months old, Y/N
                                      h212, # Whether a friend or relative died since study child was 18 months old, Y/N
                                      h213, # Whether one of mums children was ill since study child was 18 months old, Y/N
                                      h214, # Whether partner was ill since study child was 18 months old, Y/N
                                      h215, # Whether a friend or relative was ill since study child was 18 months old, Y/N
                                      h216, # Whether mum was admitted to hospital since study child was 18 months old, Y/N
                                      h220, # Whether mum was very ill since study child was 18 months old, Y/N
                                      h221, # Whether partner lost job since study child was 18 months old, Y/N
                                      h222, # Whether partner had problems at work since study child was 18 months old, Y/N
                                      h223, # Whether mum had problems at work since study child was 18 months old, Y/N
                                      h224, # Whether mum lost job since study child was 18 months old, Y/N
                                      h231, # Whether mum moved house since study child was 18 months old, Y/N
                                      h240, # Whether mum became pregnant since study child was 18 months old, Y/N
                                      h241, # Whether mum started a new job since study child was 18 months old, Y/N
                                      h242, # Whether mum returned to work since study child was 18 months old, Y/N
                                      h243, # Whether mum had a miscarriage since study child was 18 months old, Y/N
                                      h245, # Whether mum took an exam since study child was 18 months old, Y/N
                                      h249, # Whether house or car was burgled since study child was 18 months old, Y/N
                                      h250, # Whether partner started a new job since study child was 18 months old, Y/N
                                      h251, # Whether a pet died since study child was 18 months old, Y/N
                                      h252, # Whether mum had an accident since study child was 18 months old, Y/N)
                                      j300, # Partner Died > CH 30 MTHs y/n
                                      j301, # 1 of MUMs Children Died> CH 30 MTHs y/n
                                      j302, # MUMs FRD or Relative Died> CH 30 MTHs y/n
                                      j303, # 1 of MUMs CDRN Ill> CH 30 MTHs y/n
                                      j304, # Partner Ill> CH 30 MTHs y/n
                                      j305, # MUM FRD or Relative Ill> CH 30 MTHs y/n
                                      j306, # MUM in HOSP> CH 30 MTHs y/n
                                      j310, # MUM was Very Ill> CH 30 MTHs y/n
                                      j311, # Partner Lost Job> CH 30 MTHs y/n
                                      j312, # Partner Had PROBs at Work> CH 30 MTHs y/n
                                      j313, # MUM Had PROBs at Work> CH 30 MTHs y/n
                                      j314, # MUM lost Job> CH 30 MTHs y/n
                                      j321, # MUM Moved House> CH 30 MTHs y/n
                                      j330, #MUM Became PREG> CH 30 MTHs y/n
                                      j331, # MUM Began New Job> CH 30 MTHs y/n
                                      j332, # MUM Returned to Work> CH 30 MTHs y/n
                                      j333, # MUM Miscarried> CH 30 MTHs y/n
                                      j335, # MUM Took An Exam> CH 30 MTHs y/n
                                      j339, # MUMs House or Car Burgled> CH 30 MTHs y/n
                                      j340, # Partner Began New Job> CH 30 MTHs y/n
                                      j341, # MUMs Pet Died> CH 30 MTHs y/n
                                      j342, # MUM Had Accident> CH 30 MTHs y/n) 
                                      k4000, # Mothers partner died in past year
                                      k4001, # Mothers child died in past year
                                      k4002, # D3: Mothers friend or relative died in past year
                                      k4003, # D4: Mothers child was ill in past year
                                      k4004, # D5: Mothers partner was ill in past year
                                      k4005, # D6: Mothers friend or relative was ill in past year
                                      k4006, # D7: Mother was admitted to hospital in past year
                                      k4010, # D11: Mother was very ill in past year
                                      k4011, # Mothers partner lost job in past year
                                      k4012, # Mothers partner had problems at work in past year
                                      k4013, # Mother had problems at work in past year
                                      k4014, # Mother lost her job in past year
                                      k4021, # Mother moved house in past year
                                      k4030, # Mother became pregnant in past year
                                      k4031, # Mother started a new job in past year
                                      k4032, #  Mother returned to work in past year
                                      k4033, # Mother had a miscarriage in past year
                                      k4035, # Mother took an examination in past year
                                      k4039, # Mothers house or car was burgled in past year
                                      k4040, # Mothers partner started a new job in past year
                                      k4041, # Mothers pet died in past year
                                      k4042, # Mother had Accident in past year
                                      l4000, # Respondent's partner died since study child's 5th birthday
                                      l4001, # One of respondent's children died since study child's 5th birthday
                                      l4002, # Respondent's friend/relative died since study child's 5th birthday
                                      l4003, # One of respondent's children was ill since study child's 5th birthday
                                      l4004, # Respondent's partner was ill since study child's 5th birthday
                                      l4005, # Respondent's friend/relative was ill since study child's 5th birthday
                                      l4006, # Respondent was admitted to hospital since study child's 5th birthday
                                      l4010, # Respondent was very ill since study child's 5th birthday
                                      l4011, # Respondent's partner lost their job since study child's 5th birthday
                                      l4012, # Respondent's partner had problems at work since study child's 5th birthday
                                      l4013, # Respondent had problems at work since study child's 5th birthday
                                      l4014, # Respondent lost their job since study child's 5th birthday
                                      l4021, # Respondent moved house since study child's 5th birthday
                                      l4030, # Respondent became pregnant since study child's 5th birthday
                                      l4031, # Respondent started new job since study child's 5th birthday
                                      l4032, # Respondent returned to work since study child's 5th birthday
                                      l4033, # Respondent had miscarriage since study child's 5th birthday
                                      l4035, # Respondent took an examination since study child's 5th birthday
                                      l4039, # Respondent's house/car was burgled since study child's 5th birthday
                                      l4042, # Respondent's partner started new job since study child's 5th birthday
                                      l4043, # A pet of respondent died since study child's 5th birthday
                                      l4044, # Respondent had an accident since study child's 5th birthday
                                      l4041, # One of respondent's children started new school since study child's 5th birthday
                                      kd500a, # Ch taken into car
                                      kd501a, # A pet died (adj)
                                      kd502a, # Ch moved home (adj)
                                      kd503a, # Ch had fright (adj)
                                      kd506a, # Ch separated from mum for > a wk (adj)
                                      kd507a, # Ch separated from dad for > a wk (adj)
                                      kd508a, # CH Acquired New Parent > 6 MTHS
                                      kd509a, # Ch had a new sibling (adj)
                                      kd510a, # Ch admitted to hospital (adj)
                                      kd511a, # Ch changed carer (adj)
                                      kd512a, # Ch separated from someone else (adj)
                                      kd513a, # Ch started nursery (adj))
                                      kf450, # Child taken into care > 18 months, Y/N
                                      kf451, # A pet died > 18 months, Y/N
                                      kf452, # Child moved home > 18 months, Y/N
                                      kf453, # Child had fright > 18 months, Y/N
                                      kf456, # Child sep.from mother >1wk >18 mths, Y/N
                                      kf457, # Child sep.from father >1wk >18 mths, Y/N
                                      kf458, # Child got new parent > 18 months, Y/N
                                      kf459, # Child got new sibling > 18 months, Y/N
                                      kf460, # Child admitted to hospital >18 mths, Y/N
                                      kf461, # Child changed carer > 18 months, Y/N
                                      kf462, # Child sep.from somebody > 18 months, Y/N
                                      kf463, # Child started new creche >18 months, Y/N)
                                      kj460, # Child Taken Into Care Y/N
                                      kj461, # Pet died Y/N
                                      kj462, # Child Moved Home Y/N
                                      kj463, # Child Had Shock Y/N
                                      kj466, # Child & Mum Separated Y/N
                                      kj467, # Child & Dad Separated Y/N
                                      kj468, # Child Got a New Parent Y/N
                                      kj469, # Child Got a New Sibling Y/N
                                      kj470, # Child Admitted To Hospital Y/N
                                      kj471, # Child Changed Carer Y/N
                                      kj472, # Child Separated From Someone Else Y/N
                                      kj473, # Child Started New Creche Y/N)
                                      kl470, # Child taken into care since age 3
                                      kl471, # A pet died since child age 3
                                      kl472, # Child moved home since age 3
                                      kl473, # Child had shock or fright since age 3
                                      kl476, # Child separated from mother since age 3
                                      kl477, # Child separated from father since age 3
                                      kl478, # Child acquired new mother or father since age 3
                                      kl479, # Child had new brother or sister since age 3
                                      kl480, # Child admitted to hospital since age 3
                                      kl481, # Child changed care taker since age 3
                                      kl482, # Child separated from someone else since age 3
                                      kl483, # Child started new nursery/kindergarten since age 3
                                      kl484, # Child started school since age 3)
                                      kn4000, #  Child taken into care in past 15 months
                                      kn4001, # Child's pet die in past 15 months
                                      kn4002, # Child move home in past 15 months
                                      kn4003, # Child have a fright or shock in past 15 months
                                      kn4006, # Child separated from mother in past 15 months
                                      kn4007, # Child separated from Father in past 15 months
                                      kn4008, # Child acquire new parent in past 15 months
                                      kn4009, # Child have a new brother or sister in past 15 months
                                      kn4010, #  Child admitted to hospital in past 15 months
                                      kn4011, # Child's main carer change in past 15 months
                                      kn4012, # Child separated from another person in past 15 months
                                      kn4013, # Child start a new nursery in past 15 months
                                      kn4014, # Child start school in past 15 months)
                                      kq360, # Child was taken into care since his/her 5th birthday (Y/N)
                                      kq361, # A pet died since child's 5th birthday (Y/N)
                                      kq362, # Child moved home since his/her 5th birthday (Y/N)
                                      kq363, # Child had a shock/fright since his/her 5th birthday (Y/N)
                                      kq366, #  Somebody in the family died since child's 5th birthday (Y/N)
                                      kq367, # Child was separated from his/her mother since his/her 5th birthday (Y/N)
                                      kq368, # Child was separated from his/her father since his/her 5th birthday (Y/N)
                                      kq369, # Child acquired a new mother/father since his/her 5th birthday (Y/N)
                                      kq370, # Child had a new brother or sister since his/her 5th birthday (Y/N)
                                      kq371, # Child was admitted to hospital since his/her 5th birthday (Y/N)
                                      kq372, # Child changed care taker since his/her 5th birthday (Y/N)
                                      kq373) # Child was separated from another close person since his/her 5th birthday (Y/N)) # Had an accident since PREG

# Creating a data frame containing the newly created binary LE variables

LE_postnatal_binary <- data.frame(e400a_rec,
                                  e401a_rec,
                                  e402a_rec,
                                  e403a_rec,
                                  e404a_rec,
                                  e405a_rec,
                                  e406a_rec,
                                  e410a_rec,
                                  e411a_rec,
                                  e412a_rec,
                                  e413a_rec,
                                  e414a_rec,
                                  e421a_rec,
                                  e429a_rec,
                                  e430a_rec,
                                  e431a_rec,
                                  e432a_rec,
                                  e433a_rec,
                                  e434a_rec,
                                  e436a_rec,
                                  e439a_rec,
                                  e440a_rec,
                                  f220a_rec,
                                  f221a_rec,
                                  f222a_rec,
                                  f223a_rec,
                                  f224a_rec,
                                  f225a_rec,
                                  f226a_rec,
                                  f230a_rec,
                                  f231a_rec,
                                  f232a_rec,
                                  f233a_rec,
                                  f234a_rec,
                                  f241a_rec,
                                  f253a_rec,
                                  f251a_rec,
                                  f250a_rec,
                                  f252a_rec,
                                  f255a_rec,
                                  f260a_rec,
                                  f261a_rec,
                                  g300a_rec,
                                  g301a_rec,
                                  g302a_rec,
                                  g303a_rec,
                                  g304a_rec,
                                  g305a_rec,
                                  g306a_rec,
                                  g310a_rec,
                                  g311a_rec,
                                  g312a_rec,
                                  g313a_rec,
                                  g314a_rec,
                                  g321a_rec,
                                  g330a_rec,
                                  g331a_rec,
                                  g332a_rec,
                                  g333a_rec,
                                  g335a_rec,
                                  g339a_rec,
                                  g340a_rec,
                                  g341a_rec,
                                  g342a_rec,
                                  h210a_rec,
                                  h211a_rec,
                                  h212a_rec,
                                  h213a_rec,
                                  h214a_rec,
                                  h215a_rec,
                                  h216a_rec,
                                  h220a_rec,
                                  h221a_rec,
                                  h222a_rec,
                                  h223a_rec,
                                  h224a_rec,
                                  h231a_rec,
                                  h240a_rec,
                                  h241a_rec,
                                  h242a_rec,
                                  h243a_rec,
                                  h245a_rec,
                                  h249a_rec,
                                  h250a_rec,
                                  h251a_rec,
                                  h252a_rec,
                                  j300a_rec,
                                  j301a_rec,
                                  j302a_rec,
                                  j303a_rec,
                                  j304a_rec,
                                  j305a_rec,
                                  j306a_rec,
                                  j310a_rec,
                                  j311a_rec,
                                  j312a_rec,
                                  j313a_rec,
                                  j314a_rec,
                                  j321a_rec,
                                  j330a_rec,
                                  j331a_rec,
                                  j332a_rec,
                                  j333a_rec,
                                  j335a_rec,
                                  j339a_rec,
                                  j340a_rec,
                                  j341a_rec,
                                  j342a_rec,
                                  k4000a_rec,
                                  k4001a_rec,
                                  k4002a_rec,
                                  k4003a_rec,
                                  k4004a_rec,
                                  k4005a_rec,
                                  k4006a_rec,
                                  k4010a_rec,
                                  k4011a_rec,
                                  k4012a_rec,
                                  k4013a_rec,
                                  k4014a_rec,
                                  k4021a_rec,
                                  k4030a_rec,
                                  k4031a_rec,
                                  k4032a_rec,
                                  k4033a_rec,
                                  k4035a_rec,
                                  k4039a_rec,
                                  k4040a_rec,
                                  k4041a_rec,
                                  k4042a_rec,
                                  l4000a_rec,
                                  l4001a_rec,
                                  l4002a_rec,
                                  l4003a_rec,
                                  l4004a_rec,
                                  l4005a_rec,
                                  l4006a_rec,
                                  l4010a_rec,
                                  l4011a_rec,
                                  l4012a_rec,
                                  l4013a_rec,
                                  l4014a_rec,
                                  l4021a_rec,
                                  l4030a_rec,
                                  l4031a_rec,
                                  l4032a_rec,
                                  l4033a_rec,
                                  l4035a_rec,
                                  l4039a_rec,
                                  l4042a_rec,
                                  l4043a_rec,
                                  l4044a_rec,
                                  l4041a_rec,
                                  kd500a_rec,
                                  kd501a_rec,
                                  kd502a_rec,
                                  kd503a_rec,
                                  kd506a_rec,
                                  kd507a_rec,
                                  kd508a_rec,
                                  kd509a_rec,
                                  kd510a_rec,
                                  kd511a_rec,
                                  kd512a_rec,
                                  kd513a_rec,
                                  kf450a_rec,
                                  kf451a_rec,
                                  kf452a_rec,
                                  kf453a_rec,
                                  kf456a_rec,
                                  kf457a_rec,
                                  kf458a_rec,
                                  kf459a_rec,
                                  kf460a_rec,
                                  kf461a_rec,
                                  kf462a_rec,
                                  kf463a_rec,
                                  kj460a_rec,
                                  kj461a_rec,
                                  kj462a_rec,
                                  kj463a_rec,
                                  kj466a_rec,
                                  kj467a_rec,
                                  kj468a_rec,
                                  kj469a_rec,
                                  kj470a_rec,
                                  kj471a_rec,
                                  kj472a_rec,
                                  kj473a_rec,
                                  kl470a_rec,
                                  kl471a_rec,
                                  kl472a_rec,
                                  kl473a_rec,
                                  kl476a_rec,
                                  kl477a_rec,
                                  kl478a_rec,
                                  kl479a_rec,
                                  kl480a_rec,
                                  kl481a_rec,
                                  kl482a_rec,
                                  kl483a_rec,
                                  kl484a_rec,
                                  kn4000a_rec,
                                  kn4001a_rec,
                                  kn4002a_rec,
                                  kn4003a_rec,
                                  kn4006a_rec,
                                  kn4007a_rec,
                                  kn4008a_rec,
                                  kn4009a_rec,
                                  kn4010a_rec,
                                  kn4011a_rec,
                                  kn4012a_rec,
                                  kn4013a_rec,
                                  kn4014a_rec,
                                  kq360a_rec,
                                  kq361a_rec,
                                  kq362a_rec,
                                  kq363a_rec,
                                  kq366a_rec,
                                  kq367a_rec,
                                  kq368a_rec,
                                  kq369a_rec,
                                  kq370a_rec,
                                  kq371a_rec,
                                  kq372a_rec,
                                  kq373a_rec)



detach(alspac.table)


#Checking correlations btw columns of LE_postnatal_continuous and columns of LE_postnatal_binary
#corbetw2mat(LE_prenatal_continuous, LE_prenatal_binary, what = "paired")    

# for factor-based dataframe
corbetw2mat(data.matrix(LE_postnatal_continuous), LE_postnatal_binary, what = "paired")    

# EW: all correlation large (above 0.8), but all negative, which is okay, as 'did not happen' level often last (=highest)

####################################################################################################################################################

# 2. CONTEXTUAL RISKS
# Because text coding for each timepoint is different, we have to specify the wording in each timepoint. 


# our R data file uses factor levels (not numeric) 
# define levels first
yes = c("Affected a lot","MOD affected","Mildly affected","No effect")
no = c("Did not happen")

# EW: "Other" "DK" set to missing

# now check if these levels are present and no other levels were missed out 
vars = c("e418", # Income reduced since MID PREG	
         "e423", # Became homeless since PREG
         "e424") # Major financial PROB since MID PREG	

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])


yes = c("Y much affected","Y MOD affected","Y mildly affected","Y but N effect")
no = c("N did not happen")

# EW: "NK" set to missing

vars = c("f238",	# Reduced income
         "f243", # Became homeless
         "f244") # Major financial problems

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

yes = c("Yes Big Effect","Yes Some Effect","Yes Mild Effect","Yes No Effect")
no = c("Did Not Happen")        

vars = c("g318",	
         "g323",	
         "g324")	

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])


yes = c("yes had big effect","yes medium effect","yes mild effect","yes but no effect")
no = c("didnt happen")   

#EW: "other" "dk" set to missing

vars = c("h228",	
         "h233",	
         "h234")

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

yes = c("Yes, not affect at all","Yes, mildly affected","Yes, moderately affected","Yes and affected a lot")
no = c("No")         

# EW: "No/Missing"set to missing

vars = c("j318",	
         "j323",
         "j324")	

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

yes = c("Yes, affected a lot","Yes, moderately affected","Yes, mildly affected","Yes, did not affect at all")
no = c("No, did not happen")          

# EW: "Other text answer" set to missing

vars = c("k4018",	
         "k4023",
         "k4024")

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

yes = c("Yes & affected respondent a lot","Yes, moderately affected","Yes, mildly affected","Yes, did not affect respondent at all")
no = c("No, did not happen")    

#EW: "Other" "DK" set to missing

vars = c("l4018",
         "l4023",
         "l4024") # Major financial PROB since PREG


for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])



# Creating a data frame with the original CR variables 
attach(alspac.table)

CR_postnatal_continuous <- data.frame(e418, # Income reduced since MID PREG	
                                      f238,	# Reduced income
                                      g318,	
                                      h228,	
                                      j318,	
                                      k4018,	
                                      l4018,
                                      e423, # Became homeless since PREG	
                                      f243, # Became homeless	
                                      g323,	
                                      h233,	
                                      j323,	
                                      k4023,	
                                      l4023,
                                      e424, # Major financial PROB since MID PREG	
                                      f244, # Major financial problems
                                      g324,	
                                      h234,	
                                      j324,	
                                      k4024,	
                                      l4024) # Major financial PROB since PREG

# Creating a data frame containing the newly created binary CR variables

CR_postnatal_binary <- data.frame(e418a_rec, # Income reduced since MID PREG	
                                  f238a_rec,	# Reduced income
                                  g318a_rec,	
                                  h228a_rec,	
                                  j318a_rec,	
                                  k4018a_rec,	
                                  l4018a_rec,
                                  e423a_rec, # Became homeless since PREG	
                                  f243a_rec, # Became homeless	
                                  g323a_rec,	
                                  h233a_rec,	
                                  j323a_rec,	
                                  k4023a_rec,	
                                  l4023a_rec,
                                  e424a_rec, # Major financial PROB since MID PREG	
                                  f244a_rec, # Major financial problems
                                  g324a_rec,	
                                  h234a_rec,	
                                  j324a_rec,	
                                  k4024a_rec,	
                                  l4024a_rec) # Major financial PROB since PREG


detach(alspac.table)

# Checking correlations btw columns of CR_postnatal_continuous and columns of CR_postnatal_binary
#corbetw2mat(CR_postnatal_continuous, CR_postnatal_binary, what = "paired")  
corbetw2mat(data.matrix(CR_postnatal_continuous), CR_postnatal_binary, what = "paired")  

# EW: again, large but negative correlations

####################################################################################################################################################


# 3. PARENTAL RISKS 


#RECODE TO BINARY

# our R data file uses factor levels (not numeric) 
# define levels first

# now check if these levels are present and no other levels were missed out 
yes = c("Affected a lot","MOD affected","Mildly affected","No effect")
no = c("Did not happen")

# EW: "Other" "DK" set to missing

vars = c("e407", #Trouble with law since MID PREG
         "e416", # PTNR in trouble with law since MID PREG) # Major financial PROB since MID PREG	
         "e427", # Attempted suicide since MID PREG
         "e428", # Convicted since MID PREG
         "e435") # Attempted abortion since MID PREG

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

yes = c("Y much affected","Y MOD affected","Y mildly affected","Y but N effect")
no = c("N did not happen")

# EW: NK set to missing

vars = c("f227",	#Mum in trouble with law Y/N
         "f236", # Partner in trouble with law	
         "f248", # 	Attempted suicide
         "f249", # Court conviction
         "f254")	#Mum had abortion

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

yes = c("Yes Big Effect","Yes Some Effect","Yes Mild Effect","Yes No Effect")
no = c("Did Not Happen")        

# EW: "Other" "DK" set to missing

vars = c("g307",
         "g316",
         "g328",
         "g329",
         "g334")	

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

yes = c("yes had big effect","yes medium effect","yes mild effect","yes but no effect")
no = c("didnt happen")   

# EW: "other" "dk" set to missing

vars = c("h217",
         "h226",
         "h238",
         "h239",
         "h244")


for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

yes = c("Yes, not affect at all","Yes, mildly affected","Yes, moderately affected","Yes and affected a lot")
no = c("No")         

# EW: "No/Missing" set to missing

vars = c("j307",
         "j316",
         "j328",
         "j329",
         "j334")	

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

yes = c("Yes, affected a lot","Yes, moderately affected","Yes, mildly affected","Yes, did not affect at all")
no = c("No, did not happen")          

# EW: 'Other text answer' set to missing

vars = c("k4007",
         "k4016",
         "k4028",
         "k4029",
         "k4034")

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

#EW
yes = c("Yes & affected respondent a lot","Yes, moderately affected","Yes, mildly affected","Yes, did not affect respondent at all")
no = c("No, did not happen")          

# EW: + Other and DK

vars = c("l4007",
         "l4016",
         "l4028",
         "l4029",
         "l4034") # Major financial PROB since PREG

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])


# Creating a data frame with the original PR variables 
attach(alspac.table)

PR_postnatal_continuous <- data.frame(e407, #Trouble with law since MID PREG
                                      f227,	#Mum in trouble with law Y/N
                                      g307,	
                                      h217,	
                                      j307,	
                                      k4007,
                                      l4007,
                                      e416, # PTNR in trouble with law since MID PREG
                                      f236, # Partner in trouble with law	
                                      g316,
                                      h226,	
                                      j316,	
                                      k4016,
                                      l4016,
                                      e427, # Attempted suicide since MID PREG
                                      f248, # 	Attempted suicide
                                      g328,	
                                      h238,	
                                      j328,	
                                      k4028,	
                                      l4028,
                                      e428, # Convicted since MID PREG
                                      f249, # Court conviction
                                      g329,
                                      h239,
                                      j329,
                                      k4029,
                                      l4029,
                                      e435, # Attempted abortion since MID PREG
                                      f254,	#Mum had abortion
                                      g334,
                                      h244,
                                      j334,	
                                      k4034,	
                                      l4034)

# Creating a data frame containing the newly created binary PR variables 

PR_postnatal_binary <- data.frame(e407a_rec, #Trouble with law since MID PREG
                                  f227a_rec,	#Mum in trouble with law Y/N
                                  g307a_rec,	
                                  h217a_rec,	
                                  j307a_rec,	
                                  k4007a_rec,
                                  l4007a_rec,
                                  e416a_rec, # PTNR in trouble with law since MID PREG
                                  f236a_rec, # Partner in trouble with law	
                                  g316a_rec,
                                  h226a_rec,	
                                  j316a_rec,	
                                  k4016a_rec,
                                  l4016a_rec,
                                  e427a_rec, # Attempted suicide since MID PREG
                                  f248a_rec, # 	Attempted suicide
                                  g328a_rec,	
                                  h238a_rec,	
                                  j328a_rec,	
                                  k4028a_rec,	
                                  l4028a_rec,
                                  e428a_rec, # Convicted since MID PREG
                                  f249a_rec, # Court conviction
                                  g329a_rec,
                                  h239a_rec,
                                  j329a_rec,
                                  k4029a_rec,
                                  l4029a_rec,
                                  e435a_rec, # Attempted abortion since MID PREG
                                  f254a_rec,	#Mum had abortion
                                  g334a_rec,
                                  h244a_rec,
                                  j334a_rec,	
                                  k4034a_rec,	
                                  l4034a_rec) # Tried to have abortion


detach(alspac.table)

# Checking correlations btw columns of PR_postnatal_continuous and columns of PR_postnatal_binary
#corbetw2mat(PR_postnatal_continuous, PR_postnatal_binary, what = "paired")  
corbetw2mat(data.matrix(PR_postnatal_continuous), PR_postnatal_binary, what = "paired")  


####################################################################################################################################################

# RECODING M_DEPRESSION AND P_DEPRESSION 

####################################################################################################################################################

# MOTHER

# Dichotomising items for had depression (to move to recoding items section) 

# our R data file uses factor levels (not numeric) 
# define levels first
yes = c("Y saw DR", "Y did not see DR", "Yes & saw Dr", "Yes,Did not see  Dr", "yes and saw doctor", "yes didnt see doctor", 
        "Yes & saw DR", "Yes didnt see DR", "Yes, consulted doctor", "Yes, did not consult doctor", "Yes & consulted doctor", "Yes but did not consult doctor",
        "Yes, consulted doctor", "Yes, did not consult doctor")

no = c("N","No","no")    

# now check if these levels are present and no other levels were missed out 
vars = c("f021", 
         "g021", 
         "h013", 
         "j012",
         "k1011",
         "l3011",
         "p1011")

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
 #readline(prompt = "levels ok? Press [enter] to continue")
}


# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
 # readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

####################################################################################################################################################

# PARTNER


# Dichotomising items for had depression (8m, 21m, 2y 9m, 3y 11m, 5y 1m, 6y 1m, 9y 2m)


# our R data file uses factor levels (not numeric) 
# define levels first
yes = c("Yes, Saw Doctor", "Yes, No Doctor", 
        "Yes Consulted Dr", "Yes Not Consult Dr", 
        "Yes, and consulted doctor", "Yes, but did not consult doctor", 
        "Yes and consulted doctor","Yes but did not consult doctor",
        "Yes, consulted a doctor", "Yes, did not consult a doctor", 
        "Yes and consulted a doctor", "Yes but did not consult a doctor",
        "Yes, recently", "Yes, in past not now",
        "Yes, consulted doctor", "Yes, did not consult doctor")

no = c("No", "No, never")    

# now check if these levels are present and no other levels were missed out 
vars = c("pd021", "pe021", "pf1011", "pg1011", "ph1011", "pj3011", "pl1061", "pm1011")

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}


# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

####################################################################################################################################################


# PARTNER

# Dichotomising items for had anxiety/nerves (8m, 21m, 2y 9m, 3y 11m, 5y 1m, 6y 1m, 8y 1m, 9y 2m)

levels(alspac.table$pd020) 
levels(alspac.table$pe020) 
levels(alspac.table$pf1010) 
levels(alspac.table$pg1010) 
levels(alspac.table$ph1010) 
levels(alspac.table$pj3010) 
levels(alspac.table$pm1010) 

# our R data file uses factor levels (not numeric) 
# define levels first
yes = c("Yes, Saw Doctor", "Yes, No Doctor", 
        "Yes Consulted Dr", "Yes Not Consult Dr", 
        "Yes, and consulted doctor", "Yes, but did not consult doctor", 
        "Yes and consulted doctor","Yes but did not consult doctor",
        "Yes, consulted a doctor", "Yes, did not consult a doctor", 
        "Yes and consulted a doctor", "Yes but did not consult a doctor",
        "Yes, consulted doctor", "Yes, did not consult doctor")

no = "No"  


# now check if these levels are present and no other levels were missed out 
vars = c("pd020", "pe020", "pf1010", "pg1010", "ph1010", "pj3010", "pm1010")

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}


# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])


####################################################################################################################################################

####################################################################################################################################################


# INTERPERSONAL RISKS


#RECODE TO BINARY
#RECODE TO BINARY

# our R data file uses factor levels (not numeric) 
# define levels first

# now check if these levels are present and no other levels were missed out 
yes = c("Affected a lot","MOD affected","Mildly affected","No effect")
no = c("Did not happen")

# EW: + "Other" "DK"

vars = c("e408", # Divorced since MID PREG
         "e409", #PTNR rejected CH since MID PREG
         "e415", #PTNR went away since MID PREG
         "e417", # Separated since MID PREG
         "e419", # Argued with PTNR since MID PREG
         "e420", # Argued with FAM/FRDS since MID PREG
         "e422", # PTNR hurt MUM since MID PREG
         "e425", # Married since MID PREG
         "e426", # PTNR hurt CHDR since MID PREG
         "e437", # PTNR EMOT cruel to MUM since MID PREG
         "e438") # PTNR EMOT cruel to CH since MID PREG) # Attempted abortion since MID PREG

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}

yes = c("Y much affected","Y MOD affected","Y mildly affected","Y but N effect")
no = c("N did not happen")

vars = c("f228", # Divorce 
         "f229", # Child not wanted by partner
         "f235", # Partner went away
         "f237", # Separation with partner
         "f239", # Argued with partner
         "f240", # Argued with family or friend
         "f242", # Physically hurt by partner
         "f245", # Got married
         "f246", # Parnter physically cruel to children
         "f256", # Partner emotionally cruel to Mum
         "f257", # Partner emotionally cruel to children
         "f247") # Mum physically cruel to children)	#Mum had abortion

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}

yes = c("Yes Big Effect","Yes Some Effect","Yes Mild Effect","Yes No Effect")
no = c("Did Not Happen")        

vars = c("g308", # Mum divorced >CH8MTHs
         "g309", # Partner rejected child >CH8MTHs
         "g315", # Partner went away >CH8MTHs
         "g317", # Mum and partner seperated >CH8MTHs
         "g319", # Mum argued with partner >CH8MTHs
         "g320", # Mum argued with family and friends >CH8MTHs
         "g322", # Partner physically cruel to Mum >CH8MTHs
         "g325", # Mum got married >CH8MTHs
         "g326", # Partner physically cruel to children >CH8MTHs
         "g327", # Mum physically cruel to children >CH8MTHs
         "g336", # Partner emotionally cruel to Mum >CH8MTHs
         "g337", # Partner emotionally cruel to children >CH8MTHs
         "g338") # Mum emotionally cruel to children >CH8MTHs)	

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}

yes = c("yes had big effect","yes medium effect","yes mild effect","yes but no effect")
no = c("didnt happen")   

vars = c("h218", # Whether got divorced since study child was 18 months old, Y/N
         "h219", # Whether partner rejected children since study child was 18 months old, Y/N
         "h225", # Whether partner went away since study child was 18 months old, Y/N
         "h227", # Whether mum and partner separated since study child was 18 months old, Y/N
         "h229", # Whether mum argued with partner since study child was 18 months old, Y/N
         "h230", # Whether mum argued with family and friends since study child was 18 months old, Y/N
         "h232", # Whether partner was physically cruel to mum since study child was 18 months old, Y/N
         "h235", # Whether mum got married since study child was 18 months old, Y/N
         "h236", # Whether partner was physically cruel to children since study child was 18 months old, Y/N
         "h237", # Whether mum was physically cruel to children since study child was 18 months old, Y/N
         "h246", # Whether partner was emotionally cruel to mum since study child was 18 months old, Y/N
         "h247", # Whether partner was emotionally cruel to children since study child was 18 months old, Y/N
         "h248") # Whether mum was emotionally cruel to children since study child was 18 months old, Y/N)

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}

yes = c("Yes, not affect at all","Yes, mildly affected","Yes, moderately affected","Yes and affected a lot")
no = c("No")         

vars = c("j308", # MUM Divorced> CH 30 MTHs y/n
         "j309", # MUM Found PTR Not Want CH> CH 30 MTHs y/n
         "j315", # Partner Went Away> CH 30 MTHs y/n
         "j317", # MUM & Partner Separated> CH 30 MTHs y/n
         "j319", # MUM Argued W Partner> CH 30 MTHs y/n
         "j320", # MUM Argued W FMLY & FRDs> CH 30 MTHs y/n
         "j322", # Partner PHYS Cruel to MUM> CH 30 MTHs y/n
         "j325", # MUM Got Married> CH 30 MTHs y/n
         "j326", # PTR PHYS Cruel to CDRN> CH 30 MTHs y/n
         "j327", # MUM PHYS Cruel to CDRN> CH 30 MTHs y/n
         "j336", # PTR Emotionally Cruel to MUM> CH 30 MTHs y/n
         "j337", # PTR Emotional Cruel to CDRN> CH 30 MTHs y/n
         "j338") # MUM Emotional Cruel to CDRN> CH 30 MTHs y/n)	

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}

yes = c("Yes, affected a lot","Yes, moderately affected","Yes, mildly affected","Yes, did not affect at all")
no = c("No, did not happen")          

vars = c("k4008", # Mother was divorced in past year
         "k4009", # Mother found that her partner did not want her child in past year
         "k4015", # Mothers partner went away in past year
         "k4017", # Mother and partner separated in past year
         "k4019", # Mother argued with her partner in past year
         "k4020", # Mother argued with family and friends in past year
         "k4022", # Mothers partner was physically cruel to her in past year
         "k4025", # Mother got married in past year
         "k4026", # Mothers partner was physically cruel to children in past year
         "k4027", # Mother was physically cruel to children in past year
         "k4036", # Mothers partner was emotionally cruel to her in past year
         "k4037", # Mothers partner was emotionally cruel to children in past year)
         "k4038") # Mother was emotionally cruel to children in past year


for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


yes = c("Yes & affected respondent a lot",
        "Yes, moderately affected",
        "Yes, mildly affected",
        "Yes, did not affect respondent at all")
no = c("No, did not happen")     

vars = c("l4008", # Respondent was divorced since study child's 5th birthday
         "l4009", # Respondent found their partner did not want their child since study child's 5th birthday
         "l4015", # Respondent's partner went away since study child's 5th birthday
         "l4017", # Respondent separated from partner since study child's 5th birthday
         "l4019", # Respondent argued with partner since study child's 5th birthday
         "l4020", # Respondent argued with family/friends since study child's 5th birthday
         "l4022", # Respondent's partner was physically cruel to them since study child's 5th birthday
         "l4025", # Respondent got married since study child's 5th birthday
         "l4026", # Respondent's partner physically cruel to respondent's children since study child's 5th birthday
         "l4027", # Respondent physically cruel to own children since study child's 5th birthday
         "l4036", # Respondent's partner was emotionally cruel to them since study child's 5th birthday
         "l4037", # Respondent's partner was emotionally cruel to respondent's children since study child's 5th birthday
         "l4038", # Respondent was emotionally cruel to their children since study child's 5th birthday
         "l4040") # Respondent found new partner since study child's 5th birthday


for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


##From here for the next 150 lines, these are the variables included in charlottes script for
#which we have numerous missing values, I have subsituted those which we have and included them
#as individual items as in charlottes script. 

# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

yes = c("Yes mum did","Yes partner did","Yes both did")
no = c("No not at all")

vars = c("h580", #DV_Shouted_33M 
         "h581") #DV_Hit_33M

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])


yes = c("Yes mum Did","Yes Partner Did","Yes both Did")
no = c("No not at All")

vars = c("g712",  #DV_Hit_21M
         "g713")  #DV_Break_21M

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])


# EW: unclear how coded (below is my interpretation)
yes = c("Every day","SEV times WK","C once WK","Rarely")
no = c("Never")


vars = c("ke017") #Par_Smack_2Y

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

# Creating a data frame with the original IR variables 
attach(alspac.table)

IR_postnatal_continuous <- data.frame(e408,
                                      e409,
                                      e415,
                                      e417,
                                      e419,
                                      e420,
                                      e422,
                                      e425,
                                      e426,
                                      e437,
                                      e438,
                                      f228,
                                      f229,
                                      f235,
                                      f237,
                                      f239,
                                      f240,
                                      f242,
                                      f245,
                                      f246,
                                      f256,
                                      f257,
                                      f247,
                                      g308,
                                      g309,
                                      g315,
                                      g317,
                                      g319,
                                      g320,
                                      g322,
                                      g325,
                                      g326,
                                      g327,
                                      g336,
                                      g337,
                                      g338,
                                      h218,
                                      h219,
                                      h225,
                                      h227,
                                      h229,
                                      h230,
                                      h232,
                                      h235,
                                      h236,
                                      h237,
                                      h246,
                                      h247,
                                      h248,
                                      j308,
                                      j309,
                                      j315,
                                      j317,
                                      j319,
                                      j320,
                                      j322,
                                      j325,
                                      j326,
                                      j327,
                                      j336,
                                      j337,
                                      j338,
                                      k4008,
                                      k4009,
                                      k4015,
                                      k4017,
                                      k4019,
                                      k4020,
                                      k4022,
                                      k4025,
                                      k4026,
                                      k4027,
                                      k4036,
                                      k4037,
                                      k4038,
                                      l4008,
                                      l4009,
                                      l4015,
                                      l4017,
                                      l4019,
                                      l4020,
                                      l4022,
                                      l4025,
                                      l4026,
                                      l4027,
                                      l4036,
                                      l4037,
                                      l4038,
                                      l4040, 
                                      h580, #DV_Shouted_33M 
                                      h581, #DV_Hit_33M
                                      g712, #DV_Hit_21M
                                      g713,  #DV_Break_21M
                                      ke017) #Par_Smack_2Y

IR_postnatal_binary <- data.frame(e408a_rec,
                                  e409a_rec,
                                  e415a_rec,
                                  e417a_rec,
                                  e419a_rec,
                                  e420a_rec,
                                  e422a_rec,
                                  e425a_rec,
                                  e426a_rec,
                                  e437a_rec,
                                  e438a_rec,
                                  f228a_rec,
                                  f229a_rec,
                                  f235a_rec,
                                  f237a_rec,
                                  f239a_rec,
                                  f240a_rec,
                                  f242a_rec,
                                  f245a_rec,
                                  f246a_rec,
                                  f256a_rec,
                                  f257a_rec,
                                  f247a_rec,
                                  g308a_rec,
                                  g309a_rec,
                                  g315a_rec,
                                  g317a_rec,
                                  g319a_rec,
                                  g320a_rec,
                                  g322a_rec,
                                  g325a_rec,
                                  g326a_rec,
                                  g327a_rec,
                                  g336a_rec,
                                  g337a_rec,
                                  g338a_rec,
                                  h218a_rec,
                                  h219a_rec,
                                  h225a_rec,
                                  h227a_rec,
                                  h229a_rec,
                                  h230a_rec,
                                  h232a_rec,
                                  h235a_rec,
                                  h236a_rec,
                                  h237a_rec,
                                  h246a_rec,
                                  h247a_rec,
                                  h248a_rec,
                                  j308a_rec,
                                  j309a_rec,
                                  j315a_rec,
                                  j317a_rec,
                                  j319a_rec,
                                  j320a_rec,
                                  j322a_rec,
                                  j325a_rec,
                                  j326a_rec,
                                  j327a_rec,
                                  j336a_rec,
                                  j337a_rec,
                                  j338a_rec,
                                  k4008a_rec,
                                  k4009a_rec,
                                  k4015a_rec,
                                  k4017a_rec,
                                  k4019a_rec,
                                  k4020a_rec,
                                  k4022a_rec,
                                  k4025a_rec,
                                  k4026a_rec,
                                  k4027a_rec,
                                  k4036a_rec,
                                  k4037a_rec,
                                  k4038a_rec,
                                  l4008a_rec,
                                  l4009a_rec,
                                  l4015a_rec,
                                  l4017a_rec,
                                  l4019a_rec,
                                  l4020a_rec,
                                  l4022a_rec,
                                  l4025a_rec,
                                  l4026a_rec,
                                  l4027a_rec,
                                  l4036a_rec,
                                  l4037a_rec,
                                  l4038a_rec,
                                  l4040a_rec, 
                                  h580a_rec, #DV_Shouted_33M 
                                  h581a_rec, #DV_Hit_33M
                                  g712a_rec, #DV_Hit_21M
                                  g713a_rec,  #DV_Break_21M
                                  ke017a_rec) #Par_Smack_2Y


# Checking correlations btw columns of IR_postnatal_continuous and columns of IR_postnatal_binary
#corbetw2mat(IR_postnatal_continuous, IR_postnatal_binary, what = "paired")  
corbetw2mat(data.matrix(IR_postnatal_continuous), IR_postnatal_binary, what = "paired")  


####################################################################################################################################################
####################################################################################################################################################


# DIRECT VICTIMISATION


#RECODE TO BINARY

# Here, as an exception, the coding in Charlottes script is binary 
# define levels first
yes = c("Yes & CH Very Upset", "Yes & CH Quite Upset", "Yes & CH Bit Upset", "Yes & CH Not Upset")
no = c("Did Not Happen")

# now check if these levels are present and no other levels were missed out 
vars = c("kd504a",	 # Ch physically hurt by someone (adj)
         "kd505a")	# Ch sexually abused (adj)

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("_rec",names(alspac.table), value=T)])

# However, it turns back to non-binary
# our R data file uses factor levels (not numeric) 
# define levels first
yes = c("yes child very upset", "yes quite upset", "yes bit upset","yes not upset")
no = c("no didnt happen")


vars = c("kf454",	# Child physically hurt > 18 months, Y/N
         "kf455")	# Child sexually abused > 18 months, Y/N

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

yes = c("Yes CH Very Upset", "Yes Quite Upset", "Yes Bit Upset", "Yes Not Upset")
no = c("No didnt Happen")

vars = c("kj464",	# Child Was Physically Hurt By Person Y/N
         "kj465")	# Child Sexually Abused Y/N

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)]) 

yes = c("Yes, child was very upset", "Yes, child was quite upset", "Yes, child was a bit upset", "Yes, child was not upset")
no = c("No, did not happen")       

vars = c("kl474",	# Child was physically hurt by someone since age 3 (not Y/N)
         "kl475")	# Child was sexually abused since age 3

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

yes = c("Yes And Was Very Upset", "Yes And Was Quite Upset", "Yes And Was A Bit Upset", "Yes But Was Not Upset")
no = c("No Did Not Happen")       

vars = c("kn4004",	# Child physically hurt by someone in past 15 months
         "kn4005")	# Child sexually abused in past 15 months

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])

yes = c("Yes, very upset", "Yes, quite upset", "Yes, a bit upset", "Yes, not upset")
no = c("No, did not happen")  

vars = c("kq364", # Child was physically hurt by someone since his/her 5th birthday (Y/N)
         "kq365")

for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# recode

for(i in vars){
  var.out=paste0(i,"a_rec")
  alspac.table[,var.out]=NA
  alspac.table[which(alspac.table[,i] %in% yes),var.out]=1
  alspac.table[which(alspac.table[,i] %in% no),var.out]=0
  
  # check
  print(i)
  print(table(alspac.table[,i], useNA = "always"))
  print(table(alspac.table[,var.out], useNA = "always"))
  
  #readline(prompt = "twice the same? Press [enter] to continue")
}


# check new "a_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])



# Creating a data frame with the original IR variables 
attach(alspac.table)

DV_postnatal_continuous <- data.frame(kd504a,
                                      kf454,
                                      kj464,
                                      kl474,	
                                      kn4004,	
                                      kq364,
                                      kd505a,
                                      kf455,
                                      kj465,
                                      kl475,	
                                      kn4005,
                                      kq365) # PTNR was EMOT cruel to mum since PREG

# Creating a data frame containing the newly created binary IR variables 

DV_postnatal_binary <- data.frame(kd504a_rec,	
                                  kf454a_rec,
                                  kj464a_rec,
                                  kl474a_rec,
                                  kn4004a_rec,	
                                  kq364a_rec,
                                  kd505a_rec,
                                  kf455a_rec,
                                  kj465a_rec,
                                  kl475a_rec,
                                  kn4005a_rec,	
                                  kq365a_rec)

detach(alspac.table)

# Checking correlations btw columns of DV_postnatal_continuous and columns of DV_postnatal_binary
corbetw2mat(data.matrix(DV_postnatal_continuous), DV_postnatal_binary, what = "paired")  


####################################################################################################################################################














