####################################################################################################################################################

# POSTNATAL 8-9Y

####################################################################################################################################################

# This script is used for dichotomising prenatal ELS variables into 1 = risk and 0 = no risk. 

# 1. LIFE EVENTS

# RECODE TO BINARY
# MOTHER-BASED LIFE EVENTS

# our R data file uses factor levels (not numeric) 
# define levels first
yes = c("Yes, when the study child was 6 or 7","Yes, since the study child's 8th birthday","Yes, both when the study child was 6/7 and 8+")
no = c("No, did not happen in past 3 years")

# now check if these levels are present and no other levels were missed out 

vars =  c("p2000", # Husband/partner died since the study child's 6th birthday
          "p2001", # One of mother's children died since the study child's 6th birthday
          "p2002", # Mother's friend or relative died since the study child's 6th birthday
          "p2003", # One of mother's children was ill since the study child's 6th birthday
          "p2004", # Mother's husband/partner was ill since the study child's 6th birthday
          "p2005", # Mother's friend or relative was ill since the study child's 6th birthday
          "p2006", # Mother was admitted to hospital since the study child's 6th birthday
          "p2010", # Mother was very ill since the study child's 6th birthday
          "p2011", # Mother's husband/partner lost his job since the study child's 6th birthday
          "p2012", # Mother's husband/partner had problems at work since the study child's 6th birthday
          "p2013", # Mother had problems at work since the study child's 6th birthday
          "p2014", # Mother had problems at work since the study child's 6th birthday
          "p2021", # Mother moved house since the study child's 6th birthday
          "p2030", # Mother became pregnant since the study child's 6th birthday
          "p2031", # Mother started a new job since the study child's 6th birthday
          "p2032", # Mother returned to work since the study child's 6th birthday
          "p2033", # Mother had a miscarriage since the study child's 6th birthday
          "p2035", # Mother took an examination since the study child's 6th birthday
          "p2039", # Mother's house or car was burgled since the study child's 6th birthday
          "p2041", # One of mother's children started school since the study child's 6th birthday
          "p2042", # Mother's husband/partner started a new job since the study child's 6th birthday
          "p2043", # A pet died since the study child's 6th birthday
          "p2044") #  Mother had an accident since the study child's 6th birthday

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


# check new "_rec" variables are there with meaningful values
summary(alspac.table[,grep("_rec",names(alspac.table), value=T)])



# Creating a data frame with the original LE variables 

attach(alspac.table)

LE_postnatal_8_9_mum_continuous <- data.frame(p2000, # Husband/partner died since the study child's 6th birthday
                                              p2001, # One of mother's children died since the study child's 6th birthday
                                              p2002, # Mother's friend or relative died since the study child's 6th birthday
                                              p2003, # One of mother's children was ill since the study child's 6th birthday
                                              p2004, # Mother's husband/partner was ill since the study child's 6th birthday
                                              p2005, # Mother's friend or relative was ill since the study child's 6th birthday
                                              p2006, # Mother was admitted to hospital since the study child's 6th birthday
                                              p2010, # Mother was very ill since the study child's 6th birthday
                                              p2011, # Mother's husband/partner lost his job since the study child's 6th birthday
                                              p2012, # Mother's husband/partner had problems at work since the study child's 6th birthday
                                              p2013, # Mother had problems at work since the study child's 6th birthday
                                              p2014, # Mother had problems at work since the study child's 6th birthday
                                              p2021, # Mother moved house since the study child's 6th birthday
                                              p2030, # Mother became pregnant since the study child's 6th birthday
                                              p2031, # Mother started a new job since the study child's 6th birthday
                                              p2032, # Mother returned to work since the study child's 6th birthday
                                              p2033, # Mother had a miscarriage since the study child's 6th birthday
                                              p2035, # Mother took an examination since the study child's 6th birthday
                                              p2039, # Mother's house or car was burgled since the study child's 6th birthday
                                              p2041, # One of mother's children started school since the study child's 6th birthday
                                              p2042, # Mother's husband/partner started a new job since the study child's 6th birthday
                                              p2043, # A pet died since the study child's 6th birthday
                                              p2044) #  Mother had an accident since the study child's 6th birthday



# Creating a data frame containing the newly created binary LE variables

LE_postnatal_8_9_mum_binary <- data.frame(p2000_rec,
                                          p2001_rec,
                                          p2002_rec,
                                          p2003_rec,
                                          p2004_rec,
                                          p2005_rec,
                                          p2006_rec,
                                          p2010_rec,
                                          p2011_rec,
                                          p2012_rec,
                                          p2013_rec,
                                          p2014_rec,
                                          p2021_rec,
                                          p2030_rec,
                                          p2031_rec,
                                          p2032_rec,
                                          p2033_rec,
                                          p2035_rec,
                                          p2039_rec,
                                          p2041_rec,
                                          p2042_rec,
                                          p2043_rec,
                                          p2044_rec)


detach(alspac.table)

#install package 'lineup' which contains the 'corbetw2mat' function
library(lineup)

#Checking correlations btw columns of LE_postnatal_8_9_mum_continuous and columns of LE_postnatal_8_9_mum_binary
#corbetw2mat(LE_postnatal_8_9_mum_continuous, LE_postnatal_8_9_mum_binary, what = "paired")    

# for factor-based dataframe
corbetw2mat(data.matrix(LE_postnatal_8_9_mum_continuous), LE_postnatal_8_9_mum_binary, what = "paired")    

####################################################################################################################################################

# RECODE TO BINARY
# CHILD-BASED LIFE EVENTS

# define levels first
yes = c("Yes, very upset","Yes, quite upset","Yes, bit upset", "Yes, not upset")
no = c("No")

# now check if these levels are present and no other levels were missed out 

vars =  c("kt5000", # Since 7th birthday child has been taken into care
          "kt5001", # Since 7th birthday child's pet died
          "kt5002", # Since 7th birthday child moved home
          "kt5003", # Since 7th birthday child had shock or fright
          "kt5006", # Since 7th birthday child has had someone in family die
          "kt5007", # Since 7th birthday child has been separated from mother
          "kt5008", # Since 7th birthday child has been separated from father
          "kt5009", # Since 7th birthday child has had a new mother or father
          "kt5010", # Since 7th birthday child has had a new brother or sister
          "kt5011", # Since 7th birthday child has been admitted to hospital
          "kt5012", # Since 7th birthday child has changed their caretaker
          "kt5013", # Since 7th birthday child has been separated from someone else
          "kt5014", # Since 7th birthday child has started a new school
          "kt5015") # Since 7th birthday child has lost their best friend


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


# Creating a data frame with the original LE variables 

attach(alspac.table)

LE_postnatal_8_9_child_continuous <- data.frame(kt5000,
                                                kt5001,
                                                kt5002,
                                                kt5003,
                                                kt5006,
                                                kt5007,
                                                kt5008,
                                                kt5009,
                                                kt5010,
                                                kt5011,
                                                kt5012,
                                                kt5013,
                                                kt5014,
                                                kt5015)


# Creating a data frame containing the newly created binary LE variables

LE_postnatal_8_9_child_binary <- data.frame(kt5000a_rec,
                                            kt5001a_rec,
                                            kt5002a_rec,
                                            kt5003a_rec,
                                            kt5006a_rec,
                                            kt5007a_rec,
                                            kt5008a_rec,
                                            kt5009a_rec,
                                            kt5010a_rec,
                                            kt5011a_rec,
                                            kt5012a_rec,
                                            kt5013a_rec,
                                            kt5014a_rec,
                                            kt5015a_rec)


detach(alspac.table)

#Checking correlations btw columns of LE_postnatal_8_9_child_binary and columns of LE_postnatal_8_9_child_binary
# for factor-based dataframe
corbetw2mat(data.matrix(LE_postnatal_8_9_child_continuous), LE_postnatal_8_9_child_binary, what = "paired")    

####################################################################################################################################################


# 2. CONTEXTUAL RISKS



# our R data file uses factor levels (not numeric) 
# define levels first

yes = c("Yes, when the study child was 6 or 7","Yes, since the study child's 8th birthday","Yes, both when the study child was 6/7 and 8+")
no = c("No, did not happen in past 3 years")

# now check if these levels are present and no other levels were missed out 
vars = c("p2018", # Mother's income was reduced since the study child's 6th birthday
         "p2023", # Mother became homeless since the study child's 6th birthday
         "p2024") # Mother had a major financial problem since the study child's 6th birthday


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


# check new "_rec" variables are there with meaningful values
summary(alspac.table[,grep("_rec",names(alspac.table), value=T)])



# Creating a data frame with the original CR variables 
attach(alspac.table)

CR_postnatal_8_9_continuous <- data.frame(p2018, # Mother's income was reduced since the study child's 6th birthday
                                          p2023, # Mother became homeless since the study child's 6th birthday
                                          p2024) # Mother had a major financial problem since the study child's 6th birthday

# Creating a data frame containing the newly created binary CR variables

CR_postnatal_8_9_binary <- data.frame(p2018_rec, # Mother's income was reduced since the study child's 6th birthday
                                      p2023_rec, # Mother became homeless since the study child's 6th birthday
                                      p2024_rec) # Mother had a major financial problem since the study child's 6th birthday


detach(alspac.table)

# Checking correlations btw columns of CR_postnatal_8_9_continuous and columns of CR_postnatal_8_9_binary
corbetw2mat(data.matrix(CR_postnatal_8_9_continuous), CR_postnatal_8_9_binary, what = "paired")  


####################################################################################################################################################

# 3. PARENTAL RISKS 


#RECODE TO BINARY

# our R data file uses factor levels (not numeric) 
# define levels first
yes = c("Yes, when the study child was 6 or 7","Yes, since the study child's 8th birthday","Yes, both when the study child was 6/7 and 8+")
no = c("No, did not happen in past 3 years")

# now check if these levels are present and no other levels were missed out 
vars = c("p2007", # Mother was in trouble with the law since the study child's 6th birthday
         "p2016", # Mother's husband/partner was in trouble with the law since the study child's 6th birthday
         "p2028", # Mother attempted suicide since the study child's 6th birthday
         "p2029", # Mother was convicted of an offence since the study child's 6th birthday
         "p2034") # Mother had an abortion since the study child's 6th birthday

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


# check new "_rec" variables are there with meaningful values
summary(alspac.table[,grep("_rec",names(alspac.table), value=T)])


# Creating a data frame with the original PR variables 
attach(alspac.table)

PR_postnatal_8_9_continuous <- data.frame(p2007, # Mother was in trouble with the law since the study child's 6th birthday
                                          p2016, # Mother's husband/partner was in trouble with the law since the study child's 6th birthday
                                          p2028, # Mother attempted suicide since the study child's 6th birthday
                                          p2029, # Mother was convicted of an offence since the study child's 6th birthday
                                          p2034) # Mother had an abortion since the study child's 6th birthday


# Creating a data frame containing the newly created binary PR variables 

PR_postnatal_8_9_binary <- data.frame(p2007_rec, # Mother was in trouble with the law since the study child's 6th birthday
                                      p2016_rec, # Mother's husband/partner was in trouble with the law since the study child's 6th birthday
                                      p2028_rec, # Mother attempted suicide since the study child's 6th birthday
                                      p2029_rec, # Mother was convicted of an offence since the study child's 6th birthday
                                      p2034_rec) # Mother had an abortion since the study child's 6th birthday



detach(alspac.table)

# Checking correlations btw columns of PR_postnatal_8_9_continuous and columns of PR_postnatal_8_9_binary
corbetw2mat(data.matrix(PR_postnatal_8_9_continuous), PR_postnatal_8_9_binary, what = "paired")  


####################################################################################################################################################


# INTERPERSONAL RISKS


#RECODE TO BINARY
#Split into 3 sections due to different labels

# our R data file uses factor levels (not numeric) 
# define levels first
yes = c("Yes, when the study child was 6 or 7","Yes, since the study child's 8th birthday","Yes, both when the study child was 6/7 and 8+")
no = c("No, did not happen in past 3 years")

# now check if these levels are present and no other levels were missed out 
vars = c("p2008", # Mother was divorced since the study child's 6th birthday
         "p2009", # Mother found out that her husband/partner didn't want her child since the study child's 6th birthday
         "p2015", # Mother's husband/partner went away since the study child's 6th birthday
         "p2017", # Mother and husband/partner separated since the study child's 6th birthday
         "p2019", # Mother argued with husband/partner since the study child's 6th birthday
         "p2020", # Mother argued with family and friends since the study child's 6th birthday
         "p2022", # Mother's husband/partner was physically cruel to her since the study child's 6th birthday
         "p2025", # Mother got married since the study child's 6th birthday
         "p2026", # Mother's husband/partner was physically cruel to her children since the study child's 6th birthday
         "p2027", # Mother was physically cruel to her children since the study child's 6th birthday
         "p2036", # Mother's husband/partner was emotionally cruel to her since the study child's 6th birthday
         "p2037", # Mother's husband/partner was emotionally cruel to her children since the study child's 6th birthday
         "p2038", # Mother was emotionally cruel to her children since the study child's 6th birthday
         "p2040") # Mother found a new partner since the study child's 6th birthday

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


# check new "_rec" variables are there with meaningful values
summary(alspac.table[,grep("_rec",names(alspac.table), value=T)])



# Section 2

# define levels for differently coded items
yes = c("Yes, mother did this","Yes, partner did this","Yes, both did this")
no = c("No")

vars = c("p3153", # Mother/husband/partner shouted or called one another names in the past 3 months (in Char script: DV_Shouted_9Y)
         "p3154", # Mother/husband/partner hit or slapped one another in the past 3 months (in Char script: DV_Hit_9Y)
         "p3155") # Mother/husband/partner threw or broke things in the past 3 months (in Char script: DV_Break_9Y)

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


# check new "_rec" variables are there with meaningful values
summary(alspac.table[,grep("_rec",names(alspac.table), value=T)])


# Section 3 
# define levels for differently coded items
yes = c("Every day","Several times a week","Once or twice a week","Once or twice a month", "Rarely")
no = c("Never")

vars = "ku298" # Child is slapped or hit (in Char script: Par_Smack_9Y_Any)

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


# check new "_rec" variables are there with meaningful values
summary(alspac.table[,grep("_rec",names(alspac.table), value=T)])


# Creating a data frame with the original IR variables 
attach(alspac.table)

IR_postnatal_8_9_continuous <- data.frame(p2008, # Mother was divorced since the study child's 6th birthday
                                          p2009, # Mother found out that her husband/partner didn't want her child since the study child's 6th birthday
                                          p2015, # Mother's husband/partner went away since the study child's 6th birthday
                                          p2017, # Mother and husband/partner separated since the study child's 6th birthday
                                          p2019, # Mother argued with husband/partner since the study child's 6th birthday
                                          p2020, # Mother argued with family and friends since the study child's 6th birthday
                                          p2022, # Mother's husband/partner was physically cruel to her since the study child's 6th birthday
                                          p2025, # Mother got married since the study child's 6th birthday
                                          p2026, # Mother's husband/partner was physically cruel to her children since the study child's 6th birthday
                                          p2027, # Mother was physically cruel to her children since the study child's 6th birthday
                                          p2036, # Mother's husband/partner was emotionally cruel to her since the study child's 6th birthday
                                          p2037, # Mother's husband/partner was emotionally cruel to her children since the study child's 6th birthday
                                          p2038, # Mother was emotionally cruel to her children since the study child's 6th birthday
                                          p2040, # Mother found a new partner since the study child's 6th birthday
                                          p3153, # Mother/husband/partner shouted or called one another names in the past 3 months (in Char script: DV_Shouted_9Y)
                                          p3154, # Mother/husband/partner hit or slapped one another in the past 3 months (in Char script: DV_Hit_9Y)
                                          p3155, # Mother/husband/partner threw or broke things in the past 3 months (in Char script: DV_Break_9Y)
                                          ku298) # Child is slapped or hit (in Char script: Par_Smack_9Y_Any)


# Creating a data frame containing the newly created binary IR variables 

IR_postnatal_8_9_binary <- data.frame(p2008_rec,
                                      p2009_rec,
                                      p2015_rec,
                                      p2017_rec,
                                      p2019_rec,
                                      p2020_rec,
                                      p2022_rec,
                                      p2025_rec,
                                      p2026_rec,
                                      p2027_rec,
                                      p2036_rec,
                                      p2037_rec,
                                      p2038_rec,
                                      p2040_rec,
                                      p3153_rec, # ATTENTION: in Char script this is DV_Shouted_9Y
                                      p3154_rec, # ATTENTION: in Char script this is DV_Hit_9Y
                                      p3155_rec, # ATTENTION: in Char script this is DV_Break_9Y
                                      ku298_rec) # ATTENTION: in Char script this isPar_Smack_9Y_Any


detach(alspac.table)

# Checking correlations btw columns of IR_postnatal_8_9_continuous and columns of IR_postnatal_8_9_binary
corbetw2mat(data.matrix(IR_postnatal_8_9_continuous), IR_postnatal_8_9_binary, what = "paired")  

####################################################################################################################################################

# DIRECT VICTIMIZATION


#RECODE TO BINARY

# our R data file uses factor levels (not numeric) 
# define levels first
yes = "Yes" 
no = "No"

# now check if these levels are present and no other levels were missed out 
vars = c("f8fp475", # Bullying, Child is relational victim: F8
         "f8fp470") # Bullying, Child is overt victim: F8

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


# check new "_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])



#RECODE TO BINARY

# our R data file uses factor levels (not numeric) 
# define levels first
yes = c("Yes, very upset", "Yes, quite upset", "Yes, bit upset", "Yes, not upset") 
no = "No"
# I have merged possible answers for f8fp475, f8fp470 and kt5004, kt5005, which are originally measured on a seprate scale. 
# If this causes issues in running the code, the present section should be split up according to variable response labels. 

# now check if these levels are present and no other levels were missed out 
vars = c("kt5004",  # Since 7th birthday child has been physically hurt by someone
         "kt5005")  # Since 7th birthday child has been sexually abused

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


# check new "_rec" variables are there with meaningful values
summary(alspac.table[,grep("a_rec",names(alspac.table), value=T)])



# Creating a data frame with the original DV variables 
attach(alspac.table)

DV_postnatal_8_9_continuous <- data.frame(kt5004,  # Since 7th birthday child has been physically hurt by someone
                                          kt5005)  # Since 7th birthday child has been sexually abused


# Creating a data frame containing the newly created binary DV variables 

DV_postnatal_8_9_binary <- data.frame(kt5004a_rec,  # Since 7th birthday child has been physically hurt by someone
                                      kt5005a_rec)  # Since 7th birthday child has been sexually abused


detach(alspac.table)

# Checking correlations btw columns of DV_postnatal_8_9_continuous and columns of DV_postnatal_8_9_binary
corbetw2mat(data.matrix(DV_postnatal_8_9_continuous), DV_postnatal_8_9_binary, what = "paired") 

####################################################################################################################################################