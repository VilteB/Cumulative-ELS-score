####################################################################################################################################################

# PRENATAL

####################################################################################################################################################


library(foreign)

# Reading in the data
alspac.table <- read.spss("X:/Psychology/ResearchProjects/EWalton/EarlyCause/data/ALSPAC/EarlyCause_AHupdated_CIDB2957_25MAY21.sav", use.value.label=TRUE, to.data.frame=TRUE) 

####################################################################################################################################################


# This script is used for dichotomising prenatal ELS variables into 1 = risk and 0 = no risk. 

# 1. LIFE EVENTS

# Originally, variables have been coded as:

# 1 = affected a lot
# 2 = fairly affected
# 3 = mildly affected
# 4 = not effected at all
# 5 = didn't happen
# for total number of options inlcuded need to check the dataset 

# Below script recodes them into: 

# 1 = risk (for values between 1-4)
# 0 = no risk (for value of 5) 
# any other number = NA (missing)


#RECODE TO BINARY

# our R data file uses factor levels (not numeric) 
# define levels first
yes = c("affected a lot","fairly affected","mildly affected","N effect at all")
no = c("didnt happen")

# now check if these levels are present and no other levels were missed out 
vars = c("b570", # PTNR died since PREG
         "b571", # CH died since PREG
         "b572", # Friend or relative died since PREG
         "b573", # CH was ill since PREG	
         "b574", # PTNR was ill since PREG	
         "b575", # Friend or relative was ill since PREG
         "b576", # Admitted to hospital since PREG
         "b580", # V ill since PREG
         "b581", # PTNR lost job since PREG
         "b582", # PTNR had PROBS at work since PREG	
         "b583", # PROBS at work since PREG
         "b584", # Lost job since PREG		
         "b591", # Moved house since PREG	
         "b599", # Bled & thought might miscarry	
         "b600", # Started new job since PREG
         "b601", # Test to see if baby abnormal
         "b602", # Test result suggesting POSS abnormality	
         "b603", # Told having twins
         "b604", # POSS harm to baby
         "b606", # Took an exam since PREG
         "b609", # House or car burgled since PREG
         "b610") # Had an accident since PREG

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

LE_prenatal_continuous <- data.frame(b570, # PTNR died since PREG
                                     b571, # CH died since PREG
                                     b572, # Friend or relative died since PREG
                                     b573, # CH was ill since PREG	
                                     b574, # PTNR was ill since PREG	
                                     b575, # Friend or relative was ill since PREG
                                     b576, # Admitted to hospital since PREG
                                     b580, # V ill since PREG
                                     b581, # PTNR lost job since PREG
                                     b582, # PTNR had PROBS at work since PREG	
                                     b583, # PROBS at work since PREG
                                     b584, # Lost job since PREG		
                                     b591, # Moved house since PREG	
                                     b599, # Bled & thought might miscarry	
                                     b600, # Started new job since PREG
                                     b601, # Test to see if baby abnormal
                                     b602, # Test result suggesting POSS abnormality	
                                     b603, # Told having twins
                                     b604, # POSS harm to baby
                                     b606, # Took an exam since PREG
                                     b609, # House or car burgled since PREG
                                     b610) # Had an accident since PREG

# Creating a data frame containing the newly created binary LE variables

LE_prenatal_binary <- data.frame(b570a_rec, # PTNR died since PREG
                                 b571a_rec, # CH died since PREG
                                 b572a_rec, # Friend or relative died since PREG
                                 b573a_rec, # CH was ill since PREG	
                                 b574a_rec, # PTNR was ill since PREG	
                                 b575a_rec, # Friend or relative was ill since PREG
                                 b576a_rec, # Admitted to hospital since PREG
                                 b580a_rec, # V ill since PREG
                                 b581a_rec, # PTNR lost job since PREG
                                 b582a_rec, # PTNR had PROBS at work since PREG	
                                 b583a_rec, # PROBS at work since PREG
                                 b584a_rec, # Lost job since PREG		
                                 b591a_rec, # Moved house since PREG	
                                 b599a_rec, # Bled & thought might miscarry	
                                 b600a_rec, # Started new job since PREG
                                 b601a_rec, # Test to see if baby abnormal
                                 b602a_rec, # Test result suggesting POSS abnormality	
                                 b603a_rec, # Told having twins
                                 b604a_rec, # POSS harm to baby
                                 b606a_rec, # Took an exam since PREG
                                 b609a_rec, # House or car burgled since PREG
                                 b610a_rec) # Had an accident since PREG


detach(alspac.table)

#install package 'lineup' which contains the 'corbetw2mat' function
library(lineup)

#Checking correlations btw columns of LE_prenatal_continuous and columns of LE_prenatal_binary
#corbetw2mat(LE_prenatal_continuous, LE_prenatal_binary, what = "paired")    

# for factor-based dataframe
corbetw2mat(data.matrix(LE_prenatal_continuous), LE_prenatal_binary, what = "paired")    

#Above correlattions are equivalent to: cor(b570, b570a_rec, use="pairwise", method = "pearson")


####################################################################################################################################################

# 2. CONTEXTUAL RISKS



# our R data file uses factor levels (not numeric) 
# define levels first
yes = c("affected a lot","fairly affected","mildly affected","N effect at all")
no = c("didnt happen")

# now check if these levels are present and no other levels were missed out 
vars = c("b588", # Income reduced since PREG
         #"b593", # Became homeless since PREG
         "b594") # Major financial PROB since PREG


for (i in vars){
  
  # check if required levels are present / unexpected levels are not present
  print(i)  
  print(levels(alspac.table[,i])[levels(alspac.table[,i]) %in% c(yes,no)])
  print(levels(alspac.table[,i])[!levels(alspac.table[,i]) %in% c(yes,no)])
  
  #readline(prompt = "levels ok? Press [enter] to continue")
}

# EW: excluded "b593", as already binary

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

CR_prenatal_continuous <- data.frame(b588, # Income reduced since PREG
                                     b593, # Became homeless since PREG
                                     b594) # Major financial PROB since PREG

# Creating a data frame containing the newly created binary CR variables

CR_prenatal_binary <- data.frame(b588a_rec, # Income reduced since PREG
                                 b593, # Became homeless since PREG; EW: not b593a_rec, as already binary
                                 b594a_rec) # Major financial PROB since PREG


detach(alspac.table)

# Checking correlations btw columns of CR_prenatal_continuous and columns of CR_prenatal_binary
#corbetw2mat(CR_prenatal_continuous, CR_prenatal_binary, what = "paired")  
corbetw2mat(data.matrix(CR_prenatal_continuous), CR_prenatal_binary, what = "paired")  


####################################################################################################################################################


# 3. PARENTAL RISKS 


#RECODE TO BINARY

# our R data file uses factor levels (not numeric) 
# define levels first
yes = c("affected a lot","fairly affected","mildly affected","N effect at all")
no = c("didnt happen")

# now check if these levels are present and no other levels were missed out 
vars = c("b577", # In trouble with the law since PREG
         "b586", # PTNR in trouble with law since PREG
         "pb188", # PTNR convicted of an offence since PREG 
         "b605") # Tried to have abortion

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

PR_prenatal_continuous <- data.frame(b577, # In trouble with the law since PREG
                                     b586, # PTNR in trouble with law since PREG
                                     b597, # Attempted suicide since PREG
                                     b598, # Convicted of an offence since PREG
                                     b605) # Tried to have abortion

# Creating a data frame containing the newly created binary PR variables 

PR_prenatal_binary <- data.frame(b577a_rec, # In trouble with the law since PREG
                                 b586a_rec, # PTNR in trouble with law since PREG
                                 b597, # Attempted suicide since PREG; EW: already binary
                                 b598, # Convicted of an offence since PREG; EW: already binary
                                 b605a_rec) # Tried to have abortion


detach(alspac.table)

# Checking correlations btw columns of PR_prenatal_continuous and columns of PR_prenatal_binary
#corbetw2mat(PR_prenatal_continuous, PR_prenatal_binary, what = "paired")  
corbetw2mat(data.matrix(PR_prenatal_continuous), PR_prenatal_binary, what = "paired")  


####################################################################################################################################################


# INTERPERSONAL RISKS


#RECODE TO BINARY

# our R data file uses factor levels (not numeric) 
# define levels first
yes = c("affected a lot","fairly affected","mildly affected","N effect at all")
no = c("didnt happen")

# now check if these levels are present and no other levels were missed out 
vars = c(#"b578", # Divorced since PREG; EW: already binary
  "b579", # PTNR rejected PREG
  "b585", # PTNR went away since PREG	
  #"b587", # Separated since PREG; EW: already binary	
  "b589", # Argued with PTNR since PREG
  "b590", # Argued with family or friends since PREG
  "b592", # PTNR hurt mum since PREG	
  "b595", # Got married since PREG		
  "b596", # PTNR hurt CH since PREG	
  "b607") #, # PTNR was EMOT cruel to mum since PREG
#"b608") # PTNR was EMOT cruel to mum since PREG; EW: already binary


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

IR_prenatal_continuous <- data.frame(b578, # Divorced since PREG
                                     b579, # PTNR rejected PREG
                                     b585, # PTNR went away since PREG	
                                     b587, # Separated since PREG	
                                     b589, # Argued with PTNR since PREG
                                     b590, # Argued with family or friends since PREG
                                     b592, # PTNR hurt mum since PREG	
                                     b595, # Got married since PREG		
                                     b596, # PTNR hurt CH since PREG	
                                     b607, # PTNR was EMOT cruel to mum since PREG
                                     b608) # PTNR was EMOT cruel to mum since PREG

# Creating a data frame containing the newly created binary IR variables 

IR_prenatal_binary <- data.frame(b578, # Divorced since PREG; EW: already binary
                                 b579a_rec, # PTNR rejected PREG
                                 b585a_rec, # PTNR went away since PREG		
                                 b587, # Separated since PREG; EW: already binary		
                                 b589a_rec, # Argued with PTNR since PREG	
                                 b590a_rec, # Argued with family or friends since PREG
                                 b592a_rec, # PTNR hurt mum since PREG	
                                 b595a_rec, # Got married since PREG		
                                 b596a_rec, # PTNR hurt CH since PREG	
                                 b607a_rec, # PTNR was EMOT cruel to mum since PREG
                                 b608) # PTNR was EMOT cruel to CH since PREG; EW: already binary


detach(alspac.table)

# Checking correlations btw columns of IR_prenatal_continuous and columns of IR_prenatal_binary
#corbetw2mat(IR_prenatal_continuous, IR_prenatal_binary, what = "paired")  
corbetw2mat(data.matrix(IR_prenatal_continuous), IR_prenatal_binary, what = "paired")  


####################################################################################################################################################

# Recoding prenatal depression variable for mother 
alspac.table$b371n <- as.character(alspac.table$b371) # not adding as.numeric to avoid converting labels
alspac.table$b371a_rec  <- ifelse(alspac.table$b371n %in% c(13:28), 1,
                                  ifelse(alspac.table$b371n == 'very depressed', 1, 
                                         ifelse(alspac.table$b371n %in% c(1:12), 0,
                                                ifelse(alspac.table$b371n == 'not depressed', 0, NA))))


# Recoding prenatal depression variable for partner (EPDS)
# EPDS total score partner mode imputed (>12 risk, <=12 no risk) based on ALSPAC FAI documentation
alspac.table$pb261n <- as.numeric(as.character(alspac.table$pb261))
alspac.table$pb261a_rec  <- ifelse(alspac.table$pb261n %in% c(13:27), 1,
                                   ifelse(alspac.table$pb261n %in% c(0:12), 0, NA)) 

# Recoding prenatal anxiety variable for partner (CCEI)
# A cut point which has been used previously in ALSPAC (Heron et al., 2004): >8 is risk, <= 8 no risk
alspac.table$pb234n <- as.character(alspac.table$pb234)
alspac.table$pb234a_rec  <- ifelse(alspac.table$pb234n %in% c(9:15), 1,
                                   ifelse(alspac.table$pb234n == 'Very anxious', 1,
                                          ifelse(alspac.table$pb234n %in% c(0:8), 0, 
                                                 ifelse(alspac.table$pb234n == 'Not anxious', 0, NA)))) 


# p_interpersonal_sensitivity, using 80th percentile

# Higher scores = greater interpersonal sensitivity, based on items such as 'feel insecure when saying goodbye'

# checking the distribution and dichotomising based on 80th percentile
plot(alspac.table$pb551) 

# changing a factor to numeric without changing values 
alspac.table$pb551n <- as.numeric(as.character(alspac.table$pb551)) 
quantile(alspac.table$pb551n, .8, na.rm = T) # 80th percentile is 95

alspac.table$pb551a_rec <- ifelse(alspac.table$pb551n >= 95, 1, 
                                  ifelse(alspac.table$pb551n < 95, 0, NA)) 

# checking if recoding worked
data.frame(alspac.table$pb551a_rec, alspac.table$pb551) # looks good


####################################################################################################################################################




