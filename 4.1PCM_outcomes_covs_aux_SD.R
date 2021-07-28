# Hi there, 
# the following code is collecting and merging all necessary variables for the analysis
# of the association between ELS and psycho-cardio-metabolic multi-morbidity in children 
# This includes the outcomes of interest (internalizing problems and cardio-metabolic
# risk), the covariates that are going to be used as well as the auxiliary variables 
# used in the imputation of the final dataset. 

#### ---------------------------- Dependencies ---------------------------- ####

# First, let's point to the necessary libraries
library(foreign)

# define a function that randomly shuffles internalizing and fat mass values and 
# returns the new size of the "randomly multimorbid" group.
permute <- function(df) { 
  # create empty dataset for permutation
  perm <- data.frame(1:nrow(df))
  
  perm$new_int <- sample(df$int)
  perm$new_fat <- sample(df$fat)
  perm$new_groups <- ifelse(perm$new_int == 0 & perm$new_fat == 0, 0, 
                            ifelse(perm$new_int == 1 & perm$new_fat == 0, 1, 
                                   ifelse(perm$new_int == 0 & perm$new_fat == 1, 2, 3)))
  new_n <- unname(summary(as.factor(perm$new_groups))[4])
  return(new_n)
}

# Check if the path to the data is already in memory, otherwise ask for it. 
if (exists("alspac_file") == F) { 
  alspac_file <- file.choose() 
  alspac_folder <- dirname(alspac_file) 
  # Read in the data
  alspac.table <- foreign::read.spss(alspac_file, use.value.label=TRUE, to.data.frame=TRUE) }

dep <- readRDS(file.path(alspac_folder, "raw_parent_depr_anxiety.rsd"))

# Change all names to lowercase
names(alspac.table)=tolower(names(alspac.table))

# Initiate a cov_out dataframe with id of the child as first column
cov_out <- data.frame("IDC" = paste(alspac.table$cidb2957, alspac.table$qlet, sep = "_"))

################################################################################
#### ------------------ INTERNALIZING PROBLEMS ( @ 9 ) -------------------- ####
################################################################################

# Internalizing scale @ 9 yrs # informant: MOTHER.
cov_out$int.age.10y  <-  as.numeric(levels(alspac.table$kv9991a))[alspac.table$kv9991a] / 12  # age 10 (years)
cov_out$intern_score <-  as.numeric(levels(alspac.table$kv8603))[alspac.table$kv8603] # 10yr Depression (parent 6-band computer prediction, ICD-10 and DSM-IV)

cov_out$peer_probs <- as.numeric(levels(alspac.table$ku709a))[alspac.table$ku709a]
cov_out$emot_symp <- as.numeric(levels(alspac.table$ku707a))[alspac.table$ku707a]


#Adding SDQ (contionus) internlaising score from the sum of he SDQ peer problems and emotional symptoms subscales
cov_out$SDQ_int_score <- rowSums(cov_out[,c('peer_probs', 'emot_symp')]) #SDQ peer problems + SDQ emotional symptoms subscale
cov_out$SDQ_int_age <- as.numeric(levels(alspac.table$ku991a)) /12


# cov_out$int.age.13y      <- as.numeric(levels(alspac.table$tb9991a))[alspac.table$tb9991a] # age 13 
# cov_out$intern_score.13y <- as.numeric(levels(alspac.table$tb8603))[alspac.table$tb8603]   # 13yr Depression (parent 6-band computer prediction, ICD-10 and DSM-IV)    
# cov_out$int.age.15y      <- as.numeric(levels(alspac.table$fh0011a))[alspac.table$fh0011a] # age 15 
# cov_out$intern_score.15y <- as.numeric(alspac.table$fh6876)                                # 15yr Depression (parent 6-band computer prediction, ICD-10 and DSM-IV)   
# cov_out$int.age.17y      <- as.numeric(levels(alspac.table$fj003a))[alspac.table$fj003a]   # age 17 VB: not in dataset? but was in original?
# cov_out$intern_score.17y <- as.numeric(levels(alspac.table$fjci350))[alspac.table$fjci350] # age 17 (CIS-R) 
# cov_out$int.age.22y      <- as.numeric(levels(alspac.table$ypb9992))[alspac.table$ypb9992] # age 22 VB: also not in?
# cov_out$intern_score.22y <- as.numeric(levels(alspac.table$ypb5180))[alspac.table$ypb5180] # age 22 (SMFQ; might not be the best measure)

################################################################################
#### -------------------------- FAT MASS ( @ 9 ) -------------------------- ####
################################################################################

# Although the original plan for this project involved a more comprehensive measure
# of child metabolic syndrome (see CMR_score.R file for specifics), android fat mass
# was the only metabolic risk variable that showed appreciable variance in such young 
# children and the highest correlation with internalizing (small but significant r =.12)
# It was also selected on the base of data availability both cross-sectionally and 
# for future longitudinal assessment. 

# Converting months @ 10y to years (coded F9 due to focus 9 clinic age in ALSPAC)
cov_out$fm.age.10y <- as.numeric(as.character(alspac.table$f9003c)) / 12             # age 10 (years)
cov_out$fat_mass   <- as.numeric(levels(alspac.table$f9dx126))[alspac.table$f9dx126] # trunk FM at age 10y
## ANDR @10 NOT AVAILABLE

cov_out$fat_mass_tot<- as.numeric(levels(alspac.table$f9dx135))[alspac.table$f9dx135] # total FM at age 10y
cov_out$height_10y <- as.numeric(levels(alspac.table$pub203))[alspac.table$pub203] / 100

cov_out$fmi <- cov_out$fat_mass_tot / ((cov_out$height_10y)^2)

# cov_out$fm.age.13y   <- as.numeric(levels(alspac.table$kg998a))[alspac.table$kg998a]     # age 13
# cov_out$fat_mass.13y <- as.numeric(levels(alspac.table$fg3257))[alspac.table$fg3257]     # andr FM at age 13y
# cov_out$fat_mass.15y <- as.numeric(levels(alspac.table$fh2257))[alspac.table$fh2257]     # andr FM at age 15y
# cov_out$fat_mass.17y <- as.numeric(levels(alspac.table$fjdx138))[alspac.table$fjdx138]   # andr FM at age 17y
# cov_out$fm.age.24y   <- as.numeric(levels(alspac.table$fkar0010))[alspac.table$fkar0010] # age 24
# cov_out$fat_mass.24y <- as.numeric(levels(alspac.table$fkdx1041))[alspac.table$fkdx1041] # andr FM at age 24y

# ------------------------------------------------------------------------------
# Before we can use them in the analysis, the outcome variables need to be standardized. 
# so, here we take the standard deviation score.
cov_out$intern_score_z <- as.numeric(scale(cov_out$intern_score))
cov_out$SDQ_score_z <- as.numeric(scale(cov_out$SDQ_int_score))
# cov_out$intern_score_z.13y <- as.numeric(scale(cov_out$intern_score.13y))
# cov_out$intern_score_z.15y <- as.numeric(scale(cov_out$intern_score.15y))
# cov_out$intern_score_z.17y <- as.numeric(scale(cov_out$intern_score.17y))
# cov_out$intern_score_z.22y <- as.numeric(scale(cov_out$intern_score.22y))

cov_out$fat_mass_z <- as.numeric(scale(cov_out$fat_mass))
# cov_out$fat_mass_z.13y <- as.numeric(scale(cov_out$fat_mass.13y))
# cov_out$fat_mass_z.15y <- as.numeric(scale(cov_out$fat_mass.15y))
# cov_out$fat_mass_z.17y <- as.numeric(scale(cov_out$fat_mass.17y))
# cov_out$fat_mass_z.24y <- as.numeric(scale(cov_out$fat_mass.24y))

cov_out$fmi_z <- as.numeric(scale(cov_out$fmi))
cov_out$fat_mass_tot_z <- as.numeric(scale(cov_out$fat_mass_tot))

################################################################################
#### ------------------- Construct RISK GROUPS variable ------------------- ####
################################################################################

cov_out$int = ifelse(cov_out$SDQ_score_z > quantile(cov_out$SDQ_score_z, probs = 0.8, na.rm = T), 1, 0) 
cov_out$fat = ifelse(cov_out$fat_mass_z     > quantile(cov_out$fat_mass_z,     probs = 0.8, na.rm = T), 1, 0) 
cov_out$fat = ifelse(cov_out$fmi_z     > quantile(cov_out$fmi_z,     probs = 0.8, na.rm = T), 1, 0) 
cov_out$fat = ifelse(cov_out$fat_mass_tot_z     > quantile(cov_out$fat_mass_tot_z,     probs = 0.8, na.rm = T), 1, 0) 

cov_out$risk_groups = rep(NA, nrow(cov_out))
for (i in 1:nrow(cov_out)) {
  if ( is.na(cov_out$int[i]) | is.na(cov_out$fat[i]) )  { cov_out$risk_groups[i] = NA
  } else if (cov_out$int[i] == 0 & cov_out$fat[i] == 0) { cov_out$risk_groups[i] = 0   # Healthy
  } else if (cov_out$int[i] == 1 & cov_out$fat[i] == 0) { cov_out$risk_groups[i] = 1   # High internalizing  only
  } else if (cov_out$int[i] == 0 & cov_out$fat[i] == 1) { cov_out$risk_groups[i] = 2   # High fat mass only
  } else {                                                cov_out$risk_groups[i] = 3 } # Multimorbid
}

# # Let's first factor that bad boy 
cov_out$risk_groups = factor(cov_out$risk_groups, 
                             levels = c(0:3), 
                             labels = c("healthy", "internalizing_only", "cardiometabolic_only", "multimorbid"))

summary(cov_out$risk_groups)

# attach(cov_out); plot(intern_score_z, fat_mass_z, 
# col=c("cornflowerblue","black", "red", "darkgrey", "darkgoldenrod2","chartreuse4")[risk_groups]); detach(cov_out)


################################################################################
#### ---------------------------- COVARIATES ------------------------------ ####
################################################################################

# Variables that will be used in the covariate models of this project are those 
# marked with ###. they include: 'sex', 'age_child', 'm_smoking', 'm_drinking' 
# and 'm_bmi_before_pregnancy'.

# For the other demographic auxiliary variables (used for imputation): when they 
# were assessed both prenatally and postnatally, both measures are included.
# Auxiliary variables for this project include: 'ethnicity', 'm_age_cont', parity', 
# 'gest_age_birth', 'gest_weight', 'm_bmi_pregnancy', 'm_bmi_7y',
# 'm_dep_pregnancy', 'p_dep_pregnacy', "m_dep_3y", "p_dep_3y"

# ------------------------------------------------------------------------------
### SEX of the child
cov_out$sex <- alspac.table$kz021 # 1 = Male; 2 = Female.

# ------------------------------------------------------------------------------
### AGE of the child
# Combine age of the child measured during first visit and at CBCL administration
# This value will serve as a covariate in the first adjusted model.

cov_out$age_child <- (cov_out$int.age.10y + cov_out$fm.age.10y) / 2
# cov_out$age_child.13y <- (cov_out$int.age.13y + cov_out$fm.age.13y) / 2
# cov_out$age_child.15y <-  cov_out$int.age.15y
# cov_out$age_child.17y <-  cov_out$int.age.17y
# cov_out$age_child.23y <- (cov_out$int.age.22y + cov_out$fm.age.24y) / 2

# OPTIONAL: check age difference between measurements
plot(cov_out$int.age.10y, cov_out$fm.age.10y)
summary(cov_out$int.age.10y - cov_out$fm.age.10y)

#-------------------------------------------------------------------------------
### MATERNAL SMOKING during pregnancy 
# 3 categories (never, former and current smoker)

# Ever smoked: no = 0, yes = 1
cov_out$b650r <- ifelse(alspac.table$b650 == 'N', 0, ifelse(alspac.table$b650 == 'Y', 1, NA)) 
# CIGS smoked per day during pregnancy: none = 0, occasionally or >1 = 1
cov_out$c482r <- ifelse(alspac.table$c482 == 'None', 0, ifelse(alspac.table$c482 != 'DK', 1, NA)) 

cov_out$m_smoking <- ifelse(cov_out$b650r == 0 & cov_out$c482r == 0, 0,        # Never a smoker
                            ifelse(cov_out$b650r == 1 & cov_out$c482r == 0, 1, # Former smoker
                                   ifelse(cov_out$c482r == 1, 2, NA)))         # Current smoker

#-------------------------------------------------------------------------------
### MATERNAL ALCOHOL CONSUMPTION during pregnancy
# Combined maternal drinking during the first and the last trimester of pregnancy.

# Alcohol consumption in 1-3MTHS this PREG
cov_out$b721r <- ifelse(alspac.table$b721 == 'never', 0,   
                        ifelse(alspac.table$b721 == '<1 glass PWK', 1,
                               ifelse(alspac.table$b721 == '1+ glasses PWK', 2, 
                                      ifelse(alspac.table$b721 == '1-2 glasses PDAY', 3, 
                                             ifelse(alspac.table$b721 == '3-9 glasses PDAY', 4, 
                                                    ifelse(alspac.table$b721 == '10+ glasses PDAY', 5, NA))))))
# FREQ of alcohol use in last 2MTHS of PREG
cov_out$e220r <- ifelse(alspac.table$e220 == 'Not at all', 0, 
                        ifelse(alspac.table$e220 == '<1PWK', 1,
                               ifelse(alspac.table$e220 == 'At least 1PWK', 2, 
                                      ifelse(alspac.table$e220 == '1-2 glasses daily', 3, 
                                             ifelse(alspac.table$e220 == '3-9 glasses daily', 4, 
                                                    ifelse(alspac.table$e220 == '>9 glasses daily', 5, NA))))))

cov_out$m_drinking <- rowSums(cov_out[, c('b721r', 'e220r')], na.rm = T)


#-------------------------------------------------------------------------------
## Other variables

# Ethnicity
cov_out$ethnicity <- ifelse(is.na(alspac.table$c800) & is.na(alspac.table$c801), NA, 
                            ifelse((alspac.table$c800 == 'White' & alspac.table$c801 == 'White') | 
                                     ((is.na(alspac.table$c800) | is.na(alspac.table$c801)) & 
                                        (alspac.table$c800 == 'White' | alspac.table$c801 == 'White')), 1, 0))

# Maternal weight (Kg)
cov_out$weight_pre <- as.numeric(levels(alspac.table$dw002))[alspac.table$dw002] # Pre-pregnancy weight (Kg)
cov_out$weight_8wk <- as.numeric(levels(alspac.table$ew002))[alspac.table$ew002] # kg 8w
cov_out$weight_7y  <- as.numeric(levels(alspac.table$m4220))[alspac.table$m4220] # kg 7y
cov_out$weight_8y  <- as.numeric(levels(alspac.table$n1140))[alspac.table$n1140] # kg 8y
cov_out$weight_9y  <- as.numeric(levels(alspac.table$p1290))[alspac.table$p1290] # kg 9y
cov_out$weight_avg <- rowMeans(cov_out[, c('weight_8wk','weight_7y','weight_8y','weight_9y')], na.rm = T)

# Maternal height (cm) 7y 1m (we treat height as a constant)
cov_out$height_7y <- as.numeric(levels(alspac.table$m4221))[alspac.table$m4221] / 100 # transform into meters

# BMI
cov_out$m_bmi_before_pregnancy <- cov_out$weight_pre / ((cov_out$height_7y)^2) # calculating pregnancy BMI 
cov_out$m_bmi_7yrs             <- cov_out$weight_7y  / ((cov_out$height_7y)^2) # calculating BMI at age 7
cov_out$m_bmi_childhood        <- cov_out$weight_avg / ((cov_out$height_7y)^2) # calculating childhood BMI 

# Siblings identifier
cov_out$sibling <- ifelse(alspac.table$mult == 1, 1, ifelse(alspac.table$mult == 0, 0, NA))  # multiple pregnancies in ALSPAC (exclusion criteria)

# Twin identifier
cov_out$twin <- rep(0, nrow(cov_out))
cov_out$twin[which(duplicated(alspac.table$cidb2957) | duplicated(alspac.table$cidb2957, fromLast = T))] <- 1

# Auxiliary variables (for imputation)
cov_out$parity <- as.numeric(levels(alspac.table$b032))[alspac.table$b032]  # parity (used for imputation)

# try <- cov_out[cov_out$sibling == 1, ]
# summary(try$parity)

cov_out$gest_age_birth <- as.numeric(levels(alspac.table$bestgest))[alspac.table$bestgest] # gestational age at birth (used for imputation)
cov_out$gest_weight    <- as.numeric(levels(alspac.table$kz030))[alspac.table$kz030]       # gestational weight (used for imputation)
cov_out$m_age_cont     <- as.numeric(levels(alspac.table$mz028b))[alspac.table$mz028b]     # maternal age at intake (used for imputation) 

#-------------------------------------------------------------------------------
# Maternal and paternal depression during pregnancy and childhood are calculated 
# in the CCEI EPDS script. 

# Prenatal maternal depression
cov_out$m_dep_cont_pregnancy <- dep$m_EPDS_total_18wg + dep$m_EPDS_total_32wg
# Postnatal maternal depression
post_dep_m <- sapply(dep[, c('f200', 'g290', 'h200a')], as.integer) 
cov_out$m_dep_cont_childhood <- rowSums(post_dep_m, na.rm = T)

# Prenatal paternal depression
cov_out$p_dep_cont_pregnancy <- dep$p_EPDS_total_18wg
# Postnatal paternal depression
post_dep_p <- sapply(dep[, c('pe290', 'pd200')], as.integer) 
cov_out$p_dep_cont_childhood <- rowSums(post_dep_p, na.rm = T)

#------------------------------------------------------------------------------#
# ------------------------- PERMUTATION TESTING -------------------------------#
#------------------------------------------------------------------------------#

count <- 0 
itarations <- 1000
set.seed(310896)

for (i in 1:itarations) {
  origN <- unname(summary(cov_out$risk_groups)[4])
  randN <- permute(cov_out)
  if (randN > origN) {
    count <- count + 1
  }
}

pval = round(count / itarations, 10)

################################################################################
#### --------------------------- save and run ----------------------------- ####
################################################################################

# Save the dataset in the directory where you have the raw data
saveRDS(cov_out, file.path(alspac_folder, "PCMout_cov_aux.rsd"))

# Also save the dataset in a .csv format
write.csv(cov_out, file = "PCMout_cov_aux.csv", row.names = FALSE, quote = FALSE)

