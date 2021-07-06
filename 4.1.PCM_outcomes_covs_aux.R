# Hi there, 
# the following code is collecting and merging all necessary variables for the analysis
# of the association between ELS and psycho-cardio-metabolic multi-morbidity in children 
# This includes the outcomes of interest (internalizing problems and cardio-metabolic
# risk), the covariates that are going to be used as well as the auxiliary variables 
# used in the imputation of the final dataset. 
# It does not include data used to build the ELS score exposure, for which you can 
# check out https://github.com/SereDef/cumulative-ELS-score

## For this script you are going to need the following datasets from datamanagement
# CHILDCBCL9_10082016.sav, CHILDFATMASS9_13092016.sav, 
# CHILD-ALLGENERALDATA_07072020.sav, GEDRAGSGROEP_MaternalDrinking_22112016.sav, 
# MATERNALSMOKING_22112016.sav, GEDRAGSGROEP_MaternalDrinking_22112016.sav, 
# MOTHERANTHROPOMETRY_18022013.sav, GR1003-BSI D1_22112016.sav, and 
# BSI 3 years of age_GR1065 G1-GR1066 C1_22112016.sav, GR1004-BSI G1_22112016.sav

#### ---------------------------- Dependencies ---------------------------- ####

# First, let's point to the necessary libraries
library(foreign)
library(stats)

# Defining the path to the data
# check if the path to the data is already in memory, otherwise ask for it. 

#if (exists("pathtodata") == F) { pathtodata = readline(prompt="Enter path to data: ") }

# ATTENTION!!! If prompted with an "Enter path to data:" message -> Enter the location
# of your datafiles. The code assumes that all (raw) data is stored in ONE folder.
# Do not forget the final slash in your path, and, speaking of slashes, beware of 
# OS sensitive changes when you want to modify the structure of your dirs!

#### ------------------------------ FUNCTIONS ----------------------------- ####

# Read in the data and fix all SPSS weird missing codes into NAs
# readquick <- function(filename, rootdir = pathtodata, exclude_col = "") { # only works for SPSS files
#   dat <- read.spss(paste(rootdir, filename, sep=""), 
#                    use.value.labels = F, to.data.frame = T)
#   # Get rid of all capital letters in column names (so you don't have to worry)
#   names(dat) <- tolower(names(dat))
#   # Replace values of 777, 888 or 999 with NAs unless they are IDCs or IDMs 
#   # If you do not want this to happen for any other column use the exclude_col argument. 
#   for (i in 1:length(dat)) {
#     if (colnames(dat)[i] == "idm" | colnames(dat)[i] == "idc" | colnames(dat)[i] == exclude_col) {
#       dat[,i] <- dat[,i]
#     } else {
#       dat[,i] <- ifelse(dat[,i] == 777 | dat[,i] == 888 | dat[,i] == 999, NA, dat[,i]) }
#   } 
#   return(dat)
# }

# -----------------------------------------------------------------------------#
##                        Ok, we are good to go now!                          ##
# -----------------------------------------------------------------------------#

################################################################################
#### ------------------ INTERNALIZING PROBLEMS ( @ 9 ) -------------------- ####
################################################################################

# Read in the dataset
# cbcl <- readquick("CHILDCBCL9_10082016.sav") # 9901 obs. of 627 vars

# The internalizing sub-scale of the Child Behavior Checklist (CBCL 6-18) (Achenbach, 1999)
# is an empirically based score derived from the widely used parent-report questionnaire. 
# It contains 32 items rated on a 3-point scale (0 = "Not true", 1 = "Somewhat or 
# sometimes true", 2 = "Very or often true") and referred to the past 6 months. 
# Hence the score ranges from 0 to 64. 

names(alspac.table)=tolower(names(alspac.table))

# Internalizing scale @ 9 yrs # informant: MOTHER.
internalizing = alspac.table[,c('cidb2957', # note: lowercase 'b' here
                                'qlet',
                                'kv9991a', #age 10 (months)
                                'tb9991a', #age 13
                                'fh0011a', # age 15
                                'fj003a', # age 17?  VB: not in dataset? but was in original?
                                'ypb9992', # age 22 VB: also not in?
                                'kv8603',# age 10      #'agechild_cbcl9m',
                                'tb8603', # age 13
                                'fh6876', # age 15
                                'fjci350', # age 17 (CIS-R) 
                                'ypb5180')] # age 22 (SMFQ; might not be the best measure)
#'nmisint_9m', # number of missing values in internalizing scale items
#'sum_int_9m')] # weighted sum score internalizing scale (allowing 25% missing)


for (i in c('kv9991a',
            'tb9991a',
            'fh0011a',
            'fj003a', 
            'ypb9992',
            'kv8603',
            'tb8603',
            'fjci350',
            'ypb5180')){
  internalizing[,i] = as.numeric(levels(internalizing[,i]))[internalizing[,i]]
}

internalizing$fh6876=as.numeric(internalizing$fh6876)

#corbetw2mat(internalizing.n[,-c(1:2)], internalizing[,-c(1:2)], what = "paired")  

# Calculation (based on SPSS script available on the V: drive): 
# if 24 out of 32 items are available (i.e. 75%), sum of the item
# scores * (32 / 32 - nmisnt_9m). 
# Let's make it a bit more reader friendly 

#colnames(internalizing)[3:4] <- c("n_missing_intern", "intern_score"); 

internalizing = internalizing %>% 
  rename(
    int.age.10y = kv9991a,
    int.age.13y = tb9991a,
    int.age.15y = fh0011a,
    int.age.17y = fj003a,
    int.age.22y = ypb9992,
    intern_score.10y = kv8603,
    intern_score.13y = tb8603,
    intern_score.15y = fh6876,
    intern_score.17y = fjci350,
    intern_score.22y = ypb5180
  )
# One alternative could be to use the anxious/depressed empirical sub-scale (9y, mother report)
# However I did give it a shot and it does not seem to perfom better than the internalizing one.

################################################################################
#### -------------------------- FAT MASS ( @ 9 ) -------------------------- ####
################################################################################

# Although the original plan for this project involved a more comprehensive measure
# of child metabolic syndrome (see CMR_score.R file for specifics), android fat mass
# was the only metabolic risk variable that showed appreciable variance in such young 
# children and the highest correlation with internalizing (small but significant r =.12)
# It was also selected on the base of data availability both cross-sectionally and 
# for future longitudinal assessment. 

# Read in the datasets
#fatmass <- readquick("CHILDFATMASS9_13092016.sav") # 5862 obs of 28 vars

# Select only the necessary measures
fat_mass = alspac.table[,c('cidb2957','qlet',
                           'f9003c',   # age 10 (months)              #'agechild9_visit1', # this value (age) is the same for all other datasets
                           #'kg998a', # age13
                           #'fkar0010', # age24
                           'f9dx126',  #trunkFM at age10y   #'fat_mass_androidchild9')]
                           #'fg3257',   # andrFM at age13y
                           #'fh2257',   # andrFM at age15y
                           'fjdx138',   # andrFM at age17y
                           #'fkdx1041'  #andrFM at age24y
                           )] 


for (i in c('f9003c',
            'kg998a',
            'fkar0010',
            'f9dx126', 
            'fg3257',  
            'fh2257',  
            'fjdx138', 
            'fkdx1041')){
  fat_mass[,i] = as.numeric(levels(fat_mass[,i]))[fat_mass[,i]]
}


#colnames(fat_mass)[3] <- "fat_mass"

fat_mass = fat_mass %>% 
  rename(
    fm.age.10y = f9003c,
    fm.age.13y = kg998a,
    fm.age.24y = fkar0010,
    fat_mass.10y = f9dx126,
    fat_mass.13y = fg3257,
    fat_mass.15y = fh2257,
    fat_mass.17y = fjdx138,
    fat_mass.24y = fkdx1041
  )

################################################################################
# merge the two main (mental and physical) outcomes with child sex into one dataset
PCM_outcome <- merge(internalizing, fat_mass, by = c('cidb2957','qlet'),  all.x = TRUE)

# ------------------------------------------------------------------------------
# Before we can use them in the analysis, the outcome variables need to be standardized. 
# so, here we take the standard deviation score.
PCM_outcome$intern_score_z.10y <- as.numeric(scale(PCM_outcome$intern_score.10y))
PCM_outcome$intern_score_z.13y <- as.numeric(scale(PCM_outcome$intern_score.13y))
PCM_outcome$intern_score_z.15y <- as.numeric(scale(PCM_outcome$intern_score.15y))
PCM_outcome$intern_score_z.17y <- as.numeric(scale(PCM_outcome$intern_score.17y))
PCM_outcome$intern_score_z.22y <- as.numeric(scale(PCM_outcome$intern_score.22y))

PCM_outcome$fat_mass_z.10y <- as.numeric(scale(PCM_outcome$fat_mass.10y))
PCM_outcome$fat_mass_z.13y <- as.numeric(scale(PCM_outcome$fat_mass.13y))
PCM_outcome$fat_mass_z.15y <- as.numeric(scale(PCM_outcome$fat_mass.15y))
PCM_outcome$fat_mass_z.17y <- as.numeric(scale(PCM_outcome$fat_mass.17y))
PCM_outcome$fat_mass_z.24y <- as.numeric(scale(PCM_outcome$fat_mass.24y))

################################################################################
#### ---------------------------- COVARIATES ------------------------------ ####
################################################################################

# Variables that will be used in the covariate models of this project are those 
# marked with ###. they include: 'sex', 'age_child', 'm_smoking', 'm_drinking' 
# and 'm_bmi_before_pregnancy'.

# For the other demographic auxiliary variables (used for imputation): when they 
# were assessed both prenatally and postnatally, both measures are included.
# Auxiliary variables for this project include: 'ethnicity', 'm_age', parity', 
# 'm_smoking', 'gest_age_birth', 'gest_weight', 'm_bmi_pregnancy', 'm_bmi_5yrs',
# 'sex', 'm_dep_pregnancy', 'p_dep_pregnacy', "m_dep_3yrs", "p_dep_3yrs"

# ------------------------------------------------------------------------------
### AGE of the child
# Combine age of the child measured during first visit and at CBCL administration
# This value will serve as a covariate in the first adjusted model.

#PCM_outcome$age_child = (PCM_outcome$agechild9_visit1 + PCM_outcome$agechild_cbcl9m) / 2

# in ALSPAC: average across intS and fatmass age
PCM_outcome$age_child.10y = (PCM_outcome$int.age.10y + PCM_outcome$fm.age.10y) / 2
PCM_outcome$age_child.13y = (PCM_outcome$int.age.13y + PCM_outcome$fm.age.13y) / 2
PCM_outcome$age_child.15y = PCM_outcome$int.age.15y
PCM_outcome$age_child.17y = PCM_outcome$int.age.17y
PCM_outcome$age_child.23y = (PCM_outcome$int.age.22y + PCM_outcome$fm.age.24y) / 2



# OPTIONAL: check age differnce between measurements
# plot(PCM_outcome$agechild9_visit1, PCM_outcome$agechild_cbcl9m)
# summary(PCM_outcome$agechild9_visit1 - PCM_outcome$agechild_cbcl9m)

#-------------------------------------------------------------------------------
### MATERNAL SMOKING during pregnancy 
# smokingv1 <- readquick("MATERNALSMOKING_22112016.sav") #  9778 obs of 11 variables
# 
# smoking = smokingv1[,c('idm', 'smoke_all')] # (1) never a smoker; 
# # (2) smoked until pregnancy was known (i.e., first trimester only); 
# # (3) continued smoking during pregnancy.
# colnames(smoking)[2] = "m_smoking"

# ALSPAC: 2 categories (sustained vs (stopped or never))

alspac.table$m_smoking=NA
alspac.table[which(alspac.table$e170=='N'),"m_smoking"] <- 0
alspac.table[which(alspac.table$e170=='Y'),"m_smoking"] <- 1



#-------------------------------------------------------------------------------
### MATERNAL ALCOHOL CONSUMPTION during pregnancy
# drinkingv1 <- readquick("GEDRAGSGROEP_MaternalDrinking_22112016.sav") #drinkingv2 <- readquick("MATERNALALCOHOL_22112016.sav") # old variable
# 
# drinking = drinkingv1[,c('idm', 'mdrink_updated')] # (0) never; 
# # (1) until pregnancy was known (i.e., first trimester only); 
# # (2) continued during pregnancy occasionally;
# # (3) continued during pregnancy frequently.
# colnames(drinking)[2] = "m_drinking"

# ALSPAC

alspac.table$m_drinking=NA
alspac.table[which(alspac.table$b721=='never' & alspac.table$e220=='Not at all'),"m_drinking"] <- 0
alspac.table[which(!(alspac.table$b721=='never' & alspac.table$e220=='Not at all') &
                     !(is.na(alspac.table$b721) | is.na(alspac.table$e220))),"m_drinking"] <- 1


#-------------------------------------------------------------------------------
## Other variables
# child_general <- readquick("CHILD-ALLGENERALDATA_07072020.sav") # 9901 obs of 122 
# 
# # Ethnicity recode - dichotomized into: dutch, western and non-western;
# for (i in 1:9901) {
#   if (is.na(child_general$ethnfv2[i])) { child_general$ethnicity[i] <- NA
#   } else if (child_general$ethnfv2[i] == 1) { # Dutch
#     child_general$ethnicity[i] <- 0 
#   } else if (child_general$ethnfv2[i] == 300 | child_general$ethnfv2[i] == 500 | child_general$ethnfv2[i] >= 700) { 
#     # American, western (300) Asian, western (500) European (700), Oceanie (800)
#     child_general$ethnicity[i] <- 1 
#   } else { 
#     child_general$ethnicity[i] <- 2 } 
#   # Indonesian (2), Cape Verdian (3), Maroccan (4) Dutch Antilles (5) Surinamese 
#   # (6) Turkish (7) African (200), American, non western (400), Asian, non western (600)
# }

alspac.table$ethnicity=NA
alspac.table[which(alspac.table$c800=='White' & alspac.table$c801=='White'),"ethnicity"] <- 1
alspac.table[which(!(alspac.table$c800=='White' & alspac.table$c801=='White') &
                     !(is.na(alspac.table$c800) | is.na(alspac.table$c801))),"ethnicity"] <- 0

# pre-pregnancy BMI
alspac.table$dw002=as.numeric(levels(alspac.table$dw002))[alspac.table$dw002]
alspac.table$m4221=as.numeric(levels(alspac.table$m4221))[alspac.table$m4221]
alspac.table$bmi_0 = alspac.table$dw002/((alspac.table$m4221/100)^2)

general_cov_aux = alspac.table[,c('cidb2957', 'qlet', 
                                  'm_smoking',
                                  'm_drinking',
                                  'kz021',    ### SEX -  1 = boy; 2 = girl.
                                  'ethnicity', ### ETHNICITY - dutch, western, non-western
                                  'bmi_0',     ###	MATERNAL BMI - self-reported, before pregnancy
                                  'mult',      # used for exclusion criteria 
                                  #'mother',    # mother id used to identify siblings (for exclusion)
                                  'b032',    # parity (used for imputation)
                                  'bestgest',   # gestational age at birth (used for imputation)
                                  'kz030',    # gestational weight (used for imputation)
                                  #'bmi_1',     # maternal BMI during pregnancy (used for imputation)
                                  'mz028b')] # maternal age at intake (used for imputation) 

# ALSPAC: bmi tbd
 
#maternal height at 7 years (we treat height as a constant)

m_aux_weight <- data.frame("ew002", #kg 8w
"m4220", # kg 7y
"n1140", # kg 8y
"p1290") # kg 9y 

m_aux_bmi < - (m_aux_weight/m4221)^2

m_weight_aux <- (ew002 + m4220 + n1140 + p1290 / 4)

m_weight <-  alspac.table[,c("ew002", #BMI 8w
"m4220", # BMI 7y
"n1140", # BMI 8y
"p1290")] # BMI 9y 

m_weight_aux <- "m_weight"/4

BMI_aux <- 

ls(ew002)


for (i in c('mult',
            'b032',
            'bestgest', 
            'kz030',  
            'mz028b')){
  general_cov_aux[,i] = as.numeric(levels(general_cov_aux[,i]))[general_cov_aux[,i]]
}


# Again, let's try to keep it user friendly 
# colnames(general_cov_aux)[c(3, 5,9:12)] = c("sex", "m_bmi_before_pregnancy", "gest_age_birth", 
#                                             "gest_weight", "m_bmi_pregnancy", "m_age_cont")


general_cov_aux = general_cov_aux %>% 
  rename(
    sex = kz021,
    gest_age_birth = bestgest,
    gest_weight = kz030,
    m_age_cont = mz028b,
    twin = mult,
    parity = b032,
    m_bmi_before_pregnancy = bmi_0)

# ALSPAC: the following section still needs to be done

# #-------------------------------------------------------------------------------
# # Maternal BMI at age 5 (used for imputation)
# m_anthropometry_5yrs = readquick('MOTHERANTHROPOMETRY_18022013.sav')
# m_bmi_5yrs = m_anthropometry_5yrs[,c('mother','bmimotherf5')]; colnames(m_bmi_5yrs)[2] = "m_bmi_5yrs"
# 
# # Merge with the other general variables
# general_cov_aux = merge(general_cov_aux, m_bmi_5yrs, by = 'mother',  all.x = TRUE)
# 
# 
# #-------------------------------------------------------------------------------
# # Maternal and paternal depression during pregnancy and at age 3 
# bsi_pregnancy_m = readquick('GR1003-BSI D1_22112016.sav') # 9778 obs of 261 vars
# bsi_pregnancy_p = readquick('GR1004-BSI G1_22112016.sav') # 9778 obs of 261 vars
# bsi_3yrs = readquick('BSI 3 years of age_GR1065 G1-GR1066 C1_22112016.sav') # 9897 obs of 49 vars
# 
# # Depression during pregnancy
# dep_pregnancy_m = bsi_pregnancy_m[, c('idm','dep')]; colnames(dep_pregnancy_m)[2] = c("m_dep_cont_pregnancy")
# dep_pregnancy_p = bsi_pregnancy_p[, c('idm','dep_p')]; colnames(dep_pregnancy_p)[2] = c("p_dep_cont_pregnancy")
# 
# # Merge it with the previous dataset
# general_cov_aux <- Reduce(function(x,y) merge(x = x, y = y, by = 'idm',  all.x = TRUE),
#                           list(general_cov_aux, dep_pregnancy_m, dep_pregnancy_p))
# 
# # Depression @ 3y: Items 9, 16, 17, 18, 35, and 50
# d <- data.frame(bsi_3yrs$g0100365, bsi_3yrs$g0100665, bsi_3yrs$g0100765, bsi_3yrs$g0100865, bsi_3yrs$g0101365, bsi_3yrs$g0102165, # mother report
#                 bsi_3yrs$c0100366, bsi_3yrs$c0100666, bsi_3yrs$c0100766, bsi_3yrs$c0100866, bsi_3yrs$c0101366, bsi_3yrs$c0102166) # father report
# n_items_m <- rowSums(!is.na(d[,1:6])); n_items_p <- rowSums(!is.na(d[,7:12]))
# bsi_3yrs$m_dep_cont_3yrs <- ifelse(n_items_m >= 5, yes = (rowSums(d[,1:6])/n_items_m)-1, no = NA)
# bsi_3yrs$p_dep_cont_3yrs <- ifelse(n_items_p >= 5, yes = (rowSums(d[,7:12])/n_items_p)-1, no = NA)
# 
# dep_3yrs = bsi_3yrs[, c('idc','m_dep_cont_3yrs', 'p_dep_cont_3yrs')]
# # Merge it with the previous dataset
# general_cov_aux = merge(general_cov_aux, dep_3yrs, by = 'idc',  all.x = TRUE)

#-------------------------------------------------------------------------------
################################################################################
# Merge all covariates / auxiliary variables together
# covariates_and_auxiliary <- Reduce(function(x,y) merge(x = x, y = y, by = 'idm',  all.x = TRUE),
#                                    list(general_cov_aux, smoking, drinking)) 

# ALSPAC for now:
covariates_and_auxiliary = general_cov_aux
################################################################################

################################################################################
# merging outcome variables, covariates and auxiliary variables in one dataset
PCM_project = merge(PCM_outcome, covariates_and_auxiliary, by = c('cidb2957','qlet'), all.x = T)

# A bit of a quick and dirty fix to make merging with ELS easier
# colnames(PCM_project)[which(colnames(PCM_project) == 'idc')] <- toupper('idc')
# colnames(PCM_project)[which(colnames(PCM_project) == 'idm')] <- toupper('idm')

################################################################################
#-------------------------------------------------------------------------------

################################################################################
#### --------------------------- save and run ----------------------------- ####
################################################################################

# Save the dataset in an .RData file, in the directory where the raw data are stored
save(PCM_project, file = "PCM_project.RData")

# Save covariates and auxiliary variables separately as well
save(covariates_and_auxiliary, file = 'covariates_and_auxiliary.RData')
