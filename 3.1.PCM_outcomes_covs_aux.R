# Hi there, 
# the following code is collecting and merging all necessary variables for the analysis
# of the association between ELS and psycho-cardio-metabolic multi-morbidity in children 
# This includes the outcomes of interest (internalizing problems and cardio-metabolic
# risk), the covariates that are going to be used as well as the auxiliary variables 
# used in the imputation of the final dataset. 


#here i am creating a function in order to autonomate age variables in the ALSPAC cohort
#simply input the age below, and the function will automatically update the outcome variables to match

agetemp = 13



#In this script, outcome measures in ALSPAC including internlising (line 55-75) and fatmass (line 75-100) variables, were recorded 
#at 10y, 13y, 15y, 17y and 22y. These scores can be added in order to construct multiple analyses between 
#ELS and PCM outcomes at subsequent timepoints. 


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

##############################################################################################################
#### ------------------ INTERNALIZING PROBLEMS and FATMASS ( @ 10, 13, 15, 15 & 22 ) -------------------- ####
##############################################################################################################


select_age_outcome <- function(age, df) {
  #SDQ emotional problems & total fatmass
  if (age == 10){  vars = c('ku991a', 'ku707a','f9003c', 'f9dx135') 
  
  df$int.age = as.numeric(levels(df[, vars[1]]))[df[, vars[1]]]/ 12
  df$int.score = as.numeric(levels(df[, vars [2]]))[df[, vars[2]]]
  df$fat.age = as.numeric(levels(df[, vars[3]]))[df[, vars[3]]]/ 12
  df$fat.mass = as.numeric(levels(df[, vars[4]]))[df[, vars[4]]]
  
  }
  #SDQ emotional problems & total fatmass
  else if (age == 11) { vars = c('kw9991a', 'kw6602a','fe003c', 'fedx135') 
  
  df$int.age = as.numeric(levels(df[, vars[1]]))[df[, vars[1]]] / 12
  df$int.score = as.numeric(levels(df[, vars [2]]))[df[, vars[2]]]
  df$fat.age = as.numeric(levels(df[, vars[3]]))[df[, vars[3]]]/ 12
  df$fat.mass = as.numeric(levels(df[, vars[4]]))[df[, vars[4]]]
  
  }
  #SDQ emotional problems & android fatmass
  else if (age == 13) { vars = c('ta9991a', 'ta7025a','fg0011a', 'fg3257')
  
  df$int.age = as.numeric(levels(df[, vars[1]]))[df[, vars[1]]] / 12
  df$int.score = as.numeric(levels(df[, vars [2]]))[df[, vars[2]]]
  df$fat.age = as.numeric(levels(df[, vars[3]]))[df[, vars[3]]]/ 12
  df$fat.mass = as.numeric(levels(df[, vars[4]]))[df[, vars[4]]]
  
  }
  #SDQ emotional problems & android fatmass
  else if (age == 15) { vars = c('fh0011a', 'fh6876','fh0011a', 'fh2257')
  
  df$int.age = as.numeric(levels(df[, vars[1]]))[df[, vars[1]]] / 12
  df$int.score = as.numeric(levels(df[, vars [2]]))[df[, vars[2]]]
  df$fat.age = as.numeric(levels(df[, vars[3]]))[df[, vars[3]]] / 12
  df$fat.mass = as.numeric(levels(df[, vars[4]]))[df[, vars[4]]]
  
  }
  #SDQ emotional problems & android fatmass
  else if (age == 17) { vars = c('tc9991a', 'tc4025a','fj003b', 'fjdx138')
  
  df$int.age = as.numeric(levels(df[, vars[1]]))[df[, vars[1]]] /12
  df$int.score = as.numeric(levels(df[, vars [2]]))[df[, vars[2]]]
  df$fat.age = as.numeric(levels(df[, vars[3]]))[df[, vars[3]]]
  df$fat.mass = as.numeric(levels(df[, vars[4]]))[df[, vars[4]]]
  
  
  }
  #SDQ emotional problems & android fatmass
  else if (age == 22) { vars = c('ypb9992', 'ypb5180','FKAR0010', 'FKDX1041')
  
  df$int.age = as.numeric(levels(df[, vars[1]]))[df[, vars[1]]]/ 12
  df$int.score = as.numeric(levels(df[, vars [2]]))[df[, vars[2]]]
  df$fat.age = as.numeric(levels(df[, vars[3]]))[df[, vars[3]]]/ 12
  df$fat.mass = as.numeric(levels(df[, vars[4]]))[df[, vars[4]]]
  }
  return(df[, c('int.age', 'int.score', 'fat.age', 'fat.mass')])       
  
}

cov_out_temp = select_age_outcome(agetemp, alspac.table)

cov_out = cbind(cov_out, cov_out_temp)


# ------------------------------------------------------------------------------
cor_outcome <- round(cor(cov_out[, -1], use = 'complete.obs'), 2) 
write.csv(cor_outcome, file = "corr_mat_outcomes.csv", row.names = T, quote = F)
# ------------------------------------------------------------------------------
# Before we can use them in the analysis, the outcome variables need to be standardized. 
# so, here we take the standard deviation score.
cov_out$intern_score_z <- as.numeric(scale(cov_out$int.score)) # SDQ
cov_out$fat_mass_z     <- as.numeric(scale(cov_out$fat.mass))

################################################################################
#### ------------------- Construct RISK GROUPS variable ------------------- ####
################################################################################

# make some fake data
# cov_out <- data.frame(replicate(5, sample(c(-2.0:7.0, NA), 1000, rep=TRUE)))
# colnames(cov_out) <- c('intern_score_z', 'DAWBAintern_score_z', 'fmi_z', 'fat_mass_tot_z', 'fat_mass_tru_z')

construct_grp <- function(int_var, fm_var, cutoff = 0.8, df = cov_out, permute = T) {
  df$int = ifelse(df[, int_var] > quantile(df[, int_var], probs = cutoff, na.rm = T), 1, 0) 
  df$fat = ifelse(df[, fm_var]  > quantile(df[, fm_var],  probs = cutoff, na.rm = T), 1, 0) 
  
  df$risk_groups = rep(NA, nrow(df))
  for (i in 1:nrow(df)) {
    if ( is.na(df$int[i]) | is.na(df$fat[i]) )  { df$risk_groups[i] = NA
    } else if (df$int[i] == 0 & df$fat[i] == 0) { df$risk_groups[i] = 0   # Healthy
    } else if (df$int[i] == 1 & df$fat[i] == 0) { df$risk_groups[i] = 1   # High internalizing  only
    } else if (df$int[i] == 0 & df$fat[i] == 1) { df$risk_groups[i] = 2   # High fat mass only
    } else {                                      df$risk_groups[i] = 3 } # Multimorbid
  }
  # # Let's first factor that bad boy 
  df$risk_groups = factor(df$risk_groups, 
                         levels = c(0:3), 
                         labels = c("healthy", "internalizing_only", "cardiometabolic_only", "multimorbid"))
  message(paste("Combining:", int_var, "and", fm_var, "\n"))
  print(summary(df$risk_groups))
  corz = cor(df[, c(int_var, fm_var)], use = 'complete.obs')
  plot(df[, int_var], df[, fm_var], main = paste("Corr =", round(corz[1,2], 2)), xlab = int_var, ylab = fm_var, 
       col = c("darkgreen", "blue", "darkgoldenrod2", "red")[df$risk_groups])
  
  if (permute == T) {
    count <- 0 
    iterations <- 1000
    set.seed(310896)
    
    for (i in 1:iterations) {
      origN <- unname(summary(df$risk_groups)[4])
      randN <- permute(df)
      if (randN > origN) {
        count <- count + 1
      }
    }
    
    pval = round(count / iterations, 10)
    cat("Permutation p-value:", pval)
  }
  
  return(df$risk_groups)
}

cov_out$risk_groups <- construct_grp('intern_score_z', 'fat_mass_z')
cov_out$risk_groups_rec <- relevel(cov_out$risk_groups, 'multimorbid')

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
# Combine age of the child measured during internalising and fatmass measurement
# This value will serve as a covariate in the first adjusted model.

cov_out$age_child <- (cov_out$int.age + cov_out$fat.age) / 2

# OPTIONAL: check age difference between measurements
plot(cov_out$int.age, cov_out$fat.age)
summary(cov_out$int.age - cov_out$fat.age)

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

cov_out$m_drinking <- rowMeans(cov_out[, c('b721r', 'e220r')], na.rm = F)


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

cov_out$gest_age_birth <- as.numeric(levels(alspac.table$bestgest))[alspac.table$bestgest] # gestational age at birth (used for imputation)
cov_out$gest_weight    <- as.numeric(levels(alspac.table$kz030))[alspac.table$kz030]       # gestational weight (used for imputation)
cov_out$m_age_cont     <- as.numeric(levels(alspac.table$mz028b))[alspac.table$mz028b]     # maternal age at intake (used for imputation) 

#-------------------------------------------------------------------------------
# Maternal and paternal depression during pregnancy and childhood are calculated 
# in the CCEI EPDS script. 

# Prenatal maternal depression
cov_out$m_dep_cont_pregnancy <- (dep$m_EPDS_total_18wg + dep$m_EPDS_total_32wg) / 2
# Postnatal maternal depression
post_dep_m <- sapply(dep[, c('f200', 'g290', 'h200a')], as.integer) #  NAs introduced by coercion from not depressed and very depressed
cov_out$m_dep_cont_childhood <- rowMeans(post_dep_m, na.rm = T)

# Prenatal paternal depression
cov_out$p_dep_cont_pregnancy <- dep$p_EPDS_total_18wg
# Postnatal paternal depression
post_dep_p <- sapply(dep[, c('pe290', 'pd200')], as.integer) 
cov_out$p_dep_cont_childhood <- rowMeans(post_dep_p, na.rm = T)

################################################################################
#### --------------------------- save and run ----------------------------- ####
################################################################################

# Save the dataset in the directory where you have the raw data
saveRDS(cov_out, file.path(alspac_folder, "PCMout_cov_aux.rds"))

# Also save the dataset in a .csv format
write.csv(cov_out, file = "PCMout_cov_aux.csv", row.names = F, quote = F)

