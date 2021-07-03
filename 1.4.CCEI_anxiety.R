
####################################################################################################################################################

# CALCULATION OF CCEI ANXIETY SUBSCALE & EPDS DEPRESSION TOTAL SCORES

# The first section of this script calculates CCEI anxiety subscale scores and dichotomises them into 1 = risk, 0 = no risk
# The second section of this script calculates the total EPDS depresssion score and dichotomises them into 1 = risk, 0 = no risk 

####################################################################################################################################################


# Obtaining total scores for CCEI anxiety subscale (by adding below items into a total score)

#CCEI: Mother feels upset for no obvious reason	b328, c550, e348, f150, g245, h155, k3000, l2000, r4000 (F1)
#CCEI: Mother felt like fainting	b330, c552, e350, f152, g247, h157, k3002, l2001, r4001 (F3)
#CCEI: Mother feels uneasy & restless	b333, c555, e353, f155, g250, h160, k3005, l2002, r4002 (F6)
#CCEI: Mother sometimes feels panicky	b336, c558, e356, f158, g253, h163, k3008, l2003, r4003 (F9)
#CCEI: Mother worries a lot	b339, c561, e359, f161, g256, h166, k3011, l2004, r4004 (F12)
#CCEI: Mother feels strung up inside	b342, c564, e362, f164, g259, h169, k3014, l2005, r4005 (F15)
#CCEI: Mother feels to be going to pieces	b344, c566, e364, f166, g261, h171, k3016, l2006, r4006 (F17)
#CCEI: Mother has bad upsetting dreams	b347, c569, e367, f169, g264, h174, k3019, l2007, r4007 (F20)

####################################################################################################################################################

# total scores present for: 18w gest, 32w gest, 8m, 8w, 1y 9m, 2y 9m
# total scores missing for: 5y 1m, 6y 1m  (calculating total score for these now, based on individual questionnaire items)

# Step 1: changing items to numeric 

# first doing a trial run with items in 18w gest, to check if method will give identical results to the existing total score for 18w gest

# checking items class
sapply(alspac.table[c("b328",	"b330", "b333",	"b336",	"b339",	"b342",	"b344",	"b347")], class) # these are factors with labels, need to change to numeric

# checking the coding of the item 
a <- table(alspac.table$b328) # V Often, Often, Not V often, Never

# changing the class of items to numeric
alspac.table[c("b328",	"b330", "b333",	"b336",	"b339",	"b342",	"b344",	"b347")] <- sapply(alspac.table[c("b328",	"b330", "b333",	"b336",	"b339",	"b342",	"b344",	"b347")],as.numeric)

# checking the coding of the new item 
b <- table(alspac.table$b328) # 2,3,4,5

# checking if it worked well
sapply(alspac.table[c("b328",	"b330", "b333",	"b336",	"b339",	"b342",	"b344",	"b347")], class) # all numeric now

data.frame(a,b) # checking which numbers correpond to which label to ensure recoding matches that of ALSPAC

# Step 2: recoding
# RECODING CCEI items as described in ALSPAC documentation (preparation for CCEI anxiety subscale calculation)

# f1, f15, f17, (2,3 = 2)(4,5 = 0)
alspac.table$b328a_rec <- ifelse(alspac.table$b328 %in% c(2, 3), 2, ifelse(alspac.table$b328 %in% c(4, 5), 0, NA))
alspac.table$b342a_rec <- ifelse(alspac.table$b342 %in% c(2, 3), 2, ifelse(alspac.table$b342 %in% c(4, 5), 0, NA))
alspac.table$b344a_rec <- ifelse(alspac.table$b344 %in% c(2, 3), 2, ifelse(alspac.table$b344 %in% c(4, 5), 0, NA))

# f3, f6, f12, f20, (2,3 = 2)(4 = 1)(5 = 0) 
alspac.table$b330a_rec <- ifelse(alspac.table$b330 %in% c(2, 3), 2, ifelse(alspac.table$b330 == 4, 1, ifelse(alspac.table$b330 == 5, 0, NA)))
alspac.table$b333a_rec <- ifelse(alspac.table$b333 %in% c(2, 3), 2, ifelse(alspac.table$b333 == 4, 1, ifelse(alspac.table$b333 == 5, 0, NA)))
alspac.table$b339a_rec <- ifelse(alspac.table$b339 %in% c(2, 3), 2, ifelse(alspac.table$b339 == 4, 1, ifelse(alspac.table$b339 == 5, 0, NA)))
alspac.table$b347a_rec <- ifelse(alspac.table$b347 %in% c(2, 3), 2, ifelse(alspac.table$b347 == 4, 1, ifelse(alspac.table$b347 == 5, 0, NA)))

# f9 (2,3,4 = 2) (5 = 0)
alspac.table$b336a_rec <- ifelse(alspac.table$b336 %in% c(2, 3, 4), 2, ifelse(alspac.table$b336 == 5, 0, NA))


# Look if recoding worked well 
data.frame(alspac.table$b328, alspac.table$b328a_rec) # looks good 
data.frame(alspac.table$b330, alspac.table$b330a_rec) # looks good 
data.frame(alspac.table$b336, alspac.table$b336a_rec) # looks good 

# Step 3: calculate total score 

# calculating the total CCEI anxiety subscale score using complete scores (if any time point missing, total score = NA), should we mode impute?
alspac.table$CCEI_total_18w_gest <- rowSums(alspac.table[c("b328a_rec",	"b330a_rec", "b333a_rec",	"b336a_rec",	"b339a_rec",	"b342a_rec",	"b344a_rec",	"b347a_rec")], na.rm = F)

# comparing it to the available total CCEI anxiety subscale score 
data.frame(alspac.table$b351, alspac.table$CCEI_total_18w_gest)
table(alspac.table$CCEI_total_18w_gest, exclude = NULL)
table(alspac.table$b351, exclude = NULL) # the scores are identical, means we can calculate the CCEI scores for the missing timepoints
# 0 = "not anxious", 1 - 15, 16 = "very anxious"

####################################################################################################################################################

# CALCULATING CCEI SCORES FOR 5 YEAR MISSING TIMEPOINT

# Step 1: changing items to numeric 

# checking items class
sapply(alspac.table[c("k3000",	"k3002",	"k3005",	"k3008",	"k3011",	"k3014",	"k3016",	"k3019")], class) # these are factors with labels, need to change to numeric

# checking the coding of the item 
a <- alspac.table$k3000 # V Often, Often, Not V often, Never

# changing the class of items to numeric
alspac.table[c("k3000",	"k3002",	"k3005",	"k3008",	"k3011",	"k3014",	"k3016",	"k3019")] <- sapply(alspac.table[c("k3000",	"k3002",	"k3005",	"k3008",	"k3011",	"k3014",	"k3016",	"k3019")],as.numeric)

# checking if it worked well
sapply(alspac.table[c("k3000",	"k3002",	"k3005",	"k3008",	"k3011",	"k3014",	"k3016",	"k3019")] , class) # all numeric now

# checking the coding of the new item 
b <- alspac.table$k3000 # 2,3,4,5 

data.frame(a,b) # checking which numbers correpond to which label to ensure recoding matches that of ALSPAC
# 2,3,4,5 = V Often, Often, Not V often, Never

# Step 2: recoding

# RECODING CCEI items as described in ALSPAC documentation (preparation for CCEI anxiety subscale calculation)

# f1, f15, f17, (2,3 = 2)(4,5 = 0)
alspac.table$k3000a_rec <- ifelse(alspac.table$k3000 %in% c(2, 3), 2, ifelse(alspac.table$k3000 %in% c(4, 5), 0, NA))
alspac.table$k3014a_rec <- ifelse(alspac.table$k3014 %in% c(2, 3), 2, ifelse(alspac.table$k3014 %in% c(4, 5), 0, NA))
alspac.table$k3016a_rec <- ifelse(alspac.table$k3016 %in% c(2, 3), 2, ifelse(alspac.table$k3016 %in% c(4, 5), 0, NA))

# f3, f6, f12, f20, (2,3 = 2)(4 = 1)(5 = 0) 
alspac.table$k3002a_rec <- ifelse(alspac.table$k3002 %in% c(2, 3), 2, ifelse(alspac.table$k3002 == 4, 1, ifelse(alspac.table$k3002 == 5, 0, NA)))
alspac.table$k3005a_rec <- ifelse(alspac.table$k3005 %in% c(2, 3), 2, ifelse(alspac.table$k3005 == 4, 1, ifelse(alspac.table$k3005 == 5, 0, NA)))
alspac.table$k3011a_rec <- ifelse(alspac.table$k3011 %in% c(2, 3), 2, ifelse(alspac.table$k3011 == 4, 1, ifelse(alspac.table$k3011 == 5, 0, NA)))
alspac.table$k3019a_rec <- ifelse(alspac.table$k3019 %in% c(2, 3), 2, ifelse(alspac.table$k3019 == 4, 1, ifelse(alspac.table$k3019 == 5, 0, NA)))

# f9 (2,3,4 = 2) (5 = 0)
alspac.table$k3008a_rec <- ifelse(alspac.table$k3008 %in% c(2, 3, 4), 2, ifelse(alspac.table$k3008 == 5, 0, NA))


# Step 3: calculate total score 

# calculating the total CCEI anxiety subscale score using complete scores (if any time point missing, total score = NA), should we mode impute?
alspac.table$CCEI_total_5y <- rowSums(alspac.table[c("k3000a_rec",	"k3002a_rec",	"k3005a_rec",	"k3008a_rec",	"k3011a_rec",	"k3014a_rec",	"k3016a_rec",	"k3019a_rec")], na.rm = F)


table(alspac.table$CCEI_total_5y, exclude = NULL) # looks okay
# 0 = "not anxious", 1 - 15, 16 = "very anxious"


####################################################################################################################################################

# CALCULATING CCEI SCORES FOR 6 YEAR MISSING TIMEPOINT

alspac.table$l_CCEI <- rowSums(l2000,	l2001,	l2002,	l2003,	l2004,	l2005,	l2006,	l2007)

# Step 1: changing items to numeric 

# checking items class
sapply(alspac.table[c("l2000",	"l2001",	"l2002",	"l2003",	"l2004",	"l2005",	"l2006",	"l2007")], class) # these are factors with labels, need to change to numeric

# checking the coding of the item 
a <- alspac.table$l2000 # V Often, Often, Not V often, Never

# changing the class of items to numeric
alspac.table[c("l2000",	"l2001",	"l2002",	"l2003",	"l2004",	"l2005",	"l2006",	"l2007")] <- sapply(alspac.table[c("l2000",	"l2001",	"l2002",	"l2003",	"l2004",	"l2005",	"l2006",	"l2007")],as.numeric)

# checking if it worked well
sapply(alspac.table[c("l2000",	"l2001",	"l2002",	"l2003",	"l2004",	"l2005",	"l2006",	"l2007")] , class) # all numeric now

# checking the coding of the new item 
b <- alspac.table$l2000 # 2,3,4,5 

data.frame(a,b) # checking which numbers correpond to which label to ensure recoding matches that of ALSPAC
# 2,3,4,5 = V Often, Often, Not V often, Never

# Step 2: recoding

# RECODING CCEI items as described in ALSPAC documentation (preparation for CCEI anxiety subscale calculation)

# f1, f15, f17, (2,3 = 2)(4,5 = 0)
alspac.table$l2000a_rec <- ifelse(alspac.table$l2000 %in% c(2, 3), 2, ifelse(alspac.table$l2000 %in% c(4, 5), 0, NA))
alspac.table$l2005a_rec <- ifelse(alspac.table$l2005 %in% c(2, 3), 2, ifelse(alspac.table$l2005 %in% c(4, 5), 0, NA))
alspac.table$l2006a_rec <- ifelse(alspac.table$l2006 %in% c(2, 3), 2, ifelse(alspac.table$l2006 %in% c(4, 5), 0, NA))

# f3, f6, f12, f20, (2,3 = 2)(4 = 1)(5 = 0) 
alspac.table$l2001a_rec <- ifelse(alspac.table$l2001 %in% c(2, 3), 2, ifelse(alspac.table$l2001 == 4, 1, ifelse(alspac.table$l2001 == 5, 0, NA)))
alspac.table$l2002a_rec <- ifelse(alspac.table$l2002 %in% c(2, 3), 2, ifelse(alspac.table$l2002 == 4, 1, ifelse(alspac.table$l2002 == 5, 0, NA)))
alspac.table$l2004a_rec <- ifelse(alspac.table$l2004 %in% c(2, 3), 2, ifelse(alspac.table$l2004 == 4, 1, ifelse(alspac.table$l2004 == 5, 0, NA)))
alspac.table$l2007a_rec <- ifelse(alspac.table$l2007 %in% c(2, 3), 2, ifelse(alspac.table$l2007 == 4, 1, ifelse(alspac.table$l2007 == 5, 0, NA)))

# f9 (2,3,4 = 2) (5 = 0)
alspac.table$l2003a_rec <- ifelse(alspac.table$l2003 %in% c(2, 3, 4), 2, ifelse(alspac.table$l2003 == 5, 0, NA))


# Step 3: calculate total score 

# calculating the total CCEI anxiety subscale score using complete scores (if any time point missing, total score = NA), should we mode impute?
alspac.table$CCEI_total_6y <- rowSums(alspac.table[c("l2000a_rec",	"l2001a_rec",	"l2002a_rec",	"l2003a_rec",	"l2004a_rec",	"l2005a_rec",	"l2006a_rec",	"l2007a_rec")], na.rm = F)


table(alspac.table$CCEI_total_6y, exclude = NULL) # looks okay, 7066 NA
# 0 = "not anxious", 1 - 15, 16 = "very anxious"


####################################################################################################################################################

# DICHOTOMISING 

# Scores on the CCEI anxiety subscale can range from 0 to 16. 
# Women who scored 9 or higher, a cut point which has been used previously in ALSPAC (Heron et al., 2004), were classified as having anxiety.

# Prenatal

#b351 to b351a_rec
levels(alspac.table$b351) 

alspac.table$b351a_rec <- ifelse(alspac.table$b351 %in% c(0:8), 0, 
                                 ifelse(alspac.table$b351 %in% c(9:16), 1, NA)) # CCEI anxiety subscale

                                                                                                                                                        
# checking if recoding worked
data.frame(alspac.table$b351, alspac.table$b351a_rec) # looks good



#c573 to c573a_rec
levels(alspac.table$c573) 

alspac.table$c573a_rec <- ifelse(alspac.table$c573 %in% c(1:8), 0, ifelse(alspac.table$c573 == "not anxious", 0,
                                                                          ifelse(alspac.table$c573 %in% c(9:15), 1,
                                                                                 ifelse(alspac.table$c573 == "very anxious", 1, NA)))) 
# checking if recoding worked
data.frame(alspac.table$c573, alspac.table$c573a_rec) # looks good


# Postnatal 

#e371 to e371a_rec
levels(alspac.table$e371) 

alspac.table$e371a_rec <- ifelse(alspac.table$e371 %in% c(1:8), 0, ifelse(alspac.table$e371 == "not anxious", 0,
                                                                          ifelse(alspac.table$e371 %in% c(9:15), 1,
                                                                                 ifelse(alspac.table$e371 == "very anxious", 1, NA)))) 
# checking if recoding worked
data.frame(alspac.table$e371, alspac.table$e371a_rec) # looks good


# f173 to f173a_rec
levels(alspac.table$f173) 

alspac.table$f173a_rec <- ifelse(alspac.table$f173 %in% c(1:8), 0, ifelse(alspac.table$f173 == "not anxious", 0,
                                                                          ifelse(alspac.table$f173 %in% c(9:15), 1,
                                                                                 ifelse(alspac.table$f173 == "very anxious", 1, NA)))) 
# checking if recoding  worked
data.frame(alspac.table$f173, alspac.table$f173a_rec) # looks good


# g268 to g268a_rec
levels(alspac.table$g268) 

alspac.table$g268a_rec <- ifelse(alspac.table$g268 %in% c(1:8), 0, ifelse(alspac.table$g268 == "not anxious", 0,
                                                                          ifelse(alspac.table$g268 %in% c(9:15), 1,
                                                                                 ifelse(alspac.table$g268 == "very anxious", 1, NA)))) 
# checking if recoding  worked
data.frame(alspac.table$g268, alspac.table$g268a_rec) # looks good



# h178a to h178a_rec
levels(alspac.table$h178a) 

alspac.table$h178a_rec <- ifelse(alspac.table$h178a %in% c(0:8), 0, ifelse(alspac.table$h178a %in% c(9:16), 1, NA))
                                                                                  
# checking if recoding  worked
data.frame(alspac.table$h178a, alspac.table$h178a_rec) # looks good


# CCEI_total_5y to CCEI_total_5ya_rec

table(alspac.table$CCEI_total_5y) 

alspac.table$CCEI_total_5ya_rec <- ifelse(alspac.table$CCEI_total_5y %in% c(0:8), 0, ifelse(alspac.table$CCEI_total_5y %in% c(9:16), 1, NA))

# checking if recoding  worked
data.frame(alspac.table$CCEI_total_5y, alspac.table$CCEI_total_5ya_rec) # looks good


# CCEI_total_6y to CCEI_total_6ya_rec

table(alspac.table$CCEI_total_6y) 

alspac.table$CCEI_total_6ya_rec <- ifelse(alspac.table$CCEI_total_6y %in% c(0:8), 0, ifelse(alspac.table$CCEI_total_6y %in% c(9:16), 1, NA))

# checking if recoding  worked
data.frame(alspac.table$CCEI_total_5y, alspac.table$CCEI_total_5ya_rec) # looks good


####################################################################################################################################################

##########################################################################################################################################################################

# VALID SHORT VERSION FROM HERE DOWN
##########################################################################################################################################################################

# MOTHER DEPRESSION
# This section will recode and add all of the items below to obtain EPDS total score for 5y and 8y

#EPDS: Sense of humour in past WK	k3030, n6060 #D24
#EPDS: Looked forward to things in past WK k3031, n6061  #D25
#EPDS: Unnecessary self blame in past WK	k3032, n6062 #D26
#EPDS: Unnecessary anxiety or worry in past WK	k3033, n6063 #D27
#EPDS: Unnecessary panic or fear in past WK k3034, n6064 #D28
#EPDS: Things getting too much in past WK	k3035, n6065 #D29
#EPDS: Sleeping PROB due to sadness in past WK k3036, n6066  #D30
#EPDS: Sad or miserable in past WK k3037, n6067  #D31
#EPDS: Crying due to unhappiness in past WK k3038, n6068 #D32
#EPDS: Considered selfharm in past WK k3039, n6069 #D33

##########################################################################################################################################################################

# CALCULATE EPDS TOTAL SCORE 5y

# Step 1
# Converting labels to numeric values

alspac.table[c('k3030n', 'k3031n', 'k3032n', 'k3033n', 'k3034n', 'k3035n', 'k3036n', 'k3037n', 'k3038n', 'k3039n')] <- sapply(alspac.table[c('k3030', 'k3031', 'k3032', 'k3033', 'k3034', 'k3035', 'k3036', 'k3037', 'k3038', 'k3039')], as.integer) # save as integer, so that labels would become numerical values


# Step 2
# RECODING EPDS items as described in ALSPAC documentation (preparation for EPDS total score calculation)

# d24, d25, d27, (1 = 0) (2 = 1) (3 = 2) (4 = 3)
alspac.table[c('k3030m', 'k3031m', 'k3033m')] <- 
  ifelse(alspac.table[c('k3030n', 'k3031n', 'k3033n')] == 1, 0,
         ifelse(alspac.table[c('k3030n', 'k3031n', 'k3033n')] == 2, 1,
                ifelse(alspac.table[c('k3030n', 'k3031n', 'k3033n')] == 3, 2,
                       ifelse(alspac.table[c('k3030n', 'k3031n', 'k3033n')] == 4, 3, NA)))) 

# d26, d28, d29, d30, d31, d32, d33 (1 = 3) (2 = 2) (3 = 1) (4 = 0)
alspac.table[c('k3032m', 'k3034m', 'k3035m', 'k3036m', 'k3037m', 'k3038m', 'k3039m')] <- 
  ifelse(alspac.table[c('k3032n', 'k3034n', 'k3035n', 'k3036n', 'k3037n', 'k3038n', 'k3039n')] == 1, 3,
         ifelse(alspac.table[c('k3032n', 'k3034n', 'k3035n', 'k3036n', 'k3037n', 'k3038n', 'k3039n')] == 2, 2,
                ifelse(alspac.table[c('k3032n', 'k3034n', 'k3035n', 'k3036n', 'k3037n', 'k3038n', 'k3039n')] == 3, 1,
                       ifelse(alspac.table[c('k3032n', 'k3034n', 'k3035n', 'k3036n', 'k3037n', 'k3038n', 'k3039n')] == 4, 0, NA)))) 

# Check if recoding worked well 
data.frame(alspac.table$k3039m, alspac.table$k3039n, alspac.table$k3039) # looks good 

# Step 3: calculate total score 

# calculating the EPDS total score using complete scores (if any time point missing, total score = NA), should we mode impute?
alspac.table$m_EPDS_total_5y <- rowSums(alspac.table[c('k3030m', 'k3031m', 'k3032m', 'k3033m', 'k3034m', 'k3035m', 'k3036m', 'k3037m', 'k3038m', 'k3039m')], na.rm = F)

# plot it
hist(alspac.table$m_EPDS_total_5y)
 
summary(as.factor(alspac.table$m_EPDS_total_5y)) # 15337 NA???
##########################################################################################################################################################################

# CALCULATE EPDS TOTAL SCORE 8y

# Step  1
# Converting labels to numeric values

alspac.table[c('n6060n', 'n6061n', 'n6062n','n6063n', 'n6064n', 'n6065n', 'n6066n', 'n6067n', 'n6068n', 'n6069n')] <- sapply(alspac.table[c('n6060', 'n6061', 'n6062','n6063', 'n6064', 'n6065', 'n6066', 'n6067', 'n6068', 'n6069')], as.integer) # save as integer, so that labels would become numerical values


# Step 2
# RECODING EPDS items as described in ALSPAC documentation (preparation for EPDS total score calculation)

# d24, d25, d27, (1 = 0) (2 = 1) (3 = 2) (4 = 3)
alspac.table[c('n6060m', 'n6061m', 'n6063m')]  <- ifelse(alspac.table[c('n6060n', 'n6061n', 'n6063n')]  == 1, 0,
                                                         ifelse(alspac.table[c('n6060n', 'n6061n', 'n6063n')] == 2, 1,
                                                                ifelse(alspac.table[c('n6060n', 'n6061n', 'n6063n')] == 3, 2,
                                                                       ifelse(alspac.table[c('n6060n', 'n6061n', 'n6063n')] == 4, 3, NA)))) # d24

# d26, d28, d29, d30, d31, d32, d33 (1 = 3) (2 = 2) (3 = 1) (4 = 0)
alspac.table[c('n6062m', 'n6064m', 'n6065m', 'n6066m', 'n6067m', 'n6068m', 'n6069m')] <- 
  ifelse(alspac.table[c('n6062n', 'n6064n', 'n6065n', 'n6066n', 'n6067n', 'n6068n', 'n6069n')]  == 1, 3,
         ifelse(alspac.table[c('n6062n', 'n6064n', 'n6065n', 'n6066n', 'n6067n', 'n6068n', 'n6069n')] == 2, 2,
                ifelse(alspac.table[c('n6062n', 'n6064n', 'n6065n', 'n6066n', 'n6067n', 'n6068n', 'n6069n')] == 3, 1,
                       ifelse(alspac.table[c('n6062n', 'n6064n', 'n6065n', 'n6066n', 'n6067n', 'n6068n', 'n6069n')] == 4, 0, NA)))) 


# Check if recoding worked well 
data.frame(alspac.table$n6069, alspac.table$n6069n, alspac.table$n6069m) # looks good 

# Step 3: calculate total score 

# calculating the EPDS total score using complete scores (if any time point missing, total score = NA), should we mode impute?
alspac.table$m_EPDS_total_8y <- rowSums(alspac.table[c('n6060m', 'n6061m', 'n6062m', 'n6063m', 'n6064m', 'n6065m', 'n6066m', 'n6067m', 'n6068m', 'n6069m')], na.rm = F)

# plot it 
hist(alspac.table$m_EPDS_total_8y)


##########################################################################################################################################################################

# DICHOTOMISE EPDS TOTAL SCORE - MOTHER

# Converting factors to character (does not alter content)
alspac.table[c('e391n', 'f201n', 'g291n', 'h200bn')] <- sapply(alspac.table[c('e391', 'f201', 'g291', 'h200b')], as.character) # not adding as.numeric to avoid converting labels

# Dichotomising EPDS total score for 8w
alspac.table$e391a_rec <- ifelse(alspac.table$e391n %in% c(13:29), 1,
                                 ifelse(alspac.table$e391n == 'very depressed', 1, 
                                        ifelse(alspac.table$e391n %in% c(1:12), 0,
                                               ifelse(alspac.table$e391n == 'not depressed', 0, NA)))) # EPDS (>12 risk, <=12 no risk)

# Dichotomising EPDS total score for 8m
alspac.table$f201a_rec <- ifelse(alspac.table$f201n %in% c(13:29), 1,
                                 ifelse(alspac.table$f201n == 'very depressed', 1, 
                                        ifelse(alspac.table$f201n %in% c(1:12), 0,
                                               ifelse(alspac.table$f201n == 'not depressed', 0, NA)))) # EPDS (>12 risk, <=12 no risk)

# Dichotomising EPDS total score for 1y 9m
alspac.table$g291a_rec <- ifelse(alspac.table$g291n %in% c(13:29), 1,
                                 ifelse(alspac.table$g291n == 'very depressed', 1, 
                                        ifelse(alspac.table$g291n %in% c(1:12), 0,
                                               ifelse(alspac.table$g291n == 'not depressed', 0, NA)))) # EPDS (>12 risk, <=12 no risk)

# Dichotomising EPDS total score for 2y 9m
alspac.table$h200ba_rec <- ifelse(alspac.table$h200bn %in% c(13:30), 1,
                                  ifelse(alspac.table$h200bn %in% c(0:12), 0, NA)) # EPDS (>12 risk, <=12 no risk) 



# Dichotomising EPDS total score for mother 5y
alspac.table$m_EPDS_total_5ya_rec  <- ifelse(alspac.table$m_EPDS_total_5y %in% c(13:29), 1,
                                           ifelse(alspac.table$m_EPDS_total_5y %in% c(0:12), 0, NA)) # EPDS (>12 risk, <=12 no risk)


# Dichotomising EPDS total sscore for mother 8y
alspac.table$m_EPDS_total_8ya_rec  <- ifelse(alspac.table$m_EPDS_total_8y %in% c(13:29), 1,
                                             ifelse(alspac.table$m_EPDS_total_8y %in% c(0:12), 0, NA)) # EPDS (>12 risk, <=12 no risk)

##########################################################################################################################################################################

# PARTNER DEPRESSION
# This section will recode and add all of the items below to obtain EPDS total score for 3y, 5y and 6y

#EPDS: Sense of humour in past WK	pf4030, ph3030, pj2010  # c24
#EPDS: Looked forward to things in past WK  pf4031, ph3031, pj2011 #c25
#EPDS: Unnecessary self blame in past WK	 pf4032, ph3032, pj2012 #c26
#EPDS: Unnecessary anxiety or worry in past WK	 pf4033, ph3033, pj2013 #c27
#EPDS: Unnecessary panic or fear in past WK	 pf4034, ph3034, pj2014 #c28
#EPDS: Things getting too much in past WK	pf4035, ph3035, pj2015 #c29
#EPDS: Sleeping PROB due to sadness in past WK	 pf4036, ph3036, pj2016 #c30
#EPDS: Sad or miserable in past WK	pf4037, ph3037, pj2017  #c31
#EPDS: Crying due to unhappiness in past WK	pf4038, ph3038, pj2018 #c32
#EPDS: Considered selfharm in past WK	pf4039, ph3039, pj2019 #c33

##########################################################################################################################################################################

# CALCULATE EPDS TOTAL SCORE 3y

# Step  1
# Converting labels to numeric values

alspac.table[c('pf4030n', 'pf4031n', 'pf4032n','pf4033n', 'pf4034n', 'pf4035n', 'pf4036n', 'pf4037n', 'pf4038n', 'pf4039n')] <- sapply(alspac.table[c('pf4030', 'pf4031', 'pf4032','pf4033', 'pf4034', 'pf4035', 'pf4036', 'pf4037', 'pf4038', 'pf4039')], as.integer) # save as integer, so that labels would become numerical values


# Step 2
# RECODING EPDS items as described in ALSPAC documentation (preparation for EPDS total score calculation)


# c24, c25, c27, (1=0)(2=1)(3=2)(4=3)
alspac.table[c('pf4030m', 'pf4031m', 'pf4033m')] <- ifelse(alspac.table[c('pf4030n', 'pf4031n', 'pf4033n')] == 1, 0,
                                                           ifelse(alspac.table[c('pf4030n', 'pf4031n', 'pf4033n')] == 2, 1,
                                                                  ifelse(alspac.table[c('pf4030n', 'pf4031n', 'pf4033n')] == 3, 2,
                                                                         ifelse(alspac.table[c('pf4030n', 'pf4031n', 'pf4033n')]== 4, 3, NA))))

# c26, c28, c29, c30, c31, c32, c33 (1=3)(2=2)(3=1)(4=0) 
alspac.table[c('pf4032m','pf4034m', 'pf4035m', 'pf4036m', 'pf4037m', 'pf4038m', 'pf4039m')] <- 
  ifelse(alspac.table[c('pf4032n','pf4034n', 'pf4035n', 'pf4036n', 'pf4037n', 'pf4038n', 'pf4039n')] == 1, 3,
         ifelse(alspac.table[c('pf4032n','pf4034n', 'pf4035n', 'pf4036n', 'pf4037n', 'pf4038n', 'pf4039n')] == 2, 2,
                ifelse(alspac.table[c('pf4032n','pf4034n', 'pf4035n', 'pf4036n', 'pf4037n', 'pf4038n', 'pf4039n')]== 3, 1,
                       ifelse(alspac.table[c('pf4032n','pf4034n', 'pf4035n', 'pf4036n', 'pf4037n', 'pf4038n', 'pf4039n')] == 4, 0, NA))))


# Check if recoding worked well 
data.frame(alspac.table$pf4039, alspac.table$pf4039n, alspac.table$pf4039m) # looks good 


# Step 3: calculate total score 

# calculating the EPDS total score using complete scores (if any time point missing, total score = NA), should we mode impute?
alspac.table$p_EPDS_total_3y <- rowSums(alspac.table[c('pf4030m', 'pf4031m', 'pf4032m','pf4033m', 'pf4034m', 'pf4035m', 'pf4036m', 'pf4037m', 'pf4038m', 'pf4039m')], na.rm = F)

# plot it 
hist(alspac.table$p_EPDS_total_3y)

##########################################################################################################################################################################


# CALCULATE EPDS TOTAL SCORE 5y

# Step  1
# Converting labels to numeric values

alspac.table[c('ph3030n', 'ph3031n', 'ph3032n','ph3033n', 'ph3034n', 'ph3035n', 'ph3036n', 'ph3037n', 'ph3038n', 'ph3039n')] <- sapply(alspac.table[c('ph3030', 'ph3031', 'ph3032','ph3033', 'ph3034', 'ph3035', 'ph3036', 'ph3037', 'ph3038', 'ph3039')], as.integer) # save as integer, so that labels would become numerical values


# Step 2
# RECODING EPDS items as described in ALSPAC documentation (preparation for EPDS total score calculation)


# c24, c25, c27, (1=0)(2=1)(3=2)(4=3)
alspac.table[c('ph3030m', 'ph3031m', 'ph3033m')] <- ifelse(alspac.table[c('ph3030n', 'ph3031n', 'ph3033n')] == 1, 0,
                                                           ifelse(alspac.table[c('ph3030n', 'ph3031n', 'ph3033n')] == 2, 1,
                                                                  ifelse(alspac.table[c('ph3030n', 'ph3031n', 'ph3033n')] == 3, 2,
                                                                         ifelse(alspac.table[c('ph3030n', 'ph3031n', 'ph3033n')]== 4, 3, NA))))

# c26, c28, c29, c30, c31, c32, c33 (1=3)(2=2)(3=1)(4=0) 
alspac.table[c('ph3032m','ph3034m', 'ph3035m', 'ph3036m', 'ph3037m', 'ph3038m', 'ph3039m')] <- 
  ifelse(alspac.table[c('ph3032n','ph3034n', 'ph3035n', 'ph3036n', 'ph3037n', 'ph3038n', 'ph3039n')] == 1, 3,
         ifelse(alspac.table[c('ph3032n','ph3034n', 'ph3035n', 'ph3036n', 'ph3037n', 'ph3038n', 'ph3039n')] == 2, 2,
                ifelse(alspac.table[c('ph3032n','ph3034n', 'ph3035n', 'ph3036n', 'ph3037n', 'ph3038n', 'ph3039n')]== 3, 1,
                       ifelse(alspac.table[c('ph3032n','ph3034n', 'ph3035n', 'ph3036n', 'ph3037n', 'ph3038n', 'ph3039n')] == 4, 0, NA))))


# Check if recoding worked well 
data.frame(alspac.table$ph3039, alspac.table$ph3039n, alspac.table$ph3039m) # looks good 


# Step 3: calculate total score 

# calculating the EPDS total score using complete scores (if any time point missing, total score = NA), should we mode impute?
alspac.table$p_EPDS_total_5y <- rowSums(alspac.table[c('ph3030m', 'ph3031m', 'ph3032m','ph3033m', 'ph3034m', 'ph3035m', 'ph3036m', 'ph3037m', 'ph3038m', 'ph3039m')], na.rm = F)

# plot it 
hist(alspac.table$p_EPDS_total_5y)



##########################################################################################################################################################################


# CALCULATE EPDS TOTAL SCORE 6y

# Step  1
# Converting labels to numeric values

alspac.table[c('pj2010n', 'pj2011n', 'pj2012n','pj2013n', 'pj2014n', 'pj2015n', 'pj2016n', 'pj2017n', 'pj2018n', 'pj2019n')] <- sapply(alspac.table[c('pj2010', 'pj2011', 'pj2012','pj2013', 'pj2014', 'pj2015', 'pj2016', 'pj2017', 'pj2018', 'pj2019')], as.integer) # save as integer, so that labels would become numerical values


# Step 2
# RECODING EPDS items as described in ALSPAC documentation (preparation for EPDS total score calculation)


# c24, c25, c27, (1=0)(2=1)(3=2)(4=3)
alspac.table[c('pj2010m', 'pj2011m', 'pj2013m')] <- ifelse(alspac.table[c('pj2010n', 'pj2011n', 'pj2013n')] == 1, 0,
                                                           ifelse(alspac.table[c('pj2010n', 'pj2011n', 'pj2013n')] == 2, 1,
                                                                  ifelse(alspac.table[c('pj2010n', 'pj2011n', 'pj2013n')] == 3, 2,
                                                                         ifelse(alspac.table[c('pj2010n', 'pj2011n', 'pj2013n')]== 4, 3, NA))))

# c26, c28, c29, c30, c31, c32, c33 (1=3)(2=2)(3=1)(4=0) 
alspac.table[c('pj2012m','pj2014m', 'pj2015m', 'pj2016m', 'pj2017m', 'pj2018m', 'pj2019m')] <- 
  ifelse(alspac.table[c('pj2012n','pj2014n', 'pj2015n', 'pj2016n', 'pj2017n', 'pj2018n', 'pj2019n')] == 1, 3,
         ifelse(alspac.table[c('pj2012n','pj2014n', 'pj2015n', 'pj2016n', 'pj2017n', 'pj2018n', 'pj2019n')] == 2, 2,
                ifelse(alspac.table[c('pj2012n','pj2014n', 'pj2015n', 'pj2016n', 'pj2017n', 'pj2018n', 'pj2019n')]== 3, 1,
                       ifelse(alspac.table[c('pj2012n','pj2014n', 'pj2015n', 'pj2016n', 'pj2017n', 'pj2018n', 'pj2019n')] == 4, 0, NA))))


# Check if recoding worked well 
data.frame(alspac.table$pj2019, alspac.table$pj2019n, alspac.table$pj2019m) # looks good 


# Step 3: calculate total score 

# calculating the EPDS total score using complete scores (if any time point missing, total score = NA), should we mode impute?
alspac.table$p_EPDS_total_6y <- rowSums(alspac.table[c('pj2010m', 'pj2011m', 'pj2012m','pj2013m', 'pj2014m', 'pj2015m', 'pj2016m', 'pj2017m', 'pj2018m', 'pj2019m')], na.rm = F)

# plot it 
hist(alspac.table$p_EPDS_total_6y)

##########################################################################################################################################################################

# DICHOTOMISE EPDS TOTAL SCORE - PARTNER

# Converting factors to character (does not alter content)
alspac.table[c('pc103n', 'pd201n', 'pe291n')] <- sapply(alspac.table[c('pc103', 'pd201', 'pe291')], as.character) # not adding as.numeric to avoid converting labels

# Dichotomising EPDS total score for 8w
alspac.table$pc103a_rec <- ifelse(alspac.table$pc103n %in% c(13:29), 1,
                                 ifelse(alspac.table$pc103n == 'very depressed', 1, 
                                        ifelse(alspac.table$pc103n %in% c(1:12), 0,
                                               ifelse(alspac.table$pc103n == 'not depressed', 0, NA)))) # EPDS (>12 risk, <=12 no risk)

# Dichotomising EPDS total score for 8m
alspac.table$pd201a_rec <- ifelse(alspac.table$pd201n %in% c(13:29), 1,
                                  ifelse(alspac.table$pd201n == 'very depressed', 1, 
                                         ifelse(alspac.table$pd201n %in% c(1:12), 0,
                                                ifelse(alspac.table$pd201n == 'not depressed', 0, NA)))) # EPDS (>12 risk, <=12 no risk)

# Dichotomising EPDS total score for 21m
alspac.table$pe291a_rec <- ifelse(alspac.table$pe291n %in% c(13:30), 1,
                                  ifelse(alspac.table$pe291n %in% c(0:12), 0, NA)) # EPDS (>12 risk, <=12 no risk)

# Dichotomising EPDS total score for partner 3y
alspac.table$p_EPDS_total_3ya_rec  <- ifelse(alspac.table$p_EPDS_total_3y %in% c(13:29), 1,
                                             ifelse(alspac.table$p_EPDS_total_3y %in% c(0:12), 0, NA)) # EPDS (>12 risk, <=12 no risk)


# Dichotomising EPDS total sscore for partner 5y
alspac.table$p_EPDS_total_5ya_rec <- ifelse(alspac.table$p_EPDS_total_5y %in% c(13:29), 1,
                                            ifelse(alspac.table$p_EPDS_total_5y  %in% c(0:12), 0, NA)) # EPDS (>12 risk, <=12 no risk)

# Dichotomising EPDS total sscore for partner 6y
alspac.table$p_EPDS_total_6ya_rec <- ifelse(alspac.table$p_EPDS_total_6y %in% c(13:29), 1,
                                            ifelse(alspac.table$p_EPDS_total_6y  %in% c(0:12), 0, NA)) # EPDS (>12 risk, <=12 no risk)

##########################################################################################################################################################################

# Saving
 
save(alspac.table, file ='alspac.table.Rdata')

####################################################################################################################################################



