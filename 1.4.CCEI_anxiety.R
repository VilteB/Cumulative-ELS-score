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

# Saving
 
save(alspac.table, file ='alspac.table.Rdata')

####################################################################################################################################################



