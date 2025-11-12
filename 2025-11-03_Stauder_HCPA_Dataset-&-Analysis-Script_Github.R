################################################################################
# Human Connectome Project - Aging (HCP)

# Stauder M, Horn O, Hayes SM. The role of age and physical fitness on the 
# relationship between physical activity and executive function. 
# J Int Neuropsychol Soc. 2025 Oct 23:1-11. doi: 10.1017/S1355617725101446. 

# Dataset Generation R Script

################################################################################

# load necessary packages
library(dplyr)
library(psych)
library(tidyr)
library(ggpubr)
library(e1071)
#library(tidyverse)
#library(Hmisc)
#library(caret)


################################################################################
### Creating Dataset ###
################################################################################
# Pull together data matrices 

# Independent Variables

# Demographics
demographics <- read.table("/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Raw-Data/ndar_subject01.txt", header = TRUE)
demographics <- demographics %>% select(src_subject_id, interview_age, sex, race, ethnic_group)

# Physical Activity
ipaq <- read.table("/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Raw-Data/ipaq01.txt", header = TRUE)
# ipaq2, ipaq3a, ipaq3b, ipaq4, ipaq5a, ipaq5b, ipaq6, ipaq7a, ipaq7b

# Sleep Quality
psqi <- read.table("/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Raw-Data/psqi01.txt", header = TRUE, sep = '\t')
# psqi_total, csq_psqi_se, psqip4

# Fitness Metrics (Locomotion, Endurance, Strength)
# loco_rawscore <- fastest gaitspeed time
# raw_wet <- 2m walk endurance test distance
# msgs_dominanttrial, msgs_nondominanttrial <- max grip strength dominant and non-dominant hand

fitness <- read.table("/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Raw-Data/tlbx_motor01.txt", header = TRUE)

gaitspeed <- fitness %>% select(src_subject_id, loco_rawscore)
gaitspeed <- gaitspeed[gaitspeed$loco_rawscore > 1,]

endurance <- fitness %>% select(src_subject_id, raw_wet)
endurance <- endurance[endurance$raw_wet > 1,]

strength <- fitness %>% select(src_subject_id, msgs_dominanttrial, msgs_nondominanttrial)
strength <- strength[strength$msgs_dominanttrial > 1,]

# Cardiometabolic 
# rsptc_no = total cholesterol, rsptrig_no = triglycerides, laba5 = HDL, laba8 = C-reactive protein, friedewald = LDL, glucose = glucose, insomm = insulin

meta <- read.table("/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Raw-Data/bsc01.txt", header = TRUE)
meta$insomm <-  substr(meta$insomm,1,nchar(meta$insomm)-5)
meta <- meta %>% select(src_subject_id, rsptc_no, rsptrig_no, laba5, laba8, friedewald_ldl, glucose, insomm, fasting)

vitals <- read.table("/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Raw-Data/vitals01.txt", header = TRUE)
vitals <- vitals %>% separate(bp, c('systolic', 'diastolic'))
vitals <- vitals %>% select(src_subject_id, weight_std, systolic, diastolic, vtl007)

# Menopause
meno <- read.table("/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Raw-Data/mchq01.txt", header = TRUE)

################################################################################

# Dependent (cognitive) variables

# Executive Functions

# Cognitive Flexibility
# Dimensional Change Card Sort Test (DCCS)
dccs <- read.table("/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Raw-Data/dccs01.txt", header = TRUE)
# nih_dccs_computed

# Inhibitory Control
# Flanker Task 
flanker <- read.table("/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Raw-Data/flanker01.txt", header = TRUE)
# nih_flanker_computed

# Switching
# Trails A & B
trails <- read.table("/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Raw-Data/trail_ca01.txt", header = TRUE)
# tmta_raw, tmtb_raw

# Processing Speed
# Pattern Comparison Processing Speed Test (PCPS)
pcps <- read.table("/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Raw-Data/pcps01.txt", header = TRUE)
# nih_patterncomp_raw

# Working Memory
# List Sorting Working Memory Test (LSWMT)
lswmt <- read.table("/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Raw-Data/lswmt01.txt", header = TRUE)
# tbx_ls

# MoCA
moca <- read.table("/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Raw-Data/moca01.txt", header = TRUE)
# moca_total, moca_edu


HCP <- Reduce(function(x,y) full_join(x,y, by = "src_subject_id"), list(demographics, ipaq, psqi, gaitspeed, endurance, strength, meta, vitals, dccs, flanker, trails, lswmt, pcps,  moca, meno))
HCP <- HCP[-1,]

HCP <- HCP %>% select(src_subject_id, interview_age.x, sex.x, race, ethnic_group, ipaq2, ipaq3a, ipaq3b, ipaq4, ipaq5a, ipaq5b, ipaq6, ipaq7a, ipaq7b, psqi_component1, psqi_component2, psqip4, insomnia8, psqi_com5, psqi_component6, psqi_com7, csq_psqi_se, raw_wet, msgs_dominanttrial, msgs_nondominanttrial, loco_rawscore, rsptc_no, rsptrig_no, laba5, laba8, friedewald_ldl, glucose, insomm, fasting,  weight_std, systolic, diastolic, vtl007, nih_dccs_computed, nih_dccs_unadjusted, nih_flanker_computed, nih_flanker_unadjusted, nih_patterncomp_raw, tmtb_raw, tmta_raw, tbx_ls, uss, moca_total, moca_edu, straw_code)
HCP <- HCP[-c(726, 727, 728),]




################################################################################
### Defining Variables ### 
################################################################################


HCP$ipaq2 <- as.numeric(HCP$ipaq2)
HCP$ipaq2[is.na(HCP$ipaq2)] = 0

HCP$ipaq3a <- as.numeric(HCP$ipaq3a)
HCP$ipaq3a[is.na(HCP$ipaq3a)] = 0

HCP$ipaq3b <- as.numeric(HCP$ipaq3b)
HCP$ipaq3b[is.na(HCP$ipaq3b)] = 0


HCP$ipaq4 <- as.numeric(HCP$ipaq4)
HCP$ipaq4[is.na(HCP$ipaq4)] = 0

HCP$ipaq5a <- as.numeric(HCP$ipaq5a)
HCP$ipaq5a[is.na(HCP$ipaq5a)] = 0

HCP$ipaq5b <- as.numeric(HCP$ipaq5b)
HCP$ipaq5b[is.na(HCP$ipaq5b)] = 0


HCP$ipaq6 <- as.numeric(HCP$ipaq6)
HCP$ipaq6[is.na(HCP$ipaq6)] = 0

HCP$ipaq7a <- as.numeric(HCP$ipaq7a)
HCP$ipaq7a[is.na(HCP$ipaq7a)] = 0

HCP$ipaq7b <- as.numeric(HCP$ipaq7b)
HCP$ipaq7b[is.na(HCP$ipaq7b)] = 0

# Total Days Active
HCP$ipaq8 <- HCP$ipaq2 + HCP$ipaq4 + HCP$ipaq6

HCP$ipaq_vig_min <- ((HCP$ipaq3a)*60) + (HCP$ipaq3b)
HCP$ipaq_vig_min[HCP$ipaq_vig_min > 180] <- 180
HCP$ipaq_vig <- ((HCP$ipaq2)*HCP$ipaq_vig_min)
HCP$ipaq_vig_MET <- (8.0*HCP$ipaq_vig)

HCP$ipaq_mod_min <- ((HCP$ipaq5a)*60) + (HCP$ipaq5b)
HCP$ipaq_mod_min[HCP$ipaq_mod_min > 180] <- 180
HCP$ipaq_mod <- ((HCP$ipaq4)*HCP$ipaq_mod_min)
HCP$ipaq_mod_MET <- (4.0*HCP$ipaq_mod)

HCP$ipaq_walk_min <- ((HCP$ipaq7a)*60) + (HCP$ipaq7b)
HCP$ipaq_walk_min[HCP$ipaq_walk_min > 180] <- 180
HCP$ipaq_walk <- ((HCP$ipaq6)*HCP$ipaq_walk_min)
HCP$ipaq_walk_MET <- (3.3*HCP$ipaq_walk)

#HCP_PA_Test$ipaq_mvpa <- HCP_PA_Test$ipaq_vig + HCP_PA_Test$ipaq_mod

HCP$ipaq_total <- HCP$ipaq_vig + HCP$ipaq_mod + HCP$ipaq_walk
HCP$ipaq_total_MET <- (HCP$ipaq_vig_MET + HCP$ipaq_mod_MET + HCP$ipaq_walk_MET)


# Age
HCP$interview_age.x <- as.numeric(HCP$interview_age.x)
HCP$interview_age <- (HCP$interview_age.x/12)


# Sex
HCP$sex <- as.factor(HCP$sex.x)

# Education
HCP$moca_edu <- as.numeric(HCP$moca_edu)


# Cognitive Status
HCP$moca_total <- as.numeric(HCP$moca_total)
HCP$moca_total[HCP$moca_total > 30] <- 30
HCP$moca_status <- ifelse(HCP$moca_total>=23, 0, 1)
HCP$moca_status <- as.factor(HCP$moca_status)

# Menopause Status
# 1 = "Reproductive", 2 = "Menopausal Transition", 3 = "Post Menopause"
HCP$straw_code[HCP$straw_code == 4.1] <- 1
HCP$straw_code[HCP$straw_code == 4.2] <- 1
HCP$straw_code[HCP$straw_code == 2] <- 2
HCP$straw_code[HCP$straw_code == 2.2] <- 2
HCP$straw_code[HCP$straw_code == 3] <- 2
HCP$straw_code[HCP$straw_code == 1.11] <- 3
HCP$straw_code[HCP$straw_code == 1.11004] <- 3
HCP$straw_code[HCP$straw_code == 1.12] <- 3
HCP$straw_code[HCP$straw_code == 1.12002] <- 3
HCP$straw_code[HCP$straw_code == 1.12004] <- 3
HCP$straw_code[HCP$straw_code == 1.13] <- 3
HCP$straw_code[HCP$straw_code == 1.13002] <- 3
HCP$straw_code[HCP$straw_code == 1.13004] <- 3
HCP$straw_code[HCP$straw_code == 1.14] <- 3
HCP$straw_code[HCP$straw_code == 1.14002] <- 3
HCP$straw_code[HCP$straw_code == 1.14004] <- 3
HCP$straw_code[HCP$sex.x == "M"] <- 0

HCP$straw_code <- as.factor(HCP$straw_code)


# Sleep
HCP$psqi_1 <- as.numeric(HCP$psqi_component1)
HCP$psqi_2 <- as.numeric(HCP$psqi_component2)
HCP$sleep_dur[HCP$psqip4 < 5] <- 3
HCP$sleep_dur[HCP$psqip4 >= 5 & HCP$psqip4 < 6] <- 2
HCP$sleep_dur[HCP$psqip4 >= 6 & HCP$psqip4 < 7] <- 1
HCP$sleep_dur[HCP$psqip4 >= 7] <- 0
HCP$psqi_3 <- as.numeric(HCP$sleep_dur)
HCP$psqi_4 <- as.numeric(HCP$insomnia8)
HCP$psqi_5 <- as.numeric(HCP$psqi_com5)
HCP$psqi_6 <- as.numeric(HCP$psqi_component6)
HCP$psqi_7 <- as.numeric(HCP$psqi_com7)
HCP$csq_psqi_se <- as.numeric(HCP$csq_psqi_se)
HCP$csq_psqi_se[HCP$csq_psqi_se > 100] <- 100
HCP$psqi_total <- HCP$psqi_1 + HCP$psqi_2 + HCP$psqi_3 + HCP$psqi_4 + HCP$psqi_5 + HCP$psqi_6 + HCP$psqi_7
HCP$psqi_total <- as.numeric(HCP$psqi_total)


# Fitness
HCP$loco_rawscore <- as.numeric(HCP$loco_rawscore)
HCP$gaitspeed <- (4 / HCP$loco_rawscore)

HCP$raw_wet <- as.numeric(HCP$raw_wet)
HCP$endurance <- HCP$raw_wet

HCP$msgs_dominanttrial <- as.numeric(HCP$msgs_dominanttrial)
HCP$msgs_nondominanttrial <- as.numeric(HCP$msgs_nondominanttrial)
HCP$grip <- ifelse(HCP$msgs_dominanttrial>HCP$msgs_nondominanttrial,HCP$msgs_dominanttrial,HCP$msgs_nondominanttrial)
HCP$grip <- as.numeric(HCP$grip)


# Cardiovascular / Cardiometabolic Risk

HCP$systolic <- as.numeric(HCP$systolic)
HCP$diastolic <- as.numeric(HCP$diastolic)
HCP$glucose <- as.numeric(HCP$glucose)
#HCP$MAP <- ((2*HCP$diastolic) + HCP$systolic)/3
#HCP$MAP <- as.numeric(HCP$MAP)
#HCP$BP <- as.numeric((HCP$systolic + HCP$diastolic)/2)
#HCP$height_in <- as.numeric(HCP$vtl007)
#HCP$weight_lb <- as.numeric(HCP$weight_std)
#HCP$total_chol <- as.numeric(HCP$rsptc_no)
#HCP$HDL <- as.numeric(HCP$laba5)
#HCP$LDL <- as.numeric(HCP$friedewald_ldl)
#HCP$triglyc <- as.numeric(HCP$rsptrig_no)
#HCP$CRP <- as.numeric(HCP$laba8)
#HCP$insulin <- as.numeric(HCP$insomm)
#HCP$height_m <- 0.0254 * HCP$height_in
#HCP$weight_kg <- 0.453592 * HCP$weight_lb
#HCP$BMI <- (HCP$weight_kg/(HCP$height_m * HCP$height_m))
#HCP$iHDL <- -(HCP$HDL)

#HCP$BMI_Z <- (HCP$BMI - mean(HCP$BMI, na.rm = TRUE)) / sd(HCP$BMI, na.rm = TRUE)
#HCP$MAP_Z <- (HCP$MAP - mean(HCP$MAP, na.rm = TRUE)) / sd(HCP$MAP, na.rm = TRUE)
#HCP$BP_Z <- (HCP$BP - mean(HCP$BP, na.rm = TRUE)) / sd(HCP$BP, na.rm = TRUE)
#HCP$insulin_Z <- (HCP$insulin - mean(HCP$insulin, na.rm = TRUE)) / sd(HCP$insulin, na.rm = TRUE)
#HCP$iHDL_Z <- (HCP$iHDL - mean(HCP$iHDL, na.rm = TRUE)) / sd(HCP$iHDL, na.rm = TRUE)
#HCP$triglyc_Z <- (HCP$triglyc - mean(HCP$triglyc, na.rm = TRUE)) / sd(HCP$triglyc, na.rm = TRUE)
#HCP$chol_ratio <- (HCP$total_chol / HCP$HDL)
#HCP$chol_ratio <- as.numeric(HCP$chol_ratio)


# Cognitive Outcomes

HCP$nih_dccs_computed <- as.numeric(HCP$nih_dccs_computed)
HCP$nih_dccs_unadjusted <- as.numeric(HCP$nih_dccs_unadjusted)

HCP$nih_flanker_computed <- as.numeric(HCP$nih_flanker_computed)
HCP$nih_flanker_unadjusted <- as.numeric(HCP$nih_flanker_unadjusted)

HCP$tmtb_raw <- as.numeric(HCP$tmtb_raw)
HCP$tmta_raw <- as.numeric(HCP$tmta_raw)

HCP$tbx_ls <- as.numeric(HCP$tbx_ls)
HCP$uss <- as.numeric(HCP$uss)

HCP$nih_patterncomp_raw <- as.numeric(HCP$nih_patterncomp_raw)


HCP <- HCP %>% select(src_subject_id, interview_age, sex, race, moca_total, moca_status, moca_edu, straw_code, ipaq_walk_MET, ipaq_mod_MET, ipaq_vig_MET, ipaq_total_MET, ipaq_total, nih_dccs_computed, nih_dccs_unadjusted, nih_flanker_computed, nih_flanker_unadjusted, nih_patterncomp_raw, tbx_ls, uss, tmta_raw, tmtb_raw, csq_psqi_se, psqi_total, loco_rawscore, gaitspeed, raw_wet, msgs_dominanttrial, msgs_nondominanttrial, grip, systolic, diastolic, glucose, fasting)
#HCP <- HCP_final[complete.cases(HCP_final),]

summary(HCP)

# Assorted Data Cleaning / Case Removal

# unrealistic/extremely deviant values
#which(HCP$diastolic == 27.00, arr.ind = TRUE)
# 233
#which(HCP$glucose == 310, arr.ind = TRUE)
# 250


# change unrealistic values to NA & impute values
HCP[233, "diastolic"] <- NA
HCP[250, "glucose"] <- NA


# no straw code
# two women - one 68, one 70+ --> making assumption they are post-menopausal
HCP[582, ]
HCP[601, ]

HCP[582, "straw_code"] <- 3
HCP[601, "straw_code"] <- 3


#summary(aov(formula = HCP$glucose ~ HCP$fasting))
#tapply(HCP$glucose, HCP$fasting, mean, na.rm = TRUE)
# no significant difference in glucose bw fasting vs not fasted for blood draw
#fasted_out <- which(HCP$fasting==0)
#HCP = HCP[-c(fasted_out),]

HCP$endurance <- HCP$raw_wet
HCP$trails <- -(HCP$tmtb_raw / HCP$tmta_raw)
HCP$trails <- as.numeric(HCP$trails)

summary(HCP)


HCP <- HCP %>% select(src_subject_id, interview_age, sex, race, moca_total, moca_status, moca_edu, straw_code, ipaq_walk_MET, ipaq_mod_MET, ipaq_vig_MET, ipaq_total_MET, ipaq_total, nih_dccs_computed, nih_flanker_computed, nih_patterncomp_raw, tbx_ls, tmta_raw, tmtb_raw, trails, csq_psqi_se, psqi_total, gaitspeed, endurance, grip, systolic, diastolic, glucose)

# Remove cases that dont have full cog data --> need for valid imputation
HCP <- HCP[complete.cases(HCP[ , c("nih_dccs_computed", "nih_flanker_computed", "tbx_ls", "nih_patterncomp_raw")]), ]
# 725 --> 633

################################################################################
### Missing Data, Outlier Removal, and Assumptions ###
################################################################################

############################## Missing Data Summary ############################
summary(HCP)

# PA
# Ipaq total MET - 0

# Cog
# WM (lswmt) - 91
# Flex (dccs) - 92
# Switch (tmtb/a) - 10
# Inhib (flank) - 92

dccs_out <- which(is.na(HCP$nih_dccs_computed))
flanker_out <- which(is.na(HCP$nih_flanker_computed))
lswmt_out <- which(is.na(HCP$tbx_ls))
trails_out <- which(is.na(HCP$trails))
cog_out <- c(dccs_out, flanker_out, lswmt_out, trails_out)
# 285

# HRF
# strength (grip) - 93
# endur (2mwt) - 116
# gait (4mwt) - 104

grip_out <- which(is.na(HCP$grip))
endur_out <- which(is.na(HCP$endurance))
gait_out <- which(is.na(HCP$gaitspeed))
fitness_out <- c(grip_out, endur_out, gait_out)
# 313

# CV/CM
# sys - 7
# dia - 8
# glucose - 89

systolic_out <- which(is.na(HCP$systolic))
diastolic_out <- which(is.na(HCP$diastolic))
glucose_out <- which(is.na(HCP$glucose))
CV_out <- c(systolic_out, diastolic_out, glucose_out)
# 104

# Sleep
# PSQI - 33

# Covariates
# Age - 0
# Sex - 0
# Race - 0
# Education - 1
# Cog Status - 0
# Menopause - 0



#Variables sorted by number of missings: 

#Variable                 Count
#glucose                  0.121794872
#endurance                0.038461538
#gaitspeed                0.020833333
#systolic                 0.009615385
#diastolic                0.009615385
#grip                     0.003205128
#moca_edu                 0.001602564

#interview_age            0.000000000
#sex                      0.000000000
#race                     0.000000000
#moca_total               0.000000000
#moca_status              0.000000000
#straw_code               0.000000000
#ipaq_walk_MET            0.000000000
#ipaq_mod_MET             0.000000000
#ipaq_vig_MET             0.000000000
#ipaq_total_MET           0.000000000
#ipaq_total               0.000000000

#nih_dccs_computed        0.000000000
#nih_flanker_computed     0.000000000
#tbx_ls                   0.000000000

#tmta_raw                 0.000000000
#tmtb_raw                 0.000000000
#trails                   0.000000000
#csq_psqi_se              0.000000000
#psqi_total               0.046474359



############################## Imputation ######################################

library(mice)
library(VIM)

# Whole Data set
table(is.na(HCP))
# remove sub #
# 179 of 17545 values missing; final dataset: 179 of 9,495

# Color Plot of Missing Data
aggr_plot <- aggr(HCP, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(HCP), cex.axis=.4, gap=2, ylab=c("Histogram of missing data","Pattern"))

HCP0 <- mice(HCP, m=5, method = "pmm", maxit = 50, seed = 500)
densityplot(HCP0)

summary(HCP)

HCP1 <- complete(HCP0, 1)
summary(HCP1)

HCP2 <- complete(HCP0, 2)
summary(HCP2)

HCP3 <- complete(HCP0, 3)
summary(HCP3)

HCP4 <- complete(HCP0, 4)
summary(HCP4)

HCP5 <- complete(HCP0, 5)
summary(HCP5)


HCP_Final <- complete(HCP0, 3)


#double check imputation worked
table(is.na(HCP))
table(is.na(HCP_Final))
# no missing values
# 0.00967% of data imputed


summary(HCP)
summary(HCP_Final)

t.test(HCP$systolic, HCP_Final$systolic)
t.test(HCP$diastolic, HCP_Final$diastolic)
t.test(HCP$glucose, HCP_Final$glucose)
t.test(HCP$endurance, HCP_Final$endurance)
t.test(HCP$gaitspeed, HCP_Final$gaitspeed)
t.test(HCP$grip, HCP_Final$grip)
t.test(HCP$moca_edu, HCP_Final$moca_edu)

########################## Outlier Exploration  ################################

scattOut <- function(VOI) {
  upper3 = mean(VOI,na.rm=TRUE) + 3*sd(VOI,na.rm=TRUE)
  lower3 =  mean(VOI,na.rm=TRUE) - 3*sd(VOI,na.rm=TRUE)
  upper5 = mean(VOI,na.rm=TRUE) + 5*sd(VOI,na.rm=TRUE)
  lower5 =  mean(VOI,na.rm=TRUE) - 5*sd(VOI,na.rm=TRUE)
  plot(VOI)
  abline(h=upper3,col="blue")
  abline(h=lower3,col="blue")
  abline(h=upper5,col="red")
  abline(h=lower5,col="red")
}

#tbx_ls_out <- which(HCP$tbx_ls > (mean(HCP$tbx_ls) + 3*sd(HCP$tbx_ls)) | HCP$tbx_ls < (mean(HCP$tbx_ls) - 3*sd(HCP$tbx_ls)))

# PA
# Walk
scattOut(HCP_Final$ipaq_walk_MET)
# 0
# Mod
scattOut(HCP_Final$ipaq_mod_MET)
# 12
# Vig
scattOut(HCP_Final$ipaq_vig_MET)
# 19

# Total
scattOut(HCP_Final$ipaq_total_MET)
# 20, 2 > 5 SD above mean, consider removing top two
ipaq_Q1 <- quantile(HCP_Final$ipaq_total_MET, .25)
ipaq_Q3 <- quantile(HCP_Final$ipaq_total_MET, .75)
ipaq_IQR <- IQR(HCP_Final$ipaq_total_MET)
which(HCP_Final$ipaq_total_MET < (ipaq_Q1 - 1.5*ipaq_IQR))
# 0
which(HCP_Final$ipaq_total_MET > (ipaq_Q3 + 1.5*ipaq_IQR))
# 47

## Cognitive Variables
# lswmt
scattOut(HCP_Final$tbx_ls)
# 6
tbx_Q1 <- quantile(HCP_Final$tbx_ls, .25)
tbx_Q3 <- quantile(HCP_Final$tbx_ls, .75)
tbx_IQR <- IQR(HCP_Final$tbx_ls)
tbx_out <- which(HCP_Final$tbx_ls < (tbx_Q1 - 1.5*tbx_IQR) | HCP_Final$tbx_ls > (tbx_Q3 + 1.5*tbx_IQR))
# 

#trails (tmtb/tmta)
scattOut(HCP_Final$trails)
# 14, 1 > 5 SD below mean
trails_Q1 <- quantile(HCP_Final$trails, .25)
trails_Q3 <- quantile(HCP_Final$trails, .75)
trails_IQR <- IQR(HCP_Final$trails)
which(HCP_Final$trails < (trails_Q1 - 1.5*trails_IQR))
# 39

# dccs
scattOut(HCP_Final$nih_dccs_computed)
# 8, 5 > 5 SD below mean
# 8 at or below score of 4 (no reaction time component)
dccs_Q1 <- quantile(HCP_Final$nih_dccs_computed, .25)
dccs_Q3 <- quantile(HCP_Final$nih_dccs_computed, .75)
dccs_IQR <- IQR(HCP_Final$nih_dccs_computed)
which(HCP_Final$nih_dccs_computed < (dccs_Q1 - 1.5*dccs_IQR))
# 20

# flank
scattOut(HCP_Final$nih_flanker_computed)
# 14
# 1 at or below score of 4 (no reaction time component)
flank_Q1 <- quantile(HCP_Final$nih_flanker_computed, .25)
flank_Q3 <- quantile(HCP_Final$nih_flanker_computed, .75)
flank_IQR <- IQR(HCP_Final$nih_flanker_computed)
which(HCP_Final$nih_flanker_computed < (flank_Q1 - 1.5*flank_IQR))
# 30

## Fitness
# Gait speed
scattOut(HCP_Final$gaitspeed)
# 11

# Grip
scattOut(HCP_Final$grip)
# 4

# Endurance
scattOut(HCP_Final$endurance)
# 3


## Cardiovascular Risk 
# systolic
scattOut(HCP_Final$systolic)
# 3

# diastolic
scattOut(HCP_Final$diastolic)
# 4

# glucose
scattOut(HCP_Final$glucose)
# 11


# Sleep - PSQI 
scattOut(HCP_Final$psqi_total)
# 7


## Covariates
# Age
scattOut(HCP_Final$interview_age)
# 0

# Education
scattOut(HCP_Final$moca_edu)
# 6

# MoCA
scattOut(HCP_Final$moca_total)
# 1


# Cognitive Outcomes 
dccs_out <- which(HCP_Final$nih_dccs_computed <= 4.00)
# 6
flanker_out <- which(HCP_Final$nih_flanker_computed <= 4.00)
# 2
toolbox_out <- c(dccs_out, flanker_out)
# 8

# 8 total including case 191
HCP_Final = HCP_Final[-c(toolbox_out),]
# 633 - 8 = 625

summary(HCP_Final)


######################### Transform & Standardize ##############################
################################################################################


HCP_Final$race[HCP_Final$race == "White"] <- 0
HCP_Final$race[HCP_Final$race == "American Indian/Alaska Native"] <- 1
HCP_Final$race[HCP_Final$race == "Asian"] <- 2
HCP_Final$race[HCP_Final$race == "Black or African American"] <- 3
HCP_Final$race[HCP_Final$race == "More than one race"] <- 4
HCP_Final$race[HCP_Final$race == "Unknown or not reported"] <- 5
HCP_Final$race <- as.factor(HCP_Final$race)

HCP_Final$sex <- as.character(HCP_Final$sex)
HCP_Final$sex[HCP_Final$sex == 'F'] <- 0
HCP_Final$sex[HCP_Final$sex == 'M'] <- 1
HCP_Final$sex <- as.factor(HCP_Final$sex)


HCP_Final$ipaq_walk_MET_Z <- (HCP_Final$ipaq_walk_MET - mean(HCP_Final$ipaq_walk_MET)) / sd(HCP_Final$ipaq_walk_MET)
HCP_Final$ipaq_mod_MET_Z <- (HCP_Final$ipaq_mod_MET - mean(HCP_Final$ipaq_mod_MET)) / sd(HCP_Final$ipaq_mod_MET)
HCP_Final$ipaq_vig_MET_Z <- (HCP_Final$ipaq_vig_MET - mean(HCP_Final$ipaq_vig_MET)) / sd(HCP_Final$ipaq_vig_MET)
HCP_Final$ipaq_total_MET_Z <- (HCP_Final$ipaq_total_MET - mean(HCP_Final$ipaq_total_MET)) / sd(HCP_Final$ipaq_total_MET)


HCP_Final$dccs_Z <- (HCP_Final$nih_dccs_computed - mean(HCP_Final$nih_dccs_computed)) / sd(HCP_Final$nih_dccs_computed)
HCP_Final$flanker_Z <- (HCP_Final$nih_flanker_computed - mean(HCP_Final$nih_flanker_computed)) / sd(HCP_Final$nih_flanker_computed)
HCP_Final$lswmt_Z <- (HCP_Final$tbx_ls - mean(HCP_Final$tbx_ls)) / sd(HCP_Final$tbx_ls)
HCP_Final$EF_Z <- (HCP_Final$dccs_Z + HCP_Final$flanker_Z + HCP_Final$lswmt_Z)/3

HCP_Final$pcps_Z <- (HCP_Final$nih_patterncomp_raw - mean(HCP_Final$nih_patterncomp_raw)) / sd(HCP_Final$nih_patterncomp_raw)


HCP_Final$psqi_status <- ifelse(HCP_Final$psqi_total<5, 0, 1)
HCP_Final$psqi_status <- as.factor(HCP_Final$psqi_status)

HCP_Final$psqi_total_Z <- (HCP_Final$psqi_total - mean(HCP_Final$psqi_total)) / sd(HCP_Final$psqi_total)
HCP_Final$psqi_se_Z <- (HCP_Final$csq_psqi_se - mean(HCP_Final$csq_psqi_se)) / sd(HCP_Final$csq_psqi_se)


HCP_Final$gaitspeed_Z <- (HCP_Final$gaitspeed - mean(HCP_Final$gaitspeed)) / sd(HCP_Final$gaitspeed)
HCP_Final$endurance_Z <- (HCP_Final$endurance - mean(HCP_Final$endurance)) / sd(HCP_Final$endurance)
HCP_Final$grip_Z <- (HCP_Final$grip - mean(HCP_Final$grip)) / sd(HCP_Final$grip)
HCP_Final$fitness_Z <- (HCP_Final$gaitspeed_Z + HCP_Final$endurance_Z + HCP_Final$grip_Z)/3


HCP_Final$systolic_Z <- (HCP_Final$systolic - mean(HCP_Final$systolic)) / sd(HCP_Final$systolic)
HCP_Final$diastolic_Z <- (HCP_Final$diastolic - mean(HCP_Final$diastolic)) / sd(HCP_Final$diastolic)
HCP_Final$glucose_Z <- (HCP_Final$glucose - mean(HCP_Final$glucose)) / sd(HCP_Final$glucose)
HCP_Final$CV_Risk_Z <- (HCP_Final$systolic_Z + HCP_Final$diastolic_Z + HCP_Final$glucose_Z)/3


HCP_Final$age_Z <- (HCP_Final$interview_age - mean(HCP_Final$interview_age)) / sd(HCP_Final$interview_age)
HCP_Final$education_Z <- (HCP_Final$moca_edu - mean(HCP_Final$moca_edu)) / sd(HCP_Final$moca_edu)



#save(HCP_Final,file="C:/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/HCP_Final_ps")


################################################################################
# Human Connectome Project - Aging (HCP)

# Stauder M, Horn O, Hayes SM. The role of age and physical fitness on the 
# relationship between physical activity and executive function. 
# J Int Neuropsychol Soc. 2025 Oct 23:1-11. doi: 10.1017/S1355617725101446. 

# Analyses R Script

################################################################################

# load necessary packages
library(dplyr)
library(psych)
library(tidyr)
library(ggpubr)
library(e1071)
#library(tidyverse)
#library(Hmisc)
#library(caret)


################################################################################
############################# Load Dataset #####################################
################################################################################

load("C:/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Scripts/HCP_Final_ps")
load("C:/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Scripts/HCP_Final_comp")

HCP_Final$ipaq_MVPA_Z <- (HCP_Final$ipaq_mod_MET_Z + HCP_Final$ipaq_vig_MET_Z)/2

# remove one subject with EF Composite < -3 (only outlier)
HCP_Final <- HCP_Final[which(HCP_Final$EF_Z > -3),]
# remove one subject with CV Composite >5 (only outlier)
HCP_Final <- HCP_Final[which(HCP_Final$CV_Risk_Z < 5),]

# Checking to see if results change when removing MVPA SD outliers

#full_model_check <- lm(HCP_Final3$EF_Z ~ HCP_Final3$age_Z + HCP_Final3$sex + HCP_Final3$race + HCP_Final3$education_Z + HCP_Final3$moca_status + HCP_Final3$CV_Risk_Z + HCP_Final3$pcps_Z + HCP_Final3$fitness_Z + HCP_Final3$ipaq_MVPA_Z)
#summary(full_model_check)
#cor.test(HCP_Final3$ipaq_MVPA_Z, HCP_Final3$fitness_Z)
#summary(lm(HCP_Final3$fitness_Z~HCP_Final3$ipaq_MVPA_Z))

# there was no significant effect of removing outliers on results

# over/under meeting MVPA guidelines

#HCP_Final$MVPA_G <- ifelse(HCP_Final$ipaq_MVPA_MET >= 600, 1, 0)
# n = 232 do not meet MVPA guidelines, n = 391 meet or exceed recommendations
#HCP_Final$MVPA_G <- as.factor(HCP_Final$MVPA_G)
#summary(lm(fitness_Z~ipaq_MVPA_Z, data = subset(HCP_Final, MVPA_G == 0)))
#summary(lm(EF_Z ~ age_Z + sex + race + education_Z + moca_status + pcps_Z + CV_Risk_Z + ipaq_MVPA_Z + fitness_Z, data = subset(HCP_Final, MVPA_G == 0)))
# relationships remain largely unchanged above/below MVPA recommendations. 
# fitness sig related to EF in both groups, MVPA not sig when controlling for fitness. However, MVPA had positive trend in those not meeting guidelines, little to no relationship in those above guidelines
# relationship between reported MVPA and fitness stronger in those under the recommended MVPA 

################################################################################
### Comparing Complete Dataset vs Imputed Dataset ###

t.test(HCP_Final$interview_age, HCP_Final_comp$interview_age)

t.test(HCP_Final$EF_Z, HCP_Final_comp$EF_Z)
t.test(HCP_Final$nih_dccs_computed, HCP_Final_comp$nih_dccs_computed)
t.test(HCP_Final$nih_flanker_computed, HCP_Final_comp$nih_flanker_computed)
t.test(HCP_Final$tbx_ls, HCP_Final_comp$tbx_ls)

t.test(HCP_Final$endurance, HCP_Final_comp$endurance)
t.test(HCP_Final$grip, HCP_Final_comp$grip)
t.test(HCP_Final$gaitspeed, HCP_Final_comp$gaitspeed)

t.test(HCP_Final$ipaq_MVPA_Z, HCP_Final_comp$ipaq_MVPA_Z)
t.test(HCP_Final$ipaq_mod_MET, HCP_Final_comp$ipaq_mod_MET)
t.test(HCP_Final$ipaq_vig_MET, HCP_Final_comp$ipaq_vig_MET)

HCP_Final_comp$complete_case <- 1
HCP_Final$complete_case <- HCP_Final_comp$complete_case[match(HCP_Final$src_subject_id, HCP_Final_comp$src_subject_id)]
HCP_Final$complete_case[is.na(HCP_Final$complete_case)] = 0
HCP_Final$complete_case <- as.factor(HCP_Final$complete_case)
# complete case = 1, case w/ missing data = 0 
# n=517 complete cases, n=106 w/ missing data


summary(aov(formula = HCP_Final$age_Z ~ HCP_Final$complete_case))
TukeyHSD(aov(formula = HCP_Final$age_Z ~ HCP_Final$complete_case))
# no sig diff in age, 0.03 SD diff

summary(aov(formula = HCP_Final$ipaq_MVPA_Z ~ HCP_Final$complete_case))
TukeyHSD(aov(formula = HCP_Final$ipaq_MVPA_Z ~ HCP_Final$complete_case))
# complete cases had 0.19 SD higher MVPA scores (sig)


summary(aov(formula = HCP_Final$EF_Z ~ HCP_Final$complete_case))
TukeyHSD(aov(formula = HCP_Final$EF_Z ~ HCP_Final$complete_case))
# complete cases had 0.19 SD higher EF scores (sig)

summary(aov(formula = HCP_Final$dccs_Z ~ HCP_Final$complete_case))
TukeyHSD(aov(formula = HCP_Final$dccs_Z ~ HCP_Final$complete_case))
# complete cases had 0.17 SD higher dccs scores (trend)

summary(aov(formula = HCP_Final$flanker_Z ~ HCP_Final$complete_case))
TukeyHSD(aov(formula = HCP_Final$flanker_Z ~ HCP_Final$complete_case))
# complete cases had 0.13 SD higher flanker scores (not sig)

summary(aov(formula = HCP_Final$lswmt_Z ~ HCP_Final$complete_case))
TukeyHSD(aov(formula = HCP_Final$lswmt_Z ~ HCP_Final$complete_case))
# complete cases had 0.27 SD higher lswmt scores (sig)



summary(aov(formula = HCP_Final$fitness_Z ~ HCP_Final$complete_case))
TukeyHSD(aov(formula = HCP_Final$fitness_Z ~ HCP_Final$complete_case))
# complete cases had 0.10 SD higher fitness scores (not sig)

summary(aov(formula = HCP_Final$endurance_Z ~ HCP_Final$complete_case))
TukeyHSD(aov(formula = HCP_Final$endurance_Z ~ HCP_Final$complete_case))
# complete cases had 0.24 SD higher endurance (sig)

summary(aov(formula = HCP_Final$grip_Z ~ HCP_Final$complete_case))
TukeyHSD(aov(formula = HCP_Final$grip_Z ~ HCP_Final$complete_case))
# complete cases had 0.13 SD higher grip (not sig)

summary(aov(formula = HCP_Final$gaitspeed_Z ~ HCP_Final$complete_case))
TukeyHSD(aov(formula = HCP_Final$gaitspeed_Z ~ HCP_Final$complete_case))
# complete cases were 0.07 SD slower (not sig)

################################################################################
### Analyses ###
################################################################################

# Demographics Summary
# Age
# Mean: 59.24 (14.97), Range: 36.00 - 100.00

# Sex
# M(1) - 263 (42.22%)
# F(0) - 360 (57.78%)

# Race
# White (0) - 445 (71.43%)
# AI/AN (1) - 2 (0.32%)
# Asian (2) - 48 (7.70%)
# Black/AA (3) -  88 (14.13%)
# Mixed (4) - 27 (4.33%)
# Un/NR (5) - 13 (2.09%)

# Education 
# Mean: 17.50 (2.16), Range: 7.0 - 21.0

# MoCA Cognitive Status (Cut off: 23)
# Cognitively Normal: 567 (91.01%), MCI: 56 (8.99%)

# MoCA Full score 
# Mean: 26.38 (2.52), Range: 19 - 30

# Menopausal Status 
# Men: 263 (%), Reproductive: 73 (%), Peri-menopausal 44 (%), Post-menopausal 243 (%)

# IPAQ Total MET
# Mean: 2938.2 (2738.3), Range: 0.0 - 16,638.0
# IPAQ Walk MET
# Mean: 1139 (1206.6), Range: 0.0 - 4158

# IPAQ Moderate MET
# Mean: 686 (991.5), 0.0 - 5,040
# IPAQ Vigorous MET
# Mean: 1114 (1699.3), 0.0 - 10,080
# IPAQ MVPA MET
# Mean: 1799 (2204.2), Range: 0 - 12,480

# Fitness
# Gaitspeed (m/s)
# Mean: 1.27 (0.25), Range: 0.47 - 2.42
# Grip Strength (lbs)
# Mean: 74.64 (24.71), Range: 21.4 - 168.1
# 2MWT (ft)
# Mean: 607.4 (101.61), Range: 194 - 907

# CV Risk
# Systolic BP (mmHg)
# Mean: 129.3 (16.3), Range: 79 - 189 
# Diastolic BP (mmHg)
# Mean: 79.5 (10.4), Range: 51 - 114
# Glucose (??)
# Mean: 101.7 (15.5), Range: 64 - 249

# Cognition
# PCPS
# Mean: 43.13 (8.96), Range: 16 - 101

# Flanker
# Mean: 8.01 (0.81), Range: 4.13 - 10.00
# DCCS
# Mean: 8.17 (0.91), Range: 5.4 - 10.00
# LSWMT
# Mean: 16.83 (2.95), Range: 7 - 26

#mean(HCP_Final$)
#sd(HCP_Final$)
#table(HCP_Final$)

#mean(HCP_Final$CV_Risk_Z[which(HCP_Final$sex==1)])

# Correlations: VOI x Age

cor.test(HCP_Final$age_Z, HCP_Final$ipaq_total_MET_Z)
# r = -0.086, p = 0.031
cor.test(HCP_Final$age_Z, HCP_Final$ipaq_walk_MET_Z)
# r = -0.138, p < 0.001
cor.test(HCP_Final$age_Z, HCP_Final$ipaq_mod_MET_Z)
# r = -0.0019, p = 0.961
cor.test(HCP_Final$age_Z, HCP_Final$ipaq_vig_MET_Z)
# r = -0.040, p = 0.315

cor.test(HCP_Final$age_Z, HCP_Final$fitness_Z)
# r = -0.447, p < 0.001
cor.test(HCP_Final$age_Z, HCP_Final$gaitspeed_Z)
# r = -0.24, p < 0.001
cor.test(HCP_Final$age_Z, HCP_Final$grip_Z)
# r = -0.292, p < 0.001
cor.test(HCP_Final$age_Z, HCP_Final$endurance_Z)
# r = -0.479, p < 0.001

cor.test(HCP_Final$age_Z, HCP_Final$CV_Risk_Z)
# r = 0.228, p < 0.001
cor.test(HCP_Final$age_Z, HCP_Final$systolic_Z)
# r = 0.341, p < 0.001
cor.test(HCP_Final$age_Z, HCP_Final$diastolic_Z)
# r = -0.033, p = 0.407
cor.test(HCP_Final$age_Z, HCP_Final$glucose_Z)
# r = 0.208, p < 0.001

cor.test(HCP_Final$age_Z, HCP_Final$EF_Z)
# r = -0.491, p < 0.001
cor.test(HCP_Final$age_Z, HCP_Final$flanker_Z)
# r = -0.443, p < 0.001
cor.test(HCP_Final$age_Z, HCP_Final$dccs_Z)
# r = -0.400, p < 0.001
cor.test(HCP_Final$age_Z, HCP_Final$lswmt_Z)
# r = -0.335, p < 0.001
cor.test(HCP_Final$age_Z, HCP_Final$pcps_Z)
# r = -0.496, p < 0.001

cor.test(HCP_Final$EF_Z, HCP_Final$pcps_Z)
# r = 0.559 p < 0.001

# Group Differences: VOI x Sex

summary(aov(formula = HCP_Final$ipaq_MVPA_Z ~ HCP_Final$sex))
# NS: p = 0.08
TukeyHSD(aov(formula = HCP_Final$ipaq_MVPA_Z ~ HCP_Final$sex))
# diff = 0.114, 95% CI = -0.014 - 0.242, p = 0.082

summary(aov(formula = HCP_Final$fitness_Z ~ HCP_Final$sex))
# p < 0.001
TukeyHSD(aov(formula = HCP_Final$fitness_Z ~ HCP_Final$sex))
# diff = 0.673, 95% CI = 0.566 - 0.78, p < 0.001

summary(aov(formula = HCP_Final$CV_Risk_Z ~ HCP_Final$sex))
# p < 0.001
TukeyHSD(aov(formula = HCP_Final$CV_Risk_Z ~ HCP_Final$sex))
# diff = 0.328, 95% CI = 0.217 - 0.439, p < 0.001

#summary(aov(formula = HCP_Final$psqi_total_Z ~ HCP_Final$sex))
# NS: 

summary(aov(formula = HCP_Final$EF_Z ~ HCP_Final$sex))
# NS: p = 0.28
summary(aov(formula = HCP_Final$pcps_Z ~ HCP_Final$sex))
# NS: p = 0.65


# Group Differences: VOI x Race

summary(aov(formula = HCP_Final$ipaq_total_MET_Z ~ HCP_Final$race))
# NS: p = 0.209
summary(aov(formula = HCP_Final$fitness_Z ~ HCP_Final$race))
# NS: p = 0.632
summary(aov(formula = HCP_Final$CV_Risk_Z ~ HCP_Final$race))
# NS: p = 0.827
#summary(aov(formula = HCP_Final$psqi_total_Z ~ HCP_Final$race))
# NS
summary(aov(formula = HCP_Final$EF_Z ~ HCP_Final$race))
# p < 0.001
TukeyHSD(aov(formula = HCP_Final$EF_Z ~ HCP_Final$race))
# 3-0 -0.3445978 -0.60499141 -0.08420421 0.0023382
# 3-2 -0.4536359 -0.85412949 -0.05314237 0.0159570
# 4-3  0.6944517  0.20341528  1.18548820 0.0008399

# Scatter Plots by VOI

ggscatter(HCP_Final, x = "ipaq_MVPA_Z", y = "EF_Z", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", main = "Moderate-Vigorous Physical Activity and Executive Function",
          xlab = "MVPA (Z-score)", ylab = "Executive Function (Z-score)")

ggscatter(HCP_Final, x = "ipaq_MVPA_Z", y = "fitness_Z", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", main = "Moderate-Vigorous Physical Activity and Physical Fitness",
          xlab = "MVPA (Z-score)", ylab = "Physical Fitness (Z-score)")

ggscatter(HCP_Final, x = "fitness_Z", y = "EF_Z", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", main = "Fitness and Executive Function",
          xlab = "Physical Fitness (Z-score)", ylab = "Executive Function (Z-score)")


################################################################################
# Cronbach's alpha

library(ltm)

exec <- HCP_Final %>% dplyr::select(dccs_Z, flanker_Z, lswmt_Z)
cronbach.alpha(exec, CI = TRUE)
# alpha = 0.718, 95% CI = 0.679 - 0.752

res_exec <- Hmisc::rcorr(as.matrix(exec))
res_exec$r
res_exec$P
# all EF measures sig correlated
# r-values: 0.356, 0.365, 0.656


fit <- HCP_Final %>% dplyr::select(endurance_Z, grip_Z, gaitspeed_Z)
cronbach.alpha(fit, CI = TRUE)
# alpha = 0.618, 95% CI = 0.562 - 0.669

res_fit <- Hmisc::rcorr(as.matrix(fit))
res_fit$r
res_fit$P
# all fitness measures sig correlated
# r-values range from 0.21 - 0.46


cv <- HCP_Final %>% dplyr::select(systolic_Z, diastolic_Z, glucose_Z)
cronbach.alpha(cv, CI = TRUE)
# alpha = 0.576, 95% CI = 0.506 - 0.635

res_cv <- Hmisc::rcorr(as.matrix(cv))
res_cv$r
res_cv$P
# all CV risk variables sig correlated
# r-values range from 0.14 - 0.59

################################################################################
# Executive Function Composite CFA

library(dplyr)
library(psych)
library(tidyr)
library(ggpubr)
library(lavaan)

EF_model1 <-
  ' EF =~ dccs_Z + flanker_Z + lswmt_Z
'
EF_fit1 <- cfa(EF_model1, data=HCP_Final, std.lv = TRUE, meanstructure = TRUE)
summary(EF_fit1, rsquare=TRUE, fit.measures = TRUE)

#idx <- lavInspect(fit1, "case.idx") # list: 1 vector per group
#HCP_Final$EF_L <- lavPredict(fit1)         # list: 1 matrix per group

fit_model1 <-
  ' Fitness =~ endurance_Z + grip_Z + gaitspeed_Z
'
fit1 <-cfa(fit_model1, data=HCP_Final, std.lv = TRUE, meanstructure = TRUE)
summary(fit1, rsquare=TRUE, fit.measures = TRUE)

################################################################################
# Sample Characteristics


#HCP_Final$sex <- as.character(HCP_Final$sex)
#HCP_Final$sex[HCP_Final$sex == 'F'] <- 0
#HCP_Final$sex[HCP_Final$sex == 'M'] <- 1

# Race fitness differences

HCP_Final$race <- as.factor(HCP_Final$race)

summary(aov(formula = HCP_Final$ipaq_MVPA_Z ~ HCP_Final$race))
# p = 0.308

summary(aov(formula = HCP_Final$gaitspeed_Z ~ HCP_Final$race))
# p = 0.601

summary(aov(formula = HCP_Final$grip_Z ~ HCP_Final$race))
# p = 0.081

summary(aov(formula = HCP_Final$endurance_Z ~ HCP_Final$race))
# p = 0.122

summary(aov(formula = HCP_Final$EF_Z ~ HCP_Final$race))
# p < 0.001
TukeyHSD(aov(formula = HCP_Final$EF_Z ~ HCP_Final$race))
#            diff         lwr        upr     p adj
# 3-0 -0.39371769 -0.62531990 -0.1621155 0.0000221
# 3-2 -0.47440946 -0.83074000 -0.1180789 0.0021439
# 4-3  0.53197863  0.09863393  0.9653233 0.0063846



# Correlation Matrix ###########################################################

HCP_Cor_Matrix <-  HCP_Final %>% select(interview_age, moca_total, moca_edu, ipaq_walk_MET_sqrt_Z, ipaq_mod_MET_sqrt_Z, ipaq_vig_MET_sqrt_Z, ipaq_total_MET_sqrt_Z, grip_Z, gaitspeed_Z, endurance_Z, fitness_Z, systolic_Z, diastolic_Z, glucose_Z, CV_Risk_Z, psqi_total_Z, psqi_se_Z, flanker_Z, dccs_Z, lswmt_Z, tmtb_Z, EF_Z)

com_var_test_matrix <- HCP_Final %>% dplyr::select(ipaq_walk_MET_Z, ipaq_mod_MET_Z, ipaq_vig_MET_Z, ipaq_total_MET_Z, grip_Z, gaitspeed_Z, endurance_Z, fitness_Z, flanker_Z, dccs_Z, lswmt_Z, pcps_Z, EF_Z)

cor.all_data <- cor(com_var_test_matrix, method ="pearson", use = "pairwise.complete.obs")

library(Hmisc)
res2 <- rcorr(as.matrix(com_var_test_matrix))
res2$r
res2$P

#p.mat <- cor.mtest(cor.all_data)
#corrplot(cor.all_data, order="hclust", p.mat = p.mat, sig.level = 0.05, type = "upper")

#corrplot(res2$r, type = "upper", order = "hclust", p.mat = res2$P, tl.col = "black", tl.srt = 45, sig.level = 0.05, insig = "blank")

library("PerformanceAnalytics")
HCP_Analytics <-  HCP_Final[, c("age_Z", "ipaq_MVPA_Z", "grip_Z", "gaitspeed_Z", "endurance_Z", "pcps_Z", "flanker_Z", "dccs_Z", "lswmt_Z")]
chart.Correlation(HCP_Analytics, histogram=TRUE, pch=36)

################################################################################
# FULL MULTIPLE REGRESSION

HCP_Final$race <- as.factor(HCP_Final$race)
HCP_Final$straw_code <- as.factor(HCP_Final$straw_code)
HCP_Final$moca_status <- as.factor(HCP_Final$moca_status)
HCP_Final$sex <- as.factor(HCP_Final$sex)


full_model <- lm(HCP_Final$EF_Z ~ HCP_Final$age_Z + HCP_Final$sex + HCP_Final$race + HCP_Final$education_Z + HCP_Final$moca_status + HCP_Final$pcps_Z + HCP_Final$CV_Risk_Z + HCP_Final$ipaq_MVPA_Z + HCP_Final$fitness_Z)
summary(full_model)

#cognorm <- HCP_Final[HCP_Final$moca_status=="0",]
#full_model <- lm(cognorm$EF_Z ~ cognorm$age_Z + cognorm$sex + cognorm$race + cognorm$education_Z + cognorm$CV_Risk_Z + cognorm$pcps_Z + cognorm$fitness_Z + cognorm$ipaq_MVPA_Z)
#summary(full_model)


model.mvpa1 <- lm(HCP_Final$EF_Z ~ HCP_Final$age_Z + HCP_Final$race + HCP_Final$sex + HCP_Final$education_Z + HCP_Final$moca_status + HCP_Final$pcps_Z + HCP_Final$CV_Risk_Z + HCP_Final$fitness_Z)
summary(model.mvpa1) 
model.mvpa2 <- lm(HCP_Final$EF_Z ~ HCP_Final$age_Z + HCP_Final$race + HCP_Final$sex + HCP_Final$education_Z + HCP_Final$moca_status + HCP_Final$pcps_Z + HCP_Final$CV_Risk_Z + HCP_Final$fitness_Z + HCP_Final$ipaq_MVPA_Z)
summary(model.mvpa2)
anova(model.mvpa1, model.mvpa2)



model.fit1 <- lm(HCP_Final$EF_Z ~ HCP_Final$age_Z + HCP_Final$race + HCP_Final$sex + HCP_Final$education_Z + HCP_Final$moca_status + HCP_Final$pcps_Z + HCP_Final$CV_Risk_Z + HCP_Final$ipaq_MVPA_Z)
summary(model.fit1)
model.fit2 <- lm(HCP_Final$EF_Z ~ HCP_Final$age_Z + HCP_Final$race + HCP_Final$sex + HCP_Final$education_Z + HCP_Final$moca_status + HCP_Final$pcps_Z + HCP_Final$CV_Risk_Z + HCP_Final$ipaq_MVPA_Z + HCP_Final$fitness_Z)
summary(model.fit2)
anova(model.fit1, model.fit2)



# REGRESSION ASSUMPTIONS

library(psych)
library(tidyverse)
library(broom)
library(Hmisc)
library(caret)
library(lmtest)

model.diag.metrics <- augment(full_model)
head(model.diag.metrics)

par(mfrow = c(2, 2))
plot(full_model)


# Linearity
plot(full_model, 1, id.n = 0)
# no apparent pattern observed

# Normality of Residuals
hist(full_model$residuals)
plot(full_model, 2, id.n = 0)
# histogram appears approximately normal
# probability plot falls underneath normal plot on lower end
# warrants checking for outliers w/ high leverage

# Homogeneity of Variance
plot(full_model, 3, id.n = 0)
# appears acceptable
# 

# Homoscedacity
lmtest::bptest(full_model)
# BP = 15.865, p = 0.233
# not significant so no sig heteroscedacity

plot(full_model, 5, id.n = 0)
# ~4 possible outliers (standardized residuals > 3 standard errors from regression line)
# but, no high leverage points (points all within acceptable cook's distance)

# 109, 125, 387, 418, 429, 532 out -> standardized residuals beyond +/- 3
# explore 129, 413 -> high leverage statistics; hat-values > 2(9+1)/717 = 0.027894

# Influential Cases - Cook's Distance
plot(full_model, 4, id.n = 10)
HCP_Final$cooks <- cooks.distance(full_model)
# appears acceptable


# Multicollinearity - Variance Inflation Factors (VIF)
# This step uses Variance Inflation Factor scores to screen for multicollinearity. 
# Scores greater than 10 are "red flags" but there are no official cut-off values.
library(car)
car::vif(full_model)
# - results show none of our variables have concerning VIF scores. 




# RELAIMPO

library(relaimpo)
library(ggpubr)
library(ggpattern)

calc.relimp(full_model, type = "lmg", diff = TRUE, rank = TRUE)

df <- read_excel("C:/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Scripts/df.xlsx")

df$group <- c("Demographic", "Demographic", "Demographic", "Demographic", "Cognitive", "Cognitive", "Health", "Health", "Health")
df$group <- factor(df$group, levels = c("Demographic", "Cognitive", "Health"))

a <- ggplot(data=df, aes(x=variable, y=variance, fill=group, pattern=group)) + geom_bar(stat="identity") +
  ylim(c(0,0.175)) + theme_minimal() + scale_fill_manual(name ="Legend", values = c("Cognitive" = "#56B4E9", "Demographic" = "#E69F00", "Health" = "red")) +
  scale_x_discrete(limits=c("Age", "Sex", "Race", "Education", "Cognitive Status", "Processing Speed", "Cardiometabolic Risk", "MVPA", "Fitness"), guide = guide_axis(angle = 45)) +
  labs(y="Variance Explained", x = " ") + theme(axis.title = element_text(size=8), axis.text = element_text(size = 8)) + theme_light(base_size = 12) + geom_text(aes(label=variance), vjust=-0.3, size=2.7) +
  theme(legend.position = "top", legend.text=element_text(size=8), axis.title=element_text(size=10), legend.title = element_blank(), legend.key.size = unit(0.5, 'cm')) +
  guides(fill=guide_legend(
    keywidth=0.125,
    keyheight=0.125,
    default.unit="inch")
  )
ggsave("C:/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Manuscript/Figures/Figure-1_relaimpo_no-legend-title.png", a, width = 3.37, height = 3.76, units = "in", dpi = 300, bg = "white")

############################ PROCESS MODERATION ################################
# must run entire 'process' script to load function
# load("C:/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HCP/Matthew_Masters/Scripts/process")


# Aim 2 Moderation

HCP_Final$sex.num <- as.numeric(HCP_Final$sex)
HCP_Final$moca_status.num <- as.numeric(HCP_Final$moca_status)
HCP_Final$race.num <- as.numeric(HCP_Final$race)
HCP_Final$straw_code.num <- as.numeric(HCP_Final$straw_code)


# PA X Age Moderation

process(data = HCP_Final, y = "EF_Z", x = "ipaq_MVPA_Z", w = "age_Z", cov = c("race.num", "sex.num", "education_Z", "moca_status.num", "fitness_Z", "CV_Risk_Z", "pcps_Z"), model = 1, modelbt = 1, jn = 1, plot = 1)

x1 <- c(-0.6723, -0.2684, 0.6000, -0.6723, -0.2684, 0.6000, -0.6723, -0.2684, 0.6000)
w1 <- c(-1.1536, -1.1536, -1.1536, -0.0845, -0.0845, -0.0845, 1.1199, 1.1199, 1.1199)
y1 <- c(0.2403, 0.2300, 0.2080, 0.0544, 0.0340, -0.0098, -0.1550, -0.1868, -0.2552)
wmarker1 <- c(15, 15, 15, 16, 16, 16, 17, 17, 17)

plot(y=y1, x=x1, cex = 3.0, cex.lab = 2.0, cex.axis = 2.2, pch=wmarker1, xlim = c(-1.0, 1.0), ylim = c(-0.5, 0.7), col="black")
legend.txt <- c("Age 16% (44.4 years)", "Age 50% (59.2 years)", "Age 84% (74.1 years)")
legend("topleft", legend = legend.txt, cex=2.0, lty = c(1, 3, 6), lwd = c(3, 4, 3), pch = c(15, 16, 17))
lines(x1[w1==-1.1536], y1[w1==-1.1536], lwd=4, col="black")
lines(x1[w1==-0.0845], y1[w1==-0.0845], lwd=5, lty=3, col="black")
lines(x1[w1==1.1199], y1[w1==1.1199], lwd=4, lty=6, col="black")


# Fitness x Age Moderation

process(data = HCP_Final, y = "EF_Z", x = "fitness_Z", w = "age_Z", cov = c("race.num", "sex.num", "education_Z", "moca_status.num", "ipaq_MVPA_Z", "CV_Risk_Z", "pcps_Z"), model = 1, modelbt = 1, jn = 1, plot = 1)

x2 <- c(-0.7185, 0.0663, 0.7228, -0.7185, 0.0663, 0.7228, -0.7185, 0.0663, 0.7228)
w2 <- c(-1.1536, -1.1536, -1.1536, -0.0845, -0.0845, -0.0845, 1.1199, 1.1199, 1.1199)
y2 <- c(0.0344, 0.2410, 0.4138, -0.1770, 0.0432, 0.2275, -0.4151, -0.1795, 0.0176)
wmarker2 <- c(15, 15, 15, 16, 16, 16, 17, 17, 17)

plot(y=y2, x=x2, cex = 3.0, cex.lab = 2.0, cex.axis = 2.2, pch=wmarker2, xlim = c(-1.0, 1.0), ylim = c(-0.5, 0.7), col="black")
legend.txt <- c("Age 16% (44.4 years)", "Age 50% (59.2 years)", "Age 84% (74.1 years)")
legend("topleft", legend = legend.txt, cex=2.0, lty = c(1, 3, 6), lwd = c(3, 4, 3), pch = c(15, 16, 17))
lines(x2[w2==-1.1536], y2[w2==-1.1536], lwd=4, col="black")
lines(x2[w2==-0.0845], y2[w2==-0.0845], lwd=5, lty=3, col="black")
lines(x2[w2==1.1199], y2[w2==1.1199], lwd=4, lty=6, col="black")

# PA x Fitness Moderation
#process(data = HCP_Final, y = "EF_Z", x = "ipaq_MVPA_Z", w = "fitness_Z", cov = c("age_Z", "race.num", "straw_code.num", "education_Z", "moca_status.num", "CV_Risk_Z", "pcps_Z"), model = 1, jn = 1, plot = 1)
#x2 <- c(-0.8194, -0.2561, 0.7568, -0.8194, -0.2561, 0.7568, -0.8194, -0.2561, 0.7568)
#w2 <- c(-0.7473, -0.7473, -0.7473, 0.0634, 0.0634, 0.0634, 0.7353, 0.7353, 0.7353)
#y2 <- c(-0.1547, -0.1668, -0.1887, 0.0441, 0.0247, -0.0102, 0.2089, 0.1835, 0.1378)
#wmarker1 <- c(15, 15, 15, 16, 16, 16, 17, 17, 17)
#plot(y=y2, x=x2, cex = 1.5, cex.lab = 1.5, pch=wmarker1, xlim = c(-1, 1), ylim = c(-0.5, 0.5), col="black", xlab="Physical Activity (Z)", ylab = "Executive Function (Z)")
#legend.txt <- c("Fitness 16th %", "Fitness 50th %", "Fitness 84th %")
#legend("topleft", legend = legend.txt, cex=1, lty = c(1, 3, 6), lwd = c(2, 3, 2), pch = c(15, 16, 17))
#lines(x2[w2==-0.7473], y2[w2==-0.7473], lwd=2, col="red")
#lines(x2[w2==0.0634], y2[w2==0.0634], lwd=3, lty=3, col="black")
#lines(x2[w2==0.7353], y2[w2==0.7353], lwd=2, lty=6, col="blue")

# PA x CV Risk Moderation
#process(data = HCP_Final, y = "EF_Z", x = "ipaq_MVPA_Z", w = "CV_Risk_Z", cov = c("age_Z", "race.num", "straw_code.num", "education_Z", "moca_status.num", "fitness_Z", "pcps_Z"), model = 1, jn = 1, plot = 1)
#x3 <- c(-0.8194, -0.2561, 0.7568, -0.8194, -0.2561, 0.7568, -0.8194, -0.2561, 0.7568)
#w3 <- c(-0.7280, -0.7280, -0.7280, -0.0443, -0.0443, -0.0443, 0.7159, 0.7159, 0.7159)
#y3 <- c(0.0104, -0.0035, -0.0285, 0.0275, 0.0081, -0.0267, 0.0465, 0.0211, -0.0246)
#wmarker1 <- c(15, 15, 15, 16, 16, 16, 17, 17, 17)
#plot(y=y3, x=x3, cex = 1.5, cex.lab = 1.5, pch=wmarker1, xlim = c(-1, 1), ylim = c(-0.5, 0.5), col="black", xlab="Physical Activity (Z)", ylab = "Executive Function (Z)")
#legend.txt <- c("Cardiovascular Risk 16th %", "Cardiovascular Risk 50th %", "Cardiovascular Risk 84th %")
#legend("topleft", legend = legend.txt, cex=1, lty = c(1, 3, 6), lwd = c(2, 3, 2), pch = c(15, 16, 17))
#lines(x3[w3==-0.7280], y3[w3==-0.7280], lwd=2, col="blue")
#lines(x3[w3==-0.0443], y3[w3==-0.0443], lwd=3, lty=3, col="black")
#lines(x3[w3==0.7159], y3[w3==0.7159], lwd=2, lty=6, col="red")


################################################################################
# Aim 3 Mediation



# PROCESS MEDIATION 

# add save fxn

# Simple mediation w/ Fitness Composite
process(data = HCP_Final, y = "EF_Z", x = "ipaq_MVPA_Z", m = "fitness_Z", cov = c("age_Z", "sex.num", "race.num", "education_Z", "moca_status.num", "CV_Risk_Z", "pcps_Z"), model = 4, total = 1, normal = 1, save = 1, seed = 31216)

# Simple Mediation w/ Fitness Composite, Moderation by Age

process(data = HCP_Final, y = "EF_Z", x = "ipaq_MVPA_Z", m = "fitness_Z", w = "age_Z", cov = c("sex.num", "race.num", "education_Z", "moca_status.num", "CV_Risk_Z", "pcps_Z"), model = 59, total = 1, jn = 1, plot = 1, contrast = 1, save = 1, seed = 31216)



# Multiple Mediation w/ Fitness Subcomponents
process(data = HCP_Final, y = "EF_Z", x = "ipaq_MVPA_Z", m = c("gaitspeed_Z", "endurance_Z", "grip_Z"), cov = c("age_Z", "sex.num", "race.num", "education_Z", "moca_status.num", "CV_Risk_Z", "pcps_Z"), model = 4, total = 1, normal = 1, contrast = 1, seed = 31216)

# Multiple Mediation w/ Fitness Subcomponents, Moderation by Age
process(data = HCP_Final, y = "EF_Z", x = "ipaq_MVPA_Z", m = c("gaitspeed_Z", "endurance_Z", "grip_Z"), w = "age_Z", cov = c("sex.num", "race.num", "education_Z", "moca_status.num", "CV_Risk_Z", "pcps_Z"), model = 59, total = 1, normal = 1, contrast = 1, seed = 31216)
