# PA-Fitness-EF_MStauder
A repository containing the code used for "The role of age and physical fitness on the relationship between physical activity and executive function" (Stauder et al. 2025)
  https://www.cambridge.org/core/journals/journal-of-the-international-neuropsychological-society/article/role-of-age-and-physical-fitness-on-the-relationship-between-physical-activity-and-executive-function/51453C4E86769B0F30AC8A6FB31EA54F

# About

- Coding Language: [R]
- Version: [version 4.1.1]
- Required Packages:
    library(dplyr)
    library(psych)
    library(tidyr)
    library(ggpubr)
    library(e1071)
    library(tidyverse)
    library(Hmisc)
    library(caret)
    library(mice)
    library(VIM)
    library(ltm)
    library(lavaan)
    library("PerformanceAnalytics")
    library(broom)
    library(lmtest)
    library(car)
    library(relaimpo)
    library(ggpubr)
    library(ggpattern)
    library(process)

# Usage

Goal: three aims: 1) to examine the relative importance and magnitude of the direct associations of physical activity and physical fitness on executive function performance compared to other variables known to be associated with executive function such as age and processing speed, 2) to investigate the interaction of physical activity and physical fitness with age on executive function performance, and 3) to explore the relative indirect relationship of physical activity on executive function performance via their associations with physical fitness metrics such as cardiorespiratory fitness, strength, and speed.
Original Usage: [all data obtained from HCP] Predictor variables = demographic variables, processing speed, physical fitness/performance variables (2-minute walk test, grip strength, gait speed); Outcome variables = Composite Executive Function Score (average performance across NIH Toolbox dimensional change card sort, list sort working memory, and flanker tasks). 

How to Use: 
- To run this code, you will use the attached R code and HCP Lifespan 2.0 Data release (available at [https://www.humanconnectome.org/study/hcp-lifespan-aging]).

# BBAL File Paths
 - Dataset: "\Projects\BADS_2021E0199\HCP\Matthew_Masters\Raw-Data"
 - Dataset Documentation: "\Projects\BADS_2021E0199\HCP\Matthew_Masters\Literature\$HCP&Methodology"
         *Folder contains documentation for HCP project, HCP supplemental material, and LS 2.0 data release ("LS_2.0_Release_Appendix_1" and "LS_2.0_Release_Appendix_2") + access instructions
 - Script:   "\Projects\BADS_2021E0199\HCP\Matthew_Masters\Scripts\2025-11-03_Stauder_HCPA_Dataset-&-Analysis-Script_Github.R"
