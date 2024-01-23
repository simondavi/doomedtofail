# doomedtofail_v1-1.R
#
# Comment: 
#
# Input: SC5_CohortProfile_D_18-0-0.sav
#        SC5_pTargetCATI_D_18-0-0.sav
#        SC5_pTargetCAWI_D_18-0-0.sav
#        SC5_spSchool_D_18-0-0.sav
#        SC5_spVocTrain_D_18-0-0.sav
# Output: --
#
# Contents: (1) Set Working Directory, Load Packages
#           (2) Read Data and Data Management
#           (3) Merge Data
#           (4) Compute Scale Scores and New Variables
#           (5) LCA
#           (6) SEM
#
# Last mod: Jan/15/2024, David Simon


####  ------------- (1) Set Working Directory, Load Packages ------------- ####

library(haven)           # to import SPSS files
library(tidyverse)       # for data management
library(rquery)          # to copy STATA merge behavior
library(fauxnaif)        # for defining missing values 
                           # (remotes::install_github("rossellhayes/fauxnaif")
library(psych)           # for scale construction
library(tidySEM)         # for LCA
library(rqdatatable)



#### ----------------- (2) Read Data and Data Management ----------------- ####

# Cohort Profile as a starting point

cohort <- haven::read_sav("Data_SC5_D_18-0-0/SC5_CohortProfile_D_18-0-0.sav") %>%
          dplyr::select(ID_t, wave, cohort,
                                    tx80121,      # oversample of tea edu (= 1)
                                    tx80107) %>%  # first participation in wave
          dplyr::filter(wave == 1)                       


# Filter measures from CATI:

cati_w1 <- haven::read_sav("Data_SC5_D_18-0-0/SC5_pTargetCATI_D_18-0-0.sav") %>%
           dplyr::select(ID_t, wave, tg02001_ha,  # intended degree
                                     tg24150_g1,  # foreign student (!= 3)
                                     t731301_g1,  # parental education
                                     t731351_g1,  
                                     t731403_g8,  # parental occupation
                                     t731453_g8,
                                     t405060_g1,  # migration background
                                     t405090_g1,
                                     t70000m, t70000y, # date of birth
                                     t700001) %>% # gender
           dplyr::filter(wave == 1)
           
cati_b5 <- haven::read_sav("Data_SC5_D_18-0-0/SC5_pTargetCATI_D_18-0-0.sav") %>%
           dplyr::select(ID_t, wave, t66800a, t66800f,           # extra
                                     t66800b, t66800g, t66800k,  # agree
                                     t66800c, t66800h,           # consc
                                     t66800d, t66800i,           # neuro
                                     t66800e, t66800j) %>%       # opene
          dplyr::filter(wave == 3)
  
cati_b5_w10 <- haven::read_sav("Data_SC5_D_18-0-0/SC5_pTargetCATI_D_18-0-0.sav") %>%
               dplyr::select(ID_t, wave, t66800a, t66800f,           
                                         t66800b, t66800g, t66800k,  
                                         t66800c, t66800h,           
                                         t66800d, t66800i,           
                                         t66800e, t66800j) %>%      
               dplyr::filter(wave == 10)

cati_do1 <- haven::read_sav("Data_SC5_D_18-0-0/SC5_pTargetCATI_D_18-0-0.sav") %>%
            dplyr::select(ID_t, wave, tg60031) %>%
            dplyr::filter(wave == 10)

  
# Filter measures from CAWI:

cati_do2 <- haven::read_sav("Data_SC5_D_18-0-0/SC5_pTargetCAWI_D_18-0-0.sav") %>%
            dplyr::select(ID_t, wave, tg60021) %>%
            dplyr::filter(wave == 11)

cawi_w8 <- haven::read_sav("Data_SC5_D_18-0-0/SC5_pTargetCAWI_D_18-0-0.sav") %>%
           dplyr::select(ID_t, wave, tg61031, tg61032, tg61033,  # int mo
                                     tg61061, tg61062, tg61063,
                                     tg61041, tg61042, tg61043,  
                                     tg61021, tg61022, tg61023,  # ext mo
                                     tg61011, tg61012, tg61013,
                                     tg61071, tg61072, tg61073,  
                                     tg61051, tg61052, tg61053) %>%
           dplyr::filter(wave == 8)

cawi_sp1 <- haven::read_sav("Data_SC5_D_18-0-0/SC5_pTargetCAWI_D_18-0-0.sav") %>%
            dplyr::select(ID_t, wave, tg53232, tg53234, tg53236,  # ac int
                                      tg53231, tg53233, tg53235,
                                      tg53211, tg53212, tg53213,
                                      tg53111, tg53112, tg53112,  # so int
                                      tg53121, tg53122, tg53123,
                                      t241011, t241012, t241013,  # for lo  
                                      t246411, t246412, t246413) %>%
                                      # practice orientation
            dplyr::filter(wave == 2)

cawi_sp2 <- haven::read_sav("Data_SC5_D_18-0-0/SC5_pTargetCAWI_D_18-0-0.sav") %>%
            dplyr::select(ID_t, wave, t261843, t261845, t261846) %>%  # infor lo
            dplyr::filter(wave == 4)
                                      
                         
# Filter measures from spSchool:

spsch <- haven::read_sav("Data_SC5_D_18-0-0/SC5_spSchool_D_18-0-0.sav") %>%
         dplyr::select(ID_t, wave, spell, subspell, ts11204,       # school
                                                    ts11209,       # certificate
                                                    ts11218) %>%   # GPA
         dplyr::filter(subspell == 0)  %>%  # keep full / harmonized episodes
         dplyr::group_by(ID_t) %>% dplyr::slice_max(order_by = spell, 
                                                    with_ties = T) %>%
                                   dplyr::slice_tail(n = 1)  # keep highest spell 


# Filter measures from spVocTrain:

spvoc_pr <- haven::read_sav("Data_SC5_D_18-0-0/SC5_spVocTrain_D_18-0-0.sav") %>%
            dplyr::select(ID_t, wave, spell, subspell, tx20100,  # linkage
                                                       ts15201,  # type of voc
                                                       ts1512m,  # end year voc
                                                       ts1512y,  # end month voc
                                                       ts15218) %>%  # finished 
           dplyr::filter(subspell == 0 & tx20100 == 1 
                         & ts15201 >= 1 & ts15201 <= 4 
                         & ts1512y < 2010 & ts1512m < 10) %>%  
           # keep full / harmonized episodes & completion before WT 2010
           dplyr::group_by(ID_t) %>%
           dplyr::summarise(ts15218 = case_when(any(ts15218 == 1) ~ 1,
                                                any(ts15218 == 2) 
                                                & any(is.na(ts15218)) ~ NA,     # oder ~2 (?) 
                                                all(ts15218 == 2) ~ 2))
           # summarize the data per person across spells


spvoc <- haven::read_sav("Data_SC5_D_18-0-0/SC5_spVocTrain_D_18-0-0.sav") %>%                     # (?) später nochmal Filter mit h_aktstu
         dplyr::select(ID_t, wave, spell, subspell, 
                       h_aktstu,        # 1st study episode WT 2010
                       tg24202_g1,      # type of intended teaching degree
                       tg24170_g5,      # subject group
                       tg01003_ha) %>%  # Type of higher education inst         # Im Vergleich zu oben nicht nur harmonized data, weil dann nicht immer wave 1
         dplyr::filter(wave == 1) %>%
         dplyr::filter(h_aktstu == 1) %>%
         dplyr::group_by(ID_t) %>% dplyr::slice_max(order_by = spell,
                                                    with_ties = T) %>%
         dplyr::slice_tail(n = 1)  
         
# View(spvoc %>% dplyr::filter(ID_t == "7002071"))

# filt <- haven::read_sav("Data_SC5_D_18-0-0/SC5_spVocTrain_D_18-0-0.sav") %>%                     
#        dplyr::select(ID_t, wave, h_aktstu, spell) %>%  
#        dplyr::filter(wave == 1)

# View(filt %>% dplyr::filter(ID_t == "7002071"))
  


####  -------------------------- (3) Merge Data -------------------------- ####

# 1. CohortProfile + pTargetCATI (1:1)
# 2. + pTargetCAWI (1:1)
# 3. + spSchool (1:m)     
# 4. + spVocTrain (1:m)
#
# every data set is prepared 1 line per person, so that 1:1 merging works and
# results in wide data as required 
#
# To combine multiple tables in rquery one uses the natural_join operator:
# In the rquery natural_join, rows are matched by column keys and any two 
# columns with the same name are coalesced (meaning the first table with a 
# non-missing values supplies the answer) jointype = 'LEFT' is used to augment 
# the left table with additional values from the right (as ordered)

data <- rquery::natural_join(cohort, cati_w1,
                             by = "ID_t",
                             jointype = "LEFT")

# View(cohort %>% dplyr::filter(ID_t == "7036532"))
# View(data %>% dplyr::filter(ID_t == "7036532"))

data <- rquery::natural_join(data, cati_b5,
                             by = "ID_t",
                             jointype = "LEFT")

data <- rquery::natural_join(data, cati_b5_w10,  # supply Big 5
                             by = "ID_t",        # with answers from wave 10
                             jointype = "LEFT") 

# View(cati_b5 %>% dplyr::filter(ID_t == "7031531"))
# View(cati_b5_w10 %>% dplyr::filter(ID_t == "7031531"))
# View(data %>% dplyr::filter(ID_t == "7031531")) 

data <- rquery::natural_join(data, cawi_w8,
                             by = "ID_t",
                             jointype = "LEFT") 

data <- rquery::natural_join(data, cawi_sp1,
                             by = "ID_t",
                             jointype = "LEFT") 

data <- rquery::natural_join(data, cawi_sp2,
                             by = "ID_t",
                             jointype = "LEFT")

data <- rquery::natural_join(data, cati_do1,
                             by = "ID_t",
                             jointype = "LEFT")

data <- rquery::natural_join(data, cati_do2,
                             by = "ID_t",
                             jointype = "LEFT") 

data <- rquery::natural_join(data, spsch,
                             by = "ID_t",
                             jointype = "LEFT") 

data <- rquery::natural_join(data, spvoc_pr,
                             by = "ID_t",
                             jointype = "LEFT") 

data <- rquery::natural_join(data, spvoc,
                             by = "ID_t",
                             jointype = "LEFT") %>%
  
                             dplyr::filter(tx80121 == 1) %>%
                             # oversample of tea edu
                             dplyr::filter(tg02001_ha >= 1 & tg02001_ha <= 7) %>%
                             # intended degree = teacher education (wave 1)
                             dplyr::filter(tg24150_g1 != 3) %>%
                             # exclude foreign students
                             dplyr::filter(h_aktstu == 1) %>%                   # Hier weden keine Daten mehr ausgeschlossen, 
                                                                                # Warum hat h_aktstu keine Missings in wave 1 
                             # only start of studies WT 2010
                             dplyr::filter(tx80107 == 1) %>%
                             # only first participation in wave 1
                             dplyr::select(-wave) %>%
                             # drop wave, contains no information anymore
                             
                             dplyr::mutate(across(everything(), 
                                                  ~ fauxnaif::na_if_in(., ~ . < 0)))
                             # replace all negative Values with NA  
                             # Nicht nötig, warum gibt es keine negativen NAs, z.B. t261846 (vgl. NEPSplorer)

# Only one ID per line?                                                   
# x <- data %>% dplyr::count(ID_t)
# min(x$n)
# max(x$n)

# colMeans(is.na(data))

####  ------------ (4) Compute Scale Scores and New Variables ------------ ####

## entry characteristics
                             
# SES: Parental education (par_edu), parental occupation (par_ocu)
data2 <- data %>%
  dplyr::mutate(par_edu = case_when((t731301_g1 < 9 & t731351_g1 < 9) ~ 1,
                                    (t731301_g1 >= 9 & t731351_g1 >= 9) ~ 3,
                                    (t731301_g1 >= 9 | t731351_g1 >= 9) ~ 2,
                                    TRUE ~ as.numeric(NA))) %>%
                                    # 1 = no parent tertiary education, 2 = one 
                                    # parent, 3 = both parents
  
  dplyr::mutate(par_ocu = case_when((t731403_g8 >= 8 | t731453_g8 >= 8) ~ 1,    
                                    (t731403_g8 <= 7 | t731453_g8 <= 7) ~ 2,
                                    TRUE ~ as.numeric(NA)))
                                    # 1 = working class, 
                                    # 2 = mixed and service class

# migration background
data3 <- data2 %>% 
  dplyr::mutate(mig_bac = case_when((t405060_g1 <= 2 & t405090_g1 <= 2) ~ 1,
                                    (t405060_g1 == 3 | t405090_g1 == 3) ~ 2,
                                    TRUE ~ as.numeric(NA)))
                                    # 1 = both parents born in Germany, 
                                    # 2 = at least one abroad

# View(data3 %>% dplyr::select(mig_bac, t405060_g1, t405090_g1))

# educational background (type of entrance certificate (schoool), GPA, prior 
# completion of vocational training)
data4 <- data3 %>% 
  dplyr::mutate(typ_sch = case_when(ts11204 != 8 ~ 1,
                                    ts11204 == 8 ~ 2,
                                    TRUE ~ as.numeric(NA)))  %>%
                                    # 1 = non-Gymnasium,
                                    # 2 = Gymnasium (also Kolleg)
  
  dplyr::mutate(paa_gpa = case_when(ts11218 >= 3.6 &  ts11218 <= 4.0 ~ 1,
                                    ts11218 >= 2.6 &  ts11218 <= 3.5 ~ 2,
                                    ts11218 >= 1.6 &  ts11218 <= 2.5 ~ 3,
                                    ts11218 >= 1.0 &  ts11218 <= 1.5 ~ 4,
                                    TRUE ~ as.numeric(NA)))  %>%
                                    # 1 = sufficient, 2 = satisfactory, 
                                    # 3 = good, 4 = very good)
  
  dplyr::mutate(voc_tra = case_when(ts15218 == 1 ~ 1,
                                    ts15218 == 2 ~ 2,
                                    TRUE ~ as.numeric(NA)))
                                    # 1 = no, 2 = yes

# individual characteristics 
# (big 5, motivation for choosing teacher education (femola))

# scales
data5 <- data4 %>% 
  dplyr::mutate(big_ext = rowMeans(subset(data4, select = c(t66800a, t66800f)),
                                   na.rm = TRUE))  %>%
  dplyr::mutate(big_agr = rowMeans(subset(data4, select = c(t66800b, t66800g, 
                                                            t66800k)), 
                                   na.rm = TRUE))  %>%
  dplyr::mutate(big_con = rowMeans(subset(data4, select = c(t66800c, t66800h)), 
                                   na.rm = TRUE))  %>%
  dplyr::mutate(big_neu = rowMeans(subset(data4, select = c(t66800d, t66800i)), 
                                   na.rm = TRUE))  %>%
  dplyr::mutate(big_ope = rowMeans(subset(data4, select = c(t66800e, t66800j)), 
                                   na.rm = TRUE))  %>%
  dplyr::mutate(fem_edi = rowMeans(subset(data4, select = c(tg61031, tg61032, 
                                                            tg61033)), 
                                   na.rm = TRUE))  %>%
  dplyr::mutate(fem_ssi = rowMeans(subset(data4, select = c(tg61061, tg61062, 
                                                            tg61063)), 
                                   na.rm = TRUE))  %>%
  dplyr::mutate(fem_abe = rowMeans(subset(data4, select = c(tg61041, tg61042, 
                                                            tg61043)), 
                                   na.rm = TRUE))  %>%
  dplyr::mutate(fem_tff = rowMeans(subset(data4, select = c(tg61021, tg61022, 
                                                            tg61023)), 
                                   na.rm = TRUE))  %>%
  dplyr::mutate(fem_fis = rowMeans(subset(data4, select = c(tg61011, tg61012, 
                                                            tg61013)), 
                                   na.rm = TRUE))  %>%
  dplyr::mutate(fem_lod = rowMeans(subset(data4, select = c(tg61071, tg61072, 
                                                            tg61073)), 
                                   na.rm = TRUE))  %>%
  dplyr::mutate(fem_soi = rowMeans(subset(data4, select = c(tg61051, tg61052, 
                                                            tg61053)),
                                   na.rm = TRUE)) %>%
  
  dplyr::mutate(fem_inm = rowMeans(subset(data4, select = c(tg61031, tg61032, 
                                                            tg61033, tg61061, 
                                                            tg61062, tg61063,
                                                            tg61041, tg61042, 
                                                            tg61043)),
                                   na.rm = TRUE)) %>%
                                  
  dplyr::mutate(fem_exm = rowMeans(subset(data4, select = c(tg61021, tg61022, 
                                                            tg61023, tg61011, 
                                                            tg61012, tg61013,
                                                            tg61071, tg61072, 
                                                            tg61073, tg61051, 
                                                            tg61052, tg61053)),
                                   na.rm = TRUE))



## current study situation
# to be continued

#### ------------------------------ (5) LCA ------------------------------ ####

data6 <- data5 %>% 
  dplyr::mutate_all( ~ ifelse(is.nan(.), NA, .))

# Tutorial: Recommended Practices in Latent Class Analysis using the 
# Open-Source R-Package tidySEM; van Lissa et al. (2023)
  
  
# select variables
df0 <- data6 %>%
  dplyr::select(par_edu, par_ocu, mig_bac, typ_sch, paa_gpa,
                voc_tra, big_ext, big_agr, big_con, big_neu,
                big_ope, fem_inm, fem_exm)

# examine variables
desc0 <- tidySEM::descriptives(df0)
desc0 <- desc0[, c("name", "type", "n", "missing", "unique", "mode",
                 "mode_value", "v")]

# transform variables
#
# continuous variables have type numeric or integer; ordered variables such as 
# Likert scales, and binary should be converted to mxFactor; nominal variables 
# should be converted to binary dummy variables using mx_dummies(); continuous 
# variables should have many unique values, if not, it may be better to model 
# them as ordinal

# lev_inm <- list(levels(as.factor(data6$fem_inm)))
# lev_exm <- list(levels(as.factor(data6$fem_exm)))

data7 <- data6 %>% 
  dplyr::mutate(par_edu = mxFactor(par_edu, 
                                   levels = c("1", "2", "3"))) %>% 
  dplyr::mutate(par_ocu = mxFactor(par_ocu, 
                                   levels = c("1", "2"))) %>%
  dplyr::mutate(mig_bac = mxFactor(mig_bac, 
                                   levels = c("1", "2"))) %>%          
  dplyr::mutate(typ_sch = mxFactor(typ_sch, 
                                   levels = c("1", "2"))) %>%
  dplyr::mutate(paa_gpa = mxFactor(paa_gpa, 
                                   levels = c("1", "2", "3", "4", "5"))) %>%
  dplyr::mutate(voc_tra = mxFactor(voc_tra, 
                                   levels = c("1", "2"))) %>%
  dplyr::mutate(big_ext = mxFactor(big_ext, 
                                   levels = c("1.5", "2", "2.5", "3", "3.5", 
                                              "4", "4.5", "5"))) %>%
   dplyr::mutate(big_agr = mxFactor(big_agr, 
                                   levels = c("1.66666666666667", "2", 
                                              "2.33333333333333", 
                                              "2.66666666666667", "3", 
                                              "3.33333333333333", 
                                              "3.66666666666667", "4", 
                                              "4.33333333333333", 
                                              "4.66666666666667", "5"))) %>% 
  dplyr::mutate(big_con = mxFactor(big_con, 
                                   levels = c("1.5", "2", "2.5", "3", "3.5", 
                                              "4", "4.5", "5"))) %>% 
  dplyr::mutate(big_neu = mxFactor(big_neu, 
                                   levels = c("1", "1.5", "2", "2.5", "3", 
                                              "3.5", "4", "4.5"))) %>% 
  dplyr::mutate(big_ope = mxFactor(big_ope, 
                                   levels = c("1", "1.5", "2", "2.5", "3", 
                                              "3.5", "4", "4.5", "5"))) %>%
   dplyr::mutate(fem_inm = mxFactor(fem_inm, 
                                   levels = c("1", "1.11111111111111", 
                                              "1.66666666666667",
                                              "1.88888888888889", "2",
                                              "2.11111111111111", "2.125",
                                              "2.22222222222222", "2.25",
                                              "2.33333333333333", "2.375", 
                                              "2.44444444444444", 
                                              "2.55555555555556", 
                                              "2.66666666666667", "2.75",
                                              "2.77777777777778", "2.875",
                                              "2.88888888888889", "3", 
                                              "3.11111111111111", "3.125", "3.2",
                                              "3.22222222222222", "3.25", 
                                              "3.33333333333333", "3.375",
                                              "3.44444444444444", "3.5",
                                              "3.55555555555556", "3.625",
                                              "3.66666666666667", "3.75", 
                                              "3.77777777777778", "3.8", 
                                              "3.875", "3.88888888888889",
                                              "4"))) %>% 
 dplyr::mutate(fem_exm = mxFactor(fem_exm,
                                  levels = c("1", "1.08333333333333",
                                             "1.16666666666667", "1.25",
                                             "1.33333333333333",
                                             "1.36363636363636",
                                             "1.41666666666667",
                                             "1.45454545454545", "1.5", 
                                             "1.54545454545455", 
                                             "1.58333333333333",
                                             "1.63636363636364",
                                             "1.66666666666667",
                                             "1.72727272727273", "1.75",
                                             "1.81818181818182",
                                             "1.83333333333333",
                                             "1.91666666666667", "2",
                                             "2.08333333333333",
                                             "2.09090909090909",
                                             "2.16666666666667", 
                                             "2.18181818181818", "2.25",
                                             "2.27272727272727",
                                             "2.33333333333333",
                                             "2.36363636363636",
                                             "2.41666666666667",
                                             "2.45454545454545", "2.5",
                                             "2.58333333333333",
                                             "2.66666666666667",
                                             "2.72727272727273", "2.75", 
                                             "2.81818181818182", 
                                             "2.83333333333333",
                                             "2.90909090909091",
                                             "2.91666666666667", "3",
                                             "3.08333333333333", 
                                             "3.09090909090909",
                                             "3.16666666666667", "3.25",  
                                             "3.27272727272727",
                                             "3.33333333333333",
                                             "3.41666666666667", "3.5",
                                             "3.83333333333333",
                                             "3.91666666666667"))) 

# examine variables variables again
df <- data7 %>%
  dplyr::select(par_edu, par_ocu, mig_bac, typ_sch, paa_gpa,
                voc_tra, big_ext, big_agr, big_con, big_neu,
                big_ope, fem_inm, fem_exm)

desc <- tidySEM::descriptives(df)
desc <- desc[, c("name", "type", "n", "missing", "unique", "mode",
                 "mode_value", "v")]

df_plot <- df

names(df_plot) <- paste0("Value.", names(df_plot))
df_plot <- reshape(df_plot, varying = names(df_plot), direction = "long")

ggplot(df_plot, aes(x = Value)) + geom_bar() + facet_wrap(~time,
                                                          scales = "free") + theme_bw()

# conducting lca
set.seed(123)
res <-  mx_lca(data = df, classes = 1:4, run = T)

# Solution found!  Final fit=64781.809 (started at 65662.79)  (11 attempt(s): 11 valid, 0 errors)
# 
# Error in update_thresholds(zscore) : 
#   Could not complete thresholds; either specify all thresholds by hand, or remove constraints.

