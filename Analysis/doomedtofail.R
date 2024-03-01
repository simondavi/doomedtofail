# doomedtofail.R
#
# Comment: 
#
# Input: SC5_CohortProfile_D_18-0-0.sav
#        SC5_pTargetCATI_D_18-0-0.sav
#        SC5_pTargetCAWI_D_18-0-0.sav
#        SC5_spSchool_D_18-0-0.sav
#        SC5_spVocTrain_D_18-0-0.sav # unvollständig
# Output: --
#
# Contents: (1) Load Packages
#           (2) Read Data and Data Management
#           (3) Merge Data
#           (4) Compute Scale Scores and New Variables
#           (5) LCA
#           (6) SEM


####  ------------- (1) Load Packages ------------- ####
renv::restore()

library(haven)          # to import SPSS files
library(tidyverse)      # for data management
library(reshape)
library(reshape2)
library(rquery)         # to copy STATA merge behavior
library(rqdatatable)
library(fauxnaif)       # for defining missing values 
                          # (remotes::install_github("rossellhayes/fauxnaif")
library(psych)          # for scale construction
library(depmixS4)       # for LCA
library(bayestestR)     # convert BIC indices to Bayes Factors 
library(scatterplot3d)
library(LCAplotter)     # library(devtools)
                          # devtools::install_github("DavidykZhao/LCA_plotter")
library(report)         # for producing reports 
library(lavaan)         # for SEM
library(ggpubr)         # for arranging plots



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
                                     t731453_g14,  # parental occupation
                                     t731403_g14,
                                     t405060_g1,  # migration background
                                     t405090_g1,
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


cati_doi <- haven::read_sav("Data_SC5_D_18-0-0/SC5_pTargetCATI_D_18-0-0.sav") %>%
            dplyr::select(ID_t, wave, tg64051) %>%
            dplyr::filter(wave == 9)

  
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
                         & ts15201 <= 4 
                         & ts1512y <= 2010 & ts1512m <= 10) %>%  
           # keep full / harmonized episodes & completion before WT 2010
           dplyr::group_by(ID_t) %>%
           dplyr::summarise(ts15218 = case_when(any(ts15218 == 1) ~ 3,
                                                any(ts15218 == 2) 
                                                & any(is.na(ts15218)) ~ as.numeric(NA), 
                                                all(ts15218 == 2) ~ 4))
           # summarize the data per person across spells
           # 3 = yes, 4 = no


spvoc <- haven::read_sav("Data_SC5_D_18-0-0/SC5_spVocTrain_D_18-0-0.sav") %>%   # (?) später nochmal Filter mit h_aktstu
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


# Filter measures from StudyStates:
ststa <- haven::read_sav("Data_SC5_D_18-0-0/SC5_StudyStates_D_18-0-0.sav") %>%
         dplyr::select(ID_t, wave, tx24022,  # Episode number
                                   tx15318) %>%  # Successful completion
         dplyr::filter(tx24022 > 0) %>% 
         dplyr::group_by(ID_t) %>%
         dplyr::summarise(tx15318 = case_when(any(tx15318 == 1) ~ 3, 
                                              any(tx15318 == 0)
                                              & all(tx15318 != 1) ~ 4,
                                              any(is.na(tx15318)) ~ as.numeric(NA)))

# Filter measures from Basics:
basic <- haven::read_sav("Data_SC5_D_18-0-0/SC5_Basics_D_18-0-0.sav") %>% 
         dplyr::select(ID_t, tx29000)  # age


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

data <- rquery::natural_join(data, cati_doi,
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
                             jointype = "LEFT") 


data <- rquery::natural_join(data, ststa,
                             by = "ID_t",
                             jointype = "LEFT") 
  
data <- rquery::natural_join(data, basic,
                             by = "ID_t",
                             jointype = "LEFT") %>%
  
                             # dplyr::filter(tx80121 == 1) %>%
                             # oversample of tea edu
                             dplyr::filter(tg02001_ha <= 7) %>%
                             #intended degree = teacher education (wave 1)
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
                             # Nicht nötig, warum gibt es keine negativen NAs, 
                             # z.B. t261846 (vgl. NEPSplorer)

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
                                    (t731301_g1 >= 9 & t731351_g1 < 9 | 
                                       t731301_g1 < 9 & t731351_g1 >= 9) ~ 2,
                                    TRUE ~ as.numeric(NA))) %>%
                                    # 1 = no parent tertiary education, 2 = one 
                                    # parent, 3 = both parents
  dplyr::group_by(ID_t) %>% 
  dplyr::mutate(hisei = max(t731453_g14, t731403_g14)) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(hisei = case_when((is.na(t731453_g14) |
                                    is.na(t731403_g14)) ~ as.numeric(NA),
                                  TRUE ~ as.numeric(hisei))) 

# migration background
data3 <- data2 %>% 
  dplyr::mutate(mig_bac = case_when((t405060_g1 <= 2 & t405090_g1 <= 2) ~ 0,    # NA situation?
                                    (t405060_g1 == 3 | t405090_g1 == 3) ~ 1,
                                    TRUE ~ as.numeric(NA)))
                                    # 0 = both parents born in Germany, 
                                    # 1 = at least one abroad

# View(data3 %>% dplyr::select(mig_bac, t405060_g1, t405090_g1))

# educational background (type of entrance certificate (schoool), GPA, prior 
# completion of vocational training)
data4 <- data3 %>% 
  dplyr::mutate(typ_sch = case_when(ts11204 != 8 ~ 0,
                                    ts11204 == 8 ~ 1,
                                    TRUE ~ as.numeric(NA)),
                                    # 0 = non-Gymnasium,
                                    # 1 = Gymnasium (also Kolleg)
  
#  dplyr::mutate(paa_gpa = case_when(ts11218 >= 3.6 & ts11218 <= 4.0 ~ 1,
#                                    ts11218 >= 2.6 & ts11218 < 3.6 ~ 2,
#                                    ts11218 >= 1.6 & ts11218 < 2.6 ~ 3,
#                                    ts11218 >= 1.0 & ts11218 < 1.6 ~ 4,
#                                    TRUE ~ as.numeric(NA)))  %>%
#                                    # 1 = sufficient, 2 = satisfactory, 
#                                    # 3 = good, 4 = very good)
  
# gpa as continous variable
                paa_gpa = ts11218,
  
# dplyr::mutate(voc_tra = case_when(ts15218 == 4 ~ 1,
#                                    ts15218 == 3 ~ 2,
#                                    TRUE ~ as.numeric(NA)))
#                                    # 1 = no, 2 = yes

# binary variable for vocational training
                voc_tra = ifelse(ts15218 == 4 | is.na(ts15218), 0, 1),

# gender
                gender = ifelse(t700001 == 1, 1, 0),
                # 1 = male
                # 0 = female

# age
                age =  tx29000)

# individual characteristics 
# (big 5, motivation for choosing teacher education (femola))

# scales

# Big 5
# recode inverse items big 5:
data4$t66800a <- 6 - data4$t66800a
data4$t66800g <- 6 - data4$t66800g
data4$t66800c <- 6 - data4$t66800c
data4$t66800d <- 6 - data4$t66800d
data4$t66800e <- 6 - data4$t66800e

data5 <- data4 %>% 
  dplyr::mutate(big_ext = rowMeans(subset(data4, select = c(t66800a, t66800f)),
                                   na.rm = TRUE),
                big_agr = rowMeans(subset(data4, select = c(t66800b, t66800g,
                                                            t66800k)),
                                   na.rm = TRUE),
                big_con = rowMeans(subset(data4, select = c(t66800c, t66800h)),
                                   na.rm = TRUE),
                big_neu = rowMeans(subset(data4, select = c(t66800d, t66800i)),
                                   na.rm = TRUE),
                big_ope = rowMeans(subset(data4, select = c(t66800e, t66800j)),
                                   na.rm = TRUE),
                fem_edi = rowMeans(subset(data4, select = c(tg61031, tg61032,
                                                            tg61033)),
                                   na.rm = TRUE),
                fem_ssi = rowMeans(subset(data4, select = c(tg61061, tg61062, 
                                                            tg61063)), 
                                   na.rm = TRUE),
                fem_abe = rowMeans(subset(data4, select = c(tg61041, tg61042,
                                                            tg61043)), 
                                   na.rm = TRUE),
                fem_tff = rowMeans(subset(data4, select = c(tg61021, tg61022,
                                                            tg61023)), 
                                   na.rm = TRUE),
                fem_fis = rowMeans(subset(data4, select = c(tg61011, tg61012,
                                                            tg61013)), 
                                   na.rm = TRUE),
                fem_lod = rowMeans(subset(data4, select = c(tg61071, tg61072, 
                                                            tg61073)), 
                                   na.rm = TRUE),
                fem_soi = rowMeans(subset(data4, select = c(tg61051, tg61052, 
                                                            tg61053)),
                                   na.rm = TRUE),
                
                fem_inm = rowMeans(subset(data4, select = c(tg61031, tg61032, 
                                                            tg61033, tg61061, 
                                                            tg61062, tg61063,
                                                            tg61041, tg61042, 
                                                            tg61043)),
                                   na.rm = TRUE),
                                                
                fem_exm = rowMeans(subset(data4, select = c(tg61021, tg61022, 
                                                            tg61023, tg61011, 
                                                            tg61012, tg61013,
                                                            tg61071, tg61072, 
                                                            tg61073, tg61051, 
                                                            tg61052, tg61053)),
                                   na.rm = TRUE))


## current study situation                                                      # Nummerierung dataX überarbeiten (?)

# social and academic integration
data5 <- data5 %>% 
  dplyr::mutate(aca_int = rowMeans(subset(data5, select = c(tg53232, tg53234, 
                                                            tg53236, tg53231, 
                                                            tg53233, tg53235,
                                                            tg53211, tg53212, 
                                                            tg53213)),
                                   na.rm = TRUE),
                soc_int = rowMeans(subset(data5, select = c(tg53111, tg53112, 
                                                            tg53112, tg53121, 
                                                            tg53122, tg53123)),
                                   na.rm = TRUE))

## decision

# drop out
data5 <- data5 %>% 
 dplyr::mutate(stu_com = case_when(tx15318 == 3 ~ 0, 
                                   tx15318 == 4 ~ 1,
                                   TRUE ~ as.numeric(NA)))
  
                                 

#### ------------------------------ (5) LCA ------------------------------ ####

data6 <- data5 %>% 
  dplyr::mutate(across(everything(), ~ ifelse(is.nan(.x), NA, .x)))

dat_lca <- data6 %>% 
  dplyr::mutate(
    across(c(gender,
             par_edu,
             mig_bac),
           as.factor))

dat_lca <- dat_lca %>%
  dplyr::select(age,
                gender,
                par_edu,
                hisei,
                mig_bac,
                paa_gpa, 
                big_ext,
                big_agr,
                big_con,
                big_neu,
                big_ope,
                fem_inm,
                fem_exm)

## step 1: model specification

# class = 1
m1 <- mix(list(gender ~ 1, par_edu ~ 1, mig_bac ~ 1, 
               hisei ~ 1, paa_gpa ~ 1, 
               big_ext ~ 1, big_agr ~ 1, big_con ~ 1, big_neu ~ 1, big_ope ~ 1,
               fem_inm ~ 1, fem_exm ~ 1, age ~ 1 ), 
          data = dat_lca, nstates = 1,
          family=list(multinomial("identity"), multinomial("identity"), 
                      multinomial("identity"), 
                      gaussian(), gaussian(),
                      gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), 
                      gaussian(), gaussian(), gaussian()))

# class = 2
m2 <- mix(list(gender ~ 1, par_edu ~ 1, mig_bac ~ 1, 
               hisei ~ 1, paa_gpa ~ 1, 
               big_ext ~ 1, big_agr ~ 1, big_con ~ 1, big_neu ~ 1, big_ope ~ 1,
               fem_inm ~ 1, fem_exm ~ 1, age ~ 1 ), 
          data = dat_lca, nstates = 2,
          family=list(multinomial("identity"), multinomial("identity"), 
                      multinomial("identity"), 
                      gaussian(), gaussian(),
                      gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), 
                      gaussian(), gaussian(), gaussian()))

# class = 3
m3 <- mix(list(gender ~ 1, par_edu ~ 1, mig_bac ~ 1, 
               hisei ~ 1, paa_gpa ~ 1, 
               big_ext ~ 1, big_agr ~ 1, big_con ~ 1, big_neu ~ 1, big_ope ~ 1,
               fem_inm ~ 1, fem_exm ~ 1, age ~ 1 ), 
          data = dat_lca, nstates = 3,
          family=list(multinomial("identity"), multinomial("identity"), 
                      multinomial("identity"), 
                      gaussian(), gaussian(),
                      gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), 
                      gaussian(), gaussian(), gaussian()))

# class = 4
m4 <- mix(list(gender ~ 1, par_edu ~ 1, mig_bac ~ 1, 
               hisei ~ 1, paa_gpa ~ 1, 
               big_ext ~ 1, big_agr ~ 1, big_con ~ 1, big_neu ~ 1, big_ope ~ 1,
               fem_inm ~ 1, fem_exm ~ 1, age ~ 1 ), 
          data = dat_lca, nstates = 4,
          family=list(multinomial("identity"), multinomial("identity"), 
                      multinomial("identity"), 
                      gaussian(), gaussian(),
                      gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), 
                      gaussian(), gaussian(), gaussian()))

# class = 5
m5 <- mix(list(gender ~ 1, par_edu ~ 1, mig_bac ~ 1, 
               hisei ~ 1, paa_gpa ~ 1, 
               big_ext ~ 1, big_agr ~ 1, big_con ~ 1, big_neu ~ 1, big_ope ~ 1,
               fem_inm ~ 1, fem_exm ~ 1, age ~ 1 ), 
          data = dat_lca, nstates = 5,
          family=list(multinomial("identity"), multinomial("identity"), 
                      multinomial("identity"), 
                      gaussian(), gaussian(),
                      gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), 
                      gaussian(), gaussian(), gaussian()))

# class = 6
m6 <- mix(list(gender ~ 1, par_edu ~ 1, mig_bac ~ 1, 
               hisei ~ 1, paa_gpa ~ 1, 
               big_ext ~ 1, big_agr ~ 1, big_con ~ 1, big_neu ~ 1, big_ope ~ 1,
               fem_inm ~ 1, fem_exm ~ 1, age ~ 1 ), 
          data = dat_lca, nstates = 6,
          family=list(multinomial("identity"), multinomial("identity"), 
                      multinomial("identity"), 
                      gaussian(), gaussian(),
                      gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), 
                      gaussian(), gaussian(), gaussian()))

# class = 7
m7 <- mix(list(gender ~ 1, par_edu ~ 1, mig_bac ~ 1, 
               hisei ~ 1, paa_gpa ~ 1, 
               big_ext ~ 1, big_agr ~ 1, big_con ~ 1, big_neu ~ 1, big_ope ~ 1,
               fem_inm ~ 1, fem_exm ~ 1, age ~ 1 ), 
          data = dat_lca, nstates = 7,
          family=list(multinomial("identity"), multinomial("identity"), 
                      multinomial("identity"), 
                      gaussian(), gaussian(),
                      gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), 
                      gaussian(), gaussian(), gaussian()))

# class = 8
m8 <- mix(list(gender ~ 1, par_edu ~ 1, mig_bac ~ 1, 
               hisei ~ 1, paa_gpa ~ 1, 
               big_ext ~ 1, big_agr ~ 1, big_con ~ 1, big_neu ~ 1, big_ope ~ 1,
               fem_inm ~ 1, fem_exm ~ 1, age ~ 1 ), 
          data = dat_lca, nstates = 8,
          family=list(multinomial("identity"), multinomial("identity"), 
                      multinomial("identity"), 
                      gaussian(), gaussian(),
                      gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), 
                      gaussian(), gaussian(), gaussian()))

# class = 9
m9 <- mix(list(gender ~ 1, par_edu ~ 1, mig_bac ~ 1, 
               hisei ~ 1, paa_gpa ~ 1, 
               big_ext ~ 1, big_agr ~ 1, big_con ~ 1, big_neu ~ 1, big_ope ~ 1,
               fem_inm ~ 1, fem_exm ~ 1, age ~ 1 ), 
          data = dat_lca, nstates = 9,
          family=list(multinomial("identity"), multinomial("identity"), 
                      multinomial("identity"), 
                      gaussian(), gaussian(),
                      gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), 
                      gaussian(), gaussian(), gaussian()))

# class = 10
m10 <- mix(list(gender ~ 1, par_edu ~ 1, mig_bac ~ 1, 
               hisei ~ 1, paa_gpa ~ 1, 
               big_ext ~ 1, big_agr ~ 1, big_con ~ 1, big_neu ~ 1, big_ope ~ 1,
               fem_inm ~ 1, fem_exm ~ 1, age ~ 1 ), 
          data = dat_lca, nstates = 10,
          family=list(multinomial("identity"), multinomial("identity"), 
                      multinomial("identity"), 
                      gaussian(), gaussian(),
                      gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), 
                      gaussian(), gaussian(), gaussian()))


## step 2: model fit
set.seed(123)

fit_m1 <- fit(m1, verbose = FALSE, method = 
              emcontrol = em.control(random.start = TRUE, 
                                     maxit = 5000,
                                     crit = "absolute",
                                     classification = c("soft")))

fit_m2 <- fit(m2, verbose = FALSE,
              emcontrol = em.control(random.start = TRUE, 
                                     maxit = 5000,
                                     crit = "absolute",
                                     classification = c("soft")))

fit_m3 <- fit(m3, verbose = FALSE, 
              emcontrol = em.control(random.start = TRUE, 
                                     maxit = 5000,
                                     crit = "absolute",
                                     classification = c("soft")))

fit_m4 <- fit(m4, verbose = FALSE, 
              emcontrol = em.control(random.start = TRUE, 
                                     maxit = 5000,
                                     crit = "absolute",
                                     classification = c("soft")))

fit_m5 <- fit(m5, verbose = FALSE,  
              emcontrol = em.control(random.start = TRUE, 
                                     maxit = 5000,
                                     crit = "absolute",
                                     classification = c("soft")))

fit_m6 <- fit(m6, verbose = FALSE,
              emcontrol = em.control(random.start = TRUE,
                                     maxit = 5000,
                                     crit = "absolute",
                                     classification = c("soft")))

fit_m7 <- fit(m7, verbose = FALSE,
              emcontrol = em.control(random.start = TRUE,
                                     maxit = 5000,
                                     crit = "absolute",
                                     classification = c("soft")))

fit_m8 <- fit(m8, verbose = FALSE,
              emcontrol = em.control(random.start = TRUE,
                                     maxit = 5000,
                                     crit = "absolute",
                                     classification = c("soft")))

fit_m9 <- fit(m9, verbose = FALSE,
              emcontrol = em.control(random.start = TRUE,
                                     maxit = 5000,
                                     crit = "absolute",
                                     classification = c("soft")))

fit_m10 <- fit(m10, verbose = FALSE,
              emcontrol = em.control(random.start = TRUE,
                                     maxit = 5000,
                                     crit = "absolute",
                                     classification = c("soft")))


## generate dataframe with fit-values
results <- data.frame(Modell = c("Modell"),
                      log_likelihood = logLik(fit_m1),
                      BIC = BIC(fit_m1),
                      AIC = AIC(fit_m1),
                      aBIC=  (-2*as.numeric(logLik(fit_m1))) + (log(nobs(fit_m1)) * freepars(fit_m1)),
                      cAIC = (-2*as.numeric(logLik(fit_m1))) + freepars(fit_m1) * (1 + log(nobs(fit_m1)))
                      )
                                                                                
results$Modell <- as.integer(results$Modell)
results[1,1] <- c("Modell 1")
results[2,1] <- c("Modell 2")
results[3,1] <- c("Modell 3")
results[4,1] <- c("Modell 4")
results[5,1] <- c("Modell 5")
results[6,1] <- c("Modell 6")
results[7,1] <- c("Modell 7")
results[8,1] <- c("Modell 8")
results[9,1] <- c("Modell 9")
results[10,1] <- c("Modell 10")

results[2,2] <- logLik(fit_m2)
results[3,2] <- logLik(fit_m3)
results[4,2] <- logLik(fit_m4)
results[5,2] <- logLik(fit_m5)
results[6,2] <- logLik(fit_m6)
results[7,2] <- logLik(fit_m7)
results[8,2] <- logLik(fit_m8)
results[9,2] <- logLik(fit_m9)
results[10,2] <- logLik(fit_m10)

results[2,3] <- BIC(fit_m2)
results[3,3] <- BIC(fit_m3)
results[4,3] <- BIC(fit_m4)
results[5,3] <- BIC(fit_m5)
results[6,3] <- BIC(fit_m6)
results[7,3] <- BIC(fit_m7)
results[8,3] <- BIC(fit_m8)
results[9,3] <- BIC(fit_m9)
results[10,3] <- BIC(fit_m10)

results[2,4] <- AIC(fit_m2)
results[3,4] <- AIC(fit_m3)
results[4,4] <- AIC(fit_m4)
results[5,4] <- AIC(fit_m5)
results[6,4] <- AIC(fit_m6)
results[7,4] <- AIC(fit_m7)
results[8,4] <- AIC(fit_m8)
results[9,4] <- AIC(fit_m9)
results[10,4] <- AIC(fit_m10)

results[2,5] <- (-2*as.numeric(logLik(fit_m2))) + (log(nobs(fit_m2)) * freepars(fit_m2)) #aBIC
results[3,5] <- (-2*as.numeric(logLik(fit_m3))) + (log(nobs(fit_m3)) * freepars(fit_m3))
results[4,5] <- (-2*as.numeric(logLik(fit_m4))) + (log(nobs(fit_m4)) * freepars(fit_m4))
results[5,5] <- (-2*as.numeric(logLik(fit_m5))) + (log(nobs(fit_m5)) * freepars(fit_m5))
results[6,5] <- (-2*as.numeric(logLik(fit_m6))) + (log(nobs(fit_m6)) * freepars(fit_m6))
results[7,5] <- (-2*as.numeric(logLik(fit_m7))) + (log(nobs(fit_m7)) * freepars(fit_m7))
results[8,5] <- (-2*as.numeric(logLik(fit_m8))) + (log(nobs(fit_m8)) * freepars(fit_m8))
results[9,5] <- (-2*as.numeric(logLik(fit_m9))) + (log(nobs(fit_m9)) * freepars(fit_m9))
results[10,5] <- (-2*as.numeric(logLik(fit_m10))) + (log(nobs(fit_m10)) * freepars(fit_m10))

results[2,6] <- (-2*as.numeric(logLik(fit_m2))) + freepars(fit_m2) * (1 + log(nobs(fit_m2))) #cAIC
results[3,6] <- (-2*as.numeric(logLik(fit_m3))) + freepars(fit_m3) * (1 + log(nobs(fit_m3)))
results[4,6] <- (-2*as.numeric(logLik(fit_m4))) + freepars(fit_m4) * (1 + log(nobs(fit_m4)))
results[5,6] <- (-2*as.numeric(logLik(fit_m5))) + freepars(fit_m5) * (1 + log(nobs(fit_m5)))
results[6,6] <- (-2*as.numeric(logLik(fit_m6))) + freepars(fit_m6) * (1 + log(nobs(fit_m6)))
results[7,6] <- (-2*as.numeric(logLik(fit_m7))) + freepars(fit_m7) * (1 + log(nobs(fit_m7)))
results[8,6] <- (-2*as.numeric(logLik(fit_m8))) + freepars(fit_m8) * (1 + log(nobs(fit_m8)))
results[9,6] <- (-2*as.numeric(logLik(fit_m9))) + freepars(fit_m9) * (1 + log(nobs(fit_m9)))
results[10,6] <- (-2*as.numeric(logLik(fit_m10))) + freepars(fit_m10) * (1 + log(nobs(fit_m10)))

## entropy
# model 2
postprob_m2 <- depmixS4::posterior(fit_m2)
entropy_m2 <- as.data.frame(postprob_m2)
entropy_m2 <- cbind(postprob_m2$S1,
                    postprob_m2$S2)

entropy_m2 <- 1 - 
  (
    (sum(-entropy_m2*log(entropy_m2), na.rm = TRUE)) /
      (nrow(entropy_m2)*log(ncol(entropy_m2)))
  )


# model 3
postprob_m3 <- depmixS4::posterior(fit_m3)
entropy_m3 <- as.data.frame(postprob_m3)
entropy_m3 <- cbind(postprob_m3$S1,
                    postprob_m3$S2,
                    postprob_m3$S3)

entropy_m3 <- 1 - 
  (
    (sum(-entropy_m3*log(entropy_m3), na.rm = TRUE)) /
      (nrow(entropy_m3)*log(ncol(entropy_m3)))
  )


# model 4
postprob_m4 <- depmixS4::posterior(fit_m4)
entropy_m4 <- as.data.frame(postprob_m4)
entropy_m4 <- cbind(postprob_m4$S1,
                    postprob_m4$S2,
                    postprob_m4$S3,
                    postprob_m4$S4)

entropy_m4 <- 1 - 
  (
    (sum(-entropy_m4*log(entropy_m4), na.rm = TRUE)) /
      (nrow(entropy_m4)*log(ncol(entropy_m4)))
  )


# model 5
postprob_m5 <- depmixS4::posterior(fit_m5)
entropy_m5 <- as.data.frame(postprob_m5)
entropy_m5 <- cbind(postprob_m5$S1,
                    postprob_m5$S2,
                    postprob_m5$S3,
                    postprob_m5$S4,
                    postprob_m5$S5)

entropy_m5 <- 1 - 
  (
    (sum(-entropy_m5*log(entropy_m5), na.rm = TRUE)) /
      (nrow(entropy_m5)*log(ncol(entropy_m5)))
  )


# model 6
postprob_m6 <- depmixS4::posterior(fit_m6)
entropy_m6 <- as.data.frame(postprob_m6)
entropy_m6 <- cbind(postprob_m6$S1,
                    postprob_m6$S2,
                    postprob_m6$S3,
                    postprob_m6$S4,
                    postprob_m6$S5,
                    postprob_m6$S6)

entropy_m6 <- 1 - 
  (
    (sum(-entropy_m6*log(entropy_m6), na.rm = TRUE)) /
      (nrow(entropy_m6)*log(ncol(entropy_m6)))
  )


# model 7
postprob_m7 <- depmixS4::posterior(fit_m7)
entropy_m7 <- as.data.frame(postprob_m7)
entropy_m7 <- cbind(postprob_m7$S1,
                    postprob_m7$S2,
                    postprob_m7$S3,
                    postprob_m7$S4,
                    postprob_m7$S5,
                    postprob_m7$S6,
                    postprob_m7$S7)

entropy_m7 <- 1 - 
  (
    (sum(-entropy_m7*log(entropy_m7), na.rm = TRUE)) /
      (nrow(entropy_m7)*log(ncol(entropy_m7)))
  )


# model 8
postprob_m8 <- depmixS4::posterior(fit_m8)
entropy_m8 <- as.data.frame(postprob_m8)
entropy_m8 <- cbind(postprob_m8$S1,
                    postprob_m8$S2,
                    postprob_m8$S3,
                    postprob_m8$S4,
                    postprob_m8$S5,
                    postprob_m8$S6,
                    postprob_m8$S7,
                    postprob_m8$S8)

entropy_m8 <- 1 - 
  (
    (sum(-entropy_m8*log(entropy_m8), na.rm = TRUE)) /
       (nrow(entropy_m8)*log(ncol(entropy_m8)))
   )


# model 9
postprob_m9 <- depmixS4::posterior(fit_m9)
entropy_m9 <- as.data.frame(postprob_m9)
entropy_m9 <- cbind(postprob_m9$S1,
                    postprob_m9$S2,
                    postprob_m9$S3,
                    postprob_m9$S4,
                    postprob_m9$S5,
                    postprob_m9$S6,
                    postprob_m9$S7,
                    postprob_m9$S8,
                    postprob_m9$S9)

entropy_m9 <- 1 - 
  (
    (sum(-entropy_m9*log(entropy_m9), na.rm = TRUE)) /
      (nrow(entropy_m9)*log(ncol(entropy_m9)))
  )


# model 10
postprob_m10 <- depmixS4::posterior(fit_m10)
entropy_m10 <- as.data.frame(postprob_m10)
entropy_m10 <- cbind(postprob_m10$S1,
                    postprob_m10$S2,
                    postprob_m10$S3,
                    postprob_m10$S4,
                    postprob_m10$S5,
                    postprob_m10$S6,
                    postprob_m10$S7,
                    postprob_m10$S8,
                    postprob_m10$S9,
                    postprob_m10$S10)

entropy_m10 <- 1 - 
  (
    (sum(-entropy_m10*log(entropy_m10), na.rm = TRUE)) /
      (nrow(entropy_m10)*log(ncol(entropy_m10)))
  )


# add entropy to results
results[2,7] <- entropy_m2
results[3,7] <- entropy_m3
results[4,7] <- entropy_m4
results[5,7] <- entropy_m5
results[6,7] <- entropy_m6
results[7,7] <- entropy_m7
results[8,7] <- entropy_m8
results[9,7] <- entropy_m9
results[10,7] <- entropy_m10
names(results)[7] <- paste("entropy")

results

#elbow plot
results$Modell <- factor(results$Modell, levels = c("Modell 1", "Modell 2", 
                                                    "Modell 3", "Modell 4", 
                                                    "Modell 5", "Modell 6", 
                                                    "Modell 7", "Modell 8", 
                                                    "Modell 9", "Modell 10"))
elbow_plot1 <- results %>% 
  ggplot(aes(x = Modell, y = BIC, group = 1)) + geom_point() + geom_line()
elbow_plot2 <- results %>% 
  ggplot(aes(x = Modell, y = aBIC, group = 1)) + geom_point() + geom_line() 
elbow_plot3 <- results %>% 
  ggplot(aes(x = Modell, y = cAIC, group = 1)) + geom_point() + geom_line()
elbow_plot <-  ggarrange(elbow_plot1, elbow_plot2, elbow_plot3, 
                         ncol = 1, nrow = 3)


## mean posterior probabilities
# model 3
postprob_m3 <- depmixS4::posterior(fit_m3)
postprob_m3$state <- as.factor(postprob_m3$state)

meanpostprob1 <- as.data.frame(postprob_m3) %>% 
  group_by(state) %>%
  summarise(s1 = mean(S1))
meanpostprob2 <- as.data.frame(postprob_m3) %>% 
  group_by(state) %>%
  summarise(s2 = mean(S2))
meanpostprob3 <- as.data.frame(postprob_m3) %>% 
  group_by(state) %>%
  summarise(s3 = mean(S3))

meanpostprob_m3 <- round(cbind(meanpostprob1$s1, 
                               meanpostprob2$s2, 
                               meanpostprob3$s3), 2)

# model 4
postprob_m4 <- depmixS4::posterior(fit_m4)
postprob_m4$state <- as.factor(postprob_m4$state)

meanpostprob1 <- as.data.frame(postprob_m4) %>% 
  group_by(state) %>%
  summarise(s1 = mean(S1))
meanpostprob2 <- as.data.frame(postprob_m4) %>% 
  group_by(state) %>%
  summarise(s2 = mean(S2))
meanpostprob3 <- as.data.frame(postprob_m4) %>% 
  group_by(state) %>%
  summarise(s3 = mean(S3))
meanpostprob4 <- as.data.frame(postprob_m4) %>% 
  group_by(state) %>%
  summarise(s4 = mean(S4))

meanpostprob_m4 <- round(cbind(meanpostprob1$s1, 
                               meanpostprob2$s2, 
                               meanpostprob3$s3,
                               meanpostprob4$s4), 2)

# model 8
postprob_m8 <- depmixS4::posterior(fit_m8)
postprob_m8$state <- as.factor(postprob_m8$state)

meanpostprob1 <- as.data.frame(postprob_m8) %>% 
  group_by(state) %>%
  summarise(s1 = mean(S1))
meanpostprob2 <- as.data.frame(postprob_m8) %>% 
  group_by(state) %>%
  summarise(s2 = mean(S2))
meanpostprob3 <- as.data.frame(postprob_m8) %>% 
  group_by(state) %>%
  summarise(s3 = mean(S3))
meanpostprob4 <- as.data.frame(postprob_m8) %>% 
  group_by(state) %>%
  summarise(s4 = mean(S4))
meanpostprob5 <- as.data.frame(postprob_m8) %>% 
  group_by(state) %>%
  summarise(s5 = mean(S5))
meanpostprob6 <- as.data.frame(postprob_m8) %>% 
  group_by(state) %>%
  summarise(s6 = mean(S6))
meanpostprob7 <- as.data.frame(postprob_m8) %>% 
  group_by(state) %>%
  summarise(s7 = mean(S7))
meanpostprob8 <- as.data.frame(postprob_m8) %>% 
  group_by(state) %>%
  summarise(s8 = mean(S8))

meanpostprob_m8 <- round(cbind(meanpostprob1$s1, 
                               meanpostprob2$s2, 
                               meanpostprob3$s3,
                               meanpostprob4$s4,
                               meanpostprob5$s5,
                               meanpostprob6$s6,
                               meanpostprob7$s7,
                               meanpostprob8$s8), 2)



# bayes factor
bf_test <- bayestestR::bic_to_bf(c(BIC(fit_m8), BIC(fit_m9)),
                                 denominator = BIC(fit_m8), log = T)

# Plot model 8
posterior_states <- depmixS4::posterior(fit_m8)
posterior_states$state <- as.factor(posterior_states$state)

plot_data <- cbind(dat_lca, posterior_states) %>% 
  pivot_longer(age:fem_exm, 
               names_to = "measure", 
               values_to = "value",
               values_transform = as.numeric)

plot_data$measure <- as.factor(plot_data$measure)
levels(plot_data$measure) <- c("Age",
                               "Agreeableness",
                               "Conscientiousness",
                               "Extraversion",
                               "Neuroticism",
                               "Openess",
                               "Extrinsic Motivation",
                               "Intrinsic Motivation",
                               "Gender (Male)",
                               "HISEI",
                               "Immigrant Background",
                               "Grade Point Average (-)",
                               "First-Generation-Student")

# max_lca_probs <- cbind(dat_lca, posterior_states) %>% 
#  mutate(ID_t = 1:nrow(.)) %>% 
#  gather("state_prob", "value", starts_with("S", ignore.case = FALSE)) %>% 
#  group_by(ID_t) %>% 
#  summarize(max_prob=max(value))
# 
# summary(max_lca_probs) 

plot_data$var_type <- ifelse(plot_data$measure == "Gender (Male)" |
                               plot_data$measure == "First-Generation-Student" | 
                               plot_data$measure == "Immigrant Background", "categorical",
                             ifelse(plot_data$measure == "Openess" | 
                                      plot_data$measure == "Conscientiousness"|
                                      plot_data$measure == "Extraversion" | 
                                      plot_data$measure == "Agreeableness" | 
                                      plot_data$measure == "Neuroticism" |
                                      plot_data$measure == "Extrinsic Motivation" |
                                      plot_data$measure == "Intrinsic Motivation" |
                                      plot_data$measure == "HISEI" |
                                      plot_data$measure == "Grade Point Average (-)" |
                                      plot_data$measure == "Age", "continuous", NA))

plot_data_continuous <- plot_data %>%
  filter(var_type == "continuous") %>%
  group_by(measure) %>%
  mutate(z = scale(value)) %>%
  ungroup()

plot_continuous <- ggplot(plot_data_continuous, aes(x = measure, y = z, 
                                                    color = state,
                                                    group = state)) +
  stat_summary(geom = "point", fun = mean, size = 3) +
  stat_summary(fun = mean, geom = "line", size = 0.5)+
  guides(x =  guide_axis(angle = 45))


prob_categorical <- summary(fit_m8)
prob_categorical <- as.data.frame(prob_categorical)

df_prob_categorical <- rbind(prob_categorical$Re1.1,  # gender: male 
                             prob_categorical$Re2.1,  # first-generation-student
                             prob_categorical$Re3.1)  # migration background 

df_prob_categorical <- as.data.frame(df_prob_categorical)
df_prob_categorical$measure <- c("Gender (Male)",             # gender
                                 "First-Generation-Student",  # par_edu
                                 "Immigrant Background")      # mig_bac

df_prob_categorical <- df_prob_categorical %>% 
  pivot_longer(1:8, names_to = "state", 
               values_to = "value",
               values_transform = as.numeric) 

df_prob_categorical$state <- as.factor(df_prob_categorical$state)
levels(df_prob_categorical$state) <- c(1, 2, 3, 4, 5, 6, 7, 8)

plot_categorical <- ggplot(df_prob_categorical, aes(x = measure, y = value, 
                                                    color = state, group = state)) +
  stat_summary(geom = "point", fun = mean, size = 3) +
  stat_summary(fun = mean, geom = "line", size = 0.5) +
  guides(x =  guide_axis(angle = 45))

df_plot_both1 <- as.data.frame(plot_data_continuous)
df_plot_both1 <- df_plot_both1 %>% 
  dplyr::select(measure, state, z) %>% 
  dplyr::rename(value = z)

df_plot_both2 <- as.data.frame(df_prob_categorical)
df_plot_both <- rbind(df_plot_both1, df_plot_both2)

plot_both <- ggplot(df_plot_both, aes(x = measure, y = value, 
                                      color  =state, group = state)) +
  stat_summary(geom = "point", fun = mean, size = 3) +
  stat_summary(fun = mean, geom = "line", size = 0.5) +
  guides(x =  guide_axis(angle = 45)) +
  scale_x_discrete(limits=c("Age", 
                            "Openess","Conscientiousness","Extraversion", 
                            "Agreeableness", "Neuroticism",
                            "Extrinsic Motivation", "Intrinsic Motivation",
                            "Grade Point Average (-)",  "HISEI", 
                            "First-Generation-Student", "Immigrant Background",
                            "Gender (Male)")) +
  expand_limits(y = c(-1, 1)) +
  scale_y_continuous(
    name = "z-Value \n",
    sec.axis = sec_axis(trans = ~ . *1, name = "Conditional Probabilities of  \n At-Risk Category\n",
                        breaks = seq(0, 1, by = 0.25),)) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             color = "grey", size = 0.5) +
  xlab("")



#### ------------------------------ (6) ANOVA ------------------------------ ####
data6 <- data6 %>%
  dplyr::mutate(rsk_grp = ifelse(class == 3, 1, 0)) 

data6$rsk_grp <- as.factor(data6$rsk_grp)

mean_socint2 <- aggregate(data6$soc_int, list(data6$rsk_grp), mean , na.rm = T)
boxplot_socint <- ggplot(filter(data6, !is.na(rsk_grp)), aes(x = rsk_grp, y = soc_int)) + geom_boxplot()

anova_socint2 <- aov(soc_int ~ rsk_grp, data = data6)
summary(anova_socint2)
report(anova_socint2)

anova_acaint2 <- aov(aca_int ~ rsk_grp, data = data6)
summary(anova_acaint2)
report(anova_acaint2)


#### --------------------------- (6) SEM ---------------------------- ####
  
test <- (-2*as.numeric(logLik(fit_m3))) + ((log((nobs(fit_m3) + 2)/24)) * freepars(fit_m3))

BIC_m3a <- (-2*as.numeric(logLik(fit_m3))) + (log(nobs(fit_m3)) * freepars(fit_m3))
BIC_m3b <- (-2*as.numeric(logLik(fit_m3))) + (log(sum(ntimes(fit_m3))) * freepars(fit_m3))


############################## END Syntax Jürgen ############################