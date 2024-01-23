# doomedtofail_v1-2.R
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
# Last mod: Jan/17/2024, David Simon


####  ------------- (1) Set Working Directory, Load Packages ------------- ####

library(haven)           # to import SPSS files
library(tidyverse)       # for data management
library(rquery)          # to copy STATA merge behavior
library(fauxnaif)        # for defining missing values 
                           # (remotes::install_github("rossellhayes/fauxnaif")
library(gtools)          # for create a factor variable using quantiles
library(psych)           # for scale construction
library(poLCA)           # for LCA
library(MASS)            # for LCA
library(scatterplot3d)   # for LCA
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
  
  dplyr::mutate(par_ocu = case_when((t731403_g8 >= 8 & t731453_g8 >= 8) ~ 1,    
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
                                    ts11218 >= 2.6 &  ts11218 < 3.6 ~ 2,
                                    ts11218 >= 1.6 &  ts11218 < 2.6 ~ 3,
                                    ts11218 >= 1.0 &  ts11218 < 1.6 ~ 4,
                                    TRUE ~ as.numeric(NA)))  %>%
                                    # 1 = sufficient, 2 = satisfactory, 
                                    # 3 = good, 4 = very good)
  
  dplyr::mutate(voc_tra = case_when(ts15218 == 1 ~ 0,
                                    ts15218 == 2 ~ 1,
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


## current study situation                                                      # Nummerierung dataX überarbeiten (?)

# social and academic integration
data5 <- data5 %>% 
  dplyr::mutate(aca_int = rowMeans(subset(data5, select = c(tg53232, tg53234, 
                                                            tg53236, tg53231, 
                                                            tg53233, tg53235,
                                                            tg53211, tg53212, 
                                                            tg53213)),
                                   na.rm = TRUE)) %>%
  dplyr::mutate(soc_int = rowMeans(subset(data5, select = c(tg53111, tg53112, 
                                                            tg53112, tg53121, 
                                                            tg53122, tg53123)),
                                   na.rm = TRUE))
                                   

#### ------------------------------ (5) LCA ------------------------------ ####

data6 <- data5 %>% 
  dplyr::mutate(across(everything(), ~ ifelse(is.nan(.x), NA, .x)))

# create a factor variable (low, medium, high (tertiles)) for big5 and femola              

# Kategorien vereinfachen, 
# Sonst kein Maximum der Likelihood
# ALERT: iterations finished, MAXIMUM LIKELIHOOD NOT FOUND - ab 3 Klassen 
 # Terzile funktionieren nicht so gut
 # siehe table(data6$ext_ter) - siehe Alternative nächster Abschnitt

# data7 <- data6 %>% 
#   dplyr::mutate(ext_ter =  gtools::quantcut(big_ext, 2), 
#                 ext_rnk = as.numeric(ext_ter))  %>%
#   
#   dplyr::mutate(agr_ter =  gtools::quantcut(big_agr, 2), 
#                 agr_rnk = as.numeric(ext_ter))  %>% 
#   
#   dplyr::mutate(con_ter =  gtools::quantcut(big_con, 2), 
#                 con_rnk = as.numeric(con_ter))  %>% 
#   
#   dplyr::mutate(neu_ter =  gtools::quantcut(big_neu, 2), 
#                 neu_rnk = as.numeric(neu_ter))  %>% 
#   
#   dplyr::mutate(ope_ter =  gtools::quantcut(big_ope, 2), 
#                 ope_rnk = as.numeric(ope_ter))  %>% 
#   
#   dplyr::mutate(inm_ter =  gtools::quantcut(fem_inm, 2), 
#                 inm_rnk = as.numeric(inm_ter))  %>% 
#   
#   dplyr::mutate(exm_ter =  gtools::quantcut(fem_exm, 2), 
#                 exm_rnk = as.numeric(exm_ter))


# Alternative
# data7 <- data6 %>% 
#   dplyr::mutate(ext_rnk = case_when(big_ext <= 5/4 ~ 1,
#                                     between(big_ext, 5/4, 10/4) ~ 2,
#                                     between(big_ext, 10/4, 15/4) ~ 3,
#                                     big_ext >= 15/4 ~ 4,
#                                     TRUE ~ as.numeric(NA))) %>%
#   dplyr::mutate(agr_rnk = case_when(big_agr <= 5/4 ~ 1,
#                                     between(big_agr, 5/4, 10/4) ~ 2,
#                                     between(big_agr, 10/4, 15/4) ~ 3,
#                                     big_agr >= 15/4 ~ 4,
#                                     TRUE ~ as.numeric(NA))) %>% 
#   dplyr::mutate(con_rnk = case_when(big_con <= 5/4 ~ 1,
#                                     between(big_con, 5/4, 10/4) ~ 2,
#                                     between(big_con, 10/4, 15/4) ~ 3,
#                                     big_con >= 15/4 ~ 4,
#                                     TRUE ~ as.numeric(NA))) %>% 
#   dplyr::mutate(neu_rnk = case_when(big_neu <= 5/4 ~ 1,
#                                     between(big_neu, 5/4, 10/4) ~ 2,
#                                     between(big_neu, 10/4, 15/4) ~ 3,
#                                     big_neu >= 15/4 ~ 4,
#                                     TRUE ~ as.numeric(NA))) %>% 
#   dplyr::mutate(ope_rnk = case_when(big_ope <= 5/4 ~ 1,
#                                     between(big_ope, 5/4, 10/4) ~ 2,
#                                     between(big_ope, 10/4, 15/4) ~ 3,
#                                     big_ope >= 15/4 ~ 4,
#                                     TRUE ~ as.numeric(NA))) %>% 
#   dplyr::mutate(inm_rnk = case_when(fem_inm <= 4/3 ~ 1,
#                                     between(fem_inm, 4/3, 8/3) ~ 2,
#                                     fem_inm >= 8/3 ~ 3,
#                                     TRUE ~ as.numeric(NA))) %>% 
#   dplyr::mutate(exm_rnk = case_when(fem_exm <= 4/3 ~ 1,
#                                     between(fem_exm, 4/3, 8/3) ~ 2,
#                                     fem_exm >= 8/3 ~ 3,
#                                     TRUE ~ as.numeric(NA)))


# Alternative
data7 <- data6 %>%
  dplyr::mutate(fem_inm_di = ifelse(fem_inm <= 2.5, 0, 1),
                fem_exm_di = ifelse(fem_exm <= 2.5, 0, 1),
                big_ext_di = ifelse(big_ext <= 3, 0, 1),
                big_agr_di = ifelse(big_agr <= 3, 0, 1),
                big_con_di = ifelse(big_con <= 3, 0, 1),
                big_neu_di = ifelse(big_neu <= 3, 0, 1),
                big_ope_di = ifelse(big_ope <= 3, 0, 1),
                paa_gpa_di = ifelse(paa_gpa <= 2, 0, 1),
                par_edu_di = ifelse(par_edu == 1, 0, 1),
                mig_bac_di = mig_bac - 1,
                par_ocu_di = par_ocu - 1,
                typ_sch_di = typ_sch - 1,
                voc_tra_di = voc_tra)


# manifest variables must contain only integer values:

dat_lca <- data7 %>% 
  dplyr::mutate(
    across(c(par_edu_di,
             par_ocu_di,
             mig_bac_di,
             typ_sch_di,
             paa_gpa_di,
             voc_tra_di,
             big_ext_di,
             big_agr_di,
             big_con_di,
             big_neu_di,
             big_ope_di,
             fem_inm_di,
             fem_exm_di),
           as.factor))
  

## LCA
## Tutorial: https://statistics.ohlsen-web.de/latent-class-analysis-polca/
## Paper: https://osf.io/preprints/psyarxiv/97uab
  
  
# select variables
# dat_lca <- data8 %>%
#   dplyr::select(par_edu_di,
#                 par_ocu_di,
#                 mig_bac_di,
#                 typ_sch_di,
#                 paa_gpa_di,
#                 voc_tra_di,
#                 big_ext_di,
#                 big_agr_di,
#                 big_con_di,
#                 big_neu_di,
#                 big_ope_di,
#                 fem_inm_di,
#                 fem_exm_di)

# define function
f <- with(dat_lca, cbind(par_edu_di,
                         par_ocu_di,
                         mig_bac_di,
                         typ_sch_di,
                         paa_gpa_di,
                         voc_tra_di,
                         big_ext_di,
                         big_agr_di,
                         big_con_di,
                         big_neu_di,
                         big_ope_di,
                         fem_inm_di,
                         fem_exm_di) ~ 1)
  


max_II <- -100000
min_bic <- 100000


# define empty results object to save fit indices
results <- data.frame(classes = as.numeric(),
                      log_likelihood = as.numeric(),
                      df = as.numeric(),
                      BIC = as.numeric(),
                      ABIC =  as.numeric(),
                      CAIC = as.numeric() ,
                      likelihood_ratio = as.numeric())

# loop over latent class analyses with different number of classes
for(i in 1:6){  # for 1 to ... classes make
  set.seed(123)
  lc <- poLCA(f, dat_lca, 
              nclass=i, 
              maxiter=3000, 
              tol=1e-5, 
              na.rm=FALSE,  
              nrep=10, 
              verbose=TRUE, 
              calc.se=TRUE)
  
  # getting best model by BIC
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model<-lc
  }
  
  # saving the results to look for yourself
  results <- results %>%
    add_row(classes = i,
            log_likelihood = lc$llik,
            df = lc$resid.df,
            BIC = lc$bic,
            ABIC =  (-2 * lc$llik) + ((log((lc$N + 2) / 24)) * lc$npar),
            CAIC = (-2 * lc$llik) + lc$npar * (1 + log(lc$N)), 
            likelihood_ratio = lc$Gsq)
  }


results



## Plot results ############################################################## #
## (a) plot by poLCA
plot(LCA_best_model) # this is not very aufschlussreich

# ## (b) plot density of participants in each class as ridges
# ##     (which isn't entirely correct, as they aren't continuous,
# ##      but it gives a good overview)
# 
# # make long data set
# data_plot <- data.frame(dat_lca, class = LCA_best_model$predclass) %>%
#   pivot_longer(1:13, names_to = "names", values_to = "value") # keep class variable
# 
# # ggridges side by side
# ggplot(data_plot, aes(x=value, y=names, fill = as.factor(class))) +
#   geom_density_ridges_gradient(scale = 0.9, bandwidth = .5) +
#   scale_fill_viridis_d(alpha = .4) +
#   scale_x_continuous(limits = c(1,4), breaks = c(1,2,3,4)) +
#   theme_ridges()

## (c) plot bars
# make long data set with conditional probabilities
data_plot <- as.data.frame(LCA_best_model$probs)%>%
  dplyr::mutate(class = c(1,2)) %>%         # add class variable
  pivot_longer(1:26, names_to = "names", values_to = "value") %>% # keep class variable
  dplyr::filter(!str_detect(names, ".0")) %>% # just keep the probabilities for 1 (as those from 0 and 1 add up to 1)
  dplyr::mutate(names = gsub(".1", "", names))  # delete the ".1" at the end of variable name



ggplot(data_plot, aes(x=as.factor(names), y = value, fill = as.factor(class))) +
  geom_col(position = position_dodge(width=.4)) +
  scale_fill_viridis_d(alpha = .4)