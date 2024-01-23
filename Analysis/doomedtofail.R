# doomedtofail.R
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
  dplyr::mutate_all( ~ ifelse(is.nan(.), NA, .))

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
data7 <- data6 %>% 
  dplyr::mutate(ext_rnk = case_when(big_ext <= 5/4 ~ 1,
                                    between(big_ext, 5/4, 10/4) ~ 2,
                                    between(big_ext, 10/4, 15/4) ~ 3,
                                    big_ext >= 15/4 ~ 4,
                                    TRUE ~ as.numeric(NA))) %>%
  dplyr::mutate(agr_rnk = case_when(big_agr <= 5/4 ~ 1,
                                    between(big_agr, 5/4, 10/4) ~ 2,
                                    between(big_agr, 10/4, 15/4) ~ 3,
                                    big_agr >= 15/4 ~ 4,
                                    TRUE ~ as.numeric(NA))) %>% 
  dplyr::mutate(con_rnk = case_when(big_con <= 5/4 ~ 1,
                                    between(big_con, 5/4, 10/4) ~ 2,
                                    between(big_con, 10/4, 15/4) ~ 3,
                                    big_con >= 15/4 ~ 4,
                                    TRUE ~ as.numeric(NA))) %>% 
  dplyr::mutate(neu_rnk = case_when(big_neu <= 5/4 ~ 1,
                                    between(big_neu, 5/4, 10/4) ~ 2,
                                    between(big_neu, 10/4, 15/4) ~ 3,
                                    big_neu >= 15/4 ~ 4,
                                    TRUE ~ as.numeric(NA))) %>% 
  dplyr::mutate(ope_rnk = case_when(big_ope <= 5/4 ~ 1,
                                    between(big_ope, 5/4, 10/4) ~ 2,
                                    between(big_ope, 10/4, 15/4) ~ 3,
                                    big_ope >= 15/4 ~ 4,
                                    TRUE ~ as.numeric(NA))) %>% 
  dplyr::mutate(inm_rnk = case_when(fem_inm <= 4/3 ~ 1,
                                    between(fem_inm, 4/3, 8/3) ~ 2,
                                    fem_inm >= 8/3 ~ 3,
                                    TRUE ~ as.numeric(NA))) %>% 
  dplyr::mutate(exm_rnk = case_when(fem_exm <= 4/3 ~ 1,
                                    between(fem_exm, 4/3, 8/3) ~ 2,
                                    fem_exm >= 8/3 ~ 3,
                                    TRUE ~ as.numeric(NA)))

# manifest variables must contain only integer values:

data8 <- data7 %>% 
  dplyr::mutate(par_edu = as.integer(par_edu)) %>%
  dplyr::mutate(par_ocu = as.integer(par_ocu)) %>%
  dplyr::mutate(mig_bac = as.integer(mig_bac)) %>%
  dplyr::mutate(typ_sch = as.integer(typ_sch)) %>%
  dplyr::mutate(paa_gpa = as.integer(paa_gpa)) %>%
  dplyr::mutate(voc_tra = as.integer(voc_tra)) %>%
  dplyr::mutate(ext_rnk = as.integer(ext_rnk)) %>%
  dplyr::mutate(agr_rnk = as.integer(agr_rnk)) %>% 
  dplyr::mutate(con_rnk = as.integer(con_rnk)) %>% 
  dplyr::mutate(neu_rnk = as.integer(neu_rnk)) %>% 
  dplyr::mutate(ope_rnk = as.integer(ope_rnk)) %>% 
  dplyr::mutate(inm_rnk = as.integer(inm_rnk)) %>% 
  dplyr::mutate(exm_rnk = as.integer(exm_rnk))
  

## LCA
## Tutorial: https://statistics.ohlsen-web.de/latent-class-analysis-polca/
## Paper: https://osf.io/preprints/psyarxiv/97uab
  
  
# select variables
dat_lca <- data8 %>%
  dplyr::select(par_edu, par_ocu, mig_bac, typ_sch, paa_gpa,
                voc_tra, ext_rnk, agr_rnk, con_rnk, neu_rnk,
                ope_rnk, inm_rnk, exm_rnk)

# define function
f <- with(dat_lca, cbind(par_edu, par_ocu, mig_bac, typ_sch, paa_gpa,
                         voc_tra, ext_rnk, agr_rnk, con_rnk, neu_rnk,
                         ope_rnk, inm_rnk, exm_rnk) ~ 1)
  
set.seed(123)
lc1 <- poLCA(f, dat_lca, nclass = 1, na.rm = FALSE, nrep = 10, maxiter = 3000)
lc2 <- poLCA(f, dat_lca, nclass = 2, na.rm = FALSE, nrep = 10, maxiter = 3000)
lc3 <- poLCA(f, dat_lca, nclass = 3, na.rm = FALSE, nrep = 10, maxiter = 3000)
lc4 <- poLCA(f, dat_lca, nclass = 4, na.rm = FALSE, nrep = 10, maxiter = 3000)
lc5 <- poLCA(f, dat_lca, nclass = 5, na.rm = FALSE, nrep = 10, maxiter = 3000)
lc6 <- poLCA(f, dat_lca, nclass = 6, na.rm = FALSE, nrep = 10, maxiter = 3000)

# generate dataframe with fit-values

results <- data.frame(Modell = c("Modell 1"),
                      log_likelihood = lc1$llik,
                      df = lc1$resid.df,
                      BIC = lc1$bic,
                      ABIC =  (-2 * lc1$llik) + ((log((lc1$N + 2) / 24)) * lc1$npar),
                      CAIC = (-2 * lc1$llik) + lc1$npar * (1 + log(lc1$N)), 
                      likelihood_ratio = lc1$Gsq)

results$Modell<-as.integer(results$Modell)
results[1,1] <- c("Modell 1")
results[2,1] <- c("Modell 2")
results[3,1] <- c("Modell 3")
results[4,1] <- c("Modell 4")
results[5,1] <- c("Modell 5")
results[6,1] <- c("Modell 6")

results[2,2] <- lc2$llik
results[3,2] <- lc3$llik
results[4,2] <- lc4$llik
results[5,2] <- lc5$llik
results[6,2] <- lc6$llik

results[2,3] <- lc2$resid.df
results[3,3] <- lc3$resid.df
results[4,3] <- lc4$resid.df
results[5,3] <- lc5$resid.df
results[6,3] <- lc6$resid.df

results[2,4] <- lc2$bic
results[3,4] <- lc3$bic
results[4,4] <- lc4$bic
results[5,4] <- lc5$bic
results[6,4] <- lc6$bic

results[2,5] <- (-2*lc2$llik) + ((log((lc2$N + 2)/24)) * lc2$npar) #abic
results[3,5] <- (-2*lc3$llik) + ((log((lc3$N + 2)/24)) * lc3$npar)
results[4,5] <- (-2*lc4$llik) + ((log((lc4$N + 2)/24)) * lc4$npar)
results[5,5] <- (-2*lc5$llik) + ((log((lc5$N + 2)/24)) * lc5$npar)
results[6,5] <- (-2*lc6$llik) + ((log((lc6$N + 2)/24)) * lc6$npar)
 
results[2,6] <- (-2*lc2$llik) + lc2$npar * (1 + log(lc2$N)) #caic
results[3,6] <- (-2*lc3$llik) + lc3$npar * (1 + log(lc3$N))
results[4,6] <- (-2*lc4$llik) + lc4$npar * (1 + log(lc4$N))
results[5,6] <- (-2*lc5$llik) + lc5$npar * (1 + log(lc5$N))
results[6,6] <- (-2*lc6$llik) + lc6$npar * (1 + log(lc6$N))
 
results[2,7] <- lc2$Gsq
results[3,7] <- lc3$Gsq
results[4,7] <- lc4$Gsq
results[5,7] <- lc5$Gsq
results[6,7] <- lc6$Gsq

# add entropy

entropy <- function (p) sum(-p*log(p))

results$R2_entropy
results[1,8] <- c("-")

error_prior<-entropy(lc2$P) # class proportions model 2
error_post<-mean(apply(lc2$posterior,1, entropy),na.rm = TRUE)
results[2,8]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc3$P) # class proportions model 3
error_post<-mean(apply(lc3$posterior,1, entropy),na.rm = TRUE)
results[3,8]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc4$P) # class proportions model 4
error_post<-mean(apply(lc4$posterior,1, entropy),na.rm = TRUE)
results[4,8]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc5$P) # class proportions model 5
error_post<-mean(apply(lc5$posterior,1, entropy),na.rm = TRUE)
results[5,8]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc6$P) # class proportions model 6
error_post<-mean(apply(lc6$posterior,1, entropy),na.rm = TRUE)
results[6,8]<-round(((error_prior-error_post) / error_prior),3)

colnames(results)<-c("Model","log-likelihood","resid. df","BIC","aBIC","cAIC",
                     "likelihood-ratio","Entropy")

lca_results <- results

results

plot(lc3)

# average latent class probabilities for most likely latent class membership

# The average latent posterior probabilities are presented in a matrix with 
# diagonals representing the average probability of a person being assigned to 
# a class given his or her scores on the indicator variables used to create the 
# classes. Higher diagonal values (i.e., closer to 1.0) are desirable. 
# Off-diagonal elements in the posterior probability matrix contain prob-
# abilities of cases that belong in one class being assigned to another class in 
# the current solution.
#
# We agree that greater than .90 is ideal; but if other criteria are met and the 
# model is theoretically supported, probabilities between .80 and .90 are 
# acceptable (Weller et al., 2020). 
# https://journals.sagepub.com/doi/full/10.1177/0095798420930932) 

meanpostprob <- round(aggregate(x = lc3$posterior,
                                by = list(lc3$predclass),FUN = "mean") , 2)

# assign each case to a specific class (group) based on their posterior class 
# membership probabilities:
data6$class <- lc3$predclass

data6$class <- as.factor(data6$class)
a <- aggregate(data6$soc_int, list(data6$class), mean , na.rm = T)

b <- ggplot(data6, aes(x=class, y=soc_int)) + geom_violin()

library(report)
anova_socint <- aov(soc_int ~ class, data = data6)
summary(anova_socint)
# report(anova_socint)
# The main effect of class is statistically significant and very small

anova_acaint <- aov(aca_int ~ class, data = data6)
summary(anova_acaint)
# The main effect of class is statistically significant and very small

# class x dropoutintention(welle 9)
data6$tg64051 <- as.factor(data6$tg64051)
a <- xtabs( ~ class + tg64051, data = data6)
