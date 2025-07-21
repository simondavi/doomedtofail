# 01_datamanagement_doomedtofail.R
#
# Comment: 
#
# Input: SC5_CohortProfile_D_18-0-0.sav
#        SC5_pTargetCATI_D_18-0-0.sav
#        SC5_pTargetCAWI_D_18-0-0.sav
#        SC5_spSchool_D_18-0-0.sav
#        SC5_spVocTrain_D_18-0-0.sav 
#        SC5_StudyStates_D_18-0-0.dta
#        SC5_Basics_D_18-0-0.sav
# Output: data_lca_doomedtofail.csv
#         data_doomedtofail.Rda
#
# Contents: (1) Load Packages
#           (2) Read Data and Data Management
#           (3) Merge Data
#           (4) Compute Scale Scores and New Variables
#           (5) Output

####  ------------- (1) Load Packages ------------- ####

library(haven)          # to import SPSS files
library(tidyverse)      # for data management
library(reshape)
library(reshape2)
library(rquery)         # to copy STATA merge behavior
library(rqdatatable)
library(fauxnaif)       # for defining missing values 
                          # (remotes::install_github("rossellhayes/fauxnaif")
library(psych)          # for scale construction

# renv::clean()
# renv::snapshot()
# renv::restore()

#### ----------------- (2) Read Data and Data Management ----------------- ####

# cohort profile as a starting point

cohort <- haven::read_sav("Data_SC5_D_18-0-0/SC5_CohortProfile_D_18-0-0.sav") %>%
          dplyr::select(ID_t, wave, cohort,
                                    tx80121,       # oversample of tea edu (= 1)
                                    tx80107,       # first participation in wave 1
                                    tx8600y,       # survey year  
                                    tx8600m,       # survey month
                                    tx8600d) %>%   # survey day
          dplyr::filter(wave == 1)                       


# filter measures from CATI:

cati_w1 <- haven::read_sav("Data_SC5_D_18-0-0/SC5_pTargetCATI_D_18-0-0.sav") %>%
           dplyr::select(ID_t, wave, tg02001_ha,  # intended degree
                                     tg24150_g1,  # foreign student (!= 3)      
                                     t731301_g1,  # parental education
                                     t731351_g1,  
                                     t731453_g14, # parental occupation
                                     t731403_g14,
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


# filter measures from CAWI:

cawi_w8 <- haven::read_sav("Data_SC5_D_18-0-0/SC5_pTargetCAWI_D_18-0-0.sav") %>%
           dplyr::select(ID_t, wave, tg61031, tg61032, tg61033,  # int mo
                                     tg61061, tg61062, tg61063,
                                     tg61041, tg61042, tg61043,  
                                     tg61021, tg61022, tg61023,  # ext mo
                                     tg61011, tg61012, tg61013,
                                     tg61071, tg61072, tg61073,  
                                     tg61051, tg61052, tg61053,
                                     tg53221) %>%                # dropout intention
           dplyr::filter(wave == 8)

cawi_w8_wtg <- haven::read_sav("Data_SC5_D_18-0-0/SC5_pTargetCAWI_D_18-0-0.sav") %>% # retrospective data from those
               dplyr::select(ID_t, wave, tg61131, tg61132, tg61133,                  # who already have a teaching degree
                                         tg61161, tg61162, tg61163,
                                         tg61141, tg61142, tg61143,  
                                         tg61121, tg61122, tg61123,
                                         tg61111, tg61112, tg61113,
                                         tg61171, tg61172, tg61173,  
                                         tg61151, tg61152, tg61153) %>%
               dplyr::filter(wave == 8) %>%
               dplyr::rename(tg61031 = tg61131,
                             tg61032 = tg61132,
                             tg61033 = tg61133,
                             
                             tg61061 = tg61161,
                             tg61062 = tg61162,
                             tg61063 = tg61163,
                             
                             tg61041 = tg61141,
                             tg61042 = tg61142,
                             tg61043 = tg61143,
                             
                             tg61021 = tg61121,
                             tg61022 = tg61122,
                             tg61023 = tg61123,
                             
                             tg61011 = tg61111,
                             tg61012 = tg61112,
                             tg61013 = tg61113,
                             
                             tg61071 = tg61171,
                             tg61072 = tg61172,
                             tg61073 = tg61173,
                             
                             tg61051 = tg61151,
                             tg61052 = tg61152,
                             tg61053 = tg61153)

cawi_sp1 <- haven::read_sav("Data_SC5_D_18-0-0/SC5_pTargetCAWI_D_18-0-0.sav") %>%
            dplyr::select(ID_t, wave, tg53231, tg53233, tg53235,  # aca int
                                      tg53211, tg53212, tg53213,
                                      tg53111, tg53112, tg53113,  # soc int
                                      tg53114, tg53121, tg53122,  
                                      tg53123) %>%
            dplyr::filter(wave == 2)
           
              
# filter measures from spSchool:

spsch <- haven::read_sav("Data_SC5_D_18-0-0/SC5_spSchool_D_18-0-0.sav") %>%
         filter(ts11211 %in% c(1, 2)) %>%
         group_by(ID_t) %>%
         mutate(spell = row_number(), maxspell = n()) %>%
         filter(spell == maxspell) %>%  # for each person secondary school leaving grade is 
                                        # used of the latest school episode possible 
                                        # -> most likely higher education entrance qualification
         filter(ts11218 <= 4 & !is.na(ts11218)) %>%  # values above 4.0 as missing
         select(ID_t, ts11218) 


# filter measures from spVocTrain:

spvoc <- haven::read_sav("Data_SC5_D_18-0-0/SC5_spVocTrain_D_18-0-0.sav") %>%   
         dplyr::select(ID_t, wave, spell, subspell, 
                       h_aktstu,        # 1st study episode WT 2010
                       tg24202_g1,      # type of intended teaching degree
                       tg24170_g5,      # subject group
                       tg01003_ha) %>%  # type of higher education inst         
         dplyr::filter(wave == 1) %>%
         dplyr::filter(h_aktstu == 1) %>%
         dplyr::group_by(ID_t) %>% dplyr::slice_max(order_by = spell,
                                                    with_ties = TRUE) %>%
         dplyr::slice_tail(n = 1)  


# filter measures from StudyState:

ststa_help <- haven::read_dta("Data_SC5_D_18-0-0/SC5_StudyStates_D_18-0-0.dta") %>% 
              dplyr::select(ID_t, wave,
                            tx15322) %>%   # intended vocational qualification
              dplyr::filter(wave == 2) %>%
              dplyr::mutate(still_teaedu = ifelse(tx15322 %in% c(12, 14, 17), 1, 0)) %>%  # 1 = still teacher education
              select(ID_t, still_teaedu) # 1 = still teacher education

# purpose: define people who are not enrolled in a teacher education program at 
# measurement of integration and may therefore have changed their intended degree
# they will be assigned NA on integration (see bewlow).


# filter measures from StudyStates = definition of dropout:                     

# 0 = graduate = successfully completed at least one study episode; 
#     BA, MA, state examination (in teacher education)
#
# 1 = dropout = all study episodes have ended, none has been successfully 
#     completed; additionally: self-reported dropouts in the last CAWI 
#     if none of the study episodes has been successfully completed and no 
#     study episode is confirmed ongoing
#
# 2 = studying = study episode confirmed to be ongoing in wave 15 and 
#     participation in wave 15 and none successfully completed before
#
# Source: https://forum.lifbi.de/t/sc5-studienabbruch-syntax-vorschlag/3963
# and: https://forum.lifbi.de/t/sc5-studienabbruch/3956


ststa_tmp <- haven::read_dta("Data_SC5_D_18-0-0/SC5_StudyStates_D_18-0-0.dta") %>% 
             dplyr::select(ID_t, wave, tx24001,      # interview order
                                       tx24100,      # Status of studies (completed,ongoing)
                                       tx15318,      # Successful completion
                                       tx15317,      # Vocational qualification
                                       tx15322) %>%  # Intended qualification
              dplyr::filter(wave >= 2 & wave <= 15) %>%  # period of observation wave 2 to 15
 
              dplyr::filter(!tx24100 %in% c(-20)) %>%  # delete missing episodes

              dplyr::filter(tx15322 %in% c(12, 14, 17) | is.na(tx15322))       # tea edu completed? all subjects: c(7:19, 29) 

any_com_df <- ststa_tmp %>%  
              dplyr::group_by(ID_t) %>%                                         # any completion?
              dplyr::summarise(any_com = max(tx15318, na.rm = FALSE))           # 0 = no, 1 = yes
 
las_sta_df <- ststa_tmp %>%   
              dplyr::arrange(ID_t, tx24001) %>%                                 # last state available
              dplyr::group_by(ID_t) %>%                                         # 0 = all episodes completed
              dplyr::summarise(las_sta = last(tx24100))                         # 1 = episodes ongoing, 2 = some ongoing

ststa <- merge(any_com_df, las_sta_df, by = c("ID_t"), all.x = FALSE) %>%        
         dplyr::mutate(dro_out = (any_com == 0 & las_sta == 0))               # create drop out variable

# add CAWI data                                                                 
cawi_do <- haven::read_sav("Data_SC5_D_18-0-0/SC5_pTargetCAWI_D_18-0-0.sav") %>%
           dplyr::select(ID_t, wave, tg51004, # degree course abandoned?
                                     tg51000) # currently studying?

tg51004_df <- cawi_do %>%                                                       # summarise to one line per ID
              dplyr::group_by(ID_t) %>%
              dplyr::summarise(tg51004_one = case_when(any(tg51004 == 3) ~ 3))  # 3 = I've given up studying completely

tg51000_df <- cawi_do %>%
              dplyr::group_by(ID_t) %>%
              dplyr::summarise(tg51000_one = case_when(any(tg51000 == 2) ~ 2))  # 2 = I've given up studying completely.

ststa <- merge(ststa, tg51004_df, by = c("ID_t"), all.x = FALSE)
ststa <- merge(ststa, tg51000_df, by = c("ID_t"), all.x = FALSE)

# Do people even have participated in the last survey (here wave 15)?
part_stat <- haven::read_sav("Data_SC5_D_18-0-0/SC5_CohortProfile_D_18-0-0.sav") %>%
             dplyr::select(ID_t, wave, tx80220)  %>%  # participation/drop-out status
             dplyr::filter(wave == 15) 

# Which wave was their last state?
las_wav_df <- ststa_tmp %>%   
              dplyr::arrange(ID_t, tx24001) %>%                                 
              dplyr::group_by(ID_t) %>%                                         
              dplyr::summarise(las_wav = last(wave))  

ststa <- merge(ststa, part_stat, by = c("ID_t"), all.x = FALSE)
ststa <- merge(ststa, las_wav_df, by = c("ID_t"), all.x = FALSE)

# create finale dropout state
  
ststa <- ststa %>%
  dplyr::mutate(dro_fin = case_when(
    
# people are considered dropout if dropout is true or conditions on tg51004 or 
# tg51000 are met
  dro_out == TRUE | tg51004_one == 3 | tg51000_one == 2 ~ 1,                    # dropout = 1
  
# people are considered as graduated if they completed any state in regards to 
# the prior defined range of intended vocational qualifications
  any_com ==  1 ~ 0,                                                            # graduate = 0

# people are considered still studying if they participated in the latest wave 
# (here: wave 15) and this wave equals there last state and at least some study 
# episodes are ongoing
  las_sta != 0 & tx80220 == 1 & las_wav == 15 ~ 2,                              # studying = 2
  TRUE ~ as.numeric(NA)                                                         # Notiz: Anpassen, wenn last wave != 15
  ))                                                                            

# ~ 280 (nach (3) Merge Data), wirkt erstmal zu niedrig (ca. 5%)
# bei anderen Autoren: 8 % observed (ganzes sample (imputiert))
# vielleicht passt es aber schon (Anzunehmen wären ca. 15%)


# filter measures from Basics:
basic <- haven::read_sav("Data_SC5_D_18-0-0/SC5_Basics_D_18-0-0.sav") %>%       
         dplyr::select(ID_t, t70000m,  # month of birth 
                             t70000y)  # year of birth 


####  -------------------------- (3) Merge Data -------------------------- ####

# every data set is prepared 1 line per person, so that 1:1 merging works and
# results in wide data as required 
#
# to combine multiple tables in rquery one uses the natural_join operator:
# In the rquery natural_join, rows are matched by column keys and any two 
# columns with the same name are coalesced (meaning the first table with a 
# non-missing values supplies the answer) jointype = 'LEFT' is used to augment 
# the left table with additional values from the right (as ordered).

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

data <- rquery::natural_join(data, cawi_w8_wtg, # supply femola
                             by = "ID_t",       # with answers from students
                             jointype = "LEFT") # with a teaching degree

data <- rquery::natural_join(data, cawi_sp1,
                             by = "ID_t",
                             jointype = "LEFT") 

data <- rquery::natural_join(data, spsch,
                             by = "ID_t",
                             jointype = "LEFT") 

data <- rquery::natural_join(data, spvoc,
                             by = "ID_t",
                             jointype = "LEFT")

data <- rquery::natural_join(data, ststa_help,
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
                             dplyr::filter(h_aktstu == 1) %>%                   
                             # only start of studies WT 2010
                             dplyr::filter(tx80107 == 1) %>%
                             # only first participation in wave 1
                             dplyr::select(-wave) %>%
                             # drop wave, it contains no information anymore
                             
                             dplyr::mutate(across(everything(), 
                                                  ~ fauxnaif::na_if_in(., ~ . < 0)))
                             # replace all negative Values with NA  

# Only one ID per line?                                                   
# x <- data %>% dplyr::count(ID_t)
# min(x$n)
# max(x$n)

# colMeans(is.na(data))


####  ------------ (4) Compute Scale Scores and New Variables ------------ ####

## entry characteristics
                             
# students background: Parental education (par_edu), SES (hisei)
data2 <- data %>%
  dplyr::mutate(par_edu = case_when((t731301_g1 < 9 & t731351_g1 < 9) ~ 1,
                                    (t731301_g1 >= 9 & t731351_g1 >= 9) ~ 3,
                                    (t731301_g1 >= 9 & t731351_g1 < 9 | 
                                     t731301_g1 < 9 & t731351_g1 >= 9) ~ 2,
                                    TRUE ~ as.numeric(NA))) %>%
                                    # 1 = no parent tertiary education, 2 = one 
                                    # parent, 3 = both parents
  dplyr::group_by(ID_t) %>% 
  dplyr::mutate(hisei = max(t731453_g14, t731403_g14)) %>%  # ISCO-08
  dplyr::ungroup() %>%
  dplyr::mutate(hisei = case_when((is.na(t731453_g14) |
                                   is.na(t731403_g14)) ~ as.numeric(NA),
                                  TRUE ~ as.numeric(hisei))) 

# academic aptitude: secondary school leaving grade (+ age, gender)
data3 <- data2 %>% 
  dplyr::mutate(aca_abi = (5 - ts11218),              # inverse grade
                gender = ifelse(t700001 == 1, 1, 0),  # gender
                # 1 = male
                # 0 = female
                age_month =  (tx8600y - t70000y) * 12 + (tx8600m - t70000m),
                age  = round(age_month / 12, 2))


## personality (big 5, motivation for choosing teacher education (femola))

# big 5
# recode inverse items big 5:
data3$t66800a <- 6 - data3$t66800a
data3$t66800g <- 6 - data3$t66800g
data3$t66800c <- 6 - data3$t66800c
data3$t66800d <- 6 - data3$t66800d
data3$t66800e <- 6 - data3$t66800e

data4 <- data3 %>% 
  dplyr::mutate(big_ext = rowMeans(subset(data3, select = c(t66800a, t66800f)),
                                   na.rm = TRUE),
                big_agr = rowMeans(subset(data3, select = c(t66800b, t66800g,
                                                            t66800k)),
                                   na.rm = TRUE),
                big_con = rowMeans(subset(data3, select = c(t66800c, t66800h)),
                                   na.rm = TRUE),
                big_neu = rowMeans(subset(data3, select = c(t66800d, t66800i)),
                                   na.rm = TRUE),
                big_ope = rowMeans(subset(data3, select = c(t66800e, t66800j)),
                                   na.rm = TRUE))

# femola
# evaluate factor structure

# motivation fpr choosing teacher education (intrinsic, extrinsic)
# specify the model
cfa_model_femola <- ' f_int_edi =~ tg61031 + tg61032 + tg61033
                      f_int_ssi =~ tg61061 + tg61062 + tg61063
                      f_int_abi =~ tg61041 + tg61042 + tg61043
                      f_ext_uti =~ tg61021 + tg61022 + tg61023 + 
                                   tg61011 + tg61012 + tg61013
                      f_ext_lod =~ tg61071 + tg61072 + tg61073 
                      f_ext_soi =~ tg61051 + tg61052 + tg61053 '

# fit the model
fit_cfa_femola <- lavaan::cfa(cfa_model_femola, data = data4)

# evaluate the model
summary(fit_cfa_femola, standardized = TRUE)

# evaluate the model fit
fit_mea_femola <- lavaan::fitMeasures(fit_cfa_femola, c("chisq", "df", "pvalue", "cfi", "srmr", "rmsea"))

# evaluate factor loadings
inspect_fit_cfa_femola <- lavaan::inspect(fit_cfa_femola, what = "std")
inspect_fit_cfa_femola$lambda

# delete the lowest loading item in the scale with the most items (tg61021)
# specify the model
cfa_model_femola2 <- ' f_int_edi =~ tg61031 + tg61032 + tg61033
                       f_int_ssi =~ tg61061 + tg61062 + tg61063
                       f_int_abi =~ tg61041 + tg61042 + tg61043
                       f_ext_uti =~ tg61022 + tg61023 + 
                                    tg61011 + tg61012 + tg61013
                       f_ext_lod =~ tg61071 + tg61072 + tg61073 
                       f_ext_soi =~ tg61051 + tg61052 + tg61053 '

# fit the model
fit_cfa_femola2 <- lavaan::cfa(cfa_model_femola2, data = data4)

# evaluate the model
summary(fit_cfa_femola2, standardized = TRUE)

# evaluate the model fit
fit_mea_femola2 <- lavaan::fitMeasures(fit_cfa_femola2, c("chisq", "df", "pvalue", "cfi", "srmr", "rmsea"))

# add scales to data set

data4 <- data4 %>% 
  dplyr::mutate(int_edi = rowMeans(subset(data4, select = c(tg61031, tg61032,   # educational interest
                                                            tg61033)),
                                   na.rm = TRUE),
                int_ssi = rowMeans(subset(data4, select = c(tg61061, tg61062,   # subject specific interest
                                                            tg61063)),
                                   na.rm = TRUE),
                int_abi = rowMeans(subset(data4, select = c(tg61041, tg61042,   # ability beliefs
                                                            tg61043)),
                                   na.rm = TRUE),
                
                ext_uti = rowMeans(subset(data4, select = c(tg61022, tg61023,   # utility (time for family/ leisure, financial security)
                                                            tg61011, tg61012,
                                                            tg61013)),
                                   na.rm = TRUE),
                ext_lod = rowMeans(subset(data4, select = c(tg61071, tg61072,   # low-difficulty degree programme
                                                            tg61073)),
                                   na.rm = TRUE),
                ext_soi = rowMeans(subset(data4, select = c(tg61051, tg61052,   # social influences
                                                            tg61053)),
                                   na.rm = TRUE))


## current study situation

# social and academic integration (as in Franz & Paetsch, 2023)
# recode inverse items integration:
data4$tg53231 <- 6 - data4$tg53231

data5 <- data4 %>% 
  dplyr::mutate(str_aca_int = rowMeans(subset(data4, select = c(tg53211, tg53212, 
                                                                tg53213)),
                                       na.rm = TRUE),
                nor_aca_int = rowMeans(subset(data4, select = c(tg53231, tg53233, 
                                                                tg53235)),
                                       na.rm = TRUE),
                pee_soc_int = rowMeans(subset(data4, select = c(tg53121, tg53122, 
                                                                tg53123)),
                                       na.rm = TRUE),
                fac_soc_int = rowMeans(subset(data4, select = c(tg53111, tg53112, 
                                                                tg53113, tg53114)),
                                       na.rm = TRUE)) %>% 
  
  dplyr::mutate(str_aca_int = if_else(still_teaedu != 1, as.numeric(NA), str_aca_int),
                nor_aca_int = if_else(still_teaedu != 1, as.numeric(NA), nor_aca_int),
                pee_soc_int = if_else(still_teaedu != 1, as.numeric(NA), pee_soc_int),
                fac_soc_int = if_else(still_teaedu != 1, as.numeric(NA), fac_soc_int))  
# NA if not enrolled in a teacher education program at wave 2


## decision

# drop out (using study states, defined above)

#dropout intention
data6 <- data5 %>%
  dplyr::mutate(dro_int = tg53221) 
  
data6 <- data6 %>% 
  dplyr::mutate(across(everything(), ~ ifelse(is.nan(.x), NA, .x)))
# replace NaN with NA


#### ----------------------------- (5) Output ----------------------------- ####

data7 <- data6 %>%
  dplyr::mutate(dro_out = as.numeric(dro_out)) %>%
  dplyr::select(ID_t, big_ope, big_con, big_ext, big_agr, big_neu,
                int_edi, int_ssi, int_abi, ext_uti, ext_lod, ext_soi,
                aca_abi, par_edu, hisei, 
                str_aca_int, nor_aca_int, pee_soc_int, fac_soc_int, 
                age, gender, dro_out)

# for MPlus import
write.table(data7,
            file = "Data_Gen/data_lca_doomedtofail.csv",
            sep = " ",
            row.names = FALSE,
            col.names = FALSE,
            quote = FALSE,
            na = "-99")

# save data6
data_doomedtofail <- data6
save(data_doomedtofail, file = "Data_Gen/data_doomedtofail.Rdata")
