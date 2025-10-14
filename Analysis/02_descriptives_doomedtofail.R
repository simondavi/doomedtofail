# 02_descriptives_doomedtofail.R
#
# Comment: 
#
# Input: lca_cprob_3class.dat
#        data_doomedtofail.Rdata
# Output:
#
# Contents: (1) Load Packages
#           (2) Read Data
#           (3) Normality test 
#           (4) Correlation table

####  ------------------------- (1) Load Packages -------------------------  ####
library(tidyverse)
library(corrtable)
library(psych)
library(finalfit)


#### ---------------------------- (2) Read Data ---------------------------- ####

# full dataset import (data_doomedtofail)
load(file = "Data_Gen/data_doomedtofail.Rdata")

#### ------------------------- (3) Normality test  ------------------------- ####
# Normality test 
# Mardia test (skewness and kurtosis) for multivariate normality 
data_mardia <- data %>%
  dplyr::select(-par_edu, -dro_out) %>%
  dplyr::select(big_ope, big_con, big_ext, big_agr, big_neu,
                int_edi, int_ssi, int_abi, ext_uti, ext_lod, ext_soi,
                aca_abi,
                hisei,
                str_aca_int, nor_aca_int, pee_soc_int, fac_soc_int) %>%
  na.omit(data_mardia)

mardia(data_mardia)
# data is not normal distributed

# Q-Q-Plot
data_qq <- data_mardia %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) 

ggplot(data_qq, aes(sample = value)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Q-Q-Plot per variable")


#### ------------------------ (4) Correlation table ------------------------ ####

data_cor <- data %>%
  dplyr::mutate(
    par_edu1 = if_else(par_edu == 1, 1, 0),
    par_edu2 = if_else(par_edu == 2, 1, 0),
    par_edu3 = if_else(par_edu == 3, 1, 0)) %>%
  dplyr::select(big_ope, big_con, big_ext, big_agr, big_neu,
                int_edi, int_ssi, int_abi, ext_uti, ext_lod, ext_soi,
                aca_abi, 
                par_edu, par_edu1, par_edu2, par_edu3, hisei,
                dro_out,
                str_aca_int, nor_aca_int, pee_soc_int, fac_soc_int)

cor_tab <- corrtable::correlation_matrix(
  data_cor,
  type = "spearman",
  digits = 2,
  decimal.mark = ".",
  use = "all",
  show_significance = TRUE,
  replace_diagonal = FALSE,
  replacement = ""
)

cor_tab <- cor_tab[,18:22]

# Descriptive statistics
data_des <- data_cor %>%
  dplyr::mutate(
    par_edu = as.factor(par_edu),
    dro_out = as.factor(dro_out))

finalfit::ff_glimpse(data_des, digits = 2)