# 02_descriptives_doomedtofail.R
#
# Comment: 
#
# Input: lca_cprob_3class.dat
#        data_doomedtofail.Rda
# Output:
#
# Contents: (1) Load Packages
#           (2) Read MPlus Results
#           (3) Normality test 
#           (4) Correlation table
#           (5) ANOVA

####  ------------------------- (1) Load Packages -------------------------  ####
library(tidyverse)
library(corrtable)


####  ----------------------- (2) Read MPlus Results ----------------------- ####

# MPlus import
nclass <- 3
data_lca <- read.table(paste0("Data_Gen/LCA_Results/lca_cprob_3class.dat"), na.strings = "9999.000", sep = "", header = FALSE,
                       col.names = c("big_ope", "big_con", "big_ext", "big_agr", "big_neu",
                                     "int_edi", "int_ssi", "int_abi", "ext_uti", "ext_lod", "ext_soi",
                                     "aca_abi", 
                                     "par_edu", "hisei",
                                     "str_aca_int", "nor_aca_int", "pee_soc_int", "fac_soc_int", 
                                     "dro_int", "dro_out",
                                     paste0("cprob", 1:nclass),
                                     "class",
                                     "ID_t")) %>%
            dplyr::select(ID_t, class) # only keep class-assignment

# full dataset import (data_doomedtofail)
load(file = "Data_Gen/data_doomedtofail.Rdata")

data <- merge(data_lca, data_doomedtofail, by = c("ID_t"), all.x = FALSE) 


####  ------------------------- (3) Normality test  ------------------------- ####
# Normality test 
# Mardia test (skewness and kurtosis) for multivariate normality 
data_mardia <- data %>%
  dplyr::select(-par_edu, -hisei)

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


####  ----------------------- (4) Correlation table ----------------------- ####

data_cor <- data %>%
  dplyr::mutate(
    par_edu1 = if_else(par_edu == 1, 1, 0),
    par_edu2 = if_else(par_edu == 2, 1, 0),
    par_edu3 = if_else(par_edu == 3, 1, 0)) %>%
  dplyr::select(big_ope, big_con, big_ext, big_agr, big_neu,
                int_edi, int_ssi, int_abi, ext_uti, ext_lod, ext_soi,
                aca_abi, 
                par_edu1, par_edu2, par_edu3, hisei, 
                str_aca_int, nor_aca_int, pee_soc_int, fac_soc_int, 
                dro_out)

cor_tab <- corrtable::correlation_matrix(
  data_cor,
  type = "spearman",
  digits = 2,
  decimal.mark = ".",
  use = "upper",
  show_significance = TRUE,
  replace_diagonal = FALSE,
  replacement = ""
)

cor_tab <- cor_tab[,17:21]


####  ----------------------------- (5) ANOVA ----------------------------- ####


# check differences

Lerhamt
Fächer (Stem, ja/ nein) tg24170_g5
Age 
Gender

tg24202_g1,      # type of intended teaching degree
tg24170_g5,      # subject group


