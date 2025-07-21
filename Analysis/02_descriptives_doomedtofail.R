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
#           (3) Correlation Table
#           (4) ANOVA

####  ------------------------- (1) Load Packages -------------------------  ####
library(tidyverse)
library(RColorBrewer)
library(ggplot2)


####  ----------------------- (2) Read MPlus Results ----------------------- ####

# MPlus import
nclass <- 3
data_lca <- read.table(paste0("Data_Gen/LCA_Results/lca_cprob_3class.dat"), na.strings = "9999.000", sep = "", header = FALSE,
                       col.names = c("big_ope", "big_con", "big_ext", "big_agr", "big_neu",
                                     "int_edi", "int_ssi", "int_abi", "ext_uti", "ext_lod", "ext_soi",
                                     "aca_abi", "par_edu", "hisei",
                                     "str_aca_int", "nor_aca_int", "pee_soc_int", "fac_soc_int", 
                                     "age", "gender", "dro_out",
                                     paste0("cprob", 1:nclass),
                                     "class",
                                     "ID_t")) %>%
  dplyr::select(ID_t, class) 

# full dataset import (data_doomedtofail)
load(file = "Data_Gen/data_doomedtofail.Rdata")

data <- merge(data_lca, data_doomedtofail, by = c("ID_t"), all.x = FALSE) 


####  --------------------------- (3) Correlation Table --------------------------- ####

