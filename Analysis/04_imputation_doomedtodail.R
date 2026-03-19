# 04_imputation_doomedtofail.R
#
# Comment: 
#
# Input: lca_bch_3class.da
# Output: imp_data_3class
#
# Contents: (1) Load Packages
#           (2) Imputation

####  ------------------------- (1) Load Packages -------------------------  ####
library(mice)
library(miceadds)
library(tidyverse)

####  --------------------------- (2) Imputation --------------------------- ####
set.seed(123)

# MPlus import
nclass <- 3
data_imp <- read.table(paste0("Data_Gen/LCA_Results/lca_bch_3class.dat"), na.strings = "9999.000", sep = "", header = FALSE,
                       col.names = c("par_edu",
                                     "big_ope", "big_con", "big_ext", "big_agr", "big_neu",
                                     "int_edi", "int_ssi", "int_abi", "ext_uti", "ext_lod", "ext_soi",
                                     "aca_abi",
                                     "hisei",
                                     "str_aca_int", "nor_aca_int", "pee_soc_int", "fac_soc_int", 
                                     "dro_int", "dro_out",
                                     paste0("bchw", 1:nclass),
                                     "ID_t"))
# examine data
str(data_imp)

data_imp <- data_imp %>%
  dplyr::mutate(dro_out = as.factor(dro_out)) %>%
  dplyr::mutate(par_edu = as.factor(par_edu))

# predictor matrix
predictormatrix <- mice::quickpred(data_imp, 
                                   include = c("dro_int", "dro_out"),
                                   exclude = c("bchw1", "bchw2", "bchw3", "ID_t"),
                                   mincor = 0.1) # default = 0.1
# imputation
imp_gen <- mice(data = data_imp,
                predictorMatrix = predictormatrix,
                m = 20,
                maxit = 5,            
                diagnostics = TRUE)

# check methods
imp_gen$method

# check imputation
plot(imp_gen)

# dropout: original data set
prop.table(table(data_imp$dro_out, useNA = "no"))
# dropout: imputed data set
prop.table(table(complete(imp_gen, 1)$dro_out))

# relationship par_edu - dro_out: original data set
table(data_imp$par_edu, data_imp$dro_out)
# relationship par_edu - dro_out: imputed data set
table(
  complete(imp_gen, 1)$par_edu, 
  complete(imp_gen, 1)$dro_out)

# save imputed data
miceadds::write.datlist(
  datlist = imp_gen,
  name = "Imp_Data_3class",
  Mplus = TRUE)