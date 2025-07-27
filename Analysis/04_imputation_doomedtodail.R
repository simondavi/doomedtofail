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
  dplyr::mutate(dro_out = as.factor(dro_out))

# predictor matrix
predictormatrix <- mice::quickpred(data_imp, 
                                   include = c("dro_int"),
                                   exclude = c("bchw1", "bchw2", "bchw3", "dro_out", "ID_t"),
                                   mincor = 0.1) # default = 0.1
# imputation
imp_gen <- mice(data = data_imp,
                predictorMatrix = predictormatrix,
                m = 10,
                maxit = 5,            
                diagnostics = TRUE)

# save imputed data
miceadds::write.datlist(
  datlist = imp_gen,
  name = "Imp_Data_3class",
  Mplus = TRUE)


# evaluate dropout extend in imputed data

# extract the imputed datasets into a list of data frames
imp_dat_eval <- list() 
for (i in 1:imp_gen$m) imp_dat_eval[[i]] <- complete(imp_gen, action = i) 

# bring your imputed data in long format for estimating dropout
imp_dat_eval_long <- mice::complete(imp_gen, "long", inc = FALSE) 
do_imp <- (100/nrow(imp_dat_eval_long))*(as.data.frame(table(imp_dat_eval_long$dro_out))[2,2])

# ~11%