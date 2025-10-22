# 06_padjustintegration_doomedtofail.R
#
# Comment: 
#
# Input: mi_model2.out
# Output: 
#
# Contents: (1) Load Packages
#           (2) Read MPlus Output
#           (3) Adjust P-values

#### ------------------------- (1) Load Packages -------------------------  ####
library(dplyr)
library(MplusAutomation)


#### ----------------------- (2) Read MPlus Output ------------------------ ####
out <- readModels("Analysis/Auxiliary_Models/mi_model2.out")

out_pair <- out$parameters$unstandardized
out_pair <- subset(out_pair, paramHeader == "New.Additional.Parameters")


#### ------------------------ (3) Adjust P-values ------------------------- ####
out_pair$pval_adj <- p.adjust(out_pair$pval, method = "holm")
out_pair[, c("param", "est", "pval", "pval_adj")]