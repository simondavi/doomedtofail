# mplus import

nprofiles <- 3
bchweights <- read.table(paste0("Data_Gen/LCA_Results/lca_bch_3class.dat"), na.strings = "9999.000", sep = "", header = FALSE,
                         col.names = c("big_ope", "big_con", "big_ext", "big_agr", "big_neu",
                                       "int_edi", "int_ssi", "int_abi", "ext_uti", "ext_lod", "ext_soi",
                                        "aca_abi", "par_edu", "hisei",
                                        "aca_int", "soc_int",
                                        "age", "gender", "dro_out",
                                      paste0("BCHW", 1:nprofiles),
                                      "ID_t"))

getMostLikelyMembership <- function (df) {
  apply(df,1,function(r) {
    out <- which(r == max(r))
    stopifnot(length(out) == 1)
    out <- as.character(out)
    return(out)
  })
}

bchweights$class <- getMostLikelyMembership(bchweights[,paste0("BCHW", 1:nprofiles)])

#####

library(dplyr)
library(tidyr)
library(ggplot2)

# Define continuous variables (replace with your exact variable names)
cont_vars <- c("big_ope", "big_con", "big_ext", "big_agr", "big_neu",
               "int_edi", "int_ssi", "int_abi", "ext_uti", "ext_lod", "ext_soi",
               "aca_abi", "par_edu", "hisei")

# Convert continuous vars to numeric (suppress warnings here)
bchweights <- bchweights %>%
  mutate(across(all_of(cont_vars), ~ as.numeric(.)))

# Check for NAs introduced during conversion
na_counts <- bchweights %>%
  summarise(across(all_of(cont_vars), ~ sum(is.na(.))))
print(na_counts)

# Calculate mean of continuous vars by class
cont_summary <- bchweights %>%
  group_by(class) %>%
  summarise(across(all_of(cont_vars), mean, na.rm = TRUE)) %>%
  pivot_longer(-class, names_to = "variable", values_to = "mean_value")

# Optional: order variables for nicer x-axis
cont_summary$variable <- factor(cont_summary$variable, levels = cont_vars)

# Plot means by class and variable
ggplot(cont_summary, aes(x = variable, y = mean_value, color = class, group = class)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(
    title = "Mean of Continuous Indicators by Latent Class",
    x = "Indicator Variable",
    y = "Mean (z-standardized)",
    color = "Class"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#############


