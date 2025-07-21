# 03_classplot_doomedtofail.R
#
# Comment: 
#
# Input: lca_cprob_3class.dat
#        data_doomedtofail.Rda
# Output: lca_plot_3class.png
#
# Contents: (1) Load Packages
#           (2) Read MPlus Results
#           (3) Plot Data
#           (4) Output

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


####  --------------------------- (3) Plot Data --------------------------- ####

plot_data <- data %>% 
             pivot_longer(c(big_ope, big_con, big_ext, big_agr, big_neu,
                            int_edi, int_ssi, int_abi, ext_uti, ext_lod, ext_soi,
                            aca_abi, par_edu, hisei),
                          names_to = "measure", 
                          values_to = "value",
                          values_transform = as.numeric)

plot_data$measure <- as.factor(plot_data$measure)
plot_data$measure <- recode(plot_data$measure,
                            big_ope = "Openness",
                            big_con = "Conscientiousness",
                            big_ext = "Extraversion",
                            big_agr = "Agreeableness",
                            big_neu = "Neuroticism",
                            int_edi = "Educational Interest",
                            int_ssi = "Subject Specific Interest",
                            int_abi = "Ability Beliefs",
                            ext_uti = "Utility",
                            ext_lod = "Low Difficulty",
                            ext_soi = "Social Influences",
                            aca_abi = "School Leaving Grade",
                            par_edu = "Parental Education",
                            hisei = "HISEI")

plot_data$var_type <- ifelse(plot_data$measure == "Parental Education", "categorical",
                      ifelse(plot_data$measure == "Openness" | 
                               plot_data$measure == "Conscientiousness"|
                               plot_data$measure == "Extraversion" | 
                               plot_data$measure == "Agreeableness" | 
                               plot_data$measure == "Neuroticism" |
                               plot_data$measure == "Educational Interest" |
                               plot_data$measure == "Subject Specific Interest" |
                               plot_data$measure == "Ability Beliefs" |
                               plot_data$measure == "Utility" |
                               plot_data$measure == "Low Difficulty" |
                               plot_data$measure == "Social Influences" |
                               plot_data$measure == "School Leaving Grade" |
                               plot_data$measure == "HISEI", "continuous", NA))

plot_data_continuous <- plot_data %>%
                        filter(var_type == "continuous") %>%
                        group_by(measure) %>%
                        mutate(z = scale(value)) %>%
                        ungroup()

plot_continuous <- ggplot(plot_data_continuous, aes(x = measure, y = z, 
                                                    color = class,
                                                    group = class)) +
                   stat_summary(geom = "point", fun = mean, size = 3) +
                   stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
                   stat_summary(fun = mean, geom = "line", linewidth  = 0.5) +
                   guides(x =  guide_axis(angle = 45))

prob_categorical <- par_edu_probs_mplus <- tribble(   # 1 = no parent tertiary education
  ~class, ~par_edu, ~probability,
  1,      1,      0.613,
  2,      1,      0.861,
  3,      1,      0.254,
)

df_prob_categorical <- as.data.frame(prob_categorical)

df_prob_categorical$class <- as.factor(df_prob_categorical$class)
levels(df_prob_categorical$class) <- c(1, 2, 3)

plot_categorical <- ggplot(df_prob_categorical, aes(x = class, y = probability, 
                                                    color = class, group = class)) +
                    stat_summary(geom = "point", fun = mean, size = 3) +
                    stat_summary(fun = mean) +
                    guides(x =  guide_axis(angle = 45))

df_plot_both1 <- as.data.frame(plot_data_continuous)
df_plot_both1 <- df_plot_both1 %>% 
                 dplyr::select(measure, class, z) %>% 
                 dplyr::rename(value = z)

df_plot_both2 <- df_prob_categorical %>%
                 filter(par_edu == 1) %>%
                 dplyr::mutate(measure = "Parental Education") %>%
                 dplyr::select(measure, class, value = probability)

df_plot_both1$class <- as.factor(df_plot_both1$class)
df_plot_both <- bind_rows(df_plot_both1, df_plot_both2)

plot_both <- ggplot(df_plot_both, aes(x = measure, y = value, 
                                      color  = class, group = class)) +
             stat_summary(geom = "point", fun = mean, size = 3) +
             stat_summary(fun = mean, geom = "line", linewidth = 1) +
            # stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2) +
             guides(x =  guide_axis(angle = 45)) +
             
             scale_x_discrete(limits=c("Openness",
                                       "Conscientiousness",
                                       "Extraversion",
                                       "Agreeableness",
                                       "Neuroticism",
                                       "Educational Interest",
                                       "Subject Specific Interest",
                                       "Ability Beliefs",
                                       "Utility",
                                       "Low Difficulty",
                                       "Social Influences",
                                       "School Leaving Grade",
                                       "HISEI",
                                       "Parental Education")) +
  
             expand_limits(y = c(-1, 1)) +
             scale_y_continuous(
               name = "Class mean (z-standardized) \n",
               sec.axis = sec_axis(trans = ~ . *1,
                                   name = "Conditional Probabilities of  \n being a First-Generation-Student\n",
                                   breaks = seq(0, 1, by = 0.25),)) +
             geom_hline(yintercept = 0, linetype = "dashed", 
             color = "grey", size = 0.5) +
             xlab("")

# change theme and appearance

plot_both <- plot_both + 
  scale_color_brewer(palette = "Set2") +
  labs(color = "Class") +
  theme_minimal(base_size = 11, base_family = "Arial") +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 11, family = "Arial"),
    legend.text = element_text(size = 11, family = "Arial"),
    axis.title.y.left = element_text(margin = margin(r = 10), size = 11, family = "Arial"),
    axis.title.y.right = element_text(margin = margin(l = 10), size = 11, family = "Arial"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11, family = "Arial"),
    axis.text.y = element_text(size = 11, family = "Arial"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  )


# dotted lines and labels between  conceptual block
plot_both <- plot_both +
  geom_vline(xintercept = c(5.5, 8.5, 11.5, 12.5), linetype = "dotted", color = "gray59", size = 0.8) +
  annotate("text", x = 3, y = 2, label = "Big 5", size = 4, family = "Arial") +
  annotate("text", x = 7, y = 2, label = "Intrinsic Motives", size = 4, family = "Arial") +
  annotate("text", x = 10, y = 2, label = "Extrinsic Motives", size = 4, family = "Arial") +
  annotate("text", x = 12, y = 2, label = "Academic \n Ability", size = 4, family = "Arial") +
  annotate("text", x = 13.5, y = 2, label = "Background", size = 4, family = "Arial") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", 
                                 linewidth = 0.7, 
                                 linetype = "solid"),
        axis.ticks = element_line(color = "black"))

####  ----------------------------- (4) Output ----------------------------- ####

ggsave("Data_Gen/LCA_Results/lca_plot_3class.png", plot = plot_both, width = 10, height = 6, dpi = 300)
