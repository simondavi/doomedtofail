# 03_classplot_doomedtofail.R
#
# Comment: 
#
# Input: 3_class.out
# Output: lca_plot_3class.png
#
# Contents: (1) Load Packages
#           (2) Read MPlus Output
#           (3) Plot Data
#           (4) Output

####  ------------------------- (1) Load Packages -------------------------  ####
library(tidyverse)
library(RColorBrewer)
library(MplusAutomation)


####  ----------------------- (2) Read MPlus Output ----------------------- ####
out <- MplusAutomation::readModels("Analysis/LCA/3_class.out")

plot_data_con <- out$parameters$unstandardized %>%
  dplyr::filter(paramHeader == "Means",
                param %in% c("BIG_OPE",
                             "BIG_CON",
                             "BIG_EXT",
                             "BIG_AGR",
                             "BIG_NEU",
                             "INT_EDI",
                             "INT_SSI",
                             "INT_ABI",
                             "EXT_UTI",
                             "EXT_LOD",
                             "EXT_SOI",
                             "ACA_ABI",
                             "HISEI" ))%>%
  dplyr::select(param, LatentClass, est)

plot_data_cat <- out$parameters$probability.scale %>%
  dplyr::filter(category == 1) %>%
  dplyr::mutate(est = est - out$sampstat$proportions.counts[1,3]) %>%
  dplyr::select(param, LatentClass, est)

plot_data <- dplyr::bind_rows(plot_data_con, plot_data_cat)

plot_data <- plot_data %>%
  dplyr::mutate(param_label = recode(param,
                                     BIG_OPE = "Openness",
                                     BIG_CON = "Conscientiousness",
                                     BIG_EXT = "Extraversion",
                                     BIG_AGR = "Agreeableness",
                                     BIG_NEU = "Neuroticism",
                                     INT_EDI = "Educational interest",
                                     INT_SSI = "Subject specific interest",
                                     INT_ABI = "Ability beliefs",
                                     EXT_UTI = "Utility",
                                     EXT_LOD = "Low difficulty",
                                     EXT_SOI = "Social influences",
                                     ACA_ABI = "School leaving grade",
                                     PAR_EDU = "Parental education*",
                                     HISEI = "HISEI"),
                Class = paste("Class", LatentClass))

plot_data$Class <- factor(plot_data$Class,
                         labels = c("1", "2", "3"))


####  --------------------------- (3) Plot Data --------------------------- ####
plot_lca <- ggplot(plot_data,
                   aes(x = param_label, y = est,
                       color = Class,
                       group = Class))

# dotted lines and labels between conceptual blocks
plot_lca <- plot_lca +
  geom_vline(xintercept = c(5.5, 11.3, 12.6), linetype = "dotted", color = "gray50", size = 0.8) +
  annotate("text", x = 3, y = 2, label = "Personality", size = 4, family = "Arial") +
  annotate("text", x = 8.5, y = 2, label = "Motivations for choosing\nteacher education", size = 4, family = "Arial") +
  annotate("text", x = 12, y = 2, label = "Academic \naptitude ", size = 4, family = "Arial") +
  annotate("text", x = 13.5, y = 2, label = "Background", size = 4, family = "Arial")
  
# actual plot
plot_lca <- plot_lca +
  geom_hline(yintercept = 0, linetype = "solid", 
             color = "gray80", size = 0.4) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 4) +
  scale_y_continuous(limits = c(-1, 2),
                     name = "Class mean (z-standardized)\n",
                     sec.axis = sec_axis(transform = ~ . * 1,
                                         name = "Deviation from total sample proportion\nof first-generation students\n",
                                         breaks = seq(-0.5, 0.5, by = 0.25),
                                         labels = scales::percent_format(accuracy = 1))) +
  xlab("") +
  guides(x =  guide_axis(angle = 45)) +
  scale_x_discrete(limits=c("Openness",
                            "Conscientiousness",
                            "Extraversion",
                            "Agreeableness",
                            "Neuroticism",
                            "Educational interest",
                            "Subject specific interest",
                            "Ability beliefs",
                            "Utility",
                            "Low difficulty",
                            "Social influences",
                            "School leaving grade",
                            "HISEI",
                            "Parental education*"))

# change theme and appearance
plot_lca <- plot_lca + 
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 11, base_family = "Arial") +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 11, family = "Arial"),
    legend.text = element_text(size = 11, family = "Arial"),
    axis.title.y = element_text(size = 11, family = "Arial", 
                                margin = margin(r = 10)),
    axis.text.y = element_text(size = 11, family = "Arial", colour = "black"),
    axis.title.x = element_text(size = 11, family = "Arial", 
                                margin = margin(b = 10)),
    axis.text.x = element_text(size = 11, family = "Arial", colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", 
                             linewidth = 0.7, 
                             linetype = "solid"),
    axis.ticks = element_line(color = "black")
  )


####  ----------------------------- (4) Output ----------------------------- ####

ggsave("Analysis/LCA/lca_plot_3class.png", plot = plot_lca, width = 10, height = 6, dpi = 600)
