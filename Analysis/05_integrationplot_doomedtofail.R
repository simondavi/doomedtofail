# 05_integrationplot_doomedtofail.R
#
# Comment: 
#
# Input: mi_model2.out
# Output: int_plot_3class.png
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
out <- MplusAutomation::readModels("Analysis/Auxiliary_Models/mi_model2.out")

means <- out$parameters$unstandardized %>%
  dplyr::filter(paramHeader == "Means",
                param %in% c("STR_ACA_IN", 
                             "NOR_ACA_IN", 
                             "PEE_SOC_IN", 
                             "FAC_SOC_IN"))

ci <- out$parameters$ci.unstandardized %>%
  dplyr::filter(paramHeader == "Means",
                param %in% c("STR_ACA_IN", 
                             "NOR_ACA_IN", 
                             "PEE_SOC_IN", 
                             "FAC_SOC_IN"))

means_ci <- means %>%
  left_join(ci %>% select(param, LatentClass, low2.5, up2.5),
            by = c("param", "LatentClass")) %>%
  dplyr::mutate(param_label = recode(param,
                                     STR_ACA_IN = "Structural academic",
                                     NOR_ACA_IN = "Normative academic",
                                     PEE_SOC_IN = "Peer social",
                                     FAC_SOC_IN = "Faculty social"),
                Class = paste("Class", LatentClass))

means_ci$Class <- factor(means_ci$Class,
                         labels = c("1", "2", "3"))


####  --------------------------- (3) Plot Data --------------------------- ####

plot_int <- ggplot(means_ci,
                   aes(x = param_label, y = est,
                       color = Class,
                       group = Class)) +
  geom_hline(yintercept = 0, linetype = "solid", 
             color = "gray80", size = 0.3) +
  geom_hline(yintercept = 0.5, linetype = "solid", 
             color = "gray80", size = 0.3) +
  geom_hline(yintercept = -0.5, linetype = "solid", 
             color = "gray80", size = 0.3) +
  geom_errorbar(aes(ymin = low2.5, ymax = up2.5), 
                width = 0.1, linewidth = 0.6, 
                position = position_dodge(width = 0.4)) +
  geom_point(size = 4, 
             position = position_dodge(width = 0.4)) +
  labs(x = "\n Integration", y = "Means (z-standardized) \n") + 
  scale_y_continuous(
    limits = c(-0.75, 0.5)) +
  scale_x_discrete(limits=c("Structural academic",
                            "Normative academic",
                            "Peer social",
                            "Faculty social"))

# change theme and appearance  
plot_int <- plot_int + 
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

ggsave("Analysis/Auxiliary_Models/int_plot_3class.png", plot = plot_int, 
       width = 8, height = 5, dpi = 600)