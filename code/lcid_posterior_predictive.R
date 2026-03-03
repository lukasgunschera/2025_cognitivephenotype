## ======================================================================================================================= ##
## Script:    POSTERIOR PREDICTIVE CHECK
## ======================================================================================================================= ##
## Authors:   Lukas J. Gunschera
## Date:      Mon Jul 29 13:58:29 2024
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

## SETUP ====================================================================================================================

library(renv)
renv::restore()

set.seed(777)

# set global parameters determining the model fitting process

SIMULATE_DATA <- FALSE # should data be simulated or a previous simulation loaded
FIT_CLUSTER <- FALSE # should the fitting be run on a cluster or locally
FIT_MODEL <- FALSE # should the fitting be run or loaded from file
N_SAMPLE <- 10000 # number of participants to simulate

# load required packages
library(loo)
library(here)
library(knitr)
library(purrr)
library(readr)
library(haven)
library(tidyr)
library(dplyr)
library(hexbin)
library(stringr)
library(ggplot2)
library(ggpmisc)
library(viridis)
library(magrittr)
library(cmdstanr)
library(hBayesDM)
library(bayesplot)
library(patchwork)
library(ggcorrplot)
library(viridisLite)

here::i_am("renv.lock") # set directory

source(here("code", "functions", "fun_plots.R"))
source(here("code", "functions", "fun_helper.R"))

#### Load data --------------------------------------------------------------------------------------------------------------
ppc02 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_ppc_02.RDS"))
ppc03 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_ppc_03.RDS"))
ppc04 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_ppc_04.RDS"))
ppc05 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_ppc_05.RDS"))
ppc06 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_ppc_06.RDS"))

# CORRELATIONS --------------------------------------------------------------------------------------------------------------
# Correlate posterior predictions with observed data

ppc02_corr <- ppc_correlate(ppc02, " Wave 2")
ppc03_corr <- ppc_correlate(ppc03, " Wave 3")
ppc04_corr <- ppc_correlate(ppc04, " Wave 4")
ppc05_corr <- ppc_correlate(ppc05, " Wave 5")
ppc06_corr <- ppc_correlate(ppc06, " Wave 6")

print(cor.test(ppc02$mean_real_choice, ppc02$mean_pred_choice), digits = 7)
print(cor.test(ppc03$mean_real_choice, ppc03$mean_pred_choice), digits = 7)
print(cor.test(ppc04$mean_real_choice, ppc04$mean_pred_choice), digits = 7)
print(cor.test(ppc05$mean_real_choice, ppc05$mean_pred_choice), digits = 7)
print(cor.test(ppc06$mean_real_choice, ppc06$mean_pred_choice), digits = 7)

# Combine the results into a single table
ppc_table <- bind_rows(ppc02_corr, ppc03_corr, ppc04_corr, ppc05_corr, ppc06_corr) %>%
  knitr::kable(., caption = "Correlations of predicted and real choices", align = "l", digits = 3)

## Visualise Posterior Predictive Checks -------------------------------------------------------------------------------------
ppcplot2 <- ppc02 %>%
  dplyr::mutate(real_delay_later = factor(real_delay_later)) %>%
  ggplot(aes(x = mean_real_choice, y = mean_pred_choice, colour = real_delay_later)) +
  geom_point(shape = 16, alpha = .75, position = position_jitter(width = .05), size = 2) +
  geom_smooth(method = "lm", color = "#2E2E2E", fullrange = TRUE) +
  plot_theme +
  aspect_ratio_square +
  scale_colour_viridis_d(begin = .1, end = .8, option = "C", direction = -1) +
  scale_x_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.025)) +
  scale_y_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.05)) +
  labs(x = "Observed", y = "Predicted") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = 0, xend = 1, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

ppcplot2_marg <- ggMarginal(
  ppcplot2,
  type = "density",
  groupColour = TRUE,  # Keep color grouping
  groupFill = TRUE,    # Keep fill grouping
  alpha = 0.5,
  size = 3             # Adjust size of marginal plots
)

ppcplot3 <- ppc03 %>%
  dplyr::mutate(real_delay_later = factor(real_delay_later)) %>%
  ggplot(aes(x = mean_real_choice, y = mean_pred_choice, colour = real_delay_later)) +
  geom_point(shape = 16, alpha = .75, position = position_jitter(width = .05), size = 2) +
  geom_smooth(method = "lm", color = "#2E2E2E", fullrange = TRUE) +
  plot_theme +
  aspect_ratio_square +
  scale_colour_viridis_d(begin = .1, end = .8, option = "C", direction = -1) +
  scale_x_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.025)) +
  scale_y_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.05)) +
  labs(x = "Observed", y = "Predicted") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = 0, xend = 1, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

ppcplot3_marg <- ggMarginal(
  ppcplot3,
  type = "density",
  groupColour = TRUE,  # Keep color grouping
  groupFill = TRUE,    # Keep fill grouping
  alpha = 0.5,
  size = 3             # Adjust size of marginal plots
)

ppcplot4 <- ppc04 %>%
  dplyr::mutate(real_delay_later = factor(real_delay_later)) %>%
  ggplot(aes(x = mean_real_choice, y = mean_pred_choice, colour = real_delay_later)) +
  geom_point(shape = 16, alpha = .75, position = position_jitter(width = .05), size = 2) +
  geom_smooth(method = "lm", color = "#2E2E2E", fullrange = TRUE) +
  plot_theme +
  aspect_ratio_square +
  scale_colour_viridis_d(begin = .1, end = .8, option = "C", direction = -1) +
  scale_x_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.025)) +
  scale_y_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.05)) +
  labs(x = "Observed", y = "Predicted") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = 0, xend = 1, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

ppcplot4_marg <- ggMarginal(
  ppcplot4,
  type = "density",
  groupColour = TRUE,  # Keep color grouping
  groupFill = TRUE,    # Keep fill grouping
  alpha = 0.5,
  size = 3             # Adjust size of marginal plots
)

ppcplot5 <- ppc05 %>%
  dplyr::mutate(real_delay_later = factor(real_delay_later)) %>%
  ggplot(aes(x = mean_real_choice, y = mean_pred_choice, colour = real_delay_later)) +
  geom_point(shape = 16, alpha = .75, position = position_jitter(width = .05), size = 2) +
  geom_smooth(method = "lm", color = "#2E2E2E", fullrange = TRUE) +
  plot_theme +
  aspect_ratio_square +
  scale_colour_viridis_d(begin = .1, end = .8, option = "C", direction = -1) +
  scale_x_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.025)) +
  scale_y_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.05)) +
  labs(x = "Observed", y = "Predicted") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = 0, xend = 1, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

ppcplot5_marg <- ggMarginal(
  ppcplot5,
  type = "density",
  groupColour = TRUE,  # Keep color grouping
  groupFill = TRUE,    # Keep fill grouping
  alpha = 0.5,
  size = 3             # Adjust size of marginal plots
)


ppcplot6 <- ppc06 %>%
  dplyr::mutate(real_delay_later = factor(real_delay_later)) %>%
  ggplot(aes(x = mean_real_choice, y = mean_pred_choice, colour = real_delay_later)) +
  geom_point(shape = 16, alpha = .75, position = position_jitter(width = .05), size = 2) +
  geom_smooth(method = "lm", color = "#2E2E2E", fullrange = TRUE) +
  plot_theme +
  aspect_ratio_square +
  scale_colour_viridis_d(begin = .1, end = .8, option = "C", direction = -1) +
  scale_x_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.025)) +
  scale_y_continuous(breaks = seq(0, 1, .5), expand = c(0.05, 0.05)) +
  labs(x = "Observed", y = "Predicted") +
  annotate(x = -Inf, xend = -Inf, y = 0, yend = 1, colour = "#2E2E2E", lwd = 0.75, geom = "segment") +
  annotate(x = 0, xend = 1, y = -Inf, yend = -Inf, colour = "#2E2E2E", lwd = 0.75, geom = "segment")

ppcplot6_marg <- ggMarginal(
  ppcplot6,
  type = "density",
  groupColour = TRUE,  # Keep color grouping
  groupFill = TRUE,    # Keep fill grouping
  alpha = 0.5,
  size = 3             # Adjust size of marginal plots
)


# create layout of mergerd plot
layout <- c(
  area(1, 1),
  area(1, 2),
  area(1, 3),
  area(2, 1),
  area(2, 2)
)




# Create a function to add panel labels
add_panel_label <- function(plot, label, x_pos = 0.02, y_pos = 0.98,
                            size = 12, fontface = "bold", color = "black") {
  # ggMarginal returns a special class, so we need to handle it differently
  # We'll add the label to the main ggplot before passing to ggMarginal
  return(plot)
}


# Now combine them using grid.arrange
library(gridExtra)
library(ggplot2)

# First, create a separate plot just for the legend
legend_plot <- ppc06 %>%
  dplyr::mutate(real_delay_later = factor(real_delay_later)) %>%
  ggplot(aes(x = mean_real_choice, y = mean_pred_choice, colour = real_delay_later)) +
  geom_point() +
  scale_colour_viridis_d(begin = .1, end = .8, option = "C", direction = -1, name = "Delay level") +
  plot_theme +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.direction = "horizontal")

# Extract the legend
legend_grob <- get_legend(legend_plot)

# Create the main grid of plots
main_plots <- grid.arrange(
  ppcplot2_marg, ppcplot3_marg,
  ppcplot4_marg, ppcplot5_marg,
  ppcplot6_marg,
  ncol = 3,
  nrow = 2
)

# Combine main plots with legend at the bottom
ppc_plots_all <- grid.arrange(
  main_plots,
  legend_grob,
  nrow = 2,
  heights = c(0.95, 0.05)  # Adjust these values to control space allocation
)

# Alternative: If you want a 2x3 grid with all plots aligned
# Create an empty plot for the 6th position
empty_plot <- ggplot() +
  theme_void()

# Arrange in 2x3 grid
ppc_plots_all <- grid.arrange(
  ppcplot2_marg, ppcplot3_marg,
  ppcplot4_marg, ppcplot5_marg,
  ppcplot6_marg, empty_plot,
  ncol = 3,
  nrow = 2,
  bottom = legend_grob
)

# Alternative: Using patchwork for better control with panel labels
if (!require(patchwork)) install.packages("patchwork")
library(patchwork)

# Since ggMarginal objects don't work directly with patchwork,
# we need to use grid.arrange or arrange them differently

# OPTION: Add panel labels during grid arrangement
# Create a function to add panel label to a grob
add_panel_to_grob <- function(grob, label) {
  # Create a text grob for the label
  label_grob <- textGrob(label,
                         x = unit(0.05, "npc"),
                         y = unit(0.95, "npc"),
                         hjust = 0, vjust = 1,
                         gp = gpar(fontsize = 14, fontface = "bold"))

  # Arrange the plot and label
  g <- arrangeGrob(grob,
                   top = label_grob,
                   padding = unit(0, "line"))
  return(g)
}

# Add panel labels to each plot grob
ppcplot2_marg_labeled <- add_panel_to_grob(ppcplot2_marg, "A")
ppcplot3_marg_labeled <- add_panel_to_grob(ppcplot3_marg, "B")
ppcplot4_marg_labeled <- add_panel_to_grob(ppcplot4_marg, "C")
ppcplot5_marg_labeled <- add_panel_to_grob(ppcplot5_marg, "D")
ppcplot6_marg_labeled <- add_panel_to_grob(ppcplot6_marg, "E")


# For 3-column layout with 2 rows
ppc_plots_all_3col <- grid.arrange(
  ppcplot2_marg_labeled, ppcplot3_marg_labeled, ppcplot4_marg_labeled,
  ppcplot5_marg_labeled, ppcplot6_marg_labeled, empty_plot,
  ncol = 3,
  nrow = 2,
  bottom = legend_grob
)

# Save with appropriate dimensions
ggsave(ppc_plots_all_3col,
       path = here::here("output", "images", "modelfit"),
       filename = "ppc_plots_panels.png",
       width = 12,    # Wider for 3 columns
       height = 8,    # Adjusted height
       dpi = 600,
       device = "png"
)
