## ======================================================================================================================= ##
## Script:    PUBLICATION FIGURES
## ======================================================================================================================= ##
## Authors:   Lukas J. Gunschera
## Date:      Tue Oct 14 07:59:16 2025
## ======================================================================================================================= ##
##
## ======================================================================================================================= ##

## Figure 1 =================================================================================================================

# install.packages("devtools")
devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(dplyr)
library(ggplot2)
library(viridis)

# define the categories
micro_nodes <- c("Striatum", "FPC", "dlPFC", "rIFC", "STN")
meso_nodes  <- c("Delay discounting", "Exploration", "Inhibitory control")
macro_nodes <- c("Age", "Gender")

# meso-micro weights
meso_micro <- tribble(
  ~meso,                ~micro,         ~weight,
  "Delay discounting",  "Striatum",     30,
  "Exploration",        "Striatum",     15,
  "Inhibitory control", "Striatum",     10,

  "Delay discounting",  "FPC",          0,
  "Exploration",        "FPC",          15,
  "Inhibitory control", "FPC",          0,

  "Delay discounting",  "dlPFC",        20,
  "Exploration",        "dlPFC",        10,
  "Inhibitory control", "dlPFC",        10,

  "Delay discounting",  "rIFC",         10,
  "Exploration",        "rIFC",         10,
  "Inhibitory control", "rIFC",         20,

  "Delay discounting",  "STN",          0,
  "Exploration",        "STN",          0,
  "Inhibitory control", "STN",          15
)

# equal weight for macro levels
macro_total <- 50
macro_meso <- tibble(
  macro = rep(macro_nodes, each = length(meso_nodes)),
  meso  = rep(meso_nodes, times = length(macro_nodes)),
  weight = macro_total / length(meso_nodes)  # equal split across meso
)

# combine flows
dat <- expand.grid(
  macro = macro_nodes,
  meso  = meso_nodes,
  micro = micro_nodes,
  stringsAsFactors = FALSE
) %>%
  left_join(meso_micro, by = c("meso","micro")) %>%
  left_join(macro_meso, by = c("macro","meso"), suffix = c("_meso_micro","_macro_meso")) %>%
  mutate(
    # The flow value is determined by both links
    weight = pmin(weight_meso_micro, weight_macro_meso, na.rm=TRUE)
  )

# convert to sankey format
df <- dat %>% make_long(macro, meso, micro, value = weight)

node_order <- c(micro_nodes, meso_nodes, macro_nodes)

node_order <- c(macro_nodes, meso_nodes, micro_nodes)
df$node <- factor(df$node, levels = node_order)
df$next_node <- factor(df$next_node, levels = node_order)

# plot
gg_sankey <- ggplot(df, aes(
  x = x, next_x = next_x, node = node, next_node = next_node,
  fill = factor(node), label = node, value = value
)) +
  geom_alluvial(flow.alpha = .75, width = 0.2) +
  geom_alluvial_text(size = 3.5, color = "black", angle = 90) +
  scale_fill_viridis_d(option = "D", direction = -1, begin = 0.2) +
  scale_x_discrete(labels = c("Demographics\n(Macro)", "Mechanism\n(Meso)", "Implementation\n(Micro)")) +
  theme_sankey(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "", x = NULL) +
  theme(legend.position = "none")

print(gg_sankey)

ggsave(gg_sankey, path = here::here("output", "images"), filename = "sankey_plot.png", width = 6, height = 8, dpi = 1000)
