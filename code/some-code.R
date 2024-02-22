##################################################################
##                    Proyecto: Code for HW2                    ##
##################################################################
##
## Descipción:     this script contains the some codes...
##                 
##
## Author:         Javier Mtz.-Rdz.  
##
## Creation date:  2024-02-10
##
## Email:          javier.mr@stat.ubc.ca
##
## ---------------------------
## Notes:          
## ---------------------------

# Setup ----
## Packages to use ----
pacman::p_load(tidyverse, janitor, writexl, 
               readxl, scales, mytidyfunctions,
               tsibble)

## Specify locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Disable scientific notation ----
options(scipen = 999)

# Load data ----
sug_bev <- read_csv("rawdata/june1data.csv") %>% 
  clean_names() %>% 
  arrange(count) %>% 
  mutate(week_count = ifelse(is.na(lag(dof_w)) |
                               !(lag(dof_w) == dof_w |
                                   lag(dof_w) == dof_w - 1),
                             1, 0),
         week_count = cumsum(week_count),
         date = as_date(count, # It does not match the dates very well.
                        origin = "00-10-25 UTC")) %>% 
  arrange(site, count)
# General estimations ----
## Total count 
sug_bev %>% 
  rowwise() %>% 
  mutate(total2 = sum(zero_cal, sugary, 
                      juice100, ojuice, sports, na.rm = T))

# What is total equivalent to?

# Long data
sug_bev_long <- sug_bev %>% 
  select(-c(juice100:total)) %>% 
  pivot_longer(zero_cal:sugary,
               names_to = "beverage",
               values_to = "values") %>% 
  mutate(beverage = recode(beverage, 
                           "zero_cal" = "Zero-calorie",
                           "sugary" = "Sugary"),
         intervention = recode(intervention, 
                               "wash" = "None",
                               "wash2" = "None",
                               "preint" = "None",
                               "follow" = "Follow", # Confirm categories
                               "dismes" = "Discount", # Assuming that 10% price discount was first.
                               "dis" = "Discount +\nmessaging",
                               "cal" = "Caloric content \nmessaging",
                               "excer" = "Exercise equivalents \nmessaging",
                               "both" = "Both \nmessages")) 

# Plots -----

## Sellings by site and category -----

sug_bev_long %>% 
  ggplot(aes(x = count, 
             y = values, 
             group = beverage,
             color = beverage)) +
  geom_col(aes(y = Inf,
               fill = fct_inorder(intervention)),
           color = NA,
           width = 1,
           alpha = 0.25) +
  facet_wrap(~site,
             scales = "free_y",
             ncol = 1) +
  geom_line() +
  scale_x_continuous(expand = expansion(mult = c(0.0, 0.0))) +
  scale_color_jmr(guide = guide_legend(
    nrow = 2,
    direction = "horizontal",
    title.position = "top")) +
  scale_fill_jmr(palette = "multiple",
                 guide = guide_legend(
    direction = "horizontal",
    title.position = "top")) +
  labs(colour = "Beverage",
       fill = "Intervention periods",
       x = "Days",
       y = "Sold beverages",
       caption = "Source: client submission. ") +
  theme_jmr(legend.spacing = unit(0.5, "cm"),
            legend.key.height = unit(0.7, "cm"),
            text = element_text(family = "Times New Roman"))


ggsave("figs/eda-1_time-serie.png",
       bg = "transparent",
       width = 200,                 # Ancho de la gráfica
       height = 120,
       units = "mm",
       dpi = 300)

# Seasonal plot -----
sug_bev_long %>% 
  ggplot(aes(x = dof_w, 
             y = values, 
             color = beverage,
             group = interaction(week_count, beverage))) +
  facet_wrap(~site,
             scales = "free_y",
             ncol = 1) +
  geom_line() +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_jmr(guide = guide_legend(
    nrow = 1,
    direction = "horizontal",
    title.position = "left")) +
  labs(colour = "Beverage",
       fill = "Intervention periods",
       x = "Day of the week",
       y = "Sold beverages",
       caption = "Source: client submission. ") +
  theme_jmr(legend.spacing = unit(0.5, "cm"),
            legend.key.height = unit(0.7, "cm"),
            text = element_text(family = "Times New Roman"))


ggsave("figs/eda-2_season-plot.png",
       bg = "transparent",
       width = 200,                 # Ancho de la gráfica
       height = 120,
       units = "mm",
       dpi = 300)


# Time-series analysis ----
## Set time-series tibble

sug_bev_ts <- sug_bev %>% 
  filter(site == "HF",
         !is.na(zero_cal)) %>% 
  as_tsibble(index = count)

pacman::p_load(feasts, forecast)

(hf <- sug_bev_ts %>% 
  filter(site == "HF") %>% 
  model(stl = classical_decomposition(zero_cal ~ season(7))) %>%
  components() %>% 
  autoplot() +
  theme_jmr())



