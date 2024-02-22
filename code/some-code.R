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
               readxl, scales, mytidyfunctions)

## Specify locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Disable scientific notation ----
options(scipen = 999)

# Load data ----
sug_bev <- read_csv("rawdata/june1data.csv") %>% 
  clean_names() %>% 
  mutate(date = as_date(count, # It does not match the dates very well.
                        origin = "00-10-25 UTC"))


# General estimations ----
## Total count 
sug_bev %>% 
  rowwise() %>% 
  mutate(total2 = sum(zero_cal, sugary, 
                      juice100, ojuice, sports, na.rm = T))

# What is total equivalent to?


# Plots -----

## Sellings by site and category -----

sug_bev %>% 
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
                               "both" = "Both \nmessages")) %>% 
  ggplot(aes(x = count, 
             y = values, 
             group = beverage,
             color = beverage)) +
  geom_col(aes(y = Inf,
               fill = fct_inorder(intervention)),
           color = NA,
           width = 1,
           alpha = 0.3) +
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
       fill = "Intervention",
       x = "Days",
       y = "Sold beverages",
       caption = "Source: client submission. ") +
  theme_jmr(legend.spacing = unit(0.5, "cm"),
            legend.key.height = unit(0.7, "cm"),
            text = element_text(family = "Times New Roman"))


ggsave("figs/eda_1.png",
       bg = "transparent",
       width = 200,                 # Ancho de la gráfica
       height = 120,
       units = "mm",
       dpi = 300)



