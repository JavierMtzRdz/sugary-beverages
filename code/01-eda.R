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

#' To install mytidyfunctions, you need 
#' remotes::install_github("JavierMtzRdz/mytidyfunctions")

pacman::p_load(tidyverse, janitor, writexl, 
               readxl, scales, mytidyfunctions,
               tsibble, feasts, forecast,
               patchwork)

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
                               "wash" = "Washout",
                               "wash2" = "Washout",
                               "preint" = "Pre-intervention",
                               "follow" = "Follow", # Confirm categories
                               "dis" = "Discount", # Assuming that 10% price discount was first.
                               "dismes" = "Discount +\nmessaging",
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
       x = "Days since the start of the study",
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
  geom_line(size = 0.5) +
  scale_x_continuous(breaks = 1:7,
                     expand = expansion(mult = c(0.02, 0.02))) +
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
## Set time-series tibble -----

sug_bev_ts <- sug_bev %>% 
  # filter(!is.na(zero_cal)) %>% 
  as_tsibble(index = count,
             key = c(site)) %>% 
  tsibble::fill_gaps() %>% 
  select(site, count, zero_cal, sugary) %>% 
  zoo::na.locf()

## Decomposed dataset -----
sug_bev_decompos <- sug_bev_ts %>% 
  split(.$site) %>%
  map(~ model(., stl = classical_decomposition(zero_cal ~ season(7)))) %>%
  map_df(components) %>% 
  transmute(site, count, 
            original = zero_cal, 
            beverage = "Zero-calorie",
            trend, seasonal, 
            random, season_adjust) %>% 
  as_tibble() %>% 
  bind_rows(sug_bev_ts %>% 
              split(.$site) %>%
              map(~ model(., stl = classical_decomposition(sugary ~ season(7)))) %>%
              map_df(components) %>% 
              transmute(site, count, 
                        original = sugary, 
                        beverage = "Sugary",
                        trend, seasonal, 
                        random, season_adjust)) %>% 
  left_join(sug_bev %>% 
              select(count, site, intervention), 
            by = join_by(count, site))

### Save decomposed dataset -----
# sug_bev_decompos %>% 
#   write_csv("gendata/sug_bev_decompos.csv")
### Save decomposed dataset -----
sug_bev_decompos <- read_csv("gendata/sug_bev_decompos.csv")

### Plot decomposition ----
sug_bev_decompos %>% 
  select(-season_adjust, -intervention) %>% 
  pivot_longer(c(original, trend,
                 seasonal, random)) %>% 
  mutate(name = str_to_sentence(name)) %>% 
  ggplot(aes(x = count, 
             y = value,
             color = beverage)) +
  ggh4x::facet_grid2(vars(fct_inorder(name)), vars(site),
                     scales = "free",
                     independent = "y") +
  geom_line(size = 0.3) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_color_jmr(guide = guide_legend(
    nrow = 1,
    direction = "horizontal",
    title.position = "left")) +
  labs(colour = "Beverage",
       fill = "Intervention periods",
       x = "Days since the start of the study",
       y = "Sold beverages",
       caption = "Source: client submission. ") +
  theme_jmr(legend.spacing = unit(0.5, "cm"),
            legend.key.height = unit(0.7, "cm"),
            text = element_text(family = "Times New Roman"))
  


ggsave("figs/eda-3_decomposition.png",
       bg = "transparent",
       width = 200,                 # Ancho de la gráfica
       height = 120,
       units = "mm",
       dpi = 300)

# TODO: Create database just with the trends 
# - ACF
# - PCF
