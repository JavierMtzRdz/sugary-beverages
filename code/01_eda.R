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
               cowplot, GGally)

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
                        origin = "00-10-25 UTC"),
         site = recode(site,
                       "chop" = "A",
                       "HF" = "B",
                       "NS" = "C")) %>% 
  arrange(site, count)

glimpse(sug_bev)
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
                               "both" = "Both \nmessages")) %>% 
  left_join(sug_bev %>% select(count, site, total),
            by = join_by(count, site)) %>% 
  mutate(values_percent = values/total) 

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

## Sellings by site and category as %-----

sug_bev_long %>% 
  ggplot(aes(x = count, 
             y = values_percent, 
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


ggsave("figs/eda-1.1_time-serie-percent.png",
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

## Decomposed dataset with percent----
sug_bev_ts_per <- sug_bev %>% 
  as_tsibble(index = count,
             key = c(site)) %>% 
  tsibble::fill_gaps() %>% 
  transmute(site, count, 
            zero_cal = zero_cal/total, 
            sugary = sugary / total) %>% 
  zoo::na.locf()

sug_bev_decompos_per <- sug_bev_ts_per %>% 
  split(.$site) %>%
  map(~ model(., stl = classical_decomposition(zero_cal ~ season(7)))) %>%
  map_df(components) %>% 
  transmute(site, count, 
            original = zero_cal, 
            beverage = "Zero-calorie",
            trend, seasonal, 
            random, season_adjust) %>% 
  as_tibble() %>% 
  bind_rows(sug_bev_ts_per %>% 
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
# sug_bev_decompos_per %>%
#   write_csv("gendata/sug_bev_decompos_per.csv")

### Save decomposed dataset -----
sug_bev_decompos <- read_csv("gendata/sug_bev_decompos.csv")
sug_bev_decompos_per <- read_csv("gendata/sug_bev_decompos_per.csv")

### Plot decomposition ----
sug_bev_decompos %>% 
  select(-season_adjust, -intervention) %>% 
  pivot_longer(c(original, trend,
                 seasonal, random)) %>% 
  mutate(name = str_to_sentence(name)) %>% 
  ggplot(aes(x = count, 
             y = value,
             color = beverage)) +
  # facet_grid(vars(fct_inorder(name)), vars(site),
  #                               scales = "free") +
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
       y = "Sold beverages") +
  theme_jmr(legend.spacing = unit(0.5, "cm"),
            legend.key.height = unit(0.7, "cm"),
            text = element_text(family = "Times New Roman"))
  


ggsave("figs/eda-3_decomposition.png",
       bg = "transparent",
       width = 200,                 # Ancho de la gráfica
       height = 90,
       units = "mm",
       dpi = 300)

### Plot decomposition per ----
sug_bev_decompos_per %>% 
  select(-season_adjust, -intervention) %>% 
  pivot_longer(c(original, trend,
                 seasonal, random)) %>% 
  mutate(name = str_to_sentence(name)) %>% 
  ggplot(aes(x = count, 
             y = value,
             color = beverage)) +
  # facet_grid(vars(fct_inorder(name)), vars(site),
  #            scales = "free") +
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
       y = "Sold beverages") +
  theme_jmr(legend.spacing = unit(0.5, "cm"),
            legend.key.height = unit(0.7, "cm"),
            text = element_text(family = "Times New Roman"))



ggsave("figs/eda-3-2_decomposition_per.png",
       bg = "transparent",
       width = 200,                 # Ancho de la gráfica
       height = 100,
       units = "mm",
       dpi = 300)

# Autocorrelation analysis ------
# First try
sug_bev_ts %>% 
  filter(site == "NS") %>% 
  pull(sugary) %>% 
  acf() 

sug_bev_ts %>% 
  filter(site == "NS") %>% 
  pull(zero_cal) %>% 
  acf() 

sug_bev_ts %>% 
  filter(site == "NS") %>% 
  pull(zero_cal) %>% 
  pacf() 

sug_bev_ts %>% 
  filter(site == "NS") %>% 
  pull(zero_cal) %>% 
  acf(plot= F) %>% 
  with(data.frame(lag, acf, n.used)) %>% 
  mutate(ic_alpha = qnorm((1 + (1 - 0.05))/2)/sqrt(n.used)) %>% 
  ggplot(mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  geom_hline(aes(yintercept = ic_alpha), linetype = 2, color = 'darkblue') + 
  geom_hline(aes(yintercept = -ic_alpha), linetype = 2, color = 'darkblue')


## Autocorrelation Function (ACF) and Partial Autocorrelation Function (PACF)------
### Estimate ACF and PAC

sug_bev_acf <- sug_bev_ts %>% 
  split(.$site) %>%
  map(~ pull(., zero_cal)) %>%
  map(~ acf(., plot= F)) %>%
  map(~ with(., data.frame(lag, acf, n.used))) %>% 
  map2_df(unique(sug_bev_ts$site), 
          function (.x, .y) {mutate(.x,
                                    site = .y,
                                    beverage = "Zero-calorie",
                                    test = "ACF",
                                    ic_alpha = qnorm((1 + (1 - 0.05))/2)/
                                      sqrt(n.used))}) %>% 
  bind_rows(sug_bev_ts %>% 
              split(.$site) %>%
              map(~ pull(., zero_cal)) %>%
              map(~ pacf(., plot= F)) %>%
              map(~ with(., data.frame(lag, acf, n.used))) %>% 
              map2_df(unique(sug_bev_ts$site), 
                      function (.x, .y) {mutate(.x,
                                                site = .y,
                                                beverage = "Zero-calorie",
                                                test = "PACF",
                                                ic_alpha = qnorm((1 + (1 - 0.05))/2)/
                                                  sqrt(n.used))}),
            sug_bev_ts %>% 
              split(.$site) %>%
              map(~ pull(., sugary)) %>%
              map(~ acf(., plot= F)) %>%
              map(~ with(., data.frame(lag, acf, n.used))) %>% 
              map2_df(unique(sug_bev_ts$site), 
                      function (.x, .y) {mutate(.x,
                                                site = .y,
                                                beverage = "Sugary",
                                                test = "ACF",
                                                ic_alpha = qnorm((1 + (1 - 0.05))/2)/
                                                  sqrt(n.used))}),
            sug_bev_ts %>% 
              split(.$site) %>%
              map(~ pull(., sugary)) %>%
              map(~ pacf(., plot= F)) %>%
              map(~ with(., data.frame(lag, acf, n.used))) %>% 
              map2_df(unique(sug_bev_ts$site), 
                      function (.x, .y) {mutate(.x,
                                                site = .y,
                                                beverage = "Sugary",
                                                test = "PACF",
                                                ic_alpha = qnorm((1 + (1 - 0.05))/2)/
                                                  sqrt(n.used))})) 

### Estimate ACF and PAC per

sug_bev_acf_per <- sug_bev_ts_per %>% 
  split(.$site) %>%
  map(~ pull(., zero_cal)) %>%
  map(~ acf(., plot= F)) %>%
  map(~ with(., data.frame(lag, acf, n.used))) %>% 
  map2_df(unique(sug_bev_ts_per$site), 
          function (.x, .y) {mutate(.x,
                                    site = .y,
                                    beverage = "Zero-calorie",
                                    test = "ACF",
                                    ic_alpha = qnorm((1 + (1 - 0.05))/2)/
                                      sqrt(n.used))}) %>% 
  bind_rows(sug_bev_ts_per %>% 
              split(.$site) %>%
              map(~ pull(., zero_cal)) %>%
              map(~ pacf(., plot= F)) %>%
              map(~ with(., data.frame(lag, acf, n.used))) %>% 
              map2_df(unique(sug_bev_ts_per$site), 
                      function (.x, .y) {mutate(.x,
                                                site = .y,
                                                beverage = "Zero-calorie",
                                                test = "PACF",
                                                ic_alpha = qnorm((1 + (1 - 0.05))/2)/
                                                  sqrt(n.used))}),
            sug_bev_ts_per %>% 
              split(.$site) %>%
              map(~ pull(., sugary)) %>%
              map(~ acf(., plot= F)) %>%
              map(~ with(., data.frame(lag, acf, n.used))) %>% 
              map2_df(unique(sug_bev_ts_per$site), 
                      function (.x, .y) {mutate(.x,
                                                site = .y,
                                                beverage = "Sugary",
                                                test = "ACF",
                                                ic_alpha = qnorm((1 + (1 - 0.05))/2)/
                                                  sqrt(n.used))}),
            sug_bev_ts_per %>% 
              split(.$site) %>%
              map(~ pull(., sugary)) %>%
              map(~ pacf(., plot= F)) %>%
              map(~ with(., data.frame(lag, acf, n.used))) %>% 
              map2_df(unique(sug_bev_ts_per$site), 
                      function (.x, .y) {mutate(.x,
                                                site = .y,
                                                beverage = "Sugary",
                                                test = "PACF",
                                                ic_alpha = qnorm((1 + (1 - 0.05))/2)/
                                                  sqrt(n.used))})) 

### Plot ACF
sug_bev_acf %>% 
  filter(test == "ACF") %>% 
  mutate(lag = ifelse(beverage == "Sugary", 
                      lag - 0.05, lag + 0.1)) %>% 
  ggplot(aes(x = lag, y = acf)) +
  facet_wrap(~site,
             scales = "free",
             ncol = 1) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(aes(xend = lag,
                   yend = 0,
                   color = beverage)) +
  geom_hline(aes(yintercept = ic_alpha,
                 linetype = "Limit of significance"), 
             color = 'darkblue') + 
  geom_hline(aes(yintercept = -ic_alpha,
                 linetype = "Limit of significance"), 
             color = 'darkblue') +
  scale_linetype_manual(values = "dashed") +
  scale_color_jmr(guide = guide_legend(
    nrow = 1,
    direction = "horizontal",
    title.position = "left")) +
  labs(colour = "Beverage",
       fill = "Intervention periods",
       x = "Lags",
       y = "Autocorrelation Function (ACF)",
       linetype = element_blank()) +
  theme_jmr(legend.spacing = unit(0.5, "cm"),
            legend.key.height = unit(0.7, "cm"),
            text = element_text(family = "Times New Roman"))

## Combined plot ------
sug_bev_acf %>% 
  mutate(lag = ifelse(beverage == "Sugary", 
                      lag - 0.12, lag + 0.12)) %>% 
  ggplot(aes(x = lag, y = acf,
             color = beverage)) +
  ggh4x::facet_grid2(vars(test), vars(site),
                     switch = "y",
                     scales = "free") +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(aes(xend = lag,
                   yend = 0)) +
  geom_point(size = 0.4) +
  geom_ribbon(aes(ymin = -ic_alpha, ymax=ic_alpha,
                  x = ifelse(lag > 10, lag + 1, lag - 1),
                  fill = "Limit of significance (5%)"),
              alpha = 0.2,
              linetype = "dashed",
              color = "#FF483B") +
  scale_linetype_manual(values = "dashed") +
  scale_x_continuous(expand = expansion(mult = c(0.0, 0.0))) +
  scale_color_jmr(guide = guide_legend(
    nrow = 1,
    direction = "horizontal",
    title.position = "left")) +
  labs(colour = "Beverage",
       fill = element_blank(),
       x = "Lags",
       y = element_blank(),
       linetype = element_blank()) +
  theme_jmr(legend.spacing = unit(0.5, "cm"),
            legend.key.height = unit(0.7, "cm"),
            text = element_text(family = "Times New Roman"))

ggsave("figs/eda-4_acf-pacf.png",
       bg = "transparent",
       width = 200,                 # Ancho de la gráfica
       height = 80,
       units = "mm",
       dpi = 300)

## Combined plot per ------
sug_bev_acf_per %>% 
  mutate(lag = ifelse(beverage == "Sugary", 
                      lag - 0.12, lag + 0.12)) %>% 
  ggplot(aes(x = lag, y = acf,
             color = beverage)) +
  ggh4x::facet_grid2(vars(test), vars(site),
                     switch = "y",
                     scales = "free") +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(aes(xend = lag,
                   yend = 0)) +
  geom_point(size = 0.4) +
  geom_ribbon(aes(ymin = -ic_alpha, ymax=ic_alpha,
                  x = ifelse(lag > 10, lag + 1, lag - 1),
                  fill = "Limit of significance (5%)"),
              alpha = 0.2,
              linetype = "dashed",
              color = "#FF483B") +
  scale_linetype_manual(values = "dashed") +
  scale_x_continuous(expand = expansion(mult = c(0.0, 0.0))) +
  scale_color_jmr(guide = guide_legend(
    nrow = 1,
    direction = "horizontal",
    title.position = "left")) +
  labs(colour = "Beverage",
       fill = element_blank(),
       x = "Lags",
       y = element_blank(),
       linetype = element_blank()) +
  theme_jmr(legend.spacing = unit(0.5, "cm"),
            legend.key.height = unit(0.7, "cm"),
            text = element_text(family = "Times New Roman"))

ggsave("figs/eda-4-2_acf-pacf-per.png",
       bg = "transparent",
       width = 200,                 # Ancho de la gráfica
       height = 80,
       units = "mm",
       dpi = 300)

# Scatter plots and correlations ------

## Internal plot functions
my_scatter <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) + 
    geom_point(alpha = 0.2) + 
    geom_smooth(method=lm, 
                alpha = 0.2,
                ...) 
}

my_dens <- function(data, mapping, ...) {
  ggplot(data = data, mapping=mapping) +
    geom_density(...) 
}

## Generate plot 

cor_site <- ggpairs(sug_bev, columns = c(2, 5:6), 
                    aes(colour = site),
                    upper = list(continuous = wrap("cor", size = 3,
                                                   family = "Times New Roman")),
                    lower = list(continuous = my_scatter),
                    diag = list(continuous = my_dens)) +
  scale_color_jmr() +
  scale_fill_jmr() +
  labs(subtitle = "By site") +
  theme_jmr(text = element_text(family = "Times New Roman"))

cor_inte <- ggpairs(sug_bev, columns = c(2, 5:6), 
                    aes(colour = intervention),
                    upper = list(continuous = wrap("cor", size = 2.5,
                                                   family = "Times New Roman")),
                    lower = list(continuous = my_scatter),
                    diag = list(continuous = my_dens)) +
  scale_color_jmr() +
  scale_fill_jmr() +
  labs(subtitle = "By intervention") +
  theme_jmr(text = element_text(family = "Times New Roman"))

plot_grid(
  ggmatrix_gtable(cor_site),
  ggmatrix_gtable(cor_inte),
  nrow = 1
)

ggsave("figs/eda-5_corr.png",
       bg = "transparent",
       width = 200,                 # Ancho de la gráfica
       height = 120,
       units = "mm",
       dpi = 300)
