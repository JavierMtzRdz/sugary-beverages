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
               readxl, scales)

## Specify locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Disable scientific notation ----
options(scipen = 999)

