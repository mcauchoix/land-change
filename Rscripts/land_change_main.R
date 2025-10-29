# ================================================================

# --- 0) Packages ------------------------------------------------
required <- c("sf","dplyr","httr","jsonlite","purrr","stringr","readr","units")
to_install <- required[!(required %in% installed.packages()[,1])]
if(length(to_install)) install.packages(to_install)

library(sf); library(dplyr); library(httr); library(jsonlite)
library(purrr); library(stringr); library(readr); library(units);

