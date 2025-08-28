# Big 5 Euro League Analysis (2024-2025)

# Load the required libraries
library(rvest) # webscrape
library(tidyverse)
library(ggtext)
library(readr)
library(ggrepel)
#install.packages("fmsb")  # for radar charts
library(fmsb)
library(tidyverse)
library(RColorBrewer)
# devtools::install_github("ricardo-bion/ggradar", 
#                          dependencies = TRUE)
library(ggradar)
library(dplyr)
library(scales)
library(ggplot2)
library(tibble)

# Load the required data for 24/25 Season

page_fb_ref_big_5_24_25 <- read_html("https://fbref.com/en/comps/Big5/2024-2025/nations/2024-2025-Big-5-European-Leagues-Nationalities")
table_fb_ref_big_5_24_25 <- html_table(page_fb_ref_big_5_24_25)
table_fb_ref_big_5_24_25 <- as.data.frame(table_fb_ref_big_5_24_25)

# Clean the data...

# Save data as csv in datasets
write_csv(kpl_merge_24_25, here::here("sub_pro_1_kpl_analysis",
                                     "datasets", "kpl_merge_24_25.csv"))

# Read in data
kpl_merge_24_25 <- read_csv(here::here("sub_pro_1_kpl_analysis", 
                                       "datasets", "kpl_merge_24_25.csv"))

