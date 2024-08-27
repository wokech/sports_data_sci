# Marathon World Record Running Time

# Load the required libraries
library(rvest) # webscrape

# Load the required data
# 1) Marathon world records
page <- read_html("https://en.wikipedia.org/wiki/Marathon_world_record_progression")
tables <- html_table(page)
marathon_men <- tables[[1]]
marathon_women <-tables[[2]]
