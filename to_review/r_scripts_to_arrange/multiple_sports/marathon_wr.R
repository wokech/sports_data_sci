# Marathon World Record Running Time

# Load the required libraries
library(rvest) # webscrape

# Load the required data
# 1) Marathon world records
page <- read_html("https://en.wikipedia.org/wiki/Marathon_world_record_progression")
tables <- html_table(page)
marathon_men <- tables[[1]]
marathon_women <-tables[[2]]



# Claude AI method

# Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# install.packages("devtools")
# devtools::install_github("rensa/ggflags")

library(ggflags)

# Create the dataset
data <- data.frame(
  year = c(2010, 2011, 2013, 2014, 2016, 2017, 2018, 2019, 2022, 2023),
  men = c("2:03:59", "2:03:38", "2:03:23", "2:02:57", "2:02:57", "2:02:57", "2:01:39", "2:01:39", "2:01:39", "2:00:35"),
  women = c("2:15:25", "2:15:25", "2:15:25", "2:15:25", "2:15:25", "2:15:25", "2:15:25", "2:14:04", "2:14:04", "2:11:53"),
  men_country = c("ke", "ke", "ke", "ke", "ke", "ke", "ke", "ke", "ke", "ke"),
  women_country = c("gb", "gb", "gb", "gb", "gb", "gb", "gb", "ke", "ke", "et")
)

# Function to convert time string to seconds
time_to_seconds <- function(time_str) {
  parts <- strsplit(time_str, ":")[[1]]
  as.numeric(parts[1]) * 3600 + as.numeric(parts[2]) * 60 + as.numeric(parts[3])
}

# Convert times to seconds
data$men_seconds <- sapply(data$men, time_to_seconds)
data$women_seconds <- sapply(data$women, time_to_seconds)

# Reshape data for plotting
data_long <- data %>%
  tidyr::pivot_longer(cols = c(men_seconds, women_seconds),
                      names_to = "gender",
                      values_to = "seconds") %>%
  mutate(gender = ifelse(gender == "men_seconds", "Men", "Women"),
         country = ifelse(gender == "Men", men_country, women_country))

# Create the plot
p <- ggplot(data_long, aes(x = year, y = seconds, color = gender, group = gender)) +
  geom_line() +
  geom_point(aes(country = country), size = 5) +
  geom_flag(aes(country = country), size = 5) +
  scale_y_continuous(labels = function(x) {
    paste0(floor(x / 60), ":", sprintf("%02d", round(x %% 60)))
  }) +
  scale_color_manual(values = c("Men" = "#8884d8", "Women" = "#82ca9d")) +
  labs(title = "Marathon World Record Progression (2010-2024)",
       x = "Year",
       y = "Time (HH:MM)",
       color = "Gender") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display the plot
p

# Save the plot (optional)
# ggsave("marathon_world_records.png", p, width = 10, height = 6, dpi = 300)