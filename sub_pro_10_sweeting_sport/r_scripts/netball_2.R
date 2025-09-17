# Netball Data Analysis 2 - Interactive Figures
# Alice Sweeting
# https://sportstatisticsrsweet.wordpress.com/

# A) Create a dummy dataset

# To create interactive figures - declare netball playing position
PlayingPosition = c("C", "WA", "WD", "GA", "GD", "GS", "GK")
# Declare four seperate training sessions
TrainingSession = c("Main Training", "Courtwork", "Match Simulation", "Long Court")
# Set the seed, to reuse the same set of distance variables
set.seed(28)
# Create a summary data.frame containing dummy total and sprint distance data, across all training sessions
PhysicalOutputData = data.frame(PlayingPosition = rep((PlayingPosition), each = 4),
                                TrainingSession = rep((TrainingSession), each = 1),
                                TotalDistance = runif(28, 450, 950),
                                SprintDistance = runif(28, 0, 200))
# Round the total and sprint distance columns to 1 decimal place
PhysicalOutputData$TotalDistance <- round(PhysicalOutputData$TotalDistance, digits = 1)
PhysicalOutputData$SprintDistance <- round(PhysicalOutputData$SprintDistance, digits = 1)

# B) Load dplyr and manipulate data

# As an example, create a summary column looking at weekly totals
require(dplyr)
WeekSummaryData <- PhysicalOutputData %>%
  group_by(PlayingPosition) %>%
  summarise(TD = sum(TotalDistance),
            SD = sum(SprintDistance))

# Load plotly

# Load required package
require(plotly)
# Plot - basic plotly image
plot_ly(data = WeekSummaryData, x = ~TD, y = ~PlayingPosition, type = 'bar', orientation = 'h',
        color = ~ PlayingPosition) %>%
  layout(title = "Weekly Total Distance Covered",
         yaxis = list(title = ""),
         xaxis = list(title = "Total Distance (m)"),
         showlegend = FALSE)


# To alter the hover text - show the sprint distance covered
plot_ly(WeekSummaryData, x = ~TD, y = ~PlayingPosition,
        type = 'bar',
        orientation = 'h',
        hoverinfo = 'text',
        text = ~paste('Total Distance:', TD,'m',
                      ' ',
                      'Sprint Distance:', SD, 'm'),
        color = ~ PlayingPosition) %>%
  layout(title = "Weekly Total Distance Covered",
         yaxis = list(title = ""),
         xaxis = list(title = "Total Distance (m)"),
         showlegend = FALSE)





