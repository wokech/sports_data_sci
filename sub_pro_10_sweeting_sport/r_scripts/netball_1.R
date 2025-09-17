# Netball Data Analysis 1
# Alice Sweeting
# https://sportstatisticsrsweet.wordpress.com/

# A) Create a dummy dataset

# Create a list of netball athlete positions
PlayingPosition = c("C", "WA", "WD", "GA", "GD", "GS", "GK")
# Declare two playing standards, elite and junior-elite
PlayingStandard = c("Elite", "Junior Elite")
# Set the seed, to reuse the same set of random variables
set.seed(14)
# Create a summary data.frame containing dummy total distance data, across both playing standards
SummaryData = data.frame(PlayingPosition = rep((PlayingPosition), each = 2),
                         PlayingStandard = rep((PlayingStandard), each = 1),
                         TotalDistance = runif(14, 450, 950))
# Round the total distance column to 0 decimal places
SummaryData$TotalDistance <- round(SummaryData$TotalDistance, digits = 0)

# B) Load the required packages and plot the data

# Load required package
require(ggplot2)
# Creating a publication worthy figure
ggplot(data = SummaryData, aes(x = PlayingPosition, y = TotalDistance, fill = PlayingStandard)) +
  geom_bar(width=.7, stat="identity", position="dodge", colour="Black") +
  scale_fill_grey() +
  theme_bw() +
  ylab("Total Distance (m) \n") +
  scale_y_continuous(limits = c(0, 1000), expand = c(0, 0), breaks = seq(0, 1000, by = 250)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.ticks.x = element_blank(),
        plot.background = element_blank(),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.line.x = element_line(color = "black", linewidth = .5),
        axis.line.y = element_line(color = "black", linewidth = .5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

# C) Replot and improve the plot

# Remove the legend
ggplot(data = SummaryData, aes(x = PlayingPosition, y = TotalDistance, fill = PlayingStandard)) +
  geom_bar(width=.7, stat="identity", position="dodge", colour="Black") +
  scale_fill_grey() +
  theme_bw() +
  ylab("Total Distance (m) \n") +
  scale_y_continuous(limits = c(0, 1000), expand = c(0, 0), breaks = seq(0, 1000, by = 250)) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.ticks.x = element_blank(),
        plot.background = element_blank(),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.line.x = element_line(color = "black", linewidth = .5),
        axis.line.y = element_line(color = "black", linewidth = .5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())


