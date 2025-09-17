# Netball Data Analysis 2 - Interactive Figures
# Alice Sweeting
# https://sportstatisticsrsweet.wordpress.com/
# Playing Positions: https://everythingnetball.com/netball-positions-netball-playing-positions-and-roles/

# A) Create a dummy dataset

# Create a list of netball athlete positions
Position = c("C", "WA", "WD", "GA", "GD", "GS", "GK")
# Set the seed, to reuse the same set of random variables
set.seed(14)
# Create a summary data.frame containing dummy total distance data, across both playing standards
DummyData = data.frame(Position = rep((Position), each = 100),
                       Sample = seq(1, 100, by = 1),
                       Time = seq(0.01, 1, by = 0.01),
                       Velocity = runif(100, 1, 3))

# Netball Court is 30.5m by 15.25m

# # Center position (0m to 30.5m)
# df_C <-
# 
# # Wing Attack (10.16m to 30.5m)
# df_WA <-
# 
# # Wing Defense (0m to 20.3m)
# df_WD <- 
# 
# # Goal Attack  (10.16m to 30.5m) 
# df_GA <- 
# 
# # Goal Defense (0m to 20.3m)   
# df_GD <- 
# 
# # Goal Shooter  (20.3m to 30.5m)
# df_GS <- 
# 
# # Goal Keeper (0m to 10.16m)  
# df_GK <- 
  

# Visualize the Dummy Data

ggplot(DummyData) +
  geom_point(aes(x = X, y = Y, color = Velocity)) +
  scale_colour_gradientn(colours = rainbow(7)) +
  coord_equal() +
  theme_bw() +
  theme(plot.background = element_blank(),
        legend.position="bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_path(data = NetballCourt, aes(X,Y), colour = "black", size = 1)


# Visualize the data over time

# Visualise velocity over time (seconds)
ggplot(data = DummyData,
       aes(x = Time, y = Velocity, color = Velocity)) +
  geom_point() +
  scale_colour_gradientn(colours = rainbow(7)) +
  xlab("\n Time (s)") +
  ylab(expression(Velocity ~ (m.s^-1))) +
  scale_x_continuous(limits = c(0, 60), expand = c(0, 0), breaks = seq(0, 60, by = 20)) +
  scale_y_continuous(limits = c(0, 6), expand = c(0, 0), breaks = seq(0, 6, by = 2)) +
  theme_classic() +
  theme(legend.position = "none")


# k-means clustering

# Perform k-means clustering on the velocity column
# First place the column into a matrix
Velocity <- as.matrix(DummyData$Velocity, ncol=1)
# Declare the number of clusters, for example, four groups of velocity
nClusters <- 4
# Ensure the initial cluster points are held constant
set.seed(1)
# Obtain the kMeans cluster
VelocityClusters <- kmeans(Velocity[,1], nClusters)
# To obtain the centers, size and within cluster sum of squares
VelocityClusters$centers
VelocityClusters$size
VelocityClusters$withinss


# Add the cluster data info back into the dummy data
DummyData$Cluster <- factor(VelocityClusters$cluster)
Centers <- as.data.frame(VelocityClusters$centers)


# Create a new column, based on "Centers" data.frame
DummyData$NotationalDescriptor <- factor(DummyData$Cluster,
                                         levels = c(1,2,3,4),
                                         labels = c("Walk", "Sprint", "Jog", "Run"))


# Create a new column, based on "Centers" data.frame
ggplot(data = DummyData, aes(x = NotationalDescriptor, fill = NotationalDescriptor)) +
  geom_bar(colour = "black", size = 1) +
  xlab("\n Notational Descriptor") +
  ylab("Count \n") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 2500)) +
  theme_classic() +
  theme(legend.position = "none")


# Plot based on the notational descriptor
ggplot(data = DummyData, aes(x = Time, y = Velocity, color = NotationalDescriptor)) +
  geom_point() +
  xlab("\n Time (s)") +
  ylab(expression(Velocity ~ (m.s^-1))) +
  scale_x_continuous(limits = c(0, 60), expand = c(0, 0), breaks = seq(0, 60, by = 20)) +
  scale_y_continuous(limits = c(0, 6), expand = c(0, 0), breaks = seq(0, 6, by = 2)) +
  # The line of code below changes the legend heading, when we wish to add a space between words
  labs(colour = 'Notational Descriptor') +
  theme_classic() +
  theme(legend.position = "bottom")




