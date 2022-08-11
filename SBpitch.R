# FCrStats - SBPitch
# https://github.com/FCrSTATS/SBpitch

# Statsbomb have released a free dataset to help the public get used to 
# their data product but also provide data to kick start people's journey 
# into football analytics.

# Installation
devtools::install_github("FCrSTATS/SBpitch")
library(SBpitch)

# The Default Plot

create_Pitch()

# Variables to Customise

# Basic Features

create_Pitch(BasicFeatures = TRUE)

# Middle Third Shading

create_Pitch(middlethird = TRUE)

# Box Type
# Line Design

create_Pitch(goaltype = "line")

# Box Design

create_Pitch(goaltype = "box")

# Barca Numbers Design

create_Pitch(goaltype = "barcanumbers")

# Juego de Posicion

create_Pitch(JdeP = TRUE)

# Colour Schemes

# Blue Example

create_Pitch(grass_colour = "#224C56", 
             line_colour =  "#B3CED9", 
             background_colour = "#224C56", 
             goal_colour = "#15393D")

# Night Example

create_Pitch(grass_colour = "#202020", 
             line_colour =  "#797876", 
             background_colour = "#202020", 
             goal_colour = "#131313")


# Green Example

create_Pitch(grass_colour = "#538032", 
             line_colour =  "#ffffff", 
             background_colour = "#538032", 
             goal_colour = "#000000")
