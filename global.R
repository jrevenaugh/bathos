# GLOBALS ----------------------------------------------------------------------

require(tidyverse)
require(magrittr)

# Prepare limit lines
limit_gmt <- data.frame( Latitude = c(-90, 90, -90, 90),
                         Longitude = c(-180, -180, 180, 180),
                         group = c( 1, 1, 2, 2 ) )
limit_pac <- data.frame( Latitude = c(-90, 90, -90, 90),
                         Longitude = c(0, 0, 360, 360),
                         group = c( 1, 1, 2, 2 ) )

# Prepare earthquake data frame
eqcat <- readRDS(file = "~/R-Studio/Projects/Shiny/Bathos/eqcat.RDS")
eqcat %<>% dplyr::filter(Magnitude >= 6) %>%
  select(DateTime, Magnitude, Depth, Latitude, Longitude)

# Set up depth-dependent colors
nColors <- 3
eqcat$dCol <- ifelse(eqcat$Depth < 70, 1, ifelse( eqcat$Depth < 300, 2, 3))
pal <- colorRampPalette(c("red", "green3", "blue"))(nColors)
eqColorPal <- function( depth ) pal[depth]

# Retrieve volcano and plates data frame
volcanoes <- readRDS(file = "~/R-Studio/Projects/Shiny/Bathos/volcano.RDS")
plates <- readRDS(file = "~/R-Studio/Projects/Shiny/Bathos/plates.RDS")

# Bathymetry
# Choose one: etopo5 is the best trade-off of speed and resolution
#etopo <- readRDS(file = "~/R-Studio/Projects/Shiny/Bathos/ETOPO1.RDS")
etopo <- readRDS(file = "~/R-Studio/Projects/Shiny/Bathos/ETOPO5.RDS")
#etopo <- readRDS(file = "~/R-Studio/Projects/Shiny/Bathos/ETOPO25.RDS")

# Append on front and end of bathymetry to deal with dateline
x <- etopo$x
n <- length( x ) / 2
N <- length( x )
etopo$x <- c( etopo$x[(n+1):N] - 360, etopo$x, 360 + etopo$x[1:n] )
etopo$z <- rbind( etopo$z[(n+1):N,], etopo$z, etopo$z[1:n,] )

# Color Palette
bathColorPal <- readRDS(file = "~/R-Studio/Projects/Shiny/Bathos/colors.RDS")

# Map data restriction routines
source("~/R-Studio/Projects/Shiny/Bathos/findCross.R")
source("~/R-Studio/Projects/Shiny/Bathos/findElev.R")
source("~/R-Studio/Projects/Shiny/Bathos/plotly.R")
