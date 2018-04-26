require(plotly)
require(tidyverse)
require(magrittr)

# plot_ly functions -------------------------------------------------------------

# default axis controls
axDefs <- list(
  ticks = "outside",
  autotick = TRUE,
  ticklen = 5,
  tickwidth = 1,
  zeroline = TRUE,
  showline = TRUE,
  mirror = "ticks",
  gridcolor = toRGB("gray80"),
  gridwidth = 0.5,
  zerolinecolor = toRGB("steelblue"),
  zerolinewidth = 1.5,
  linecolor = toRGB("black"),
  linewidth = 1.25 )

# Topographic profile.  Profile is dataframe with Distance and Elevation.
# Coords is matrix with long, lat columns.  vertExag is multiplicative factor
# between y and x axis scales.

plotlyProfile <- function( profile, coords, vertExag ) {
  xAxis <- axDefs
  xAxis$title <- "Distance (km)"
  xAxis$range <- range(profile$Distance)

  yAxis <- axDefs
  yAxis$title <- "Elevation (km)"
  yAxis$range <- range(profile$Elevation)
  yAxis$scaleanchor = "x"
  yAxis$scaleratio = vertExag

  profile$text <- paste0("(", round(coords[,1],3), ", ", round(coords[,2],3), ")")

  p <- plot_ly() %>%
    add_lines(data = profile,
              type = "scatter",
              x = ~Distance,
              y = ~Elevation,
              text = ~text,
              mode = "lines",
              showlegend = FALSE,
              line = list( color = "black" )) %>%
    layout(xaxis = xAxis,
           yaxis = yAxis)

  p$elementId <- NULL
  return( p )
}

# plotlySurface
# elev is list returned by findMaps
plotlySurface <- function( elev, vertExag, bathColorPal, waterOpacity) {
  elev$z <- elev$z / 1000                # Convert to km
  mercor <- cos(mean(elev$y) * pi / 180) # Apply qmap-like mercator adjustment
  h2v <- 6371 * pi / 180                 # Degrees latitude to km
  xrange <- range( elev$x )
  yrange <- range( elev$y )
  zrange <- range( elev$z )
  water <- data.frame( x = xrange,
                       y = yrange,
                       z = matrix(0, nrow = 2, ncol = 2) )
  dnorm <- max( c( diff( xrange ), diff( yrange ) ) )
  xscale <- diff( xrange ) / dnorm * mercor
  yscale <- diff( yrange ) / dnorm
  zscale <- diff( zrange ) / dnorm / h2v
  p <- plot_ly(x = elev$x, y = elev$y, z = t(elev$z)) %>%
    add_surface(cmin = bathColorPal$cmin,
                cmax = bathColorPal$cmax,
                colorscale = bathColorPal$colorscale,
                name = "Topo") %>%
    add_surface(data = water,
                x = ~x,
                y = ~y,
                z = matrix( 0, nrow = 2, ncol = 2 ),
                hoverinfo = "skip",
                cmin = -0.0000001,
                cmax = 0.0000001,
                colorscale = list( c(0, 1), c( "steelblue", "steelblue" ) ),
                opacity = waterOpacity, showscale = FALSE ) %>%
    layout( scene = list( xaxis = list(title = "East", range = xrange),
                          yaxis = list(title = "North", range = yrange),
                          zaxis = list(title = "Z (km)", range = zrange),
                          aspectratio = list(x = xscale,
                                             y = yscale,
                                             z = vertExag * zscale),
                          camera = list(eye = list(x = 0, y = -0.75, z = 0.9)))
    )
  p$elementId <- NULL
  p
}

# plotlyHypso
# elev is a dataframe with d = percent area below elevation z
plotlyHypso <- function( elev ) {
  xAxis <- axDefs
  xAxis$title <- "Areal Fraction Below Elevation"
  xAxis$range <- range(elev$d)

  yAxis <- axDefs
  yAxis$title <- "Elevation (km)"
  yAxis$range <- range(elev$z)

  p <- plot_ly() %>%
    add_lines(data = elev,
              type = "scatter",
              x = ~d,
              y = ~z,
              mode = "lines",
              showlegend = FALSE,
              line = list( color = "black" )) %>%
    layout(xaxis = xAxis,
           yaxis = yAxis)

  p$elementId <- NULL
  p
}
