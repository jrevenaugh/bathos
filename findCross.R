# findCross
# Find which map (coasts, plate boundaries) elements are in shape drawn on leaflet map
require(sp)
require(geosphere)
require(fields)

findCross <- function( shape, etopo ) {
  # shape is polyline returned from leaflet region selection toolbar
  polyline_coordinates <- shape$geometry$coordinates
  polyline.df <- as.tibble( do.call(rbind,
                                    lapply(polyline_coordinates,
                                           function(x){c(x[[1]][1],x[[2]][1])})))

  # Create matrix of linked set of GC points
  n <- dim(polyline.df)[1]
  x <- matrix( 0, nrow = n, ncol = 2 )
  for (i in 1:n) {
    x[i,] <- as.numeric(polyline.df[i,])
    if (x[i,1] > 180) x[i,1] <- x[i,1] - 360
  }

  dkm <- 111 / 12 * 0.5 # Twice over-sampled for smoothness
  for (i in 1:(n-1)) {
    distKm <- distHaversine(x[i,], x[i+1,]) / 1000
    n <- round(distKm / dkm, 0)
    if ( i == 1 ) {
      p <- gcIntermediate(x[i,], x[i+1,], n = n, addStartEnd = TRUE, sp = FALSE)
    } else {
      p <- rbind(p, gcIntermediate(x[i,], x[i+1,], n = n, addStartEnd = TRUE, sp = FALSE))
    }
  }
  intp <- interp.surface(etopo, p) / 1000
  dist <- c(0, cumsum(distGeo(p) / 1000))
  profile.df <- data_frame(Distance = dist, Elevation = intp)
  return(list(profile = profile.df, coords = p))
}

