# findElev
# Subset elev_obj based on selected rectangle
require(sp)
require(geosphere)
require(fields)

findElev <- function( shape, elev_obj ) {
  # shape is rectangle returned from leaflet region selection toolbar
  polyline_coordinates <- shape$geometry$coordinates
  polyline <- do.call(rbind,
                      lapply(polyline_coordinates[[1]],
                             function(x){c(x[[1]][1],x[[2]][1])}))

  lon_index <- vector(mode = "double", length = 3)
  lat_index <- vector(mode = "double", length = 3)

  lon_range <- range(polyline[,1])
  lat_range <- range(polyline[,2])

  for (i in c(1,2)) {
    lon_index[i] <- which.min(abs(lon_range[i] - elev_obj$x))
    lat_index[i] <- which.min(abs(lat_range[i] - elev_obj$y))
  }
  lons <- elev_obj$x[lon_index[1]:lon_index[2]]
  lats <- elev_obj$y[lat_index[2]:lat_index[1]]
  elev <- elev_obj$z[lon_index[1]:lon_index[2], lat_index[2]:lat_index[1]]
  return(list(x = lons, y = lats, z = elev))
}

