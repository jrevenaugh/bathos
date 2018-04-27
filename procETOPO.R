# Create ETOPO5.RDS file for use with bathos.

# Read ETOPO5.DAT big-endian two-byte signed integer binary
# Available from:
# https://www.ngdc.noaa.gov/mgg/global/relief/ETOPO5/TOPO/ETOPO5/

etopoIn <- "ETOPO5.DAT"         # Path to data
m <- readBin(etopoIn,
             "int",
             endian = "big",
             size = 2,
             signed = TRUE,
             n = (360 * 12 * 180 * 12))

# Convert to matrix and shift rows so it goes from -179.9167 to 180
M <- matrix(m, nrow = 4320, byrow = FALSE)
M <- rbind(M[2161:4320,], M[1:2160,])

# Reverse order of columns (-90 to 90 lat)
M <- M[,2160:1]

# Create lon, lat sequences
lon <- seq(-180 + 1 / 12, by = 1 / 12, length.out = 4320)
lat <- seq(-90, 90, 1 / 12)

# Downloaded data skips the south pole (elevation 2810 m), so let's add it
# (lat sequence already includes it).
M <- cbind(rep(2810, 4320), M)

etopo5_obj <- list(x = lon, y = lat, z = M)

etopoOut <- "ETOPO5.RDS"       # Output path
saveRDS(object = etopo5_obj, file = etopoOut )
