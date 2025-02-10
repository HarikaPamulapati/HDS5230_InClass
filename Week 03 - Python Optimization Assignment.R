library(tidyverse)
library(profvis)

# Haversine function in R
haversine <- function(lat1, lon1, lat2, lon2) {
  MILES <- 3959
  lat1 <- lat1 * pi/180
  lon1 <- lon1 * pi/180
  lat2 <- lat2 * pi/180
  lon2 <- lon2 * pi/180
  
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  MILES * c
}

# Read data and convert latitude/longitude to numeric
df <- readxl::read_excel("clinics.xls") %>%
  mutate(
    locLat = as.numeric(locLat),
    locLong = as.numeric(locLong)
  )

# Method 1: For loop
system.time({
  distances <- numeric(nrow(df))
  for(i in 1:nrow(df)) {
    distances[i] <- haversine(df$locLat[1], df$locLong[1],
                              df$locLat[i], df$locLong[i])
  }
})

# Method 2: Vectorized using sapply
system.time({
  distances <- sapply(1:nrow(df), function(i) 
    haversine(df$locLat[1], df$locLong[1],
              df$locLat[i], df$locLong[i]))
})

# Method 3: Fully vectorized
system.time({
  distances <- haversine(df$locLat[1], df$locLong[1],
                         df$locLat, df$locLong)
})