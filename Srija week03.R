# Load required libraries
library(readxl)
library(dplyr)

# Read the data
clinic_data <- read_excel("clinics.xls")

# Convert latitude and longitude to numeric
clinic_data$locLat <- as.numeric(clinic_data$locLat)
clinic_data$locLong <- as.numeric(clinic_data$locLong)

# Define the Haversine function
haversine <- function(lat1, lon1, lat2, lon2) {
  # Convert decimal degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Haversine formula
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  r <- 3959 # Earth's radius in miles
  return(r * c)
}

# Implementation 1: Basic for loop
calc_distance_loop <- function(data) {
  distances <- numeric(nrow(data))
  for(i in 1:nrow(data)) {
    distances[i] <- haversine(40.671, -73.985, 
                              data$locLat[i], 
                              data$locLong[i])
  }
  return(distances)
}

# Implementation 2: Using sapply
calc_distance_sapply <- function(data) {
  sapply(1:nrow(data), function(i) 
    haversine(40.671, -73.985, 
              data$locLat[i], 
              data$locLong[i]))
}

# Implementation 3: Fully vectorized approach
calc_distance_vectorized <- function(data) {
  lat1 <- rep(40.671, nrow(data))
  lon1 <- rep(-73.985, nrow(data))
  
  # Convert all coordinates to radians at once
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- data$locLat * pi / 180
  lon2 <- data$locLong * pi / 180
  
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  r <- 3959
  return(r * c)
}

# Function to time execution in seconds
time_execution <- function(func, data, num_runs = 5) {
  times <- numeric(num_runs)
  for(i in 1:num_runs) {
    start_time <- Sys.time()
    func(data)
    end_time <- Sys.time()
    times[i] <- as.numeric(end_time - start_time)
  }
  return(mean(times))
}

# Time each implementation
cat("Average execution times (in seconds):\n")
cat("For loop implementation:", time_execution(calc_distance_loop, clinic_data), "seconds\n")
cat("Sapply implementation:", time_execution(calc_distance_sapply, clinic_data), "seconds\n")
cat("Vectorized implementation:", time_execution(calc_distance_vectorized, clinic_data), "seconds\n")
