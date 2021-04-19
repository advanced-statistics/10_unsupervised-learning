# Set Seed
set.seed(1234)

# Load library
library(tidyverse)
library(GA)

# Create Data
item <- c(
  "raincoat",
  "pocket knife",
  "mineral water",
  "gloves",
  "sleeping bag",
  "tent",
  "portable stove",
  "canned food",
  "snacks"
)
weight <- c(2, 1, 6, 1, 4, 9, 5, 8, 3)
survival_point <- c(5, 3, 15, 5, 6, 18, 8, 20, 8)

data <- tibble(item, weight, survival_point)

# Create Chromosom Representation
chromosomes <- c(0, 1, 1, 0, 0, 0, 0, 0, 1)

# Check Item
data[chromosomes == 1, ]

# Check Survival Point
chromosomes %*% data$survival_point

# Set limit parameter
max_weight <- 25

# Create fitness Function
fitness <- function(x) {
  current_survpoint <- x %*% data$survival_point
  current_weight <- x %*% data$weight
  if (current_weight > max_weight) {
    return(0)
  }
  else {
    return(current_survpoint)
  }
}

# Create Genetic Algorithm Model
GA <- ga(
  type = "binary",
  fitness = fitness,
  nBits = nrow(data),
  maxiter = 100,
  popSize = 50,
  seed = 1234,
  keepBest = TRUE
)

# See Model Summary
summary(GA)

# Visualize Model
plot(GA, legend = FALSE)
