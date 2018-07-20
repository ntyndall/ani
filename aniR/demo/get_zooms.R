
## Zoom details

# My measured distances of zoom for a constant latitutde / longitude
myMeasure <- c(1.6, 3.2, 6.4, 12.8, 26, 52, 102.5)

# total distance
totDist <- 14.1

# The value that represented a major tick
representOnScale <- 0.1

# Get all values
values <- totDist %>%
  `/`(myMeasure) %>%
  `*`(representOnScale) %>%
  `/`(2)

# This is full ticks, but only considering half a graph so we divide by two at the end
save(values, file = "~/Documents/ani/aniR/data/zoomVals.rda")
