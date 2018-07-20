
# Get Lat / Long factors
# The zoom scale has a different effect depending on the
# longitude and latitude, this script was used to generate
# the normalised factors going from 0, 10, .. 70 in lat / long

# My measured distances for a constant latitutde
myMeasure <- c(2.6, 2.6, 2.7, 3, 3.36, 4, 5.2, 7.6)

# total distance
totDist <- 14.1

# The value that represented a major tick
representOnScale <- 0.01

# Get all values
values <- totDist %>%
  `/`(myMeasure) %>%
  `*`(representOnScale)

# Normalise by the first one
values %<>% `/`(values[1])

# Values taken from 0 to -70 in latitude
ds <- data.frame(
  x = seq(0, 70, length = 8),
  y = values,
  stringsAsFactors = FALSE
)

# Calculate a polynomial of order 3 fit
# This is the full range so the fit should be okay!
centerFit <- lm(y ~ poly(x, 3), data = ds)

# Plot just to make sure
plot(ds)
mySeq <- seq(0, 70, length = 1000)
lines(mySeq, predict(centerFit, data.frame(x = mySeq)), col='blue')

# Need to uncomment this to rewrite
# save(centerFit, getwd() %>% paste0("/data/centerFit.rda"))
