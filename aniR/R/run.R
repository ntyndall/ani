library(plumber)
library(magrittr)

p <- getwd() %>%
  paste0("/demo/api.R") %>%
  plumber::plumb() %>%
  `$`("run")

# Run on port..
p(port = 8003)
