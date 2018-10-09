# ani
A simple API built using _plumber_ to investigate open data sources in Northern Ireland. Install the package by running the following in R.
```r
library(devtools)

devtools::install_github(
  repo = "ntyndall/ani/aniR"
)
```
You can run the API locally using
```shell
Rscript -e "aniR::run()"
```
which defaults to port $8003$. A list of available endpoints are documented in the swagger docs accessible via **localhost/8003/__swagger__/**.
