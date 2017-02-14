library(tidyverse)

wdi <- read_csv("wdi2016.csv")

countryNames <- read_csv("countryNames2016.csv")

indicatorNames <- read_csv("indicatorNames2016.csv")

wdiByIndicator <- function(data) {
  
  data %>%
    group_by(`Indicator Code`,Year)%>%
    summarise(numCountries = n_distinct(`Country Code`))
  
}
  
humanTime <- function() format(Sys.time(),"%Y%m%d-%H%M%OS")













