library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)

# Create a temporary file
unemp_path <- tempfile(fileext = ".rda")

# Download the data and save it to the path of the temporary file
# avoids having to install the package from Github
download.file(
  "https://is.gd/l57cNX",
  destfile = unemp_path)

# Load the data. The data is now available as 'unemp'
load(unemp_path)

glimpse(unemp)

filtered_unemp <- unemp %>%
  filter(
    level == "Commune",
    place_name %in% c("Luxembourg", "Esch-sur-Alzette", "Wiltz")
  )

glimpse(filtered_unemp)

nested_unemp <- filtered_unemp %>%
  group_nest(place_name)

nested_unemp

nested_unemp %>%
  mutate(nrows = map(data, nrow))

nested_unemp %>%
  mutate(nrows = map_int(data, nrow))

nested_unemp %>%
  mutate(nrows = map(data, \(x)filter(x, year == 2015)))

lux_data <- nested_unemp %>%
  filter(place_name == "Luxembourg") %>%
  unnest(data)

ggplot(data = lux_data) +
  theme_minimal() +
  geom_line(
    aes(year, unemployment_rate_in_percent, group = 1)
  ) +
  labs(title = "Unemployment in Luxembourg")

make_plot <- function(x, y){
  ggplot(data = x) +
    theme_minimal() +
    geom_line(
      aes(year, unemployment_rate_in_percent, group = 1)
    ) +
    labs(title = paste("Unemployment in", y))
}

nested_unemp <- nested_unemp %>%
  mutate(plots = map2(
    .x = data, # column of data frames
    .y = place_name, # column of commune names
    .f = make_plot
  ))