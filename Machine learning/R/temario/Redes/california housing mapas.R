

load(file="c:/california housing.Rda")
library(viridis)
library(ggplot2)
qplot(longitude, latitude, data = cali,
      color = Median_House_value, size = I(0.5)) +
  scale_color_viridis()

