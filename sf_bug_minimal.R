library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

#country data
countries <- ne_countries(returnclass = "sf")
world_proj <- st_transform(countries, crs = 54009)

#points, already projected to same crs
points <- data.frame(
        land = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
                 FALSE),
           x = c(-66.89695, 143.28066, 51.63533, -76.16409, 177.77862,
                 171.00648, 101.41291, -147.60461, 117.14617, 107.37859),
           y = c(76.77662, -47.098278, -30.987178, 84.906883, -70.977776,
                 -7.352523, -78.711968, 23.036947, -51.915945, -3.458423)
)

#doesnt work
ggplot() +
  geom_sf(data = world_proj, fill = "grey80", col = "grey40", lwd = 0.3) +
  geom_point(data = points, aes(x, y, color = land)) +
  theme_minimal() +
  theme(axis.text = element_blank())

ggsave("moll_plot.png", device = "png", type = "cairo")

#works for the points, but the projection is wrong now
ggplot() +
  geom_sf(data = countries, fill = "grey80", col = "grey40", lwd = 0.3) +
  geom_point(data = points, aes(x, y, color = land)) +
  theme_minimal() +
  #coord_sf(crs = 54009) +
  theme(axis.text = element_blank())
