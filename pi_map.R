library(tidyverse)
library(data.table)
library(rnaturalearth)
library(rnaturalearthdata)
library(maptools)
library(sf)
library(SOmap)
library(ggforce)
library(gganimate)

digits <- fread("pi_billion.txt")
colnames(data)
class(data$V1)
nchar(data$V1)

data_split <- stringi::stri_sub(digits$V1, seq(1, stringi::stri_length(digits$V1),by=11), length=11)
data_split[1]

test_set <- data_split[1:1000000]
test_list <- as.list(test_set)

test_list_split <- lapply(test_list, function(x) {stringi::stri_sub(x, seq(1, stringi::stri_length(x),by=5), length=5)}  )
#test_list_split2 <- lapply(test_list_split, function(x) {stringi::stri_sub(x[3], seq(1, stringi::stri_length(x[3]),by=3), length=3)}  )
test_df_list <- lapply(test_list_split, function(x) {data.frame(x_seq = x[1], y_seq = x[2], color = x[3], stringsAsFactors = FALSE)})
#test_df_list2 <- lapply(test_list_split2, function(x) {data.frame(z_seq = x[1], color = x[2], stringsAsFactors = FALSE)})

test_df_final <- bind_rows(test_df_list, .id = "id")
#test_df2 <- bind_rows(test_df_list2)

#test_df_final <- cbind(test_df, test_df2)
test_df_final[] <- lapply(test_df_final, as.numeric)

lat_direct <- scales::rescale(test_df_final$y_seq, to = c(-90, 90))
lon_direct <- scales::rescale(test_df_final$x_seq, to = c(-180, 180))

loc_direct <- data.frame(lon = lon_direct, lat = lat_direct)

data("wrld_simpl")
pts <- SpatialPoints(loc_direct, proj4string=CRS(proj4string(wrld_simpl)))
## Find which points fall over land
ii <- !is.na(over(pts, wrld_simpl)$FIPS)
loc_direct$land <- ii


countries <- ne_countries(returnclass = "sf")
world_proj <- st_transform(countries, crs = 54009)

points2 <- SOproj(lon = loc_direct$lon, lat = loc_direct$lat, data = loc_direct$land, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0+ellps=WGS84 +datum=WGS84 +units=m")

points2 <- as.data.frame(points2)

names(points2) <- c("land", "x", "y")

points2$color <- test_df_final$color
colors <- c("#E01C70", "#6820A5", "#910599", "#31A79B", "#7BD657", "#B9E128", "#FFB3FD", "#FB9D4F", "#51ADFF", "#9239F6")

points3 <-
  points2 %>%
  filter(land) %>%
  mutate(color = as.factor(color))

#saving points because it's slow to get them
#saveRDS(points3, "pi_1mil_sets.rds")
points3 <- readRDS("pi_1mil_sets.rds")

points4 <- points3[1:30000, ]
points4 <- 
  points4 %>% 
  mutate(row = row_number())

ggplot() +
  geom_sf(data = world_proj, fill = "NA", col = "NA", lwd = 0) +
  geom_point(data = points4, aes(x, y, color = color), stroke = 0, alpha = 0.7, size = 1) +
  scale_color_manual(values = c("#E01C70", "#6820A5", "#910599", "#31A79B", "#7BD657", "#B9E128", "#FFB3FD", "#FB9D4F", "#51ADFF", "#9239F6"), guide = FALSE) +
  theme_minimal() +
  labs(title = "Digits of pi projected to the globe", subtitle = "Digit: ") +
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid.major=element_line(colour="transparent"),
        panel.background = element_rect(fill = "#14183D", color = "NA"), plot.background = element_rect(fill = "#14183D", color = "NA"), 
        panel.border = element_blank(), plot.title = element_text(color = "white"), plot.subtitle = element_text(color = "white"))

ggsave("test_png_1.png", device = "png", type = "cairo", height = 8, width = 10)


test <- 1:10
paste0("gif_frames/", "filename", str_pad(test, 3, pad="0"), ".png")

for(i in 1:300) {
  p <- 
    ggplot() +
    geom_sf(data = world_proj, fill = "NA", col = "NA", lwd = 0) +
    geom_point(data = points4[1:(i*100), ], aes(x, y, color = color), stroke = 0, alpha = 0.7, size = 1) +
    scale_color_manual(values = c("#E01C70", "#6820A5", "#910599", "#31A79B", "#7BD657", "#B9E128", "#FFB3FD", "#FB9D4F", "#51ADFF", "#9239F6"), guide = FALSE) +
    theme_minimal() +
    labs(title = "Digits of pi projected to the globe", subtitle = paste0("Digit: ", i*100*11)) +
    theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid.major=element_line(colour="transparent"),
          panel.background = element_rect(fill = "#14183D", color = "NA"), plot.background = element_rect(fill = "#14183D", color = "NA"), 
          panel.border = element_blank(), plot.title = element_text(color = "white"), plot.subtitle = element_text(color = "white"))
  
  ggsave(paste0("gif_frames/", "filename-", str_pad(i, 3, pad="0"), ".png"), p, device = "png", type = "cairo", height = 8, width = 10)
}



testing <- 
  points4 %>%
  dplyr::select(-land) %>%
  slice(1:10) %>%
  mutate(num_repeats = rev(row)) %>%
  group_by(row) %>%
  expand(count = seq(row:10), nesting(x, y, color)) %>%
  mutate(count = count + row - 1) %>%
  filter(!(row == 10 & count > 10)) %>%
  ungroup()

ggplot() +
  geom_sf(data = world_proj, fill = "NA", col = "NA", lwd = 0) +
  geom_point(data = testing, aes(x, y, color = color, group = 1), alpha = 0.7, size = 1) +
  scale_color_manual(values = c("#E01C70", "#6820A5", "#910599", "#31A79B", "#7BD657", "#B9E128", "#FFB3FD", "#FB9D4F", "#51ADFF", "#9239F6"), guide = FALSE) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid.major=element_line(colour="transparent"),
        panel.background = element_rect(fill = "#14183D", color = "NA"), plot.background = element_rect(fill = "#14183D", color = "NA"), panel.border = element_blank()) +
  transition_time(time = count) +
  ggtitle("Digits of pi: {frame_time*11}")

points5 <- 
  points4 %>%
  dplyr::select(-land) %>%
  mutate(num_repeats = rev(row)) %>%
  group_by(row) %>%
  expand(count = seq(row:10000), nesting(x, y, color)) %>%
  mutate(count = count + row - 1) %>%
  filter(!(row == 10000 & count > 10000)) %>%
  ungroup()

#animation?
pi_20000 <-
  ggplot() +
  geom_sf(data = world_proj, fill = "NA", col = "NA", lwd = 0) +
  geom_point(data = points5, aes(x, y, color = color, group = 1), alpha = 0.7, size = 1) +
  scale_color_manual(values = c("#E01C70", "#6820A5", "#910599", "#31A79B", "#7BD657", "#B9E128", "#FFB3FD", "#FB9D4F", "#51ADFF", "#9239F6"), guide = FALSE) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid.major=element_line(colour="transparent"),
        panel.background = element_rect(fill = "#14183D", color = "NA"), plot.background = element_rect(fill = "#14183D", color = "NA"), 
        panel.border = element_blank(), plot.title = element_text(size = 16, color = "white")) +
  transition_time(time = count) +
  ggtitle("Digits of pi: {frame_time*11}")

animate(pi_20000, device = "png", type = "cairo", renderer = gifski_renderer(loop = FALSE, width = 10, height = 8))

ggplot(world_project) +
  geom_sf()

ggplot() +
  geom_sf(data = world) +
  geom_point(data = loc_direct, aes(x = x, y = y, color = land), size = .01)
## Check that it worked

class(world)

ggplot() +
  geom_sf(data = world) +
  geom_point(data = loc_direct, aes(x = x, y = y), size = .01, color = "red") +
  theme_void()


#########old stuff
mean_y <- mean(test_df_final$y_seq)
mean_x <- mean(test_df_final$x_seq)

lat <- -90 + 180*(test_df_final$y_seq - mean_y)/9999
lon <- -180 + 360*(test_df_final$x_seq - mean_x)/9999

loc_test <- data.frame(x = lon, y = lat)
test_df_final$x_seq <- test_df_final$x_seq - 4999
test_df_final$y_seq <- test_df_final$y_seq - 4999
test_df_final$z_seq <- test_df_final$z_seq - 499

#project point to unit sphere
mag <- sqrt(test_df_final$x_seq^2 + test_df_final$y_seq^2 + test_df_final$z_seq^2)
?norm
lat <- asin(test_df_final$z_seq/mag)
lon <- atan2(test_df_final$y_seq, test_df_final$x_seq)
test_matrix <- as.matrix(test_df_final[, 2:3])
test_df_st <- sf::st_multipoint(test_matrix, dim = "XY")
sf_dat <- sf::st_sfc(test_df_st, crs = 4326)
sf_dat2 <- sf::st_transform(sf_dat, crs = 4326)
sf::st_geometry(sf_dat)

??st_transform
??st_sfc
