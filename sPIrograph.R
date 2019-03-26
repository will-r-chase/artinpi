library(tidyverse)
library(data.table)
library(ggforce)
library(gganimate)

digits <- fread("data/PI_100000.txt")
colnames(data)
class(data$V1)
nchar(data$V1)

data_split <- stringi::stri_sub(digits$V1, seq(1, stringi::stri_length(digits$V1),by=4), length=4)
data_split[1]

test_set <- data_split[1:50]
test_list <- as.list(test_set)

test_list_split <- lapply(test_list, function(x) {stringi::stri_sub(x, seq(1, stringi::stri_length(x),by=1), length=1)}  )
#test_list_split2 <- lapply(test_list_split, function(x) {stringi::stri_sub(x[3], seq(1, stringi::stri_length(x[3]),by=3), length=3)}  )
test_df_list <- lapply(test_list_split, function(x) {data.frame(R = x[1], r = x[2], d = x[3], outer = x[4], stringsAsFactors = FALSE)})
#test_df_list2 <- lapply(test_list_split2, function(x) {data.frame(z_seq = x[1], color = x[2], stringsAsFactors = FALSE)})

test_df_final <- bind_rows(test_df_list, .id = "id") 
#test_df2 <- bind_rows(test_df_list2)

#test_df_final <- cbind(test_df, test_df2)
test_df_final[] <- lapply(test_df_final, as.numeric)

colors = c("#E01C70", "#6820A5", "#910599", "#31A79B", "#7BD657", "#B9E128", "#FFB3FD", "#FB9D4F", "#51ADFF", "#9239F6")

pi_df <- 
  test_df_final %>%
  mutate(outer = ifelse(outer < 5, FALSE, TRUE), 
         R = R + 1, r = r + 1, d = d + 1, color = sample(colors, 50, replace = TRUE)) 

keep <- c(12, 2, 4, 8, 15, 19, 20, 22, 23, 25, 31, 32, 34, 48)

pi_df2 <- 
  pi_df %>%
  filter(id %in% keep)

ggplot() +
  geom_spiro(data = pi_df2, aes(R = R, r = r, d = d, outer = outer, color = color, group = id), size = 1) +
  theme_void() +
  scale_color_identity() +
  facet_wrap( ~id) +
  coord_equal() +
  theme(plot.background = element_rect(color = "NA", fill = "#14183D"), strip.background = element_blank(),
        strip.text.x = element_blank())

ggsave("sPIrograph_50.png", device = "png", type = "cairo", height = 20, width = 20)

ggplot() +
  geom_spiro(aes(R = 9, r = 2, d = 2, outer = TRUE)) +
  theme_void()


# create normal ggplot object
# (note: setting group = "some constant" wouldn't have change anything here;
# each row of data would still correspond to a different group value...)
p <- ggplot() +
  geom_spiro(data = pi_df2, aes(R = R, r = r, d = d, outer = outer, color = color), size = 1) +
  theme_void() +
  scale_color_identity() +
  coord_equal() +
  theme(plot.background = element_rect(color = "NA", fill = "#14183D"), strip.background = element_blank(),
        strip.text.x = element_blank())

# get layer data created by geom_spiro; sort the rows just in case
data2 <- layer_data(p) %>%
  select(x, y, colour, group, index) %>%
  arrange(group, index)

# use group as the transition variable, & set the actual group aesthetic
# to some constant
anim <- 
  ggplot(data2,
       aes(x = x, y = y, group = "a")) +
  geom_path(aes(color = colour), size = 1) + 
  theme_void() +
  scale_color_identity() +
  coord_equal() +
  theme(plot.background = element_rect(color = "NA", fill = "#14183D"), strip.background = element_blank(),
        strip.text.x = element_blank()) +
  transition_states(states = group, transition_length = 3, state_length = 3) +
  ease_aes("back-in-out")

animate(anim, nframes = 300, detail = 3, fps = 10, device = "png", type = "cairo", height = 700, width = 700)
anim_save("spiroband.gif")
