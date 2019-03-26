### idea to make hexagons, circles, or others pop in randomly to match pi
### inspiration is a candy store
### looking for bright rainbow colors
### slow at first, speed up, then slow down again ease_aes(something)
### not sure about shapes/transparency
### sonify data (collab with ian duh?)
### map characteristics from following digit or two digits?
library(ggplot2)
library(fields)
library(tidyverse)
library(gganimate)
library(ggforce)

piChar <- read.table("data/PI_10000.txt", stringsAsFactors=F, colClasses = c("character"))[1,1]
piVec <- as.numeric(strsplit(piChar, "")[[1]])

x <- y <- rep(NULL, length(piVec))
x[1] <- 0
y[1] <- 0

for (i in 2:length(piVec)){
  x[i] <- x[(i-1)] + sin((pi*2)*(piVec[i]/10))
  y[i] <- y[(i-1)] + cos((pi*2)*(piVec[i]/10))  
}

Pi.frame <- tibble(PI=piVec[-1], x=x[-length(x)], y=y[-length(y)], 
                       ID=1:(length(x)), stringsAsFactors=F)

##for testing
pi_vec <- piVec[1:10000]
x1 <- y1 <- rep(NULL, length(piVec))
x1[1] <- 0
y1[1] <- 0
for (i in 2:length(pi_vec)){
  x1[i] <- x[(i-1)] + 2*sin((pi*2)*(pi_vec[i]/10))
  y1[i] <- y[(i-1)] + 2*cos((pi*2)*(pi_vec[i]/10))  
}

pi_df <- data.frame(my_pi=pi_vec[-1], x=x1[-length(x1)], y=y1[-length(y1)], 
                       id=1:(length(x1)-1), stringsAsFactors = FALSE) %>%
  mutate(size = my_pi*2) %>%
  mutate(speed = as.integer(2*my_pi))

candy_color <- c("#F9FB21", "#FF9C59", "#f8a500", "#f46b85", "#FF4545", "#63FF6E", "#1dc875", "#1CBDEC", "#1f83ff", "#9933FF")
rainbowColDark <- c("#EFB605","#EB8612","#DD0030","#B3005A","#80348F","#3365AA","#07A071","#7EB852")
rainbowColDark <- designer.colors(n=10, col=rainbowColDark)

pi_plot <- ggplot(pi_df[1:nrow(pi_df),], aes(x=x, y=y, group = "1")) +
  geom_point(aes(color = id, size = size, alpha = speed)) + 
  scale_size_continuous(range = c(3, 9)) +
  scale_alpha_continuous(range = c(0.3, 0.6)) +
  scale_colour_gradientn(colours = candy_color) +
  coord_fixed(ratio = 1) + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_blank(),
        title = element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank()) +
        transition_events(start = id, enter_length = speed, exit_length = as.integer(1000)) +
        enter_grow() +
        exit_fade() +
        view_follow()

animate(pi_plot, nframes = 400, fps = 10, type = "cairo")
anim_save("bubble_snake.gif")


pi_plot2 <- ggplot(pi_df[1:nrow(pi_df),], aes(group = "1")) +
  geom_circle(aes(fill = id, r = size/10, x0 = x, y0 = y, alpha = speed), color = "black", n=6) + 
  scale_alpha_continuous(range = c(0.3, 0.6)) +
  scale_fill_gradientn(colours = candy_color) +
  coord_fixed(ratio = 1) + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_blank(),
        title = element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank()) +
  transition_events(start = id, enter_length = speed, exit_length = as.integer(1000)) +
  enter_grow() +
  exit_fade() 

animate(pi_plot2, nframes = 300, fps = 10, type = "cairo")

