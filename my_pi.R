### idea to make hexagons, circles, or others pop in randomly to match pi
### inspiration is a candy store
### looking for bright rainbow colors
### slow at first, speed up, then slow down again ease_aes(something)
### not sure about shapes/transparency
### sonify data (collab with ian duh?)
### map characteristics from following digit or two digits?
library(ggplot2)
library(tidyverse)
library(gganimate)

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
                       ID=1:(length(x)-1), stringsAsFactors=F)

##for testing
pi_vec <- piVec[1:100]
for (i in 2:length(pi)){
  x[i] <- x[(i-1)] + sin((pi*2)*(pi[i]/10))
  y[i] <- y[(i-1)] + cos((pi*2)*(pi[i]/10))  
}

pi_df <- data.frame(my_pi=pi_vec[-1], x=x[-length(x)], y=y[-length(y)], 
                       id=1:(length(x)-1)) %>%
  mutate(size = my_pi*3) %>%
  mutate(speed = as.integer(2*my_pi))

candy_color <- c("#F9FB21", "#FF9C59", "#f8a500", "#f46b85", "#FF4545", "#63FF6E", "#1dc875", "#1CBDEC", "#1f83ff", "#9933FF")
rainbowColDark <- c("#EFB605","#EB8612","#DD0030","#B3005A","#80348F","#3365AA","#07A071","#7EB852")
rainbowColDark <- designer.colors(n=10, col=rainbowColDark)

pi_plot <- ggplot(pi_df[1:nrow(pi_df),], aes(x=x, y=y, group = "1")) +
  geom_point(aes(color = id, size = size), alpha = 0.8) + 
  scale_size_continuous(range = c(min(pi_df$size), max(pi_df$size))) +
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
        transition_events(start = id, enter_length = speed, exit_length = as.integer(100)) +
        enter_fade() +
        enter_grow() +
        exit_fade()

animate(pi_plot, nframes = 300, fps = 10, type = "cairo")

+
  transition_reveal(id = "1", along = ID)
