library(fields)
library(gganimate)

#Read in pi string and turn into vector
piChar <- read.table("data/PI_10000.txt", stringsAsFactors=F, colClasses = c("character"))[1,1]
piVec <- as.numeric(strsplit(piChar, "")[[1]])

#Starting at number 3 at position 0,0
x <- y <- rep(NULL, length(piVec))
x[1] <- 0
y[1] <- 0

#Calculate new position for each digit, based on the position of the old digit and the 
#angle is determined by the digit itself
for (i in 2:length(piVec)){
  x[i] <- x[(i-1)] + sin((pi*2)*(piVec[i]/10))
  y[i] <- y[(i-1)] + cos((pi*2)*(piVec[i]/10))  
}

#color schemes
rainbowColDark <- c("#EFB605","#EB8612","#DD0030","#B3005A","#80348F","#3365AA","#07A071","#7EB852")
rainbowColDark <- designer.colors(n=10, col=rainbowColDark)

#Save all information in a data frame for plotting with ggplot2
Pi.frame <- data.frame(PI=piVec[-1], x=x[-length(x)], y=y[-length(y)], 
                       ID=1:(length(x)-1), stringsAsFactors=F)

##THIS WORKS##
#Color according to position in pi, thus looping throug the chosen colors once
piPlot <- ggplot(Pi.frame[1:nrow(Pi.frame),], aes(x=x, y=y, group = "1")) +
  geom_path(aes(color = ID), size=0.7) + 
  scale_colour_gradientn(colours = rainbowColDark) +
  coord_fixed(ratio = 1) + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_blank(),
        title = element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank()) +
        transition_reveal(id = "1", along = ID)

animate(piPlot, nframes = 100, fps = 10, type = "cairo", renderer = gifski_renderer(loop = FALSE))

##THIS DOESNT WORK##
piPlot + view_follow()