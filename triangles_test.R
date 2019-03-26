library(deldir)
library(sf)
library(sp)
library(tidyverse)

#idea sameple pi in groups of 3 digits, each digit is an angle, sum is size (or 1st num is size)

piChar <- read.table("data/PI_10000.txt", stringsAsFactors=F, colClasses = c("character"))[1,1]
piVec <- as.numeric(strsplit(piChar, "")[[1]])












################################################
data <- data.frame(x = runif(20), y = runif(20))
x <- data[,"x"]
y <- data[,"y"]
pts = SpatialPointsDataFrame(cbind(x, y), data, match.ID = T)
vor_desc = tile.list(deldir(pts@coords[, 1], pts@coords[, 2],
                            suppressMsge = TRUE))
vor_polygons <- lapply(1:(length(vor_desc)), function(i) {
  tmp <- cbind(vor_desc[[i]]$x, vor_desc[[i]]$y)
  tmp <- rbind(tmp, tmp[1, ])
  Polygons(list(Polygon(tmp)), ID = i)
})
rownames(pts@data) = sapply(slot(SpatialPolygons(vor_polygons),
                                 "polygons"), slot, "ID")
vor_spdf = SpatialPolygonsDataFrame(SpatialPolygons(vor_polygons),
                                    data = pts@data)

polys <- fortify(vor_spdf, id = "ID")
ggplot(polys, aes(x = long, y = lat)) + geom_polygon()

triangles<-triang.list(deldir(points, plotit = TRUE))
for(tri in triangles) {
  polygon(as.list(tri))
}

tri_list <- rbind_list(as.list(triangles))

ggplot(data = tri_list, aes(x = x, y = y)) + geom_polygon()

?tile.list
