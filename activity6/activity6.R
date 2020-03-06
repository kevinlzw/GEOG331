#install.packages(c("raster","sp","rgdal","rgeos","plyr"))
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)


#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("Y:\\Students\\klian\\a06\\GNPglaciers\\GNPglaciers_1966.shp")
plot(g1966, axes = TRUE)
#str(g1966)
g1998 <- readOGR("Y:\\Students\\klian\\a06\\GNPglaciers\\GNPglaciers_1998.shp")
g2005 <- readOGR("Y:\\Students\\klian\\a06\\GNPglaciers\\GNPglaciers_2005.shp")
g2015 <- readOGR("Y:\\Students\\klian\\a06\\GNPglaciers\\GNPglaciers_2015.shp")
g1966@proj4string