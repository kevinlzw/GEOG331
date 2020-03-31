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
g1998 <- readOGR("Y:\\Students\\klian\\a06\\GNPglaciers\\GNPglaciers_1998.shp")
g2005 <- readOGR("Y:\\Students\\klian\\a06\\GNPglaciers\\GNPglaciers_2005.shp")
g2015 <- readOGR("Y:\\Students\\klian\\a06\\GNPglaciers\\GNPglaciers_2015.shp")
str(g2015)
#data stores all accompanying info/measurements for each spatial object
head(g2015@data)
#polygons stores the coordinates for drawing the polygons
g2015@polygons[[1]]
g1966@proj4string


spplot(g1966, "GLACNAME")

#check glacier names
g1966@data$GLACNAME

g2015@data$GLACNAME

#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))


#read in rgb imagery from landsat
redL <- raster("Y:\\Students\\klian\\a06\\glacier_09_05_14\\l08_red.tif")
greenL <- raster("Y:\\Students\\klian\\a06\\glacier_09_05_14\\l08_green.tif")
blueL <- raster("Y:\\Students\\klian\\a06\\glacier_09_05_14\\l08_blue.tif")

#check coordinate system
redL@crs

#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("Y:\\Students\\klian\\a06\\NDVI\\NDVI_",ndviYear[i],".tif"))
  
}

str(NDVIraster[[1]])

#get projection
NDVIraster[[1]]@crs

#####################################
#####Q3#####
#####################################
par(mfrow=c(1,2))
plot(NDVIraster[[1]], axes = TRUE)
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin", axes = TRUE)
plot(g1966, col="palegreen2", border=NA, add=TRUE)


#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

#####################################
#####Q4#####
#####################################
par(mfrow=c(1,1))
plot(NDVIraster[[13]], axes = FALSE)
plot(g2015p, border="BLACK", add=TRUE)


#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
}   

#####################################
#####Q5#####
#####################################
g2015p@data$Percent <- ((gAll$a1966m.sq - gAll$a2015m.sq) / gAll$a1966m.sq)
spplot(g2015p, "Percent")


diffPoly <- gDifference(g1966p, g2015p)
plot(diffPoly)



#####################################
#####Q6#####
#####################################

#subset the glacier with the largest % loss
gmaxlost1966 <- subset(g1966, g2015p@data$Percent == max(g2015p@data$Percent))
gmaxlost1998 <- subset(g1998, g2015p@data$Percent == max(g2015p@data$Percent))
gmaxlost2005 <- subset(g2005, g2015p@data$Percent == max(g2015p@data$Percent))
gmaxlost2015 <- subset(g2015, g2015p@data$Percent == max(g2015p@data$Percent))
par(mai=c(1,1,1,1))
par(xpd=T)
plotRGB(rgbL, ext=c(273000,275000,5426000,5429010), stretch="lin", axes=TRUE, main = "Boulder Glacier lost 84.72% of its area")
#add polygons to plot
plot(gmaxlost1966, col="tan3", border=NA, add=TRUE)
plot(gmaxlost1998, col="royalblue3", add=TRUE, border=NA)
plot(gmaxlost2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(gmaxlost2015, col="tomato3", add=TRUE, border=NA)
legend("bottomright", c("1966", "1998", "2005", "2015"),
       col=c("tan3","royalblue3", "darkgoldenrod4","tomato3"), pch=c(15))



#plot with NDVI
plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
plot(diffPoly,col="black", border=NA,add=TRUE)

#extract NDVI values
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}

plot(ndviYear, meanDiff, type="b",
     xlab= "Year",
     ylab="Average NDVI (unitless)",
     pch=19)


#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)


#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units

#convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)

#rasterize gralciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)

meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)

#####################################
#####Q9#####
#####################################

meanChange <- meanChange[2:40, 1:2]
g2015p@data$meanChange <- meanChange[,2]

spplot(g2015p, "meanChange")


#####################################
#####Q11#####
#####################################

NDVIavg <- calc(NDVIstack, mean)

#maximum
NDVIavg@data@max
NDVImean <- zonal(NDVIavg, glacZones, "mean")
NDVImean <- NDVImean[2:40, 1:2]
g2015p@data$NDVImean <- NDVImean[,2]
col <- c()
for(i in 1:length(g2015p@data$NDVImean)){
  if(g2015p@data$NDVImean[i] <=0.2){
    col <- c(col, "red")
  }
  else if(g2015p@data$NDVImean[i] > 0.2 & g2015p@data$NDVImean[i] <=0.4){
    col <- c(col, "brown")
  }
  else if(g2015p@data$NDVImean[i] > 0.4 & g2015p@data$NDVImean[i] <=0.6){
    col <- c(col, "cadetblue")
  }
  else if(g2015p@data$NDVImean[i] > 0.6 & g2015p@data$NDVImean[i] <=0.8){
    col <- c(col, "blue")
  }
  else if(g2015p@data$NDVImean[i] > 0.8 & g2015p@data$NDVImean[i] < 1){
    col <- c(col, "blue4")
  }
}
par(mai= c(0.5, 0.5, 0.5, 0.5))
g2015p@data$col <- col
plot(NDVIavg, axes = FALSE, ext =c(-110000,0,20000,120000))
plot(g2015p, add=TRUE, col=paste(g2015p@data$col),border=FALSE)
legend("bottomleft", c("mean < 0.2", "mean < 0.4", "mean < 0.6", "mean < 0.8","mean < 1"),
       col=c("red","brown", "cadetblue","blue","blue4"), cex = 0.8 ,pch=c(15), bty = "n")
