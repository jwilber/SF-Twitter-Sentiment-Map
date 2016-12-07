# Nearest Neighbor Interpolation

library(gstat)
library(ggplot2)
library(dplyr)
library(raster)

# =========================================
# Load in data
# =========================================
tweet_spdf <- readOGR("../data", "tweet_spdf", stringsAsFactors = FALSE)

sf <- readOGR("../data", "sf_shape", stringsAsFactors = FALSE)

# =========================================
# Create Grid over SF
# =========================================
x.range <- as.integer(range(tweets_spdf@coords[,1]))
y.range <- as.integer(range(tweets_spdf@coords[,2]))

plot(tweets_spdf)
plot(sf, add=T)

# use locator to click and get axis
#locator(2)

x.range <- c(-122.3485,-122.5284)
y.range <- c(37.70548,37.83853)


grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=-.001), 
                   y=seq(from=y.range[1], to=y.range[2], by=.001))

coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE
proj4string(grd) = CRS("+proj=longlat +datum=WGS84")

# =========================================
# Plot Grid
# =========================================
plot(grd, cex=1.5)
plot(sf,add=T)

points(tweets_spdf, pch=1, col='red', cex=.1)
title("Interpolation Grid and Sample Points")

# =========================================
# Inverse Distance Weighting
# =========================================


idw<-idw(formula=sentiment ~ 1, locations=tweets_spdf, newdata=grd)

idw.output=as.data.frame(idw)
names(idw.output)[1:3]<-c("Longitude","Latitude","sent")
head(idw.output)

sf_outline <- fortify(sf, region="neighborho")
#

sf.contour <- sf # use this to clip
sf.contour.dfr <- fortify(sf.contour, region = "neighborho") # use this to plot


idw.r <- rasterFromXYZ(idw.output[, c("Longitude", "Latitude", "sent")])
idw.crp <- crop(idw.r, sf.contour)
idw.msk <- mask(idw.crp, sf.contour)
idw.msk.dfr <- as.data.frame(rasterToPoints(idw.msk))
names(idw.msk.dfr)[1:2] <- c("Longitude", "Latitude") 


# =========================================
# Plots
# =========================================

# good plot
ggplot() + geom_tile(data = idw.msk.dfr, 
                     alpha = 1,
                     aes(x = Longitude,
                         y = Latitude,
                         fill = sent)) + 
  scale_fill_gradient(low = "black", high = "cyan") +
  geom_path(data = sf.contour.dfr, aes(long, lat, group = group), color = "darkgrey") +
  #geom_point(data = as.data.frame(tweets_spdf), aes(x = longitude, y = latitude), shape = 16, cex = 0.5, color = "red") + 
  labs(fill = "sentiment", title = "Inverse Distance Weighting Interpolation of Tweet Sentiment") +
  coord_equal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())




# shit plot

plot <- ggplot(data=idw.output,aes(x=Longitude,y=Latitude)) 
layer1<-c(geom_tile(data=idw.output,aes(fill=sent)))
layer2<-c(geom_path(data=sf_outline,aes(long, lat, group=group),colour = "grey40", size=1))
# now add all of the data together
plot+layer1+layer2+scale_fill_gradient(low="yellow", high="red")+coord_equal()

