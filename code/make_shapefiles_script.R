# ====================================================================
# ====================================================================
# make_shapefiles_script:
#   creates 2 shapefiles, one of SF, other of tweets
# ====================================================================
# ====================================================================


# ====================================================================
# Load Libraries
# ====================================================================
library(rgdal)
library(dplyr)
library(sp)

# Script to convert data to shapefile

tweets <- read.csv("../data/tweets_df.csv")[2:6]



tweets <- tweets %>%
  filter(longitude > -123 & longitude < -122,
         latitude > 37 & latitude < 38)

# ====================================================================
# Convert to SpatialPointsDataFrame (how R handles point processes)
# ====================================================================

# make SPDF
tweet_spdf <- tweets
coordinates(tweet_spdf) = ~ longitude + latitude
class(tweets) # SPDF
# Specify CRS (geographic CRS):
proj4string(tweet_spdf) = CRS("+proj=longlat +datum=WGS84")

plot(tweet_spdf)


# ====================================================================
# Read in shapefile of SF
# ====================================================================
setwd("~/Desktop/SF-Twitter-Sentiment-Map/data")
sf_shapefile <- readOGR(dsn = ".", layer = "planning_neighborhoods")

# ====================================================================
# Reproject SF shapefile into same CRS as tweets
# ====================================================================
newProj <- "+proj=longlat +datum=WGS84"
sf <- spTransform(sf_shapefile, newProj)


plot(sf, col='tomato')
plot(tweet_spdf, add=T, col='skyblue1')

# ====================================================================
# Keep only tweets inside polygon
# ====================================================================

over_tweets <- over(tweet_spdf, sf)
in_bnds <- which(!is.na(over_tweets))
tweets_spdf <- tweet_spdf[in_bnds,]

plot(sf, col='tomato')
plot(tweets_spdf,add=T, col='skyblue1', pch=20)

# ====================================================================
# Save as shapefiles
# ====================================================================

# save tweets
writeOGR(tweets_spdf, "../data", "tweet_spdf", "ESRI Shapefile")
# save sf shapefile
writeOGR(sf, "../data", "sf_shape", "ESRI Shapefile")

# call below to read back in
#tweet_spdf <- readOGR("../data", "tweet_spdf", stringsAsFactors = FALSE)


# ====================================================================
# Distance stuff
# ====================================================================

# Step 1: Get park ids
public_parks <- data.frame(longitude=numeric(), latitude= numeric()) 
public_parks[1,] <- c(-122.4271,37.7598)
public_parks[2,] <- c(-122.4276, 37.7916)
public_parks[3,] <- c(-122.4061,37.7771)

# step 2: Get distance between parks and each tweet
tweets$latitude <- as.numeric(tweets$latitude)
tweets$longitude <- as.numeric(tweets$longitude)

distMat <- distm(public_parks, tweets[2:3], fun=distHaversine)


# Step 3: Find k-Closest Tweets:
library(FastKNN)
k=3
n=3
nn = matrix(0,n,k) # n x k
for (i in 1:n)
  nn[i,] = k.nearest.neighbors(i, distMat, k = k)

nn
# Step 4: Now we have indices of k-closest tweets.
# We can determine if more of them are positive or negative. We expect positive

# Create piecharts/histograms for various things (pubs/ public parks, etc.) detailing
# whether or not closer tweet seniment scores are happier or sadder






# Step 2: Convert park to SpatialPointsDataFrame
coordinates(public_parks) = ~ longitude + latitude
proj4string(public_parks) = CRS("+proj=longlat +datum=WGS84")
plot(sf)
plot(tweets_spdf,add=T)
plot(public_parks, pch=8, col='tomato', add=T)


# =========================================
# Ideas
# =========================================

# cluster pos vs negative
# counties pos vs negative (over)
# Find lon/lat of specific parks
# find distances between tweets and them
# create histograms showing number of negative sentiments or positive sentiments there