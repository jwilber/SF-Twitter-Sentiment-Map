# ======================================================
# ======================================================
# Point Pattern Analysis Script
# ======================================================
# ======================================================


library(spatstat)
# ======================================================
# Load in data
# ======================================================

tweet_spdf <- readOGR("../data", "tweet_spdf", stringsAsFactors = FALSE)

sf <- readOGR("../data", "sf_shape", stringsAsFactors = FALSE)

# subset out only tweets in San Francisco
tweets_sf <- tweet_spdf[sf,]

# plot to make sure
plot(sf, col='coral')
plot(tweets_sf, col='blue', add=T, pch=11,cex=.2)

# ======================================================
# Create ppp object (for use via spatstat)
# ======================================================
window <- as.owin(sf)
plot(window)

tweets.ppp <- ppp(x=tweets_sf@coords[,1],y=tweets_sf@coords[,2],window=window)

# plot to inspect
plot(tweets.ppp,pch=16,cex=0.5, main="Tweets")

# ======================================================
# Kernel Density Estimation
# ======================================================
plot(density(tweets.ppp, sigma = .02195), main="Kernel Density Estimation of Tweets")

# ======================================================
# Stienen Diagram
# ======================================================
plot(tweets.ppp %mark% (nndist(tweets.ppp)/2), markscale = 3,
     main = "Stienen Diagram of Tweets")


# ======================================================
# Analyze Distribution of Positive and Negative Tweets
# ======================================================
tweet_df <- read.csv("../data/tweets_df.csv")
tweet_pos <- tweet_df[which(tweet_df$sent_label=="Positive"),]
tweet_neg <- tweet_df[which(tweet_df$sent_label=="Negative"),]

# make into SpatialPointsDataFrames
coordinates(tweet_pos) = ~ longitude + latitude
proj4string(tweet_pos) = CRS("+proj=longlat +datum=WGS84")
coordinates(tweet_neg) = ~ longitude + latitude
proj4string(tweet_neg) = CRS("+proj=longlat +datum=WGS84")

# ppp
tweet_pos.ppp <- ppp(x=tweet_pos@coords[,1],y=tweet_pos@coords[,2],window=window)
tweet_neg.ppp <- ppp(x=tweet_neg@coords[,1],y=tweet_neg@coords[,2],window=window)

# ======================================================
# G-Function
# ======================================================
par(pty = "s")
# G
G_pos <- Gest(tweet_pos.ppp)
G_neg <- Gest(tweet_neg.ppp)
G_all <- Gest(tweets.ppp)
par(mfrow=c(1,2))
plot(G_pos, main="G-Functin of Positive Tweets")
plot(G_neg, main="G-Function of Negative Tweets")

# ======================================================
# K-Function
# ======================================================
# set up data for tweets: must have < 3000 points in ppp data structure

K_pos <- Kest(tweet_pos.ppp)
K_neg <- Kest(tweet_neg.ppp)



tweets_k <- tweets_sf[sample(nrow(tweets_sf),700),]
k.ppp <- ppp(x=tweets_k@coords[,1],y=tweets_k@coords[,2],window=window)
par(pty = "s")
Ktweets <- Kest(k.ppp)
par(mfrow=c(1,2))
plot(Ktweets, main="K-Function of Tweets")

#
plot(split(tweets.ppp))


# =========================================
# Ideas
# =========================================

# Describe data process
# Classify Sentiment
# Plot tweets
# point process stuff
# k-function for both positive and negative tweets
# g-function for both positive and negative tweets
# inverse distance weighting interpolation
# Investigate public park thesis


#
