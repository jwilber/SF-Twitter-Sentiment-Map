# ================================================================
# mongo_script:
# Objective:
#    Initialize mongodb database and stream tweets from twitter api.
#    Store tweets in the database, subset what we want, and save them.
# ================================================================

# ================================================================
# Load library
# ================================================================
library(streamR)
library(ROAuth)
library(mongolite)
library(jsonlite)
library(ggplot2)
library(ggmap)
library(dplyr)

# ================================================================
# Initialize mongodb database
# ================================================================
m <- mongo(collection = "tweets")

# ================================================================
# Load api credentials
# ================================================================
load("my_oauth.Rdata")

# ================================================================
# Stream tweets: set up streaming parameters
#   - More detailed streaming done in stream_script.R
# ================================================================
tweets_file  <- "tweets.json"
track        <- NULL
follow       <- NULL
loc          <- c(-122.75,36.8,-121.75,37.8)
lang         <- NULL
minutes      <- 1
time         <- minutes*15
tweets       <- NULL
filterStream(file.name = file, 
             track = track,
             follow = follow, 
             locations = loc, 
             language = lang,
             timeout = time, 
             tweets = tweets, 
             oauth = my_oauth,
             verbose = TRUE)

# ================================================================
# Insert tweets into mongodb database
# ================================================================
m$insert(parseTweets(tweets_file))

# ================================================================
# Take out only geotagged tweets
# ================================================================
tweets <- m$find('{"lon": { "$ne": null } }', 
              fields = '{"lat": 1, "lon":1, "text":1 }')


# ================================================================
# Remove #job or #hiring tweets, as these are worthless
# ================================================================
nonjobs <- grep("^((?!#[Jj]ob|#[Hh]iring).)*$", tweets$text, perl=T)
tweets  <- tweets[nonjobs,]

# ================================================================
# Plot new tweets
# ================================================================
data <- read.csv("datasofar.csv")[,2:4]
SF1  <- get_map(location = "San Francisco", zoom = 14, maptype = "roadmap",
               source = "google")
ggmap(SF1) + 
  geom_point(aes(x=as.numeric(lon), y=as.numeric(lat)), data=tweets, col='purple', pch=19)


# ================================================================
# Change names of columns so they can be combined with previous data
# ================================================================
tweets        <- tweets[,2:4]
names(tweets) <- c("text", "latitude", "longitude")

# ----------------------------
# Load previous data and combine it with new data
# ----------------------------
data <- read.csv("datasofar.csv")
data <- data[,2:4]
data <- rbind(data,tweets)
write.csv(data, file="datasofar.csv")

#data <- subset(data, longitude < -122.3 & longitude > -122.7)
#data <- subset(data, latitude < 38 & latitude > 37.6)
#write.csv(data, file = "datasofar.csv")
#data <- rbind(data, tweets)

# ----------------------------
# Plot all tweets
# ----------------------------
SF1 <- get_map(location = "San Francisco", zoom = 14, maptype = "roadmap",
               source = "google")
ggmap(SF1) +geom_point(aes(x=as.numeric(longitude), 
                           y=as.numeric(latitude)), data=data, col='red', pch=19) +
  geom_point(aes(x=as.numeric(lon), y=as.numeric(lat)), data=tweets, col='green', pch=19, cex=2)

# ================================================================
# close mongodb connection
# ================================================================
m$drop()



# ================================================================
# Work to do still
# ================================================================

# create sentiment analysis column
# create quadrats
# take average score within each quadrat
# color code base on quadrat value
# find long/lat for parks
# create bufferzone around parks
# is the average score in bufferzones different than the average score outside?
   # how to phrase this as a hypothesis test? doI just select random parts?

# To test clusters:
# - k-function
# - L-function
# Permutatoin Test
