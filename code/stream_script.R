# ================================================================
# stream_scrip:
# Objective:
#    Scrape tweets from twitter api as they're tweeted.
#    
# *Note* - This doesn't need to be run, mongo_script.R will
# run this process and store the tweets. This script shows the
# details involved left behind the scenes in mongo_script.R
# ================================================================


# ================================================================
# Load library
# ================================================================
library(streamR)
library(ROAuth)

# ================================================================
# Set up required api-values
# ================================================================
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "idOphOWLzt4s4FS9EKhsC4TuZ"
consumerSecret <- "3IOw5pBftmxzZOlMJmOAskDVrQGtoAVxfLkDO33SJTDm5gwsR5"

my_oauth <- OAuthFactory$new(consumerKey = consumerKey, 
                             consumerSecret = consumerSecret, 
                             requestURL = requestURL, 
                             accessURL = accessURL, 
                             authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))


# ================================================================
# Save above values so they can be called in mongo_script.R
# ================================================================
#save(my_oauth, file = "my_oauth.Rdata")


# ================================================================
# Stream tweets (R unusable while streaming)
# ================================================================
load("my_oauth.Rdata")
file = "tweets.json"
track = NULL
follow = NULL
loc = c(-122.75,36.8,-121.75,37.8)
lang = NULL
minutes = 1
time = minutes*15
tweets = NULL
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
# Collect tweets and get only geotagged tweets
# ================================================================
tweets.df6<- parseTweets(file)

tw6 <- tweets.df6 %>% filter(!is.na(lat) & !is.na(lon))
tw6 <- tw6[,c(1,4,5)]
# write.csv(tw6, file="~/Desktop/tweets/sfdata.csv")
# ================================================================
# End Script:
# What's below is an example of combining tweets and may be ignored
# ================================================================












# ================================================================
# Stuff to ignore that I don't want to lose yet
# ================================================================
data1 <- read.csv("~/Desktop/tweets/sfdata1.csv")[,c(2,16,17)]
data2 <- read.csv("~/Desktop/tweets/sfdata2.csv")[,c(2,40,41)]
data3 <- read.csv("~/Desktop/tweets/sfdata3.csv")[,c(2,40,41)]
data4 <- read.csv("~/Desktop/tweets/sfdata4.csv")[,c(2,40,41)]
data5 <- read.csv("~/Desktop/tweets/sfdata5.csv")[,c(2,40,41)]
data6 <- read.csv("~/Desktop/tweets/sfdata6.csv")[,c(2,40,41)]

names(data1)[2:3] = c("lon","lat")
dat <- rbind(data1,data2,data3,data4,data5)

data <- rbind(c(data1,data2,data3,data4,data5))
tweets.df <- parseTweets(file)
# Now we can inspect the table and save it.
View(tweets.df)





# collect tweets into database
# define quadrats
# capture sentiment of tweets, create a new column with this
#     - do it binary and continuous (sentimentr)
# plot this sentiment and color code based on sentiment

# Find public parks
# Create buffer zones around these
# Average sentiment (continous) scores OR take difference of positive - negative


# Map
# Create quadrats
# average score within quadrat - this is quadrats score
# create cholo




SF1 <- get_map(location = "San Francisco", zoom = 12, maptype = "roadmap",
               source = "google")
ggmap(SF1) +geom_point(aes(x=longitude, y=latitude), data=goldengate, col='red', pch=19)


names(goldengate)






par(mar=c(0,0,0,0))
m.proj <- map("usa", proj="albers", param=c(39, 45), col="#000000", fill=FALSE, bg=NA, lwd=0.4)
loc.proj <- mapproject(dat$lon, dat$lat)
pts <- as.points(list(x=loc.proj$x, y=loc.proj$y))
points(pts, pch=21, cex=0.1, col="#00000020")

blackWhite <- colorRampPalette(c("white", "black"))
pal <- colorRampPalette(c("#ef8a62","#67a9cf"))

m.proj <- map("usa", proj="albers", param=c(39, 45), col=NA, fill=FALSE, bg=NA, lwd=0.4)

xlim <- match(NA, m.proj[["x"]]) - 1
ylim <- match(NA, m.proj[["y"]]) - 1
uspoly.proj <- as.matrix(cbind(m.proj[["x"]][1:xlim], m.proj[["y"]][1:ylim]))
smoo.proj <- kernel2d(pts, uspoly.proj, h0=.01, nx=500, ny=500)

image(smoo.proj, uspoly.proj, add=TRUE, col=pal(10))


map("state", proj="albers", param=c(39, 45), col="#999999", fill=FALSE, bg=NA, lwd=0.2, add=TRUE, resolution=1)
