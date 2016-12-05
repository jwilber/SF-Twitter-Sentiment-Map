# ================================================================
# hashtag_script:
# Objective:
#    Use twitter api to scrape tweets based on a particular hashtag.
#    
# *Note* - this script is no longer used in this analysis
# ================================================================


# ================================================================
# Load libraries
# ================================================================
library(twitteR)
library(data.table)
library(dplyr)




# ================================================================
# Set up API credentials
# ================================================================

consumer_key        <- "v5mHBeEJs9PuKK13pERMiVvH8"
consumer_secret     <- "LKYwNzc9WNrUK2mclUzbSjhKqJxO6YanNaGZvytra0q5HQeXBc"
access_token        <- "796875093456494592-yj2jsH547MlGYpLt81ZBtaelicZTJDr"
access_secret       <- "4Ga8kVBYrugyCK9KyETKSCoWosvRTvueSQyENPLcd2Ppv"
options(httr_oauth_cache=T) #This will enable the use of a local file to cache OAuth access credentials between R sessions.
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

# ================================================================
# Get tweets only in SF
# ================================================================
sfggptweets = searchTwitter("#goldengatepark", n=200, geocode='37.7609,-122.4350,4mi')


ggp <- rbindlist(lapply(sfggptweets, as.data.frame))
ggp$longitude <- as.numeric(ggp$longitude); ggp$longitude[is.na(ggp$longitude)] = 0
ggp$latitude <- as.numeric(ggp$latitude); ggp$latitude[is.na(ggp$latitude)] = 0
ggp$longitude <- -122.4862
ggp$longitude <- jitter(ggp$longitude, .005)
ggp$latitude <- 37.7695
ggp$latitude <- jitter(ggp$latitude, .005)

ggp <- data.frame(ggp)[,c(1,15,16)]
  
# ================================================================
# Save tweets
# ================================================================
#write.csv(ggp, file = "~/Desktop/tweets/goldengatepark.csv")


