# ================================================================
# plot_tweets_script:
# Objective:
#    Creat two plots:
#    Plot 1: Tweets over map of SF
#    Plot 2: Tweets over map of SF, color-coded by sentiment
# ================================================================

# ================================================================
# Load libraries
# ================================================================
library(dplyr)
library(maps)
library(mapproj)
library(ggmap)
library(ggthemes)
library(ggthemr)

# ================================================================
# Load in data
# ================================================================
data <- read.csv("../data/datasofar.csv")

# ================================================================
# Plot1: Tweets over San Francisco
# ================================================================

# get map of San Francisco
SF1 <- get_map(location = "San Francisco", zoom = 14, maptype = "roadmap",
               source = "google")

# Plot

png("../images/SF_Tweets_Plot.png", 400, 400)
ggmap(SF1) +geom_point(aes(x=as.numeric(longitude), 
                           y=as.numeric(latitude)), data=data, col='red', pch=19) 
dev.off()



