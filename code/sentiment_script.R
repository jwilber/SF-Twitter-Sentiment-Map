# sentiment
library(sentimentr)

# =========================================
# Load data
# =========================================
tweets             <- read.csv("../data/datasofar.csv")[2:4]

# =========================================
# remove duplicate values
# =========================================
tweets             <- tweets[!duplicated(tweets),]
# =========================================
# Calculate sentiment metrics
# =========================================
sentiment          <- sentiment_by(tweets$text)
sent_score         <- sentiment$ave_sentiment
sent_binary        <- sent_score > 0
sent_bin2          <- sent_binary + 1
tweets$sentiment   <- sent_score
tweets$sent_label <- c("Negative","Positive")[sent_bin2]

# =========================================
# preview dataframe
# =========================================
tweets[1001:1009,]

# =========================================
# save new datafrane
# =========================================
write.csv(tweets, file="../data/tweets_df.csv")
