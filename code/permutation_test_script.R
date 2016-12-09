# ====================================================================
# Load Libraries
# ====================================================================
library(rgdal)
library(dplyr)
library(sp)
library(ggplot2)
library(ggthemes)
# Script to convert data to shapefile

tweets <- read.csv("../data/tweets_df.csv")[2:6]



tweets <- tweets %>%
  filter(longitude > -123 & longitude < -122,
         latitude > 37 & latitude < 38)




# ====================================================================================
# Distance stuff
# ====================================================================================


# ====================================================================================
# Step 1: Create data.frame of public park locations
# ====================================================================================
public_parks <- data.frame(longitude=numeric(), latitude= numeric()) 
public_parks[1,] <- c(-122.4271,37.7598)
public_parks[2,] <- c(-122.4276, 37.7916)
public_parks[3,] <- c(-122.4061,37.7771)
public_parks[4,] <- c(-122.4862, 37.7694)
public_parks[5,] <- c(-122.4697, 37.7873)
public_parks[6,] <- c(-122.4194, 37.7193)
public_parks[7,] <- c(-122.4810, 37.7183)
# plot them 



# ====================================================================================
# step 2: Get distance between parks and each tweet
# ====================================================================================
tweets$latitude <- as.numeric(tweets$latitude)
tweets$longitude <- as.numeric(tweets$longitude)

distMat <- distm(public_parks, tweets[2:3], fun=distHaversine)

# ====================================================================================
# Step 3: Find k-Closest Tweets:
# ====================================================================================
library(FastKNN)
# Get 10 closest tweets
k=10
n=4
nn = matrix(0,n,k) # n x k
for (i in 1:n) {
  nn[i,] = k.nearest.neighbors(i, distMat, k = k)
}
nn

plot(tweets$sentiment[nn[3,]],type='l', pch=23, lty=2,ylim=c(-1,1))
lines(tweets$sentiment[nn[3,]])
lines(tweets$sentiment[nn[2,]], col='red')


# ====================================================================================
# Step 4: Create vector where closest tweets are true, else value is false
# ====================================================================================
trt_vec <- vector(mode="logical", length=ncol(distMat))
trt_vec[nn] <- TRUE

# ====================================================================================
# Step 5: Create Experiment dataset: sen
# ====================================================================================
experiment_data <- data.frame(sentiment=as.numeric(tweets$sentiment), 
                              trt=trt_vec)

# ====================================================================================
# EDA of experiment data
# ====================================================================================
boxplot(sentiment~trt, data=experiment_data)

# ====================================================================================
# Step 6: Calculate test statistic: mean sentiment score is the same between groups
# ====================================================================================

diffMeans <- function(data, hasTrt){
  # computes our test statistics: the difference of means
  # hasTrt: boolean vector, TRUE if has treatment
  test_stat <- mean(data[hasTrt]) - mean(data[!hasTrt])
  return(test_stat)
}

currentStat <- diffMeans(experiment_data$sentiment, experiment_data$trt)

# ====================================================================================
# Step 7: Bootstrap
# ====================================================================================
simPermDsn <- function(data, hasTrt, testStat, k=100){
  # Simulates the permutation distribution for our data
  # hasTrt: boolean indicating whether group is treatment or control
  # testStat: function defining the test statistic to be computed
  # k: # of samples to build dsn.      
  sim_data   <- replicate(k, sample(data))
  test_stats <- apply(sim_data, 2,
                      function(x) {testStat(x, hasTrt)})
  return( test_stats)
}

permutation_results <- simPermDsn(data=experiment_data$sentiment, 
                                  hasTrt=experiment_data$trt, 
                                  testStat=diffMeans,
                                  k=5000)

# ====================================================================
# Visualize Bootstrapped results
# ====================================================================
hist(permutation_results)
abline(v=currentStat)

# same plot as above but much prettier
pr <- as.data.frame(permutation_results)
ggplot(pr, aes(x=permutation_results)) + 
  geom_histogram(fill='skyblue3', alpha=.8) +
  geom_vline(xintercept = currentStat, colour='tomato', alpha=.9, size=1) +
  theme_economist() + scale_color_economist() +
  labs(title="Distribution of Bootstrapped Test Statistics: 20 Nearest Tweets", xlab = "Difference of Means", ylab="Count")

# ====================================================================================
# Step 8: Get Actual P-Value
# ====================================================================================

permutationTest <- function(data, hasTrt, testStat, k=5000){
  # Performs permutation test for our data, given some pre-selected
  # test statistic, testStat
  currentStat    <- testStat(data, hasTrt)
  simulatedStats <- simPermDsn(data, hasTrt,testStat, k)
  
  # 2-way P-value
  pValue         <- sum(abs(simulatedStats) >= currentStat)  / k
  
  return(pValue)
}

pval <- permutationTest(experiment_data$sentiment, 
                        experiment_data$trt,
                        diffMeans)
cat("P-Value: ", pval)

# ====================================================================================
# Repeat above, but for nearest-neighbor values of 5, 10, 15, 20
# ====================================================================================

# People are happier near public parks :)


# We can determine if more of them are positive or negative. We expect positive

# Create piecharts/histograms for various things (pubs/ public parks, etc.) detailing
# whether or not closer tweet seniment scores are happier or sadder
