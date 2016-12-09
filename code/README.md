## code/

***

### Scripts

**permutation_test_script.R**

Defines dataframe of public park locations
Test that computes permutation test for our data to answer the following question: 
 - Is sentiment different near public parks?

**inverse_distance_weighting_script.R**

Performs inverse distance weighting interpolation on our data. Outputs plot to image folder.

**point_pattern_analysis_script.R**

Creates `ppp` data structures (to be used with the `spatstat` package) and computes point pattern analysis methods:

  - Kernel Density Estimation
  - K-Function
  - G-Function
  

**make_shapefiles_script.R**

Script that converts tweet data into shapefile with same CRS as the San Francisco shapefile.

**sentiment_script.R**

Calculates sentiment score for tweets.

**mongo_script.R** 

Script that loads streamed tweets from a given location to a mongodb database. The tweets are then sorted for geotagged tweets and subsetted as necessary.

**hashtag_script.R**

Details scraping tweets from the Twitter API that contain a selected hashtag



**plot_tweets_script.R**

Plots tweets over map of San Francisco

**stream_script.R**

Details streaming tweets from Twitter API into R

#### To be added

- sentiment classifier script
- buffer creater zip
- counter script
- clustering scripts
 
