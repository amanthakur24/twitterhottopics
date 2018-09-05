##
## Load Libraries

library(twitteR)
library(base64enc)
library(ggmap)

### Tweet Bank Script. Saves RDS files ###
# Connect Twitter API
my_setup <- function() {
  ckey = " use twitter a/c key "
  csec = " use twitter a/c key "
  akey = " use twitter a/c key "
  asec = " use twitter a/c key "
  setup_twitter_oauth(ckey, csec, akey, asec)  
}

# Login API
my_setup()  


# Location and Radius(Km)
# key <- "AIzaSyAHbKWGMwqv0GJhaVj1XKMXFZeLdzzXK" 
# google_geocode(address = "Dublin ,Ireland", key = key)
loc <-   geocode("Dublin, Ireland")


radius<- 100
ntweets<- 50000


# Retrieve Tweets
tweets_pull <-searchTwitter("dublin", n = ntweets, lang ='en',
                            #resultType="popular",
                            since='2018-08-10',
                            until='2018-08-23',
                            geocode=paste(loc$lat,loc$lon,
                                          paste0(radius, "km"),
                                          sep=","))

# make data frame
df_tweets <- twListToDF(tweets_pull)    
# do.call("rbind", lapply(tweets_pull, as.data.frame))


# Save your corpus
# save(df_tweets, file = "~/dublin2_tweets.Rdata")
saveRDS(df_tweets, file = "~/dublin2_01jul23aug.Rds", compress = 'xz')
