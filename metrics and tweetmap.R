##
## Load Libraries
library(twitteR)
library(base64enc)
library(ggmap)
library(googleway)
library(topicmodels)
library(slam)
library(Rmpfr)
library(tm)
library(textstem)
library(stringr)
library(devtools)
library(graph)
library(Rgraphviz)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(topicmodels) 
library(data.table)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(scales)
library(reshape2)
library(dplyr)
library(plyr)
library(LDAvis)
library(LDAvisData)
library(textmineR)




#################################################################

# Read twitter dataset file
df_tweets <- readRDS(" ") #put file path here

# Map Visualization of Tweets
Place <- "Louth, Ireland"
d <- subset(df_tweets, df_tweets$longitude>0 | df_tweets$longitude<0)
rownames(d) <- seq(length=nrow(d))
d <- d[,c("latitude", "longitude")]
d$count <- 6
d$longitude <- as.numeric(as.character(d$longitude))
d$latitude <- as.numeric(as.character(d$latitude))

PlacePlot <- get_map(Place, source="google",  zoom=8, maptype="toner")
p <- ggmap(PlacePlot)
p + geom_point(data=d, aes(x=d$longitude, y=d$latitude, color="point"), size=d$count, alpha=0.5) + 
  ggtitle('Tweet Map') + labs(colour = "Tweet Location")

# # Cache Map Data
# x <- ggmap(PlacePlot)
# cachemap <- tempfile()
# t <- saveRDS(x, file = cachemap)
# x <- readRDS(t)
# ggmap(x)





# Extract only text
tweets <- df_tweets[c("text")]


## Tweet Cleaning
# Make Corpus
myCorpus <- iconv(tweets$text, to = "utf-8")
myCorpus <- Corpus(VectorSource(tweets$text))

# Remove RT, @, etc.
myCorpus <- tm_map(myCorpus, gsub,
                   pattern = '(RT|via)((?:\\b\\W*@\\w+)+)',
                   replacement = '')

# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

myCorpus <- tm_map(myCorpus, gsub,
                   pattern = ':|@\\w+',
                   replacement = '')

myCorpus <- tm_map(myCorpus, gsub,
                   pattern = '[[:punct:]]',
                   replacement = '')

# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)

# convert to lower case
myCorpus <- tm_map(myCorpus, tolower)

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

# remove stopwords
myStopwords <- c(stopwords('english'), stopwords('SMART'), c("r","use", "see",
                                         "used", "via", "amp", "i", "im",
                                         "he","she","them","us", "we", "youre",
                                         "ive","have","this","that","fuck","gonna"))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# Lemmatization of words
myCorpus <- tm_map(myCorpus, lemmatize_words)



## Coherence with textmineR
# LDA with textmineR

cleanDF <- data.frame(text = sapply(myCorpus, as.character), stringsAsFactors = FALSE)
cleanDF$id <- seq.int(nrow(cleanDF))
cleanDF <- cleanDF[c("id", "text")]

dtm_rm <- CreateDtm(doc_vec = cleanDF, # character vector of documents
                 #doc_names = movie_review$id, # document names
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE,
                 ngram_window = c(1,2), # minimum and maximum n-gram length
                 verbose = TRUE, # Turn off status bar for this demo
                 cpus = 4) # default is all available cpus on the system

# # Sampling down to smaller DTM
# dtm_rm <- dtm_rm[ sample(1:nrow(dtm_rm), 500) , ]
# 
# # Find the sum of words in each Document
# rowTotals <- apply(dtm_rm , 1, sum)
# #remove all docs without words
# dtm_rm  <- dtm_rm[rowTotals> 0, ]

model1 <- FitLdaModel(dtm = dtm_rm, 
                      k = 10, 
                      iterations = 500,
                      alpha = 0.1, # this is the default value
                      beta = 0.05, # this is the default value
                      cpus = 4) 

model1$top_terms <- GetTopTerms(phi = model1$phi, M = 8)
head(t(model1$top_terms))


## Performance Calculations
# R-squared 
model1$r2 <- CalcTopicModelR2(dtm = dtm_rm, 
                             phi = model1$phi,
                             theta = model1$theta,
                             cpus = 4)
model1$r2

# Coherence Value
model1$coherence <- CalcProbCoherence(phi = model1$phi, dtm = dtm_rm, M = 100)
summary(model1$coherence)
hist(model1$coherence, 
     col= "blue", 
     main = "Histogram of probabilistic coherence")

# Log Likelihood (does not consider the prior) 
model1$ll <- CalcLikelihood(dtm = dtm_rm, 
                           phi = model1$phi, 
                           theta = model1$theta,
                           cpus = 2)

model1$ll

# Prevalence Calculation
model1$prevalence <- colSums(model1$theta) / sum(model1$theta) * 100
summary(model1$prevalence)

# textmineR has a naive topic labeling tool based on probable bigrams
model1$labels <- LabelTopics(assignments = model1$theta > 0.05, 
                            dtm = dtm_rm,
                            M = 2)
head(model1$labels)
# put them together, with coherence into a summary table
model1$summary <- data.frame(topic = rownames(model1$phi),
                            label = model1$labels,
                            coherence = round(model1$coherence, 3),
                            prevalence = round(model1$prevalence,3),
                            top_terms = apply(model1$top_terms, 2, function(x){
                              paste(x, collapse = ", ")
                            }),
                            stringsAsFactors = FALSE)
model1$summary[ order(model1$summary$prevalence, decreasing = TRUE) , ][ 1:10 , ]

## For visualization with LDAvis
##
lda.topics <- as.matrix(topics(model1))
write.csv(lda.topics,file=paste("topic_model",m,"DocsToTopics.csv"))
lda.terms <- as.matrix(terms(model1,t))
write.csv(lda.terms,file=paste("topic_model",m,"TopicsToTerms.csv"))
lda.terms[1:t,]
topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

serVis(topicmodels2LDAvis(lda))


###### CTM ########
cleanDF2 <- cleanDF[1:100,]
dtm_rm_2 <- CreateDtm(doc_vec = cleanDF2, # character vector of documents
                    #doc_names = movie_review$id, # document names
                    lower = TRUE, # lowercase - this is the default value
                    remove_punctuation = TRUE, # punctuation - this is the default
                    remove_numbers = TRUE,
                    ngram_window = c(1, 1), # minimum and maximum n-gram length
                    verbose = TRUE, # Turn off status bar for this demo
                    cpus = 4) # default is all available cpus on the system

# Find the sum of words in each Document
rowTotals <- apply(dtm_rm_2 , 1, sum)
#remove all docs without words
dtm_rm_2   <- dtm_rm_2[rowTotals> 0, ]
model2 <- FitCtmModel(dtm=dtm_rm_2, k=8)

## Performance Calculations
# R-squared 
model2$r2 <- CalcTopicModelR2(dtm = dtm_rm_2, 
                              phi = model2$phi,
                              theta = model2$theta,
                              cpus = 4)
model2$r2

# Coherence Value
model2$coherence <- CalcProbCoherence(phi = model2$phi, dtm = dtm_rm, M = 10)
summary(model2$coherence)
hist(model2$coherence, 
     col= "blue", 
     main = "Histogram of probabilistic coherence")

# Log Likelihood (does not consider the prior) 
model2$ll <- CalcLikelihood(dtm = dtm_rm, 
                            phi = model2$phi, 
                            theta = model2$theta,
                            cpus = 2)

model2$ll

# Prevalence Calculation
model2$prevalence <- colSums(model2$theta) / sum(model2$theta) * 100
summary(model2$prevalence)
