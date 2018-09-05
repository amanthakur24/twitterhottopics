#
# Install necessary packages
install.packages("topicmodels", dependencies = TRUE)
install.packages("slam", dependencies = TRUE)
install.packages("Rmpfr", dependencies = TRUE)
install.packages("tm", dependencies = TRUE)
install.packages("stringr", dependencies = TRUE)
install.packages("ggmap", dependencies = TRUE)
install.packages("devtools", dependencies = TRUE)
install.packages("graph", dependencies = TRUE)
install.packages("Rgraphviz", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("wordcloud", dependencies = TRUE)
install.packages("twitteR", dependencies = TRUE)
install.packages("wordcloud2", dependencies = TRUE)
install.packages("syuzhet", dependencies = TRUE)
install.packages("lubridate", dependencies = TRUE)
install.packages("textstem", dependencies = TRUE)
install.packages("googleway", dependencies = TRUE)
install.packages("writexl", dependencies = TRUE)

source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz", dependencies = TRUE)



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
library(writexl)


#################################################################

# Read the dataset file 
df_tweets <- readRDS(" ")   # put path to .RDS file here

# Extract only text
tweets <- df_tweets[c("text")]

# ## Print Clean Data Frame##
# library(gridExtra)
# pdf("df_tweets.pdf", height=8, width=2.5)
# grid.table(df_tweets[1:5,])
# dev.off()
# write_xlsx(df_tweets,"./input df.xlsx")
# write_xlsx(as.data.frame(z1[1:4,1:5]),"./dtm.xlsx")
# ##

## Tweet Cleaning
# Make Corpus
myCorpus <- iconv(tweets$text, to = "utf-8")
myCorpus <- Corpus(VectorSource(tweets$text))

# Remove RT, @, etc.
myCorpus <- tm_map(myCorpus, gsub,
                   pattern = '(RT|via)((?:\\b\\W*@\\w+)+)',
                   #pattern = '@.|RT.|\\ã.|\\â.|\\¢.|\\¬.|\\å.|\\???.|\\°.|\\T.',
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
myStopwords <- c(stopwords('english'), stopwords('SMART'), c("r","use","dont", "see",
                                                             "used", "via", "amp", "i", "im","he","she","them",
                                                             "us", "we", "youre","ive","have","this","that","fuck","gonna"))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# Lemmatization of words
myCorpus <- tm_map(myCorpus, lemmatize_words)

#Term Document Matrix
tdm <- TermDocumentMatrix(myCorpus,
                          control = list(wordLengths = c(3, 100)))
tdm


# Frequent words
lf = 80  # lf between 20-100
(freq.terms <- findFreqTerms(tdm, lowfreq = lf))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= lf)
df <- data.frame(term = names(term.freq), freq = term.freq)

#Plot Common Words
ggplot(df, aes(x=term, y=freq)) +  ggtitle("Plot of high frequency words") +
  geom_bar(stat="identity") +
  xlab("Terms") + ylab("Frequency") + coord_flip() +
  theme(axis.text=element_text(size=9))

# #WordCloud
# m <- as.matrix(tdm)
# ms <- data.frame(rowSums(m))
# # calculate the frequency of words and sort it by frequency
# word.freq <- sort(rowSums(m), decreasing = T)
# # colors
# pal <- brewer.pal(9, "BuGn")[-(1:4)]
# 
# # plot word cloud
# wordcloud(words = names(word.freq), 
#           freq = word.freq, min.freq = 5,
#           random.order = F,
#           max.words = 200,
#           colors = brewer.pal(8, 'Set1'),
#           scale = c(8, 0.3),
#           rot.per = 0.2)

# Find Word Associations
A1 <- findAssocs(tdm, freq.terms[1], 0.2)
A1
A2 <- findAssocs(tdm, freq.terms[2], 0.2)
A2


# Construct Directed Graph
plot(tdm, term = freq.terms, corThreshold = 0.5,
     weighting = FALSE, cex.lab = 1.5,
     attrs = list(graph = list(rankdir = "BT"),
                  node = list(shape = "rectangle",
                              fixedsize = FALSE)))

# LDA Input Calculation
BigramTokenizer <-function(x)
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)


dtm <- DocumentTermMatrix(myCorpus,
                          control = list(removeNumbers = TRUE,
                                         wordLengths = c(3, 100),
                                         minDocFreq = 1,
                                         # tokenize = BigramTokenizer,
                                         sparse = .80))
dtm
# Find the sum of words in each Document
rowTotals <- apply(dtm , 1, sum)
#remove all docs without words
dtm   <- dtm[rowTotals> 0, ]


# LDA Model
seedNum <- 1234
burnin = 1000
iter = 1000
keep = 50
m <- 12      # number of topics
t <- 10      # top t terms displayed
lda <- LDA(dtm, k = m,  method = "Gibbs",
           control = list(burnin = burnin, iter = iter,
                          keep = keep, seed=seedNum), cpus = 4) 
term <- terms(lda, t) 
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))
topics <- topics(lda) # 1st topic identified for every document (tweet)


## For visualization with LDAvis
## Document-Topic
lda.topics <- as.data.frame(topics(lda))
lda.topics$id <- seq.int(nrow(lda.topics))
write.csv(lda.topics,file=paste("topic_model",m,"DocsToTopics.csv"))

## Topic Term
lda.terms <- transpose(as.data.frame(terms(lda,t)))
lda.terms$id <- seq.int(nrow(lda.terms))
write.csv(lda.terms,file=paste("topic_model",m,"TopicsToTerms.csv"))
lda.terms[1:t,]

## Tweet-Topic-Terms Matrix
a1<- lda.terms[lda.topics[,1],1:t]
lda.topics$terms <- a1[,1:t]
a2 <- as.data.frame(df_tweets[lda.topics$id,1])
a3 <- cbind(a1,a2,lda.topics$`topics(lda)`)
a3 <- a3[c(t+1,t+2,1:t)]


## Visualizations
# Plot Topics
df_tweets2 <- as.data.frame(df_tweets[lda.topics$id,])
topics2 <- data.frame(date=as.IDate(df_tweets2$created), topic=topics)
qplot(date, ..count.., data=topics2, geom="density",
      fill=term[topic], position="stack")

# LDA Vis
topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE),
    R=2*t
  )
}

serVis(topicmodels2LDAvis(lda))

# Document Topic Matrix
gammaDF <- as.data.frame(lda@gamma) 
names(gammaDF) <- c(1:8)
# inspect...
gammaDF

# Now for each doc, find just the top-ranked topic   
toptopics <- as.data.frame(cbind(document = row.names(gammaDF), 
                                 topic = apply(gammaDF,1,
                                               function(x) names(gammaDF)[which(x==max(x))])))
# inspect...
toptopics   

## Performance
# Log Likelihood
logLik(lda)

# Perplexity
myCorpus_test = myCorpus[4500:5000]
dtm_test = DocumentTermMatrix(myCorpus_test,
                              control = list(wordLengths = c(3, 80),
                                             sparse = TRUE))

# Find the sum of words in each Document
rowTotals <- apply(dtm_test , 1, sum)
#remove all docs without words
dtm_test   <- dtm_test[rowTotals> 0, ]
perplexity(lda, dtm_test )




# ## Model 2
# ## CTM Model
##############
ctm = CTM(dtm, k=8, method="VEM", cpus = 8)
theta1 <- posterior(ctm)[["topics"]] %>% as.matrix
ctm1 = colnames(theta1)[apply(theta1,1,which.max)]
terms(ctm)
topics(ctm)
logLik(ctm)

# Find the sum of words in each Document
rowTotals <- apply(dtm_test , 1, sum)
#remove all docs without words
dtm_test   <- dtm_test[rowTotals> 0, ]
perplexity(ctm, dtm_test)







##
## Sentiment Analysis
##
cleanset <- myCorpus

# Term document matrix---------------
tdm <- TermDocumentMatrix(cleanset)
tdm

# Sparsity is presense of zeros
tdm <- as.matrix(tdm)

# Print Sub Matrix
tdm[1:10, 1:20]

# Bar plot
w <- rowSums(tdm)
w <- subset(w, w>=80)
barplot(w,
        las = 2,
        legend.text = 'Frequency',
        main = 'Bar plot of High Frequency Terms',
        xlab = 'Terms',
        ylab = 'Frequency',
        col = 'black')

# Word cloud 2
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)

w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 1,
           shape = 'square',
           rotateRatio = 0.5,
           minSize = 3)

# Sentiments Calculation
# Obtain sentiment scores
cleanset2 <- iconv(cleanset, to = "utf-8")
s <- get_nrc_sentiment(cleanset2)
head(s)

# Tweet--Sentiment Matrix
c1 <- get_nrc_sentiment(cleanset$content)

# Create Final Output Matrix and Excel
TS_DF <- data.frame(c(df_tweets$text),c(df_tweets$longitude),c(df_tweets$latitude),c(c1))
a4 <- as.data.frame(TS_DF[lda.topics$id,])
Final <-cbind(a3, a4)

# Reset Row Indices
rownames(Final) <- 1:length(a4$c.df_tweets.text.)

# Rename Columns
# Final$`df_tweets[lda.topics$id, 1]` <- NULL
Final_Loc <- Final[!is.na(Final$c.df_tweets.longitude.),]
# names(Final)[6]<-"topic number"
# names(Final)[7]<-"tweet"
# names(Final)[8]<-"longitude"
# names(Final)[9]<-"latitude"

# Write Output to .xlsx file
write_xlsx(Final,"./Tweets-Sentiments-TM.xlsx")


# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(7),
        ylab = 'Sentiment Frequency',
        main = 'Aggregate Sentiment Scores for Geolocational Tweets')


