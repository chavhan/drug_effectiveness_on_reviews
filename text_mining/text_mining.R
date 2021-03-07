############## Text Mining ###########################
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(tm)
library(slam)
library(topicmodels)
################ df having values which are clean, no na values and id has been deleted and false condition also 
############# have been eliminated #######################################

df <- rbind(drug,drug1)
dim(df)
df <- df[-1]
names(df)
df <- drop_na(df)
df <- df[- grep("</span>", df$condition),]
status(df)
#write.csv(as.data.frame(freq(df$review)),"review.csv", row.names = FALSE)
#str(df)
#write.csv(df,"clean_data.csv", row.names = FALSE)
df_short <- filter(df,df$usefulCount > 10)
status(df_short)
dim(df_short)
ind <- sample(2,nrow(df_short),replace = T, prob = c(0.95,0.05))
df_extra_short <- df_short[ind == 2,]
dim(df_extra_short)
corpus <- Corpus(VectorSource(unique(df_extra_short$review)))

# clean up the corpus using tm_map()
corpus_clean <- tm_map(corpus, (tolower))
inspect(corpus_clean,10)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
stopwords("english")
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
inspect(corpus_clean)
corpus_clean <- tm_map(corpus_clean, removePunctuation)
#inspect(corpus_clean)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
inspect(corpus_clean)

rm(tdm)
# create a term-document sparse matrix
#tdm <- TermDocumentMatrix(corpus_clean, 
#                          control = list(minWordLength=c(4,Inf),
#                                        removePunctuation = TRUE,
#                                        stopwords = TRUE,
#                                         removeNumbers = TRUE))
tdm <- TermDocumentMatrix(corpus_clean,
                                  control = list(minwordLength = c(1,Inf)))
?TermDocumentMatrix
dim(tdm)
findFreqTerms(tdm, lowfreq = 2)
#Barplot
termFrequency <- rowSums(as.matrix(tdm))
?rowSums
library(ggplot2)
library(RColorBrewer)
termFrequency <- subset(termFrequency, termFrequency>=10)
str(termFrequency)
termFrequency <- sort(termFrequency, decreasing = T)
barplot(termFrequency[1:20],las=2, col = rainbow(20))

#######################word cloud ############################################
library(wordcloud)
m <- as.matrix(tdm)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
str(m)
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq = 2, random.order = F, 
          colors=rainbow(20),scale=c(8, 0.3))
?wordcloud
pos=scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg=scan(file.choose(), what="character", comment.char=";")
neg# read-in negative-words.txt
pos.words=c(pos,"wow", "kudos", "hurray")
pos.words
pos.matches = match(names(wordFreq), pos.words)
pos.matches = !is.na(pos.matches)
freq_pos <- wordFreq[pos.matches]
barplot(freq_pos[1:20],las=2, col = rainbow(20))
p_names <- names(freq_pos)
p_names
wordcloud(p_names, freq=wordFreq, min.frq = 10, random.order = F, colors=rainbow(20))


neg.matches = match(names(wordFreq), neg)
neg.matches = !is.na(neg.matches)
freq_neg <- wordFreq[neg.matches]
str(freq_neg)
n_names <- names(freq_neg)
head(sort(freq_neg, decreasing = T))
barplot(freq_neg[1:20],las=2, col = rainbow(20))
wordcloud(n_names, freq=wordFreq, min.freq = 10, random.order = F, colors=rainbow(20))
#################### sentiments analaysis ##########################
dtm <- DocumentTermMatrix(corpus_clean)
dim(dtm)     ## 6407 14058
str(dtm)
######## Getting top10 terms 
rowTotals <- apply(dtm , 1, sum)
rowTotals1 <- rowSums(as.matrix(dtm))
rowTotals
dtm.new   <- dtm[rowTotals> 0, ]
dim(dtm.new)
(rowTotals)
library(NLP)
lda <- LDA(dtm.new, 10) # find 10 topics
class(lda)
term <- terms(lda, 10) # first 5 terms of every topic
term

####################### Emotion mining ##############################
library("syuzhet")

str(corpus_clean)
#my_example_text <- readLines(obama_tweets)
s_v <- get_sentences(df_extra_short$review)
class(s_v)
str(s_v)
(s_v)


sentiment_vector <- get_sentiment(s_v, method = "bing")
head(sentiment_vector)
sentiment_vector

afinn_s_v <- get_sentiment(s_v, method = "afinn")
head(afinn_s_v)

nrc_vector <- get_sentiment(s_v, method="nrc")
head(nrc_vector)
sentiment_vector
sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

# plot
plot(sentiment_vector[1:200], type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")

# To extract the sentence with the most negative emotional valence
sort_sentiment_vector <- sort(sentiment_vector, decreasing = T)
negative <- s_v[which.min(sentiment_vector)]
negative
##################### sorting of sentiment vector for top 5 netgative and positive ################
sort_s_vector <- order(sentiment_vector)
########## Top five negative sentiments #####################
s_v[sort_s_vector[1:5]]
#################### Top five positive sentiments ###############
s_v[sort_s_vector[39726:39730]]
# and to extract the most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive

# percentage based figures
percent_vals <- get_percentage_values(sentiment_vector)
length(percent_vals)
plot(
  percent_vals, 
  type="l", 
  main="Percentage-Based", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)

ft_values <- get_transformed_values(
  sentiment_vector, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

head(ft_values)

plot(
  ft_values, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)

# categorize each sentence by eight emotions
nrc_data <- get_nrc_sentiment(s_v)
dim(nrc_data)

# subset

sad_items <- which(nrc_data$sadness > 0)
head(s_v[sad_items])


# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:10)

