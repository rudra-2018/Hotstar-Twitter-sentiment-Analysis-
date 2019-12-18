library(fpp2)
options(repos = c(CRAN = "http://cran.rstudio.com"))
library(qdap)
text<- 'data analytics the sensation of mordern times,often misunderstood as only graphs and dashboards. Its
time for businesses to think beyond charts and stat levraging science behind the data'
freq_terms<-freq_terms(text,4)
plot(freq_terms)
##=============================================================
library(bit64)
library(twitteR)
library(ROAuth)
library(SnowballC)
library(tm)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(data.table)
library(stringi)
library(syuzhet)
library(dplyr)
library(plyr)
library(grid)
library(gridExtra)
setwd("F:\\Web & Social Media Analytics\\GA")
getwd()
api_key <- "#############################"
api_secret <- "##################################r"
access_token <- "################################################"
access_token_secret <- "########################################"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

search.string <- "#Hotstar"
no.of.tweets <- 2000
HS_tweets <- searchTwitter(search.string, n=no.of.tweets,lang="en")
n.tweet <- length(HS_tweets)
n.tweet

HS_tweets_df <- twListToDF(HS_tweets)
dim(HS_tweets_df)

View(HS_tweets_df)


setwd("F:\\Web & Social Media Analytics\\GA")
write.csv(HS_tweets_df,file=paste("HS_tweets_df.csv"))

tweets.df<-read.csv("HS_tweets_df.csv",stringsAsFactors = FALSE)
str(tweets.df)
nrow(tweets.df)
View(tweets.df)
########
HS_tweets <- tweets.df$text
head(HS_tweets)

#Convert created colum into Date format

tweets.df$created <- as.Date(tweets.df$created)

str(tweets.df)

# Create document corpus with tweet text
myCorpus<- Corpus(VectorSource(tweets.df$text))


#####convert to Lowercase  

myCorpus <- tm_map(myCorpus, content_transformer(stri_trans_tolower))
writeLines(strwrap(myCorpus[[804]]$content,60))


#####Remove the links (URLs)  

removeURL <- function(x) gsub("http[^[:space:]]*", "", x) 
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
writeLines(strwrap(myCorpus[[700]]$content,60))


#####Remove the @ (usernames)  

removeUsername <- function(x) gsub("@[^[:space:]]*", "", x)  
myCorpus <- tm_map(myCorpus, content_transformer(removeUsername))
writeLines(strwrap(myCorpus[[700]]$content,60))

#####Remove anything except the english language and space  

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)   
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
writeLines(strwrap(myCorpus[[700]]$content,60))

#replace string 

#removestr <- function(x) gsub("nzvind", "indvnz", x)
#myCorpus <- tm_map(myCorpus, content_transformer(removestr))
#writeLines(strwrap(myCorpus[[416]]$content))



##Remove stop words
myStopWords<- c((stopwords('english')),c("rt", "use", "used", "now","via", "will","amp","user","hotstar","hotst","uf","da"))
myCorpus<- tm_map(myCorpus,removeWords , myStopWords) 
writeLines(strwrap(myCorpus[[700]]$content))

#####Remove Single letter words  

removeSingle <- function(x) gsub(" . ", " ", x)   
myCorpus <- tm_map(myCorpus, content_transformer(removeSingle))
writeLines(strwrap(myCorpus[[1100]]))

#Remove extra whitespace
myCorpus<- tm_map(myCorpus, stripWhitespace)
writeLines(strwrap(myCorpus[[221]]))



#####keep a copy of "myCorpus" for stem completion later  
myCorpusCopy<- myCorpus

#####Creating a term document matrix
#tdm<- TermDocumentMatrix(myCorpus, control= list(wordLengths= c(1, Inf)))
tdm<- TermDocumentMatrix(myCorpus)
#tdm<-DocumentTermMatrix(myCorpus)
tdm

#####Find the terms used most frequently
(freq.terms <- findFreqTerms(tdm, lowfreq = 25))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 25)
df <- data.frame(term = names(term.freq), freq= term.freq)
df



#####Frequency analysis


(freq.terms <- findFreqTerms(tdm, lowfreq = 10))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 10)
df1 <- data.frame(term = names(term.freq), freq= term.freq)


(freq.terms <- findFreqTerms(tdm, lowfreq = 55))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 55)
df2 <- data.frame(term = names(term.freq), freq= term.freq)

(freq.terms <- findFreqTerms(tdm, lowfreq = 85))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 85)
df3 <- data.frame(term = names(term.freq), freq= term.freq)


#####plotting the graph of frequent terms

p1=ggplot(df1, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="@10", x="Terms", y="Term Counts")) + theme(axis.text.y = element_text(size=7))


p2=ggplot(df, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="@25", x="Terms", y="Term Counts"))+
  theme(axis.text.y = element_text(size=7))


p3=ggplot(df2, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="@55", x="Terms", y="Term Counts"))

p4=ggplot(df3, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="@85", x="Terms", y="Term Counts")) 


#####plotting the graph of frequent terms

grid.arrange(p1,p2,ncol=2)
grid.arrange(p3,p4,ncol=2)



#####calculate the frequency of words and sort it by frequency and setting up the Wordcloud

# Creating the wordcloud
set.seed(777)
word.freq <-sort(rowSums(as.matrix(tdm)), decreasing= TRUE)
#word.freq <-sort(rowSums(as.matrix(tdm)))
word.freq
pal<- brewer.pal(8, "Dark2")
#wordcloud(words = names(word.freq), freq = word.freq, min.freq = 1, random.order = F,colors = pal, max.words =100)

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10,colors = pal,use.r.layout=T,
          scale=c(2,0.5),max.words =100, random.order=F)

##### Find association with a specific keyword in the tweets - 

list1<- findAssocs(tdm, "failure", 0.5)
corrdf1 <- t(data.frame(t(sapply(list1,c))))
corrdf1

list2<- findAssocs(tdm, "nzvind", 0.5)
corrdf2 <- t(data.frame(t(sapply(list2,c))))
corrdf2

list3<- findAssocs(tdm, "wimbledon", 0.5)
corrdf3 <- t(data.frame(t(sapply(list3,c))))
corrdf3


list4<- findAssocs(tdm, "activate", 0.5)
corrdf4 <- t(data.frame(t(sapply(list4,c))))
corrdf4

list5<- findAssocs(tdm, "killing", 0.5)
corrdf5 <- t(data.frame(t(sapply(list5,c))))
corrdf5

list6<- findAssocs(tdm, "watch", 0.3)
corrdf6 <- t(data.frame(t(sapply(list6,c))))
corrdf6

list7<- findAssocs(tdm, "pack", 0.3)
corrdf7 <- t(data.frame(t(sapply(list7,c))))
corrdf7

list8<- findAssocs(tdm, "superstar", 0.4)
corrdf8 <- t(data.frame(t(sapply(list8,c))))
corrdf8

#################################################################

##### Topic Modelling to identify latent/hidden topics using LDA technique
dtm <- as.DocumentTermMatrix(tdm)

rowTotals <- apply(dtm , 1, sum)

NullDocs <- dtm[rowTotals==0, ]
dtm   <- dtm[rowTotals> 0, ]

if (length(NullDocs$dimnames$Docs) > 0) {
  tweets.df <- tweets.df[-as.numeric(NullDocs$dimnames$Docs),]
}

lda <- LDA(dtm, k = 5) # find 5 topic
term <- terms(lda, 7) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

topics<- topics(lda)
topics<- data.frame(date=(tweets.df$created), topic = topics)
qplot (date, ..count.., data=topics, geom ="density", fill= term[topic], position="stack",main="Topic Frequency")


#####Sentiment Analysis: understanding emotional valence in tweets using syuzhet


mysentiment<-get_nrc_sentiment((tweets.df$text))

# Get the sentiment score for each emotion
mysentiment.positive =sum(mysentiment$positive)
mysentiment.anger =sum(mysentiment$anger)
mysentiment.anticipation =sum(mysentiment$anticipation)
mysentiment.disgust =sum(mysentiment$disgust)
mysentiment.fear =sum(mysentiment$fear)
mysentiment.joy =sum(mysentiment$joy)
mysentiment.sadness =sum(mysentiment$sadness)
mysentiment.surprise =sum(mysentiment$surprise)
mysentiment.trust =sum(mysentiment$trust)
mysentiment.negative =sum(mysentiment$negative)



mysentiment.positive
mysentiment.anger
mysentiment.anticipation
mysentiment.disgust
mysentiment.fear
mysentiment.joy
mysentiment.sadness
mysentiment.surprise
mysentiment.trust
mysentiment.negative


# Create the bar chart
yAxis <- c(mysentiment.positive,
           + mysentiment.anger,
           + mysentiment.anticipation,
           + mysentiment.disgust,
           + mysentiment.fear,
           + mysentiment.joy,
           + mysentiment.sadness,
           + mysentiment.surprise,
           + mysentiment.trust,
           + mysentiment.negative)

xAxis <- c("Positive","Anger","Anticipation","Disgust","Fear","Joy","Sadness",
           "Surprise","Trust","Negative")
colors <- c("green","red","blue","orange","red","green","orange","blue","green","red")
yRange <- range(0,yAxis)
barplot(yAxis, names.arg = xAxis, 
        xlab = "Emotional Valence", ylab = "Score", main = "Twitter Sentiment on Hotstar", 
        sub = "Hotstar", col = colors, border = "black", xpd = F, ylim = yRange,
        axisnames = T, cex.axis = 0.8, cex.sub = 0.8, col.sub = "blue")



#####Sentiment Analysis : Plot by date - understanding cummulative sentiment score movement 


mysentimentvalues <- data.frame(get_sentiment((tweets.df$text)))
colnames(mysentimentvalues)<-"Popularity"
mysentimentvalues$Date <- tweets.df$created

result <- aggregate(Popularity ~ Date, data = mysentimentvalues, sum)
result
plot(result, type = "l",col=brewer.pal(8, "Dark2"), main = "Howzz Hotstar !!!")


