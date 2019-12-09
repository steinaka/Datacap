# Preload necessary R libary

library(rJava)
library(knitr)
library(RColorBrewer)
library(stringi)
library(wordcloud2)
library(ggplot2)
library(ngram)
library(NLP)
library(tm)
library(RWeka)
library(slam)
library(xtable)
library(downloader)
library(plyr)
library(dplyr)
library(doParallel)
library(SnowballC)
library(wordcloud)


options(mc.cores=1)

## Step 1: Download the dataset and unzip folder
## Check if directory already exists?
if(!file.exists("./final")){
  dir.create("./final")
}
Url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
## Check if zip has already been downloaded in projectData directory?
if(!file.exists("./final/Coursera-SwiftKey.zip")){
  download.file(Url,destfile="./final/Coursera-SwiftKey.zip",mode = "wb")
}
## Check if zip has already been unzipped?
if(file.exists("./final")){
  unzip(zipfile="./final/Coursera-SwiftKey.zip",exdir=".")
}
# Once the dataset is downloaded start reading it as this a huge dataset so we'll read line by line only the amount of data needed before doing that lets first list all the files in the directory
path <- file.path("./final" , "en_US")
files<-list.files(path, recursive=TRUE)
# Lets make a file connection of the twitter data set
con <- file("./final/en_US/en_US.twitter.txt", "r") 
#lineTwitter<-readLines(con,encoding = "UTF-8", skipNul = TRUE)
lineTwitter<-readLines(con, skipNul = TRUE)
# Close the connection handle when you are done
close(con)
# Lets make a file connection of the blog data set
con <- file("./final/en_US/en_US.blogs.txt", "r") 
#lineBlogs<-readLines(con,encoding = "UTF-8", skipNul = TRUE)
lineBlogs<-readLines(con, skipNul = TRUE)
# Close the connection handle when you are done
close(con)
# Lets make a file connection of the news data set
con <- file("./final/en_US/en_US.news.txt", "r") 
#lineNews<-readLines(con,encoding = "UTF-8", skipNul = TRUE)
lineNews<-readLines(con, skipNul = TRUE)
# Close the connection handle when you are done
close(con)
# Get file sizes
lineBlogs.size <- file.info("./final/en_US/en_US.blogs.txt")$size / 1024 ^ 2
lineNews.size <- file.info("./final/en_US/en_US.news.txt")$size / 1024 ^ 2
lineTwitter.size <- file.info("./final/en_US/en_US.twitter.txt")$size / 1024 ^ 2

# Get words in files
lineBlogs.words <- stri_count_words(lineBlogs)
lineNews.words <- stri_count_words(lineNews)
lineTwitter.words <- stri_count_words(lineTwitter)

# Summary of the data sets
data.frame(source = c("blogs", "news", "twitter"),
           file.size.MB = c(lineBlogs.size, lineNews.size, lineTwitter.size),
           num.lines = c(length(lineBlogs), length(lineNews), length(lineTwitter)),
           num.words = c(sum(lineBlogs.words), sum(lineNews.words), sum(lineTwitter.words)),
           mean.num.words = c(mean(lineBlogs.words), mean(lineNews.words), mean(lineTwitter.words)))
## Cleaning The Data


##------
## Display Statistics of data


WPL <- sapply(list(lineBlogs,lineNews,lineTwitter),function(x) summary(stri_count_words(x))[c('Min.','Mean','Max.')])
rownames(WPL) <- c('WPL_Min','WPL_Mean','WPL_Max')
stats <- data.frame(
  FileName=c("en_US.blogs","en_US.news","en_US.twitter"),      
  t(rbind(
    sapply(list(lineBlogs,lineNews,lineTwitter),stri_stats_general)[c('Lines','Chars'),],
    Words=sapply(list(lineBlogs,lineNews,lineTwitter),stri_stats_latex)['Words',],
    WPL)
  ))

head(stats)


##-----------


# Sample the data and set seed for reporducibility
set.seed(5000)
data.sample <- c(sample(lineBlogs, length(lineBlogs) * 0.02),
                 sample(lineNews, length(lineNews) * 0.02),
                 sample(lineTwitter, length(lineTwitter) * 0.02))

# Create corpus and clean the data
corpus <- VCorpus(VectorSource(data.sample))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)
unicorpus <- tm_map(corpus, removeWords, stopwords("en"))


##Exploratory Analysis
# we'll get the frequencies of the word
getFreq <- function(tdm) {
  freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
  return(data.frame(word = names(freq), freq = freq))
}

# Prepare n-gram frequencies
getFreq <- function(tdm) {
  freq <- sort(rowSums(as.matrix(rollup(tdm, 2, FUN = sum)), na.rm = T), decreasing = TRUE)
  return(data.frame(word = names(freq), freq = freq))
}
bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
quadgram <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
pentagram <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
hexagram <- function(x) NGramTokenizer(x, Weka_control(min = 6, max = 6))
heptagram <- function(x) NGramTokenizer(x, Weka_control(min = 7, max = 7))

# Get frequencies of most common n-grams in data sample
# freq1 <- getFreq(removeSparseTerms(TermDocumentMatrix(unicorpus), 0.999))
# save(freq1, file="nfreq.f1.RData")
# freq2 <- getFreq(TermDocumentMatrix(unicorpus, control = list(tokenize = bigram, bounds = list(global = c(5, Inf)))))
# save(freq2, file="nfreq.f2.RData")
# freq3 <- getFreq(TermDocumentMatrix(corpus, control = list(tokenize = trigram, bounds = list(global = c(3, Inf)))))
# save(freq3, file="nfreq.f3.RData")
# freq4 <- getFreq(TermDocumentMatrix(corpus, control = list(tokenize = quadgram, bounds = list(global = c(2, Inf)))))
# save(freq4, file="nfreq.f4.RData")
# freq5 <- getFreq(TermDocumentMatrix(corpus, control = list(tokenize = pentagram, bounds = list(global = c(2, Inf)))))
# save(freq5, file="nfreq.f5.RData")
# freq6 <- getFreq(TermDocumentMatrix(corpus, control = list(tokenize = hexagram, bounds = list(global = c(2, Inf)))))
# save(freq6, file="nfreq.f6.RData")

freq7 <- getFreq(TermDocumentMatrix(corpus, control = list(tokenize = heptagram, bounds = list(global = c(2, Inf)))))
save(freq7, file="nfreq.f7.RData")


nf <- list("f1" = freq1, "f2" = freq2, "f3" = freq3, "f4" = freq4, "f5" = freq5, "f6" = freq6, "f7" = freq7 )
save(nf, file="nfreq.v5.RData")

##------
## Word cloud display




wordcloud(freq1$word, freq1$freq, scale = c(3,1), max.words=100, random.order=FALSE, rot.per=0, fixed.asp = TRUE, use.r.layout = FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(freq2$word, freq2$freq, scale = c(3,1), max.words=100, random.order=FALSE, rot.per=0, fixed.asp = TRUE, use.r.layout = FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(freq3$word, freq3$freq, scale = c(3,1), max.words=100, random.order=FALSE, rot.per=0, fixed.asp = TRUE, use.r.layout = FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(freq4$word, freq4$freq, scale = c(3,1), max.words=100, random.order=FALSE, rot.per=0, fixed.asp = TRUE, use.r.layout = FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(freq5$word, freq5$freq, scale = c(3,1), max.words=100, random.order=FALSE, rot.per=0, fixed.asp = TRUE, use.r.layout = FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(freq6$word, freq6$freq, scale = c(3,1), max.words=100, random.order=FALSE, rot.per=0, fixed.asp = TRUE, use.r.layout = FALSE, colors=brewer.pal(8, "Dark2"))



##-----------












##-------------------

path1 <- "./data/en_US.blogs.txt"
path2 <- "./data/en_US.news.txt"
path3 <- "./data/en_US.twitter.txt"

# Read blogs data in binary mode
conn <- file(path1, open="rb")
blogs <- readLines(conn, encoding="UTF-8"); close(conn)
# Read news data in binary mode
conn <- file(path2, open="rb")
news <- readLines(conn, encoding="UTF-8"); close(conn)
# Read twitter data in binary mode
conn <- file(path3, open="rb")
twitter <- readLines(conn, encoding="UTF-8"); close(conn)
# Remove temporary variable
rm(conn)

# Compute statistics and summary info for each data type
WPL <- sapply(list(blogs,news,twitter),function(x) summary(stri_count_words(x))[c('Min.','Mean','Max.')])
rownames(WPL) <- c('WPL_Min','WPL_Mean','WPL_Max')
stats <- data.frame(
  FileName=c("en_US.blogs","en_US.news","en_US.twitter"),      
  t(rbind(
    sapply(list(blogs,news,twitter),stri_stats_general)[c('Lines','Chars'),],
    Words=sapply(list(blogs,news,twitter),stri_stats_latex)['Words',],
    WPL)
  ))
head(stats)
Appendix B - Sample and Clean the Data
# Set random seed for reproducibility and sample the data
set.seed(1001)
sampleBlogs <- blogs[sample(1:length(blogs), 0.01*length(blogs), replace=FALSE)]
sampleNews <- news[sample(1:length(news), 0.01*length(news), replace=FALSE)]
sampleTwitter <- twitter[sample(1:length(twitter), 0.01*length(twitter), replace=FALSE)]

# Remove unconvention/funny characters for sampled Blogs/News/Twitter
sampleBlogs <- iconv(sampleBlogs, "UTF-8", "ASCII", sub="")
sampleNews <- iconv(sampleNews, "UTF-8", "ASCII", sub="")
sampleTwitter <- iconv(sampleTwitter, "UTF-8", "ASCII", sub="")
sampleData <- c(sampleBlogs,sampleNews,sampleTwitter)

# Remove temporary variables
rm(blogs, news, twitter, path1, path2, path3)
Appendix C - Build Corpus
build_corpus <- function (x = sampleData) {
  sample_c <- VCorpus(VectorSource(x)) # Create corpus dataset
  sample_c <- tm_map(sample_c, tolower) # all lowercase
  sample_c <- tm_map(sample_c, removePunctuation) # Eleminate punctuation
  sample_c <- tm_map(sample_c, removeNumbers) # Eliminate numbers
  sample_c <- tm_map(sample_c, stripWhitespace) # Strip Whitespace
  
  # read and process a file of banned words
  bw <- read.csv(file ='Terms-to-Block.csv', stringsAsFactors=F, skip=3)
  bannedWords <- gsub(",", "", tolower(bw[,2]))
  sample_c <- tm_map(sample_c, removeWords, bannedWords) # Eliminate banned words
  sample_c <- tm_map(sample_c, removeWords, stopwords("english")) # Eliminate English stop words
  sample_c <- tm_map(sample_c, stemDocument) # Stem the document
  sample_c <- tm_map(sample_c, PlainTextDocument) # Create plain text format
}
corpusData <- build_corpus(sampleData)
Appendix D - Tokenize and Calculate Frequencies of N-Grams
library(RWeka)

getTermTable <- function(corpusData, ngrams = 1, lowfreq = 50) {
  #create term-document matrix tokenized on n-grams
  tokenizer <- function(x) { NGramTokenizer(x, Weka_control(min = ngrams, max = ngrams)) }
  tdm <- TermDocumentMatrix(corpusData, control = list(tokenize = tokenizer))
  #find the top term grams with a minimum of occurrence in the corpus
  top_terms <- findFreqTerms(tdm,lowfreq)
  top_terms_freq <- rowSums(as.matrix(tdm[top_terms,]))
  top_terms_freq <- data.frame(word = names(top_terms_freq), frequency = top_terms_freq)
  top_terms_freq <- arrange(top_terms_freq, desc(frequency))
}

tt.Data <- list(3)
for (i in 1:3) {
  tt.Data[[i]] <- getTermTable(corpusData, ngrams = i, lowfreq = 10)
}
Appendix E - Code for Plot of Sampled Corpus with Word Cloud
library(wordcloud)
library(RColorBrewer)

# Set random seed for reproducibility
set.seed(1001)
# Set Plotting in 1 row 3 columns
par(mfrow=c(1, 3))
for (i in 1:3) {
  wordcloud(tt.Data[[i]]$word, tt.Data[[i]]$frequency, scale = c(3,1), max.words=100, random.order=FALSE, rot.per=0, fixed.asp = TRUE, use.r.layout = FALSE, colors=brewer.pal(8, "Dark2"))
}
Appendix F - Make Plots
plot.Grams <- function (x = tt.Data, N=10) {
  g1 <- ggplot(data = head(x[[1]],N), aes(x = reorder(word, -frequency), y = frequency)) + 
    geom_bar(stat = "identity", fill = "green") + 
    ggtitle(paste("Unigrams")) + 
    xlab("Unigrams") + ylab("Frequency") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  g2 <- ggplot(data = head(x[[2]],N), aes(x = reorder(word, -frequency), y = frequency)) + 
    geom_bar(stat = "identity", fill = "blue") + 
    ggtitle(paste("Bigrams")) + 
    xlab("Bigrams") + ylab("Frequency") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  g3 <- ggplot(data = head(x[[3]],N), aes(x = reorder(word, -frequency), y = frequency)) + 
    geom_bar(stat = "identity", fill = "darkgreen") + 
    ggtitle(paste("Trigrams")) + 
    xlab("Trigrams") + ylab("Frequency") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  # Put three plots into 1 row 3 columns
  gridExtra::grid.arrange(g1, g2, g3, ncol = 3)
}
library(ggplot2); library(gridExtra)
plot.Grams(x = tt.Data, N = 20)