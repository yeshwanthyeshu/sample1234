# this is for practise of text mining 
path <- 'C:/Users/c_ymelpati/Desktop/hackerearth/hiring challenge'
setwd(path)

train <- read.csv('train.csv')

head(train,1)
train <- train[sample(nrow(train)),]
head(train,1)

# the train dataframe is shuffled by rows

# extracting first 1000 rows from train

tr <- train[1:1000,]
head(tr,1)
library(qdap)
tr$desc <- replace_symbol(tr$desc)
tr$desc[1]
# the below code successfully removed the special charecters 
tr$desc <- sapply(tr$desc,function(row) iconv(row, "latin1", "ASCII", sub=""))

# now lets have basic text mining
library(tm)
# creating the vector source for handing over to corpus
desc_source <- VectorSource(tr$desc)
# making a corpus vector
desc_cop <- VCorpus(desc_source)

desc_cop[[123]][1]

# making a function for successive transformations
# this is very helpful for text mining 
clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus,stripWhitespace)
  corpus <- tm_map(corpus,removeNumbers)
  corpus <- tm_map(corpus,content_transformer(tolower))
  corpus <- tm_map(corpus,removeWords,stopwords('en'))
  corpus <- tm_map(corpus,removePunctuation)
  return(corpus)
}
desc_cop <- clean_corpus(desc_cop)

# the below code succefully converts in to TDM
desc_tdm <- TermDocumentMatrix(desc_cop)
# viewing the created tdm
desc_tdm
ncol(desc_tdm)
nrow(desc_tdm)
head(desc_tdm$dimnames$Terms)
# converting the tdm to matrix of dim nrow,2
m <- as.matrix(desc_tdm)
dim(m)
head(m,5)
# now sorting the m matrix i.e the words and its freq
orders_words <- sort(rowSums(m),decreasing = TRUE)
orders_words
head(orders_words)
length(orders_words)
typeof(orders_words)
sapply(orders_words,class)
# visualizing the words frequency
barplot(orders_words[1:10],col='tan',las = 2)
# the above code successfully created the orders words with frequency

# making the wordcloud
 #for that we have to make dataframe from matrix
term_freq <- rowSums(m)
word_freq <- data.frame(term = names(term_freq),num = term_freq)
library(wordcloud)
wordcloud(word_freq$term,word_freq$num,max.words = 50, colors = 'red')

# visulizing the words frequency second method
library(qdap)
freq <- freq_terms(tr$currency,top = 5, at.least = 2,stopwords = 'Top200Words')
plot(freq)


#############
library(qdap)
#install.packages('qdap')
desc_direct <- wfm(tr$desc)
head(desc_direct)
nrow(desc_direct)
ncol(desc_direct)
desc_fre <- wfm(desc_cop)
head(desc_fre)
max(desc_fre)
nrow(desc_fre)
ncol(desc_fre)

ord <- sort(desc_fre,decreasing = TRUE)
head(ord)
# the above code only finds the number of word freq 
#######