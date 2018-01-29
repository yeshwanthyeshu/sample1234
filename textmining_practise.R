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
####################################
# lets us have another freq_terms 
# so following code
tr2 <- train[1001:2000,]
# the below code successfully removed the special charecters 
tr2$desc <- sapply(tr2$desc,function(row) iconv(row, "latin1", "ASCII", sub=""))
# creating the vector scource for corpus
desc2_source <- VectorSource(tr2$desc)
# making a corpus vector
desc2_cop <- VCorpus(desc2_source)
# cleaning the corpus with out clean_corpus function
# i.e text making mining to the corpus
desc2_cop <- clean_corpus(desc2_cop)
# the below code succefully converts in to TDM
desc2_tdm <- TermDocumentMatrix(desc2_cop)
# converting the tdm to matrix of dim nrow,2
m2 <- as.matrix(desc2_tdm)
orders2_words <- sort(rowSums(m2),decreasing = TRUE)
# visualizing the words frequency
barplot(orders2_words[1:10],col='tan',las = 2)
#mean(orders_words == orders2_words)
##########################
# by now we have two word freq matrices 
# i.e orders_words and orders2_words
# i.e desc_tdm and desc2tdm
all_desc1 <- paste(tr$desc,collapse = " ")
all_desc2 <- paste(tr2$desc,collapse = " ")
all_desc <- c(all_desc1,all_desc2)
all_vs <- VectorSource(all_desc)
all_cr <- VCorpus(all_vs)
all_cl <- clean_corpus(all_cr)
all_tdm <- TermDocumentMatrix(all_cl)
all_m <- as.matrix(all_tdm)
# this is the cloud with both data
commonality.cloud(all_m,max.words = 100,colors = "blue")

# for comparision cloud
colnames(all_tdm) <- c("first","second")
all_m <- as.matrix(all_tdm)
comparison.cloud(all_m,colors = c("orange","green"),max.words = 100)

#for pyramind plot
# extracting the common words
common_words <- subset(all_m, all_m[,1] > 0 & all_m[,2] > 0)
diff <- abs(common_words[,1] - common_words[,2])
common_words <- cbind(common_words,diff)
common_words <- common_words[order(common_words[,3],decreasing = TRUE),]
top25df <- data.frame(x = common_words[1:25,1],y=common_words[1:25,2],labels = rownames(common_words[1:25,]))

# drawing the pyramid
library(plotrix)
pyramid.plot(top25df$x,top25df$y,labels = top25df$labels,gap = 8,top.labels = c('first','words','second'),main='words in common',raxlab = NULL,unit = NULL)

# lets try the word network
word_associate(head(tr2$desc,50),match.string = c('help'),stopwords = c(Top100Words),network.plot = TRUE,cloud.colors = c("grey","black"))
