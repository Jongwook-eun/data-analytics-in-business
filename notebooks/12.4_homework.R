## apply the basic back of words approach to perform text mining analysis,
## and to use the LSA analysis to reduce dimensionality and extract semantic concepts.
## LSA (latent semantic analysis)
# LSA는 TF-IDF 기반 문서–단어 행렬에 SVD를적용 > 문서의 잠재의미구조를 추출하고 차원축소하는 기법
# >> 텍스트 데이터를 supervised learning 등에서 활용 가능한 정형 feature로 변환하는 데 사용

#install.packages("tm") 
#install.packages("lsa") 
#install.packages("SnowballC")
library(SnowballC)
library(tm) 
library(lsa) 

textdata <- read.csv("Ads.csv")
dim(textdata)
corp <- Corpus(DataframeSource(textdata[,1:2]))

#labeling outcome variable 
label <- textdata[,3]

#text preprocessing >> words를 tokens(terms)으로 만듬 
corp <- tm_map(corp,stripWhitespace) 
corp <- tm_map(corp,removePunctuation) 
corp <- tm_map(corp,removeNumbers) 
corp <- tm_map(corp,removeWords,stopwords("English")) 
corp <- tm_map(corp,stemDocument) 

tdm <- TermDocumentMatrix(corp) 
# each document only contains a small subset of all terms in the whole corpus. 
# Therefore, many elements of the TermDocumentMatrix are zeros. 
# 따라서 tdm(TermDocumentMatrix)은 sparse matrix를 일렬로 나열한list 
# i, j are the row and the column indices of those non zero elements, and the v stores that values of these non zero elements. 
# term document matrix has 1,000 colors, corresponding to the 1,000 documents in the corpus, 
# number of rows is 17,300 which corresponds to all the process the terms extracted 
# from the corpus after the cleaning and the tokenization we have just performed. 
# Each element of that term document matrix indicates the number of times a particular term has appeared in a particular document. 
# "Values are the integer counts of turn appearances." 

tdm$dimnames$Terms[1:30] 
# terms are the outcome of stemming, as many of them are clearly not complete words. 


## tfidf measure can more appropriately reflect the strength of relevance of a term to a document. 
tfidf <- weightTfIdf(tdm) 

# "Values(v) are no longer the integer counts of turn appearances but real value measures of tfidf values." 
# total number of non zero values decreases as compared to TDM 
# because some terms appear in all documents. 
# As a result, the inverse document frequency or the idf measure becomes zero, 
# causing the overall tfidf measure to be zero for these few terms. 


## LSA(latent semantic analysis)를 tfidf matrix 기반으로 수행 
lsa.tfidf <- lsa(tfidf, dim=20) 
# "dim = 20" means we'd like to extract 20 topics/concepts from all terms in the corpus.

# tk is the term concept matrix, which represents how much each term is related to the 20 extracted concepts. 
# dimension of this matrix is 17,300 x 20, corresponding to the number of terms by the number of concepts. 

# dk is the document concept matrix, which represents how much each document is related to the 20 extracted concepts. 
# dimension of this matrix is 1,000 x 20,corresponding to the number of documents by the number of concepts. 
# document concept matrix dk is what we need to construct the data frame for that subsequent analysis. 

# sk is a vector of 20 numbers, which are the 20 largest eigen values retained by the LSA analysis 

lsa.tfidf$dk[1:10,] 
# each row corresponds to one document with the indicated document ID. 
# There are 20 columns, each of which corresponds to one of the concepts extracted by the LSA analysis. 
# values suggest the relevance of each document to each concept. 

# Use these values as use as the x variables that capture the tactual characteristics of each document, 
# based on which we can predict the outcome of interest. 

# Use the document concept matrix as the main body of the dataset. 
# Where each document corresponds to one observation and the extracted concepts serve as the x variables. 

# Then append the outcome of interest as an additional column to this matrix to serve as the y variable. 
words.df <- as.data.frame(as.matrix(lsa.tfidf$dk)) 

set.seed(1111) 
train.indx <- sample(1:1000, 800) 

# combine the word.dataframe. We have just converted from the dk matrix with 
# the y variable label we have created early on through column bind. 
train.data <- cbind(label=label[train.indx],words.df[train.indx,]) 
test.data <- cbind(label=label[-train.indx],words.df[-train.indx,]) 

fix(train.data) 

#binary logit model 돌리기 
logit.res <- glm(label~., family=binomial(link=logit), data=train.data) 
summary(logit.res) 

pred <- predict(logit.res, newdata=test.data, type='response') 
yhat <- rep(1, nrow(test.data)) 
yhat[pred<0.5] <- 0 
confusion <- table(yhat, test.data$label) 
confusion 
sum(diag(confusion)) / sum(confusion) 

#vsave(lsa.tfidf, file="lsa_posts.Rda")