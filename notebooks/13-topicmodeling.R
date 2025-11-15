## ★가장재밌으면서 실생활에 다방면활용할수있는파트(textmining의 토픽 모델링)
## LDA (latent Dirichlet allocation analysis)
## topic modeling 과 LDA분석은 큰 규모의 데이터를 해석/이해하는데 큰 도움
## topic modeling and LDA analysis are very useful methods to help us quickly gain an understanding of a large volume of text documents.are very useful methods to help quickly gain an understanding of a large volume of text documents.

#install.packages("tm") 
#install.packages("topicmodels")
#install.packages("wordcloud")
#install.packages("SnowballC")
library(SnowballC)
library(tm) 
library(topicmodels)
library(wordcloud)

textdata <- read.csv("news.csv")
corp <- Corpus(DataframeSource(textdata))

#text preprocessing >> words를 tokens(terms)으로 만듬 
processedCorp <- tm_map(corp,stripWhitespace)
processedCorp <- tm_map(processedCorp,removePunctuation) 
processedCorp <- tm_map(processedCorp,removeNumbers) 
processedCorp <- tm_map(processedCorp,removeWords,stopwords("English")) 
processedCorp <- tm_map(processedCorp,stemDocument)
#a clean list of terms / ready to construct that document term matrix.

## create document term matrix
#control은 frequency filter(lowerbound는 3, upper bound는 infinity)
#최소한 3번은 나타난 term이 dtm에 포함됨

DTM <- DocumentTermMatrix(processedCorp, control = list(bounds = list(global = c(3,Inf))))
#DTM은 sparse matrix로 정리되어서 non-zero value만 list형태로 정리됨
# i, j are the row and column indexes of those non zero elements
# v stores the values of these non zero elements.

# document term matrix has 1167 rows in total,
# corresponding to the 1167 documents in the corpus,
# which are the 1167 news articles

# The column number of the document term matrix equals 5131,
# which corresponds to all processed terms extracted from the corpus.

#이경우에는 term이 더 많음
#보통은 term이 document 보다 더 많음

dim(DTM)
nTerms(DTM)
nDocs(DTM)
DTM$dimnames$Terms[1:50]

##(cf)아래는 term 개수가 document보다 작을때 행하는 작업
#term개수가 document보다 작기때문에, some documents end up with no terms at all
# the corresponding row in the document term matrix are all zeros.
# These rows may create some problems for our later analysis and hence need to be removed
#왜냐하면 We end up having these empty documents, largely because each document is relatively
# short and after removing the stopwords and stemming, many words are removed,
# and many more are further filtered by the minimum frequency requirements.
row.indx <- slam::row_sums(DTM) > 0
DTM <- DTM[row.indx,]
textdata <- textdata[row.indx,]
dim(DTM)


## LDA분석(latent Dirichlet allocation analysis)
#20개 topic 추출, MCMC sampling을 1000번실행
set.seed(1000)
tm <- LDA(DTM, 20, method="Gibbs", control=list(iter=1000, verbose = 50))

# Gibbs indicating we'll use that sampling based method,
# namely the Markov chain Monte Carlo or MCMC method,
# and use the Gibbs algorithm to sample values from the Markov chain.

# output we want from the LDA analysis is posterior distribution of the unknown parameters.
tm.res <-posterior(tm)

# Two matrices store that posterior means of that two sets of key parameters, Beta / Theta.
# Beta is each topics probability distribution over all terms,
# Theta is each document probability distribution over all topics.
beta <- tm.res$terms
dim(beta)
# 20 topics : 5131 terms

beta[,1:5]
# first five terms and how much each of the 20 topics is correlated with these five terms.

rowSums(beta)
# values along each row of the Beta matrix add up to 1

theta <- tm.res$topics
dim(theta)
# 1167 documents : 20 topics

theta[1:5,]
# first five documents probability association with the 20 topics.

rowSums(theta)[1:10]
# values along each row of the Theta matrix also add up to 1

# further examine what each topic is about
terms(tm,10)

## further examine how each document is related to the topics

# original text content of the second document in the corpus
as.character(corp[1082]$content)
barplot(theta[1082,])
# 결과를 보면, 1082번 뉴스는 most closely related to Topic 1, 19, 12

## visualization tool이용

# word cloud of the 50 words that are the most closely related to the 4번 topic,
# with the most relevant words in the largest fund sizes.
top.term.prob <- sort(beta[4,], decreasing=TRUE)[1:50]
wordcloud(names(top.term.prob), top.term.prob, random.order = FALSE)

# 3번 topic
top.term.prob <- sort(beta[3,], decreasing=TRUE)[1:50]
wordcloud(names(top.term.prob), top.term.prob, random.order = FALSE)

# 8번 topic
top.term.prob <- sort(beta[8,], decreasing=TRUE)[1:50]
wordcloud(names(top.term.prob), top.term.prob, random.order = FALSE)

