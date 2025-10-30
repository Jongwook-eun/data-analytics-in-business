# Aassociation Detection 중 Association Rules & Collaborative Filtering 존재 (unsupervised learning)
# CF에는 UBCF(userbased) 와 IBCF(itembased) 방식 존재
# UBCF는 자신의 평점들을 demean 이후, 다른 user간 similiarity 구하고, 해당 유사도들을 weight로삼아 다른 user의 평점을 가중평균해서 산출하는 방식

#install.packages("recommenderlab")
library(recommenderlab)

data(MovieLense)
#dataset contains about 100,000 ratings on a scale 1-5 from 943 users on 1,664 movies

fix(MovieLenseMeta) #examine dataframes
fix(MovieLenseUser)
getRatingMatrix(MovieLense) #row가 user, column이 rating

myrating <- matrix(NA, 1, 1664) #1,664개 영화에 대한 matrix생성
myrating[c(1,2,3,4,5)] <- c(3,5,5,4,4) #첫 5개 영화에 대해 그냥 임의의 평점 넣어놈(cold start 안되므로)
myrating <- as(myrating, "realRatingMatrix")

rec.ub <- Recommender(MovieLense, "UBCF") #User-based Collaborative Filtering을 recommender object로
pred.ub <- predict(rec.ub, myrating, n=10, type="topNList") #prediction중 가장높은 영화 10개만 추천
as(pred.ub,"list") #top 10 recommendations custom-made only for you

