library("recommenderlab")
library(caTools)
books1 <- read.csv(file.choose())
books <- books1[-c(1,2)]
View(books1)
View(books)
str(books)
hist(books$Book.Rating)

books_matrix <- as(books, 'realRatingMatrix')
movie_recomm_model1 <- Recommender(books_matrix, method="POPULAR")

##Predictions for two users 
recommended_items1 <- predict(movie_recomm_model1, books_matrix[413:414], n=5)
as(recommended_items1, "list")

##Collaborative Filtering
movie_recomm_model2 <- Recommender(books_matrix, method="UBCF")

##Predictions for two users 
recommended_items2 <- predict(movie_recomm_model2, books_matrix[413:414], n=5)
as(recommended_items2, "list")


##Matrix factorization with LIBMF
movie_recomm_model3 <- Recommender(books_matrix, method="LIBMF")

##Predictions for two users 
recommended_items3 <- predict(movie_recomm_model3, books_matrix[413:414], n=5)


##RANDOM recommendations
movie_recomm_model4 <- Recommender(books_matrix, method="RANDOM")

##Predictions for two users 
recommended_items4 <- predict(movie_recomm_model4, books_matrix[413:414], n=5)
as(recommended_items4, "list")

##RERECOMMEND Method
movie_recomm_model5 <- Recommender(books_matrix, method="RERECOMMEND")

#Predictions for two users 
recommended_items5 <- predict(movie_recomm_model5, books_matrix[413:414], n=5)
as(recommended_items5, "list")
