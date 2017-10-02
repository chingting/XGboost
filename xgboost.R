#想要知道哪種電影的類型最受歡迎，再做推薦
setwd("C:\\moviework0911")
movies = read.csv("movies.csv")
ratings = read.csv("ratings.csv")
head(movies, 5)
head(ratings, 5)


library(data.table)
genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)
genres <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres) <- c(1:ncol(genres))
head(genres, 5)

genre_list = c("Action", "Adventure", "Animation", "Children", "Comedy",
               "Crime","Documentary", "Drama", "Fantasy","Film-Noir",
               "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller",
               "War", "Western")
genre_matrix = matrix(0,nrow(genres),length(genre_list))
colnames(genre_matrix) = genre_list
for (i in 1:nrow(genres)) {
  for (c in 1:ncol(genres)) {
    genmat_col = which(genre_list == genres[i,c])
    genre_matrix[i, genmat_col] = 1
  }
}
genre_matrix = as.data.frame(genre_matrix)
colnames(genre_matrix) = genre_list
genre_matrix = cbind(movieId = movies$movieId, genre_matrix)
head(genre_matrix, 1)


users = merge(ratings, genre_matrix, by.x = "movieId", by.y = "movieId")

head(users, 1)

library(plyr)

genres_rating = NULL
for (i in 5:length(users)) {
  genres_rating = rbind(genres_rating,
                        data.frame(genres = colnames(users)[i],
                                   ratings = users[users[,i]==1,3]))}
summary = ddply(genres_rating,.(genres),
                summarise, mean_ratings = mean(ratings))
summary

users$label = ifelse(users$rating > 3, 1, 0)
train_idx = sample(1:nrow(users), floor(nrow(users)*2/3))
train_x = as.matrix(users[train_idx, 5:22])
colnames(train_x) = colnames(users)[5:22]
train_y = users[train_idx, 23]
test_x = as.matrix(users[-train_idx, 5:22])
colnames(test_x) = colnames(users)[5:22]
test_y = users[-train_idx, 23]

library(xgboost)

model = xgboost(data = train_x, label = train_y, max.depth = 20,
                eta = 0.5, nround = 30, objective = "binary:logistic",
                min_child_weight = 5)

test_y_hat = ifelse(predict(model, test_x) > 0.5, 1, 0)
mean(test_y != test_y_hat)
library(Ckmeans.1d.dp)
importance = xgb.importance(feature_names = colnames(train_x), model = model,
                            data = train_x, label = train_y)

xgb.plot.importance(importance_matrix = importance)
