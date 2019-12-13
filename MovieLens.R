if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
options(stringsAsFactors = TRUE)

# Define distance metric function (RMSE)
RMSE <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2))
}


# Data download per script provided ---------------------------------------
if(!exists('edx')){
  # Note: this process could take a couple of minutes
  
  # MovieLens 10M dataset:
  # https://grouplens.org/datasets/movielens/10m/
  # http://files.grouplens.org/datasets/movielens/ml-10m.zip
  
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                             title = as.character(title),
                                             genres = as.character(genres))
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Validation set will be 10% of MovieLens data
  
  set.seed(1)
  # if using R 3.5 or earlier, use `set.seed(1)` instead
  test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
  edx <- movielens[-test_index,]
  temp <- movielens[test_index,]
  
  # Make sure userId and movieId in validation set are also in edx set
  
  validation <- temp %>% 
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")
  
  # Add rows removed from validation set back into edx set
  
  removed <- anti_join(temp, validation)
  edx <- rbind(edx, removed)
  
  rm(dl, ratings, movies, test_index, temp, movielens, removed)
}
# High level exploration --------------------------------------------------
avg_rating <-  mean(edx$rating)

#Histogram
edx %>% ggplot(aes(rating)) +
  geom_histogram(bins = 30, color = 'black') +
  scale_y_continuous(breaks = c(1e6,2e6,3e6), labels = c(1,2,3)) +
  labs(title = 'MovieLens Rating Distribution') + xlab('Rating') + ylab('Frequency (M)')

#Summary Stats
tibble(`Number of Movies` = length(unique(edx$movieId)), 
       `Number of Users` = length(unique(edx$userId)),
       `Number of Genres` = length(unique(edx$genres)),
       `Average Rating` = round(avg_rating,2)) %>% 
  kable(booktabs = TRUE)

#More Stats
bind_rows(
  edx %>% group_by(movieId) %>% 
    summarize(N = n()) %>% 
    pull(N) %>% summary(N) %>% 
    tidy(),
  edx %>% group_by(userId) %>% 
    summarize(N = n()) %>% 
    pull(N) %>% summary(N) %>% 
    tidy()) %>% 
  data.frame() %>% 
  `rownames<-` (c('Ratings per Movie','Ratings per User')) %>% 
  kable(booktabs = TRUE, row.names = TRUE) %>% 
  kable_styling(position = 'center')

# Mean Model --------------------------------------------------------------

#Implement mean estimate
edx <- edx %>% mutate(mu = avg_rating)
mean_model <- RMSE(edx$rating, edx$mu)

#Tally
results <- data.frame(Method = 'Mean Model', RMSE = mean_model)

# Movie Effect Model ------------------------------------------------------

#Get movie effect by movie
b_i <- edx %>% 
  group_by(movieId) %>% 
  summarize(bi = mean(rating - avg_rating))

#Join back to train set
edx <- edx %>% 
  left_join(b_i, by = 'movieId')

#Tally
movie_effect <- edx %>% summarize(RMSE(rating, mu + bi)) %>% as.numeric()
results <- rbind(results,data.frame(Method = 'Movie Effect Model', RMSE = movie_effect))


# Movie + User Effect Model -----------------------------------------------

#User rating histogram
edx %>% 
  group_by(userId) %>% 
  summarize(bu = mean(rating)) %>% 
  filter(n() > 100) %>% 
  ggplot(aes(bu)) + 
  geom_histogram(bins = 30, color = 'black') +
  labs(title = 'MovieLens User Rating Distribution', subtitle = 'Users with > 100 ratings') + xlab('Rating') + 
  theme(axis.title.y = element_blank())

#Get user effect by user
b_u <- edx %>% 
  group_by(userId) %>% 
  summarize(bu = mean(rating - avg_rating - bi))

#Join back to training set
edx <- edx %>%
  left_join(b_u, by = 'userId')

#Tally
user_effect <- edx %>% summarize(RMSE(rating, avg_rating + bi + bu)) %>% as.numeric()
results <- rbind(results, data.frame(Method = 'Movie and User Effect Model', RMSE = user_effect))

# Movie + User Effect Model + Genres ------------------------------------------------------------------

#Genres histogram
edx %>% 
  group_by(genres) %>% 
  summarize(bu = mean(rating)) %>% 
  filter(n() > 100) %>% 
  ggplot(aes(bu)) + 
  geom_histogram(bins = 30, color = 'black') +
  labs(title = 'MovieLens Genres Rating Distribution', subtitle = 'Genres with > 100 ratings') + xlab('Rating') + 
  theme(axis.title.y = element_blank())

#Get genres effect by genre
b_g <- edx %>% 
  group_by(genres) %>% 
  summarize(bg = mean(rating - avg_rating - bi - bu))

#Join back to training set
edx <- edx %>% 
  left_join(b_g,by = 'genres')

#Tally
genres_effect <- edx %>% summarize(RMSE(rating,avg_rating + bi + bu + bg)) %>% as.numeric()
results <- rbind(results, data.frame(Method = 'Movie, User, and Genres Effect Model', RMSE = genres_effect))

# Regularization ----------------------------------------------------------

#Look at error terms by movie
edx %>% left_join(edx %>% count(movieId) %>% data.frame(), by = 'movieId') %>% 
  mutate(predicted = avg_rating + bi, error = rating - predicted) %>% 
  arrange(desc(abs(error))) %>% 
  select(title, error, predicted, rating, `#Ratings` = n) %>% 
  distinct() %>% 
  slice(1:10) %>% 
  kable(booktabs = TRUE) %>% 
  kable_styling(position = 'center')

#Sample lambdas
lambdas <- seq(-0.5,5,0.25)

#Gather tuning set
rmses <- sapply(lambdas, function(l){

  b_ir <- edx %>%
    group_by(movieId) %>%
    summarize(bir = sum(rating - mu) / (n() + l))

  b_ur <- edx %>%
    left_join(b_ir, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(bur = sum(rating - bir - mu) / (n() + l))
  
  b_gr <- edx %>%
    left_join(b_ir, by = 'movieId') %>%
    left_join(b_ur, by = 'userId') %>% 
    group_by(genres) %>%
    summarize(bgr = sum(rating - mu - bir - bur) / (n() + l))
  
  predicted_ratings <- edx %>% 
    left_join(b_ir, by = 'movieId') %>%
    left_join(b_ur, by = 'userId') %>%
    left_join(b_gr, by = 'genres') %>% 
    mutate(pred = mu + bir + bur + bgr) %>% 
    pull(pred)
  
  return(RMSE(predicted_ratings,edx$rating))
  
})

#Tally best result
results <- rbind(results,
                     data.frame(Method = 'Regularization Movie, User, Genres Model', RMSE = min(rmses)))

#Visualize tuning process
qplot(lambdas,rmses)

#Tally and output results
lambda <- lambdas[which.min(rmses)]

min(rmses)

results  %>% kable(booktabs = TRUE)

# Now run on validation set -----------------------------------------------

#Implement regularization on training set with optimal lambda on movie effect
b_ir <- edx %>%
  group_by(movieId) %>%
  summarize(bir = sum(rating - mu) / (n() + lambda))

#Implement regularization on training set with optimal lambda on user effect
b_ur <- edx %>%
  left_join(b_ir, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(bur = sum(rating - bir - mu) / (n() + lambda))

#Implement regularization on training set with optimal lambda on user effect
b_gr <- edx %>%
  left_join(b_ir, by = 'movieId') %>%
  left_join(b_ur, by = 'userId') %>% 
  group_by(genres) %>%
  summarize(bgr = sum(rating - mu - bir - bur) / (n() + lambda))

#Apply model to test set and gather result
validation <- validation %>% 
  left_join(b_ir, by = 'movieId') %>%
  left_join(b_ur, by = 'userId') %>%
  left_join(b_gr, by = 'genres') %>% 
  mutate(pred = avg_rating + bir + bur + bgr) 
  
validated_error <- RMSE(validation$pred,validation$rating)


