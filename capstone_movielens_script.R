#DATA CLEANING AND FORMATTING
#-------------------------------------------------

#Load the required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

# Download and format the movielens dataset
dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

#Examine the structure of the dataset
str(movielens)

#Add variables
#I will later add a feature to the model based on the number of ratings 
#each movie has received so here I create that variable in the dataset.
num_ratings_table <- movielens %>% group_by(movieId) %>% 
  summarize(num_ratings_movie = n()) 
movielens <- movielens %>% left_join(num_ratings_table, by = "movieId")

#I will also later add a feature to the model based on number of years between movie
#release and rating so here I add that variable to the dataset.
#Extract release date from movie title and create a new column for release_year
movielens <- movielens %>% mutate(release_year = as.numeric(str_sub(title, -5, -2)))
#Convert the timestamp to a date 
movielens <- movielens %>% mutate(date = as.Date(as_datetime(movielens$timestamp)))
#Add a column for the year of the rating
movielens<- movielens %>% mutate(rating_year = year(date))
#Compute number of years from release to rating and add as a variable
movielens <- movielens %>% mutate(years_to_rating = rating_year - release_year)

#Create the edx and validation datasets.
# The validation set will be 10% of the movieLens data.
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Remove from the validation set any observations with userIds, 
#movieIds or years_to_rating that are not in the edx set. 
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId") %>%
  semi_join(edx, by = "years_to_rating")

# Add observations removed from validation set back into the edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#Remove object names 
rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Now divide the edx dataset into a training and a temporary test set.
test_index <- createDataPartition(y = edx$rating, times = 1, p = .1, list = FALSE)
train <- edx[-test_index, ]
test_temp <- edx[test_index]

#Remove from the temporary test set any rows with userIds, 
#movieIds or years_to_rating that are not in the training set 
test <- test_temp %>%
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")%>%
  semi_join(train, by = "years_to_rating")

#Add back to the training set any rows removed from the temporary
#test set.
removed <- anti_join(test_temp, test)
train <- rbind(train, removed)

#Summarize the dimensions of the datasets in a table
dataset_dims <- tibble(Dataset = "edx", Rows = dim(edx)[1], 
                       Columns = dim(edx)[2])
dataset_dims<- bind_rows(dataset_dims, tibble(Dataset = "train", 
                                              Rows = dim(train)[1], 
                                              Columns = dim(train)[2]))
dataset_dims<- bind_rows(dataset_dims, tibble(Dataset = "test", 
                                              Rows = dim(test)[1], 
                                              Columns = dim(test)[2]))
dataset_dims%>%knitr::kable()

#Now check the dimensions of the edx dataset. 
dim(edx)
n_distinct(edx$movieId)
n_distinct(edx$userId)
n_distinct(edx$genres)

#DATA EXPLORATION AND VISUALIZATION
#----------------------------------
#Summarize the dimensions of the datasets in a table
dataset_dims <- tibble(Dataset = "edx", Rows = dim(edx)[1], 
                       Columns = dim(edx)[2])
dataset_dims<- bind_rows(dataset_dims, tibble(Dataset = "train", 
                                              Rows = dim(train)[1], 
                                              Columns = dim(train)[2]))
dataset_dims<- bind_rows(dataset_dims, tibble(Dataset = "test", 
                                              Rows = dim(test)[1], 
                                              Columns = dim(test)[2]))
dataset_dims%>%knitr::kable()

#Now check the number of distinct movies, users and genres. 
dim(edx)
n_distinct(edx$movieId)
n_distinct(edx$userId)
n_distinct(edx$genres)

#Examine the structure of the dataset to see all the variables.
str(edx)

#Examine the distribution of the ratings with a bar plot of count for each rating
edx %>% group_by(rating) %>%
  ggplot(aes(x = rating)) + 
  geom_bar(fill = "cornflowerblue") +
  scale_x_continuous(breaks = seq(0.5,5,0.5),
                     labels = seq(0.5,5,0.5))+
  ggtitle("Count of Ratings")

#Compute the mean rating
mean(edx$rating)

#Examine the distribution of number of movies by number of ratings
edx %>% group_by(movieId)%>% 
  summarize(Number_of_Ratings = n()) %>%
  ggplot(aes(Number_of_Ratings)) + 
  geom_histogram(fill = "cornflowerblue", col = "black", bins = 30)+
  ylab("Number_of_Movies (log10)") +
  scale_y_continuous(trans = "log10") +
  ggtitle("Number of Movies by Number of Ratings")

#Examine the distribution of number of users by number of ratings
  edx %>% group_by(userId)%>% 
    summarize(Number_of_Ratings = n()) %>%
    ggplot(aes(Number_of_Ratings)) + 
    geom_histogram(fill = "cornflowerblue", col = "black", bins = 30)+
    ylab("Number_of_Users (log10)")+
    scale_y_continuous(trans = "log10")+
    ggtitle("Number of Users by Number of Ratings")

#Examine whether movies with a lot of ratings tend to get higher ratings
num_mean_table <- edx %>% group_by(num_ratings_movie) %>%
    summarize(mean_rating = mean(rating))
num_mean_table %>% ggplot(aes(x=num_ratings_movie, y=mean_rating))+
    geom_point() +
  xlab("Ratings per Movie") +
  ylab("Average Rating") + 
  ggtitle("Average Rating vs Ratings per Movie")
#run the test for correlation
cor.test(num_mean_table$num_ratings_movie, num_mean_table$mean_rating)

#Examine the relationship between ratings and time elapsed from movie release

#create a table with years_to_rating and average ratings for all movies
#with more than 4000 ratings
time_effect_table <- edx %>% 
  filter(num_ratings_movie > 4000 & years_to_rating>=0) %>%
  group_by(years_to_rating) %>%
  summarise(average_rating = mean(rating))

#plot average rating vs years_to_rating for all movies with more than 4000 ratings
time_effect_table %>% 
  ggplot(aes(x=years_to_rating,y=average_rating)) + geom_point() + 
  geom_smooth() + 
  ylim(1.5, 5) +
  xlab("Years from Release to Rating") +
  ylab("Average Rating") + 
  ggtitle("Average Rating vs Time Elapsed Since Release")

#run the correlation between years_to_rating and average_rating
cor.test(time_effect_table$years_to_rating, time_effect_table$average_rating)

#MODEL BUILDING
#-----------------

#First build the naive model, using just the overall mean rating as
#the predicted outcome
mu <- mean(train$rating)
mu

#Create a function to compute RMSE
RMSE <- function(actual_ratings, predicted_ratings){
  sqrt(mean((actual_ratings - predicted_ratings)^2))
}

#Compute the RMSE for the naive model
RMSE_naive <- RMSE(test$rating, mu)
RMSE_naive <- format(round(RMSE_naive, 5), nsmall = 5)
RMSE_naive <- as.numeric(RMSE_naive)

#Create a table to track the models and their RMSEs.
rmse_results <- tibble(Model = c("Target", "Base Model"), 
          Features = c("", "Average rating"), RMSE = c(0.86490, RMSE_naive))
rmse_results %>% knitr::kable()

#Now create Model 1 with mean and movie bias features
#First compute movie bias from the training set.
movie_bias <- train %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

#Plot the movie biases to see how they vary
movie_bias %>% ggplot(aes(b_i)) +
  geom_histogram(bins = 10, fill = "cornflowerblue", col = "black")+
  ggtitle("Movie Bias")

#Make predictions based on Model 1
prediction1 <- mu + (test %>% left_join(movie_bias, 
                    by = "movieId") %>% pull(b_i))

#compute RMSE for Model 1
RMSE1 <- RMSE(test$rating, prediction1)
RMSE1

#Add Model 1 to the table
rmse_results <- bind_rows(rmse_results, 
                          tibble(Model = "Model1", 
                          Features = "Mean, movie", 
                          RMSE = RMSE1))
rmse_results %>% knitr::kable()

#Now create Model 2 by adding a feature for user bias
user_bias <- train %>% 
  left_join(movie_bias, by = "movieId")%>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#Plot the user biases to see how they vary
user_bias %>% ggplot(aes(b_u)) +
  geom_histogram(bins = 10, fill = "cornflowerblue", col = "black")+
  ggtitle("User Bias")

#Make predictions based on Model 2
prediction2 <- test %>% left_join(movie_bias, by = "movieId") %>% 
                        left_join(user_bias, by = "userId")%>%
                        mutate(pred = mu + b_i + b_u)%>%
                        pull(pred)

#compute RMSE for Model 2
RMSE2 <- RMSE(test$rating, prediction2)
RMSE2

#Add Model 2 to the table
rmse_results <- bind_rows(rmse_results, 
                          tibble(Model = "Model2", 
                                 Features = "Mean, movie, user", 
                                 RMSE = RMSE2))
rmse_results %>% knitr::kable()
 
#Create Model 3 by adding a feature for the number of ratings 
#each movie receives. 
num_ratings_movie_bias <- train %>% 
  left_join(movie_bias, by = "movieId")%>%
  left_join(user_bias, by = "userId")%>%
  group_by(num_ratings_movie) %>%
  summarize(b_nrm = mean(rating - mu - b_i - b_u))

#Plot the num_ratings_movie biases to see how they vary
num_ratings_movie_bias %>% ggplot(aes(b_nrm)) +
  geom_histogram(bins = 10, fill = "cornflowerblue", col = "black")+
  ggtitle("Number of Ratings Per Movie")

#Make predictions incorporating the num_ratings_movie biases
prediction3 <- test %>% left_join(movie_bias, by = "movieId") %>% 
  left_join(user_bias, by = "userId")%>%
  left_join(num_ratings_movie_bias, by = "num_ratings_movie")%>%
  mutate(pred = mu + b_i + b_u + b_nrm) %>%
  pull(pred)

#compute RMSE
RMSE3 <- RMSE(test$rating, prediction3)
RMSE3

#Add Model 3 to the table
rmse_results <- bind_rows(rmse_results, 
                          tibble(Model = "Model3", 
                                 Features = "Mean, movie, user, ratings per movie", 
                                 RMSE = RMSE3))
rmse_results %>% knitr::kable()

#Create Model 4 by adding a feature for bias based on genres.
genre_bias <- train %>% 
  left_join(movie_bias, by = "movieId")%>%
  left_join(user_bias, by = "userId")%>%
  left_join(num_ratings_movie_bias, by = "num_ratings_movie")%>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u - b_nrm))

#Plot the genre biases to see how they vary
genre_bias %>% ggplot(aes(b_g)) +
  geom_histogram(bins = 10, fill = "cornflowerblue", col = "black")

#Make predictions incorporating the genre biases
prediction4 <- test %>% left_join(movie_bias, by = "movieId") %>% 
  left_join(user_bias, by = "userId")%>%
  left_join(num_ratings_movie_bias, by = "num_ratings_movie")%>%
  left_join(genre_bias, by = "genres")%>%
  mutate(pred = mu + b_i + b_u + b_nrm + b_g)%>%
  pull(pred)

#compute RMSE
RMSE4 <- RMSE(test$rating, prediction4)
RMSE4

#Add the latest model to the table
rmse_results <- bind_rows(rmse_results, 
                          tibble(Model = "Model4", 
                                 Features = "Mean, movie, user, ratings per movie, genres", 
                                 RMSE = RMSE4))
rmse_results %>% knitr::kable()

#Create Model 5 by adding a feature for time elapsed between movie release and rating
time_bias <- train %>% 
  left_join(movie_bias, by = "movieId")%>%
  left_join(user_bias, by = "userId")%>%
  left_join(num_ratings_movie_bias, by = "num_ratings_movie")%>%
  left_join(genre_bias, by = "genres")%>%
  group_by(years_to_rating) %>%
  summarize(b_t = mean(rating - mu - b_i - b_u - b_nrm - b_g))

#Plot the time biases to see how they vary
time_bias %>% ggplot(aes(b_t)) +
  geom_histogram(bins = 10, fill = "cornflowerblue", col = "black")

#Make predictions incorporating the time biases
prediction5 <- test %>% left_join(movie_bias, by = "movieId") %>% 
  left_join(user_bias, by = "userId")%>%
  left_join(num_ratings_movie_bias, by = "num_ratings_movie")%>%
  left_join(genre_bias, by = "genres")%>%
  left_join(time_bias, by = "years_to_rating")%>%
  mutate(pred = mu + b_i + b_u + b_nrm + b_g + b_t)%>%
  pull(pred)

#compute RMSE
RMSE5 <- RMSE(test$rating, prediction5)
RMSE5

#Add the latest model to the table
rmse_results <- bind_rows(rmse_results, 
                          tibble(Model = "Model5", 
                                 Features = "Mean, movie, user, ratings per movie, genres, time", 
                                 RMSE = RMSE5))
rmse_results %>% knitr::kable()

#Now create Model 6 by adding regularization.
#First find the optimal lambda and then use it to run the model
#and compute RMSE.
lambdas <- seq(2, 6, .25)

rmses <- sapply(lambdas, function(lambda){
  
  mu <- mean(train$rating)
  
  b_i <- train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda))
  
  b_u <- train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
  
  b_nrm <- train %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(num_ratings_movie) %>%
    summarize(b_nrm = sum(rating - b_u - b_i - mu)/(n()+lambda))
  
  b_g <- train %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_nrm, by="num_ratings_movie") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_u - b_i - b_nrm - mu)/(n()+lambda))
  
  b_t <- train %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_nrm, by="num_ratings_movie") %>%
    left_join(b_g, by="genres") %>%
    group_by(years_to_rating) %>%
    summarize(b_t = sum(rating - b_u - b_i - b_nrm - b_g - mu)/(n()+lambda))
  
  predicted_ratings <- 
    test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_nrm, by="num_ratings_movie") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_t, by="years_to_rating") %>%
    mutate(pred = mu + b_i + b_u + b_nrm + b_g + b_t) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test$rating))
})

#Make a plot to see the optimal lambda
qplot(lambdas, rmses)
#Identify the optimal lambda
optimal_lambda <- lambdas[which.min(rmses)]
optimal_lambda

#Identify the RMSE associated with the optimal lambda
optimal_rmse<- rmses[which.min(rmses)]
optimal_rmse

#Add the latest model to the table
rmse_results <- bind_rows(rmse_results, 
                          tibble(Model = "Model6", 
                                 Features = "All of the above, regularized", 
                                 RMSE = optimal_rmse))
rmse_results %>% knitr::kable()

#We can improve the model further by considering that about 1500
#predicted values are below 0.5 or above 5. Since the minimum
#rating is 0.5 and the maximum is 5, we can reduce error by adjusting
#anything below 0.5 to a rating of 0.5 and anything above 5 to 
#a rating of 5.

#Examine Model 6 to see how many predictions are below 0.5 or above 5.
lambda <- 4.25
mu <- mean(train$rating)

b_i <- train %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- train %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_nrm <- train %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(num_ratings_movie) %>%
  summarize(b_nrm = sum(rating - b_u - b_i - mu)/(n()+lambda))

b_g <- train %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_nrm, by="num_ratings_movie") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_u - b_i - b_nrm - mu)/(n()+lambda))

b_t <- train %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_nrm, by="num_ratings_movie") %>%
  left_join(b_g, by="genres") %>%
  group_by(years_to_rating) %>%
  summarize(b_t = sum(rating - b_u - b_i - b_nrm - b_g - mu)/(n()+lambda))

predicted_ratings6 <- 
  test %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_nrm, by="num_ratings_movie") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_t, by="years_to_rating") %>%
  mutate(pred = mu + b_i + b_u + b_nrm + b_g + b_t) %>%
  pull(pred)

#number of ratings <0.5
sum(predicted_ratings6 <0.5)
#number of ratings >5
sum(predicted_ratings6 >5)

#Round up observations less than 0.5 and round down observations
#greater than 5
predicted_ratings6 <- ifelse(predicted_ratings6 <0.5, 0.5,
                             ifelse(predicted_ratings6 >5, 5,
                                    predicted_ratings6))

#Now calculate RMSE for the adjusted values.
FinalRMSE <- RMSE(test$rating, predicted_ratings6)
FinalRMSE

#Add the final model to the table
rmse_results <- bind_rows(rmse_results, 
                          tibble(Model = "Final Model", 
                                 Features = "Regularized movie, user, ratings per movie, genres, time + adjustment", 
                                 RMSE = FinalRMSE))
rmse_results %>% knitr::kable()

#RESULTS
#-------

#Run the model on the validation set.
lambda <- 4.25
mu <- mean(train$rating)

b_i <- train %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- train %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_nrm <- train %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(num_ratings_movie) %>%
  summarize(b_nrm = sum(rating - b_u - b_i - mu)/(n()+lambda))

b_g <- train %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_nrm, by="num_ratings_movie") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_u - b_i - b_nrm - mu)/(n()+lambda))

b_t <- train %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_nrm, by="num_ratings_movie") %>%
  left_join(b_g, by="genres") %>%
  group_by(years_to_rating) %>%
  summarize(b_t = sum(rating - b_u - b_i - b_nrm - b_g - mu)/(n()+lambda))

predicted_ratings_val <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_nrm, by="num_ratings_movie") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_t, by="years_to_rating") %>%
  mutate(pred = mu + b_i + b_u + b_nrm + b_g + b_t) %>%
  pull(pred)

#Compute adjusted ratings
predicted_ratings_val <- ifelse(predicted_ratings_val <0.5, 0.5,
                             ifelse(predicted_ratings_val >5, 5,
                                    predicted_ratings_val))

#Compute RMSE for the validation set
RMSE_val <- RMSE(validation$rating, predicted_ratings_val)
RMSE_val

