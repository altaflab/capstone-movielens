###########################################################################################################################
# Create edx set, validation set (final hold-out test set) - code provided by HarvardX: PH125.9x
###########################################################################################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
 
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
 
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
 
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
 
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                            title = as.character(title),
                                            genres = as.character(genres))
 
movielens <- left_join(ratings, movies, by = "movieId")
 
# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
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
 
# Remove temporary files to tidy environment
rm(dl, ratings, movies, test_index, temp, movielens, removed)

###########################################################################################################################
# Exploratory Analysis
###########################################################################################################################

# Create plot theme to apply to ggplot2 element text throughout report
plot_theme <- theme(plot.caption = element_text(size = 12, face = "italic"), axis.title = element_text(size = 12))

# Tabulate class of variables and first 5 rows included in edx dataset
rbind((lapply(edx, class)), head(edx)) %>%

# Plot distribution of ratings in the edx dataset
edx %>% ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.2, color = I("black")) +
  scale_y_continuous(breaks = c(1000000, 2000000), labels = c("1", "2")) +
  labs(x = "Rating", y = "Count (in millions)", caption = "Source: edx dataset") + plot_theme

# Plot average rating by movie in the edx dataset
edx %>% group_by(movieId) %>%
  summarise(ave_rating = sum(rating)/n()) %>%
  ggplot(aes(ave_rating)) +
  geom_histogram(bins=30, color = I("black")) +
  labs(x = "Average rating", y = "Number of movies", caption = "source: edx dataset") + plot_theme

# Plot number of ratings by movie in the edx dataset
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=30, color = I("black")) +
  scale_x_log10() +
  labs(x = "Movies", y = "Number of ratings", caption = "Source: edx dataset") + plot_theme

# Plot average rating by user in the edx dataset
edx %>% group_by(userId) %>%
  summarise(ave_rating = sum(rating)/n()) %>%
  ggplot(aes(ave_rating)) +
  geom_histogram(bins=30, color = I("black")) +
  labs(x = "Average rating", y = "Number of users", caption = "Source: edx dataset") + plot_theme

# Plot number of ratings by user in the edx dataset
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=30, color = I("black")) +
  scale_x_log10() +
  labs(x = "Users", y = "Number of ratings", caption = "Source: edx dataset") + plot_theme

# Separate individual genres and ranking them by the total number of ratings in the edx dataset
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(count = n(), rating = round(mean(rating), 2)) %>%
  arrange(desc(count))

# Plot average rating by genre for genre combinations with at least 100,000 ratings
edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 100000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Genre combination", y = "Average Rating", caption = "Source: edx dataset") + plot_theme

# Group and list top 10 movie titles based on number of ratings
edx %>% group_by(title) %>%
  summarise(n = n()) %>%
  slice_max(n, n=10)

# Trim and split title (year) column into title and year columns
edx <- edx %>% mutate(title = str_trim(title)) %>%
  # split title column to two columns: title and year
  extract(title, c("title_temp", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>%
  # for series take debut date
  mutate(year = if_else(str_length(year) > 4, as.integer(str_split(year, "-", simplify = T)[1]), as.integer(year))) %>%
  # replace title NA's with original title
  mutate(title = if_else(is.na(title_temp), title, title_temp)) %>%
  # drop title_tmp column
  select(-title_temp)

# Plot average rating by year of release in the edx dataset
edx %>% group_by(year) %>%
  summarise(rating = mean(rating)) %>%
  ggplot(aes(year, rating)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Release Year", y = "Average Rating", caption = "Source: edx dataset") + plot_theme

# Plot number of ratings by year of release in the edx dataset
edx %>% group_by(year) %>%
  summarise(count = n()) %>%
  ggplot(aes(year, count)) +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 800000, 200000), labels = seq(0, 800, 200)) +
  labs(x = "Release Year", y = "Number of Ratings (,000s)", caption = "Source: edx dataset") + plot_theme

# Convert timestamp column into date format, removing time data
edx <- edx %>% mutate(review_date = round_date(as_datetime(timestamp), unit = "week"))

# Plot average rating by date of review in the edx dataset
edx %>% group_by(review_date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(review_date, rating)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Date of Review", y = "Average Rating", caption = "Source: edx dataset") + plot_theme

###########################################################################################################################
# Methods - partition edx into train and test sets
###########################################################################################################################

# Create train set and test sets from edx
set.seed(2020, sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set) 
train_set <- rbind(train_set, removed)

# Remove temporary files to tidy environment
rm(test_index, temp, removed) 

###########################################################################################################################
# Methods - develop, train and test various iterations of the algorithm
###########################################################################################################################

# Create table and add target RMSE based on project objective
rmse_objective <- 0.86490
rmse_results <- data.frame(Method = "Project objective", RMSE = "0.86490", Difference = "-")

# Calculate the overall average rating across all movies included in train set
mu_hat <- mean(train_set$rating)
# Calculate RMSE between each rating included in test set and the overall average
simple_rmse <- RMSE(test_set$rating, mu_hat)

# Estimate movie effect (b_i)
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu_hat))
# Predict ratings adjusting for movie effects
predicted_b_i <- mu_hat + test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  pull(b_i)
# Calculate RMSE based on movie effects model
movie_rmse <- RMSE(predicted_b_i, test_set$rating)

# Estimate user effect (b_u)
user_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu_hat - b_i))
# Predict ratings adjusting for movie and user effects
predicted_b_u <- test_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)
# Calculate RMSE based on user effects model
user_rmse <- RMSE(predicted_b_u, test_set$rating)

# Estimate genre effect (b_g)
genre_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(genres) %>%
  summarise(b_g = mean(rating - mu_hat - b_i - b_u))
# Predict ratings adjusting for movie, user and genre effects
predicted_b_g <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  pull(pred)
# Calculate RMSE based on genre effects model
genre_rmse <- RMSE(predicted_b_g, test_set$rating)

# Estimate release year effect (b_y)
year_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  group_by(year) %>%
  summarise(b_y = mean(rating - mu_hat - b_i - b_u - b_g))
# Predict ratings adjusting for movie, user, genre and year effects
predicted_b_y <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(year_avgs, by = "year") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_y) %>%
  pull(pred)
# Calculate RMSE based on year effects model
year_rmse <- RMSE(predicted_b_y, test_set$rating)

# Estimate review date effect (b_r)
date_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(year_avgs, by = "year") %>%
  group_by(review_date) %>%
  summarise(b_r = mean(rating - mu_hat - b_i - b_u - b_g - b_y))
# Predict ratings adjusting for movie, user, genre, year and review date effects
predicted_b_r <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(year_avgs, by = "year") %>%
  left_join(date_avgs, by = "review_date") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_y + b_r) %>%
  pull(pred)
# Calculate RMSE based on review date effects model
review_rmse <- RMSE(predicted_b_r, test_set$rating)

# Generate a sequence of values for lambda ranging from 3 to 6 with 0.1 increments (inc)
inc <- 0.1
lambdas <- seq(4, 6, inc)
# Regularise model, predict ratings and calculate RMSE for each value of lambda
rmses <- sapply(lambdas, function(l){
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu_hat)/(n()+l))
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu_hat)/(n()+l))
  b_g <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarise(b_g = sum(rating - b_i - b_u - mu_hat)/(n()+l))
  b_y <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(year) %>%
    summarise(b_y = sum(rating - b_i - b_u - b_g - mu_hat)/(n()+l))
  b_r <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_y, by="year") %>%
    group_by(review_date) %>%
    summarise(b_r = sum(rating - b_i - b_u - b_g - mu_hat)/(n()+l))
  predicted_ratings <- test_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_y, by="year") %>%
    left_join(b_r, by="review_date") %>%
    mutate(pred = mu_hat + b_i + b_u + b_g + b_y + b_r) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
# Assign optimal tuning parameter (lambda)
lambda <- lambdas[which.min(rmses)]
# Minimum RMSE achieved
regularised_rmse <- min(rmses) 

###########################################################################################################################
# Methods - mutate validation dataset to reflect changes to edx (year, date of review) and run final hold-out test
###########################################################################################################################

# Use mutate function to update validation dataset in line with changes made to edx
validation <- validation %>% mutate(review_date = round_date(as_datetime(timestamp), unit = "week"))
validation <- validation %>% mutate(review_date = as_date(review_date))
validation <- validation %>% mutate(title = str_trim(title)) %>%
  # split title to title, year
  extract(title, c("title_temp", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>%
  # for series take debut date
  mutate(year = if_else(str_length(year) > 4, as.integer(str_split(year, "-", simplify = T)[1]), as.integer(year))) %>%
  # replace title NA's with original title
  mutate(title = if_else(is.na(title_temp), title, title_temp)) %>%
  # drop title_tmp column
  select(-title_temp)

# Use full edx dataset to model all effects, regularised with chosen value for lambda
b_i <- edx %>%
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu_hat)/(n()+lambda))

b_u <- edx %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - b_i - mu_hat)/(n()+lambda))

b_g <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(genres) %>%
  summarise(b_g = sum(rating - b_i - b_u - mu_hat)/(n()+lambda))

b_y <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  group_by(year) %>%
  summarise(b_y = sum(rating - b_i - b_u - b_g - mu_hat)/(n()+lambda))

b_r <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_y, by="year") %>%
  group_by(review_date) %>%
  summarise(b_r = sum(rating - b_i - b_u - b_g - b_y - mu_hat)/(n()+lambda))

# Predict ratings in validation set using final model
predicted_ratings <- validation %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_y, by="year") %>%
  left_join(b_r, by="review_date") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_y + b_r) %>%
  pull(pred)

# Calculate final validation RMSE
valid_rmse <- RMSE(validation$rating, predicted_ratings)

###########################################################################################################################
# Results - visualise each of the effects modelled and tabulate RMSE for each iteration during training in edx dataset
###########################################################################################################################

# Add naive RMSE result to table
rmse_results <- rmse_results %>% rbind(c("Simple average", round(simple_rmse,5), round(simple_rmse-rmse_objective,5)))

# Plot movie effects distribution
movie_avgs %>%
  ggplot(aes(b_i)) +
  geom_histogram(bins = 10, color = I("black")) +
  labs(x="Movie effects (b_i)", caption = "Source: train dataset") + plot_theme

# Amend table to include movie effects model RMSE result
rmse_results <- rmse_results %>% rbind(c("Movie effects (b_i)", round(movie_rmse, 5), round(movie_rmse-rmse_objective, 5)))

# Plot user effects distribution
user_avgs %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 10, color = I("black")) +
  labs(x="User effects (b_u)", caption = "Source: train dataset") + plot_theme

# Amend table to include user effects model RMSE result
rmse_results <- rmse_results %>% rbind(c("Movie + User effects (b_u)", round(user_rmse, 5), round(user_rmse-rmse_objective, 5)))

# Plot genre effects distribution
genre_avgs %>%
  ggplot(aes(b_g)) +
  geom_histogram(bins = 10, color = I("black")) +
  labs(x="Genre effects (b_g)", caption = "Source: train dataset") + plot_theme

# Amend table to include genre effects model RMSE result
rmse_results <- rmse_results %>% rbind(c("Movie, User and Genre effects (b_g)", round(genre_rmse, 5), round(genre_rmse-rmse_objective, 5)))

# Plot year of release effects distribution
year_avgs %>%
  ggplot(aes(b_y)) +
  geom_histogram(bins = 10, color = I("black")) +
  labs(x="Year effects (b_y)", caption = "Source: train dataset") + plot_theme

# Amend table to include genre effects model RMSE result
rmse_results <- rmse_results %>% rbind(c("Movie, User, Genre and Year effects (b_y)", round(year_rmse, 5), round(year_rmse-rmse_objective, 5)))

# Plot review date effects distribution
date_avgs %>%
  ggplot(aes(b_r)) +
  geom_histogram(bins = 10, color = I("black")) +
  labs(x="Review date effects (b_r)", caption = "Source: train dataset") + plot_theme

# Amend table to include review date effects model RMSE result
rmse_results <- rmse_results %>% rbind(c("Movie, User, Genre, Year and Review Date effects (b_r)", round(review_rmse, 5), round(review_rmse-rmse_objective, 5)))

# Plot RMSE results against each tuning parameter (lambda) in order to find optimal tuner
data.frame(lambdas, rmses) %>%
  ggplot(aes(lambdas, rmses)) +
  geom_point() +
  geom_hline(yintercept=min(rmses), linetype='dotted', col = "red") +
  annotate("text", x = lambda, y = min(rmses), label = lambda, vjust = -1, color = "red") +
  labs(x = "Lambda", y = "RMSE", caption = "Source: train dataset") + plot_theme

# Amend table to include regularised model RMSE result
rmse_results <- rmse_results %>% rbind(c("Regularised Movie, User, Genre, Year and Review Date effects", round(regularised_rmse, 5), format(round(regularised_rmse-rmse_objective, 5), scientific = F)))

###########################################################################################################################
# Results - tabulate result of final hold-out test of algorithm trained in full edx dataset to predict in validation datset
###########################################################################################################################

#Create table to show final validation RMSE result and project objective
final_results <- data.frame(Method = "Project objective", RMSE = "0.86490", Difference = "-") %>% rbind(c("Validation of Final Model", round(valid_rmse, 5), format(round(valid_rmse-rmse_objective, 5), scientific = F)))
