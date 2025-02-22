## Setup -----------------------------------------------------------------------

if(!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table))
  install.packages("data.table", repos = "http://cran.us.r-project.org")

if(!require(gtools)) 
  install.packages("gtools")
if(!require(pak)) 
  install.packages("pak")

# Loading the required libraries
library(dslabs)
library(tidyverse)

library(caret)
library(cowplot)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(Metrics)
library(recosystem)
library(scales)
library(stringr)
library(tibble)
library(tidyr)


library(rafalib)
library(gtools)
library(pak)

## Datasets ===================================================================

# To start with we have to generate two datasets derived from the MovieLens one:
#   
# *`edx`: we use it to develop and train our algorithms;
# * `final_holdout_test`:  according to the course requirements, 
#    we use it exclusively to evaluate the _**RMSE**_ of our final algorithm.
# 
# For this purpose the following package has been developed by the author of this 
# script: `edx.capstone.movielens.data`. The source code of the package is available 
# on `GitHub`(https://github.com/AzKurban-edX-DS/edx.capstone.movielens.data).
# 
# Let's install the development version of this package from the `GitHub` repository 
# and attach the correspondent library to the global environment:

if(!require(edx.capstone.movielens.data)) pak::pak("AzKurban-edX-DS/edx.capstone.movielens.data")
library(edx.capstone.movielens.data)

edx <- edx.capstone.movielens.data::edx
final_holdout_test <- edx.capstone.movielens.data::final_holdout_test

summary(edx)
summary(final_holdout_test)

### `edx` Dataset --------------------------------------------------------------

# Let's look into the details of the `edx` dataset:
str(edx)

#> Also, we can see that no movies have a rating of 0. 
#> Movies are rated from 0.5 to 5.0 in 0.5 increments:

#library(dplyr)
s <- edx |> group_by(rating) |>
  summarise(n = n())
print(s)

#### Movie Genres Data ---------------------------------------------------------

#>The following code computes movie rating summaries by popular genres 
#>like Drama, Comedy, Thriller, and Romance:

#library(stringr)
genres <- c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})

#### Movies' Popularity --------------------------------------------------------
#> Further, we can find out the movies that have the greatest number of ratings 
#> using the following code:

ordered_movie_ratings <- edx |> group_by(movieId, title) |>
  summarize(number_of_ratings = n()) |>
  arrange(desc(number_of_ratings))
print(head(ordered_movie_ratings))

#### Rating Distribution -------------------------------------------------------

#> The following code figure out the most given ratings in order from most to least:
ratings <- edx |>  group_by(rating) |>
  summarise(count = n()) |>
  arrange(desc(count))
print(ratings)

#> The following code allows us to summarize that in general, half-star ratings 
#> are less common than whole-star ratings (e.g., there are fewer ratings of 3.5 
#> than there are ratings of 3 or 4, etc.):
print(edx |> group_by(rating) |> summarize(count = n()))

#> We can visually see that from the following plot:
edx |>
  group_by(rating) |>
  summarize(count = n()) |>
  ggplot(aes(x = rating, y = count)) +
  geom_line() 

#> The code below demonstrates another way of visualizing the rating distribution:
edx |>
  group_by(rating) |>
  summarize(count = n()) |>
  ggplot(aes(x = rating, y = count)) +
  geom_bar(stat = "identity", fill = "#8888ff") +
  ggtitle("Rating Distribution") +
  xlab("Rating") +
  ylab("Occurrences Count") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(n.breaks = 10) +
  theme_economist() +
  theme(axis.title.x = element_text(vjust = -5, face = "bold"), 
        axis.title.y = element_text(vjust = 10, face = "bold"), 
        plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))

## Methods / Analysis ==========================================================
### Defining helper functions --------------------------------------------------

#> Let's define some helper functions that we will use in our subsequent analysis:
start_date <- function(){
  print(date())
  Sys.time()
}
end_date <- function(start){
  print(date())
  Sys.time() - start
}
rmse <- function(r) sqrt(mean(r^2))
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Because we know ratings can’t be below 0.5 or above 5, 
# we define the function clamp:
clamp <- function(x, min = 0.5, max = 5) pmax(pmin(x, max), min)

### Preparing train and set datasets -------------------------------------------

#> First, let's note that we have 10677 different movies: 
n_movies <- n_distinct(edx$movieId)
print(n_movies)

# and 69878 different users in the dataset:
n_users <- n_distinct(edx$userId)
print(n_users)

#> Now, note the expressions below which confirm the fact explained in 
#> Section 23.1.1 Movielens data
#> (https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#movielens-data) 
#> of the Course Textbook that not every user rated every movie:

max_possible_ratings <- n_movies*n_users
sprintf("Maximum possible ratings: %s", max_possible_ratings)
sprintf("Rows in `edx` dataset: %s", dim_edx[1])
sprintf("Not every movie was rated: %s", max_possible_ratings > dim_edx[1])

#> We can think of a recommendation system as filling in the `NA`s in the dataset 
#> for the movies that some or all the users do not rate. 
#> A sample from the `edx` data below illustrates this idea: 

keep <- edx |> 
  dplyr::count(movieId) |> 
  top_n(4, n) |> 
  pull(movieId)

tab <- edx |> 
  filter(movieId %in% keep) |> 
  filter(userId %in% c(13:20)) |> 
  select(userId, title, rating) |> 
  mutate(title = str_remove(title, ", The"),
         title = str_remove(title, ":.*")) |>
  pivot_wider(names_from = "title", values_from = "rating")

print(tab)

#> We split the `edx` dataset into a training set, which we will use to build 
#> and train our models, and a test set in which we will compute the accuracy 
#> of our predictions, the way described in the `Section 23.1.1 Movielens data`
#> (https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#movielens-data) 
#> of the Course Textbook:

# Ignoring the data for users who have not provided at least 100 ratings:
  edx100 <- edx |> 
  group_by(userId) |>
  filter(n() >= 100) |>
  ungroup()

print(edx100 |> summarize(n_distinct(userId), n_distinct(movieId)))

# For each one of these users, we will split their ratings into 80% for training 
# and 20% for testing:

set.seed(2006)
indexes <- split(1:nrow(edx100), edx100$userId)
test_ind <- sapply(indexes, function(i) sample(i, ceiling(length(i)*.2))) |> 
  unlist() |>
  sort()

test_set <- edx100[test_ind,] 
train_set <- edx100[-test_ind,]

# To make sure we don’t include movies in the training set that should not be 
# there, we remove entries using the semi_join function:
test_set <- test_set |> semi_join(train_set, by = "movieId") |> as.data.frame()
summary(test_set)

train_set <- mutate(train_set, userId = factor(userId), movieId = factor(movieId))
summary(train_set)

#> We will use the array representation described in `Section 17.5 of the Textbook`
#> (https://rafalab.dfci.harvard.edu/dsbook-part-2/linear-models/treatment-effect-models.html#sec-anova), 
#> for the training data. 
#> To create this matrix, we use `tidyr::pivot_wider` function:
                                                                                 
y <- dplyr::select(train_set, movieId, userId, rating) |>
 pivot_wider(names_from = movieId, values_from = rating) |>
 column_to_rownames("userId") |>
 as.matrix()

dim(y)

#> To be able to map movie IDs to titles we create the following lookup table:

movie_map <- train_set |> dplyr::select(movieId, title, genres) |> 
 distinct(movieId, .keep_all = TRUE)

summary(movie_map)

### Naive Model ----------------------------------------------------------------
# Reference: the Textbook section "23.3 A first model"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#a-first-model

#>  A model that assumes the same rating for all movies and users with all 
#>  the differences explained by random variation would look as follows:
# Y[i,j] = μ + ε[i,j]

### Naive RMSE -------------------------------------------------------
mu <- mean(y, na.rm = TRUE)
print(mu)
#> [1] 3.471931

# If we predict all unknown ratings with `μ`, we obtain the following RMSE: 
rmse(test_set$rating - mu)
#> [1] 1.05508

#> If we plug in any other number, we will get a higher RMSE. 
#> Let's prove that by the following small investigation:

deviation <- seq(0, 6, 0.1) - 3
print(deviation)

rmse_value <- sapply(deviation, function(diff){
  rmse(test_set$rating - mu + diff)
})

plot(deviation, rmse_value, type = "l")

sprintf("Minimum RMSE is achieved when the deviation from the mean is: %s", 
        deviation[which.min(rmse_value)])
#> [1] "Minimum RMSE is achieved when the deviation from the mean is: 0"


### Taking into account User effects ------------------------------------------- 
# Reference: the Textbook section "23.4 User effects"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#user-effects

# If we visualize the average rating for each user:
hist(rowMeans(y, na.rm = TRUE), nclass = 30)

# we notice that there is substantial variability across users.
#>  To account for this, we can use a linear model with a treatment effect `α[i]` 
#>  for each user. The sum `μ + α[i]` can be interpreted as the typical 
#>  rating user `i` gives to movies. We can write the model as:

# Y[i,j] = μ + α[i] + ε[i,j]

#> It can be shown that the least squares estimate `α[i]` is just the average 
#> of `y[i,j] - μ` for each user. So we can compute them this way:
a <- rowMeans(y - mu, na.rm = TRUE)

#> Finally, we are ready to compute the `RMSE` (additionally using the helper 
#> function `clamp` we defined above to keep predictions in the proper range):

# Compute the RMSE taking into account user effects:
user_effects_rmse <- test_set |> 
  left_join(data.frame(userId = as.integer(names(a)), a = a), by = "userId") |>
  mutate(resid = rating - clamp(mu + a)) |> 
  filter(!is.na(resid)) |>
  pull(resid) |> rmse()

print(user_effects_rmse)
#> [1] 0.9711968

### Taking into account Movie effects ------------------------------------------

# Reference: the Textbook section "23.5 Movie effects"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#movie-effects

#> We know from experience that some movies are just generally rated higher 
#> than others. We can use a linear model with a treatment effect `β[j]` 
#> for each movie, which can be interpreted as movie effect or the difference 
#> between the average ranking for movie `j` and the overall average `μ`:

# Y[i,j] = μ + α[i] + β[j] + ε[i,j]

#> We use an approximation by first computing the least square estimate `μ` and
#> α[i], and then estimating β[j] as the average of the residuals 
#> `y[i,j] - μ - α[i]`:

### Model building -------------------------------------------------------------
b <- colMeans(y - mu - a, na.rm = TRUE)

#> We can now construct predictors and see how much the `RMSE` improves:

user_and_movie_effects_rmse <- test_set |> 
  left_join(data.frame(userId = as.integer(names(a)), a = a), by = "userId") |>
  left_join(data.frame(movieId = as.integer(names(b)), b = b), by = "movieId") |>
  mutate(resid = rating - clamp(mu + a + b)) |>  
  filter(!is.na(resid)) |>
  pull(resid) |> rmse()

print(user_and_movie_effects_rmse)
#> [1] 0.8660078

### Utilizing Penalized least squares-------------------------------------------

# Reference: the Textbook section "23.6 Penalized least squares"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#penalized-least-squares

#> Instead of minimizing the least squares equation, 
#> we minimize an equation that adds a penalty:

#  ∑{i,j}(y[i,j] - μ - α[i] - β[j])^2 + λ*∑{j}β[j]^2

#> The values of `β[j]` that minimize this equation are:

# β[j](λ) = 1/(λ + n[j])*∑{u=1,n[i]}(Y[i,j] - μ - α[i])
# where `n[j]` is the number of ratings made for movie `j`.

#### Support function ----------------------------------------------------------

#> We will use the following function to calculate _RMSE_ in this section:
reg_rmse <- function(b){
  test_set |> 
    left_join(data.frame(userId = as.integer(names(a)), a = a), by = "userId") |>
    left_join(data.frame(movieId = as.integer(names(b)), b = b), by = "movieId") |>
    mutate(resid = rating - clamp(mu + a + b)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> rmse()
}

### Model building -------------------------------------------------------------

#> Here we will simply compute the RMSE we for different values of `λ`: 
n <- colSums(!is.na(y))

sums <- colSums(y - mu - a, na.rm = TRUE)
lambdas <- seq(0, 10, 0.1)

rmses <- sapply(lambdas, function(lambda){
  b <-  sums / (n + lambda)
  reg_rmse(b)
})

# Here is a plot of the RMSE versus `lambda`:
plot(lambdas, rmses, type = "l")

#> Now we can determine the minimal _RMSE_:
print(min(rmses))
#> [1] 0.8659219

#> which is achieved for the following `λ`:
lambda <- lambdas[which.min(rmses)] 
print(lambda)
#> [1] 2.6

#> Using minimal `λ`, we can compute the regularized estimates:
b_reg <- sums / (n + lambda)

#> Finally, let's verify that the penalized estimates 
#> we have just computed actually result in the minimal `RMSE` figured out above: 
reg_rmse(b_reg)
#> [1] 0.8659219

# Calculate Date Smoothed Effect -------------------------------------------------------
# Y[i,j] = μ + α[i] + β[j] + f(d(i,j)) + ε[i,j]

# with `j` a smooth function of `d(u,i)`

# library(lubridate)

# Plot: Average rating per year
train_set |> 
  mutate(year = year(as_datetime(timestamp))) |>
  group_by(year) |>
  summarize(avg = mean(rating)) |>
  ggplot(aes(x = year, y = avg)) +
  geom_bar(stat = "identity", fill = "#8888ff") + 
  ggtitle("Average rating per year") +
  xlab("Year") +
  ylab("Average rating") +
  scale_y_continuous(labels = comma) + 
  theme_economist() +
  theme(axis.title.x = element_text(vjust = -5, face = "bold"), 
        axis.title.y = element_text(vjust = 10, face = "bold"), 
        plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))



start <- start_date()
train_set_dm <- train_set |> 
  left_join(data.frame(userId = as.factor(names(a)), a = a), by = "userId") |>
  left_join(data.frame(movieId = as.factor(names(b)), b = b_reg), by = "movieId") |>
  mutate(rating_residue = rating - mu - a - b) |>
  mutate(date_time = as_datetime(timestamp)) |>
  mutate(date = as_date(date_time)) 

min_date <- min(train_set_dm$date)
print(min_date)

train_set_dm <- train_set_dm |>
  mutate(days = as.integer(date - min_date)) #|>
#arrange(date)

str(train_set_dm)
end_date(start)

start <- start_date()
date_global_effect <- train_set_dm |>
  group_by(days, date) |>
  summarise(de = mean(rating_residue))
end_date(start)

head(date_global_effect)
sum(is.na(date_global_effect$de))

# Train model using `loess` function with default `span` & `degree` params-----
start <- start_date()
fit <- loess(de ~ days, data = date_global_effect)
end_date(start)
date()
sum(is.na(fit$fitted))
str(fit$pars)
str(fit$fitted)

date_smoothed_effect <- as.data.frame(date_global_effect) |>
  mutate(de_smoothed = fit$fitted)
head(date_smoothed_effect)

start <- start_date()
date_smoothed_effect |>
  ggplot(aes(x = days)) +
  geom_point(aes(y = de), size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(y = de_smoothed), color = "red")
end_date(start)

# Re-train tuning `loess` function's `span` & `degree` params-------------------
fit_loess <- function(spans, dgr){
  fits <- sapply(spans, function(span){
    fit <- loess(de ~ days, span = span, degree = dgr, data = date_global_effect)
    fit$fitted
  })
}
predict_date_smoothed <- function(date_smoothed_effect){
  preds <- test_set |>
    mutate(date = as_date(as_datetime(timestamp))) |>
    left_join(data.frame(userId = as.integer(names(a)), a = a), by = "userId") |>
    left_join(data.frame(movieId = as.integer(names(b)), b = b_reg), by = "movieId") |>
    left_join(date_smoothed_effect, by='date') |>
    mutate(pred = mu + a + b + de_smoothed) |>
    pull(pred)
  
  RMSE(preds, test_set$rating)
}
predict_de_model <- function(fits){
  model_diu_rmses <- sapply(fits, function(smth){
    date_smoothed_effect <- as.data.frame(date_global_effect) |>
      mutate(de_smoothed = smth)

    predict_date_smoothed(date_smoothed_effect)
  })
}
date_smoothed_rmse <- function(spans, degree) {
  start <- start_date()
  fits <- fit_loess(spans,degree)
  end_date(start)
  
  dim(fits)
  df_fits <- as.data.frame(fits)
  #str(df_fits)
  
  start <- start_date()
  model_diu_rmses <- predict_de_model(df_fits)
  end_date(start)
  
  plot(model_diu_rmses)
  
  idx <- which.min(model_diu_rmses)
  c(spans[idx], min(model_diu_rmses))
}
#-------------------------------------------------------------------------

degree <- c(0, 1, 2)
# 1. `degree = 0` --------------------------------------------------------------
# spans <- seq(0.0003, 0.002, 0.00001)
spans <- seq(0.0005, 0.0015, 0.00001)
ds_rmse0 <- date_smoothed_rmse(spans, 
                               degree[1])
ds_rmse0
#> [1] 0.0010900 0.8644363

# 2. `degree = 1` --------------------------------------------------------------
#spans <- seq(0.0005, 0.002, 0.00001)
spans <- seq(0.001, 0.0014, 0.00001)
ds_rmse1 <- date_smoothed_rmse(spans, 
                               degree[2])
ds_rmse1
#> [1] 0.001000 0.863969

# 3. `degree = 2` --------------------------------------------------------------
#spans <- seq(0.0003, 0.01, 0.00001)
spans <- seq(0.0007, 0.002, 0.00001)
ds_rmse2 <- date_smoothed_rmse(spans, 
                               degree[3])
ds_rmse2
#> [1] 0.0013000 0.8638975

# Retrain with the best parameters figured out above ---------------------------

loess_rmse <- data.frame(degree = degree, 
                         span = c(ds_rmse0[1], ds_rmse1[1], ds_rmse2[1]),
                         rmse = c(ds_rmse0[2], ds_rmse1[2], ds_rmse2[2]))
print(loess_rmse)

idx_best_rmse <- which.min(loess_rmse$rmse)

best_degree <- loess_rmse[idx_best_rmse, 1]  # 1
best_span <- loess_rmse[idx_best_rmse, 2]# 0.00108
best_rmse <- loess_rmse[idx_best_rmse, 3]
print(best_rmse)

start <- start_date()
fit <- loess(de ~ days, 
             degree = best_degree, 
             span = best_span, 
             data = date_global_effect)
end_date(start)
sum(is.na(fit$fitted))
str(fit$pars)
str(fit$fitted)
#fit$pars
# fit$fitted

date_smoothed_effect <- as.data.frame(date_global_effect) |>
  mutate(de_smoothed = fit$fitted)
head(date_smoothed_effect)

start <- start_date()
date_smoothed_effect |>
  ggplot(aes(x = days)) +
  geom_point(aes(y = de), size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(y = de_smoothed), color = "red")
end_date(start)

date_smoothed_rmse <- predict_date_smoothed(date_smoothed_effect)
print(date_smoothed_rmse)
#> [1] 0.8638975

# preds <- final_holdout_test |>
#   mutate(date = as_date(as_datetime(timestamp))) |>
#   left_join(data.frame(userId = as.integer(names(a)), a = a), by = "userId") |>
#   left_join(data.frame(movieId = as.integer(names(b)), b = b_reg), by = "movieId") |>
#   left_join(date_smoothed_effect, by='date') |>
#   mutate(pred = mu + a + b + de_smoothed) |>
#   pull(pred)
# 
# final_test_set <- final_holdout_test[!is.na(preds),]
# final_preds <- preds[!is.na(preds)]
# 
# RMSE(final_preds, final_test_set$rating)
#> [1] 0.8641795

### Accounting for Genre effect ------------------------------------------------

# Reference: the Textbook Section "23.7 Exercises" of the Chapter "23 Regularization"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#exercises

#> The `edx` dataset also has a genres column. This column includes 
#> every genre that applies to the movie 
#> (some movies fall under several genres)[@IDS2_23-7].

# Preparing data for plotting:
genre_ratins_grp <- train_set |> 
  mutate(genre_categories = as.factor(genres)) |>
  group_by(genre_categories) |>
  summarize(n = n(), rating_avg = mean(rating), se = sd(rating)/sqrt(n())) |>
  filter(n > 20000) |> 
  mutate(genres = reorder(genre_categories, rating_avg)) |>
  select(genres, rating_avg, se, n)

dim(genre_ratins_grp)
genre_ratins_grp_sorted <- genre_ratins_grp |> sort_by.data.frame(~ rating_avg)
print(genre_ratins_grp_sorted)

# Creating plot:
genre_ratins_grp |> 
  ggplot(aes(x = genres, y = rating_avg, ymin = rating_avg - 2*se, ymax = rating_avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  ggtitle("Average rating per Genre") +
  ylab("Average rating") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

sprintf("The worst ratings were for the genre category: %s",
        genre_ratins_grp$genres[which.min(genre_ratins_grp$genres)])

sprintf("The best ratings were for the genre category: %s",
        genre_ratins_grp$genres[which.max(genre_ratins_grp$genres)])

# Alternative way of visualizing a Genre Effect
#> Reference: Article "Movie Recommendation System using R - BEST" written by 
#> Amir Moterfaker (https://www.kaggle.com/amirmotefaker)
#> (section "Average rating for each genre")[@MRS-R-BEST]
#> https://www.kaggle.com/code/amirmotefaker/movie-recommendation-system-using-r-best/notebook#Average-rating-for-each-genre

# For better visibility, we reduce the data for plotting 
# while keeping the worst and best rating rows:
plot_ind <- odd(1:nrow(genre_ratins_grp))
plot_dat <- genre_ratins_grp_sorted[plot_ind,] 

plot_dat |>
  ggplot(aes(x = rating_avg, y = genres)) +
  ggtitle("Genre Average Rating") +
  geom_bar(stat = "identity", width = 0.6, fill = "#8888ff") +
  xlab("Average ratings") +
  ylab("Genres") +
  scale_x_continuous(labels = comma, limits = c(0.0, 5.0)) +
  theme_economist() +
  theme(plot.title = element_text(vjust = 3.5),
        axis.title.x = element_text(vjust = -5, face = "bold"),
        axis.title.y = element_text(vjust = 10, face = "bold"),
        axis.text.x = element_text(vjust = 1, hjust = 1, angle = 0),
        axis.text.y = element_text(vjust = 0.25, hjust = 1, size = 9),
        plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))

# Y[i,j] = μ + α[i] + β[j] + f(d(i,j)) + ∑{k=1,K}(x[i,j]^k*𝜸[k])  + ε[i,j]
# with `x[i,j]^k = 1` if g[i,j] is genre `k`

