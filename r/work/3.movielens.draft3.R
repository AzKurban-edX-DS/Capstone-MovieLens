library(caret)
library(lubridate)
library(dplyr)

# setwd(".../Capstone-MovieLens/r/work")

# Prepare shared auxiliary functions -------------------------------------------
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

## The Netflix Prize Dataset -------------------------------------------------
# https://www.asc.ohio-state.edu/statistics/statgen/joul_aut2009/BigChaos.pdf

#> The goal of the contest is to predict the qualifying set (size: 2817131 samples) 
#> and achieve a RMSE score of at least 0.8563 on the quiz subset, 
#> to get qualifed for the Grand Prize.

### Initial Data ---------------------------------------------------------------
np_training_set_cnt <- 100480507
np_probe_set_cnt <- 1408395  # subset of `training_set`

probe_set_ratio <- np_probe_set_cnt/np_training_set_cnt
#> [1] 0.0140166

np_qualifying_set_cnt <- 2817131

quiz_set_ratio <- 0.5
test_set_ratio <- 1 - quiz_set_ratio

np_rmse_accepted_max <- 0.8563

#####################################################

# Inspired by:
# HarvardX: PH125.8x
# Data Science: Machine Learning, Section 6.2: Recommendation Systems
#> The proposed solution below uses information (including citates) from the folowing textbook:
#> "Introduction to Data Science" written by Rafael A. Irizarry:
#> https://rafalab.dfci.harvard.edu/dsbook-part-2/ 

# Reference: the Textbook section: 23.1 Case study: recommendation systems
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#sec-recommendation-systems

## Split the `edx` dataset in `train_set` & `test_set` ------------------------

#str(edx)
# 'data.frame':	9000055 obs. of  6 variables:
# $ userId   : int  1 1 1 1 1 1 1 1 1 1 ...
# $ movieId  : int  122 185 292 316 329 355 356 362 364 370 ...
# $ rating   : num  5 5 5 5 5 5 5 5 5 5 ...
# $ timestamp: int  838985046 838983525 838983421 838983392 838983392 838984474 838983653 838984885 838983707 838984596 ...
# $ title    : chr  "Boomerang (1992)" "Net, The (1995)" "Outbreak (1995)" "Stargate (1994)" ...
# $ genres   : chr  "Comedy|Romance" "Action|Crime|Thriller" "Action|Drama|Sci-Fi|Thriller" "Action|Adventure|Sci-Fi" ...

#str(final_holdout_test)

### Prepare train & test datasets ----------------------------------------------

#> Let's see the number of unique users that provided ratings 
#> and how many unique movies were rated:
edx |> summarize(n_distinct(userId), n_distinct(movieId))
#   n_distinct(userId) n_distinct(movieId)
# 1              69878               10677

#> Let's ignore the data for users who have not provided at least 100 ratings:
edx100 <- edx |> 
  group_by(userId) |>
  filter(n() >= 100) |>
  ungroup()

edx100 |> summarize(n_distinct(userId), n_distinct(movieId))
# `     n_distinct(userId)` `n_distinct(movieId)`
#                    <int>                 <int>
#   1                24115                 10665

#> For each one of these users, we will split their ratings into 80% for training 
#> and 20% for testing:

set.seed(2006)
indexes <- split(1:nrow(edx100), edx100$userId)
test_ind <- sapply(indexes, function(i) sample(i, ceiling(length(i)*.2))) |> 
  unlist() |>
  sort()

test_set <- edx100[test_ind,] 
train_set <- edx100[-test_ind,]

#> To make sure we don’t include movies in the training set that should not be 
#> there, we remove entries using the semi_join function:
test_set <- test_set |> semi_join(train_set, by = "movieId") |>
  as.data.frame()

train_set <- mutate(train_set, userId = factor(userId), movieId = factor(movieId))
head(train_set)

#> Make sure userId and movieId in final hold-out test set 
#> are also in `train_set` set
# probe_set <- probe_set_tmp |> 
#   semi_join(train_set, by = "movieId") |>
#   semi_join(train_set, by = "userId")

#> We will use the array representation described in `Section 17.5`, 
#> for the training data: we denote ranking for movie `j` by user `i`as `y[i,j]`. 
#> To create this matrix, we use pivot_wider:
  
y <- dplyr::select(train_set, movieId, userId, rating) |>
pivot_wider(names_from = movieId, values_from = rating) |>
column_to_rownames("userId") |>
as.matrix()

dim_y <- dim(y)
dim_y
#> [1] 24115 10626

movie_map <- train_set |> dplyr::select(movieId, title, genres) |> 
  distinct(movieId, .keep_all = TRUE)

str(movie_map)
head(movie_map)

## First Model -----------------------------------------------------------------
# Reference: the Textbook section "23.3 A first model"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#a-first-model

#>  A model that assumes the same rating for all movies and users with all 
#>  the differences explained by random variation would look as follows:
# Y[i,j] = μ + ε[i,j]

### Naive RMSE -------------------------------------------------------
mu <- mean(train_set$rating)
mu
#> [1] 3.471931

naive_rmse <- RMSE(test_set$rating, mu)
naive_rmse
#> [1] 1.062162

# str(final_holdout_test)
# head(final_holdout_test)

final_naive_rmse <- RMSE(final_holdout_test$rating, mu)
final_naive_rmse
#> [1] 1.061958

## User effects ---------------------------------------------------------------- 
# Reference: the Textbook section "23.4 User effects"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#user-effects

# If we visualize the average rating for each user:
hist(rowMeans(y, na.rm = TRUE), nclass = 30)

# we notice that there is substantial variability across users.
#>  To account for this, we can use a linear model with a treatment effect `α[i]` 
#>  for each user. The sum `μ + α[i]` can be interpreted as the typical 
#>  rating user `i` gives to movies. We can write the model as:

# Y[i,j] = μ + α[i] + ε[i,j]

### Support functions ---------------------------------------------------------

#> Because we know ratings can’t be below 0.5 or above 5, 
#> we define the function clamp:
clamp <- function(x, min = 0.5, max = 5) pmax(pmin(x, max), min)

# to keep predictions in that range and then compute the RMSE:
user_effects_rmse <- function(test_set, a){
  test_set |> 
    left_join(data.frame(userId = as.integer(names(a)), a = a), by = "userId") |>
    mutate(resid = rating - clamp(mu + a)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> rmse()
}

### Model training ------------------------------------------------------
#min_user_rates <- 100
n_bins <- 30

train_set |> 
  group_by(userId) |> 
  summarize(user_ratings_avg = mean(rating)) |> 
  ggplot(aes(user_ratings_avg)) + 
  geom_histogram(bins = n_bins, color = "black")

#> We can show that the least squares estimate `α[i]` is just the average 
#> of `y[i,j] - μ` for each user. So we can compute them this way:
  
a <- rowMeans(y - mu, na.rm = TRUE)

### Model testing --------------------------------------------------------------
model_user_rmse <- user_effects_rmse(test_set, a)
model_user_rmse
#> [1] 0.9718791

final_model_user_rmse <- user_effects_rmse(final_holdout_test, a)
final_model_user_rmse
#> [1] 0.9720994

## Movie Effects ---------------------------------------------------- 
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

### Support functions ---------------------------------------------------------

# to keep predictions in that range and then compute the RMSE:
user_and_movie_effects_rmse <- function(test_set, a, b){
    test_set |> 
    left_join(data.frame(userId = as.integer(names(a)), a = a), by = "userId") |>
    left_join(data.frame(movieId = as.integer(names(b)), b = b), by = "movieId") |>
    mutate(resid = rating - clamp(mu + a + b)) |>  
    filter(!is.na(resid)) |>
    pull(resid) |> rmse()
}

### Model testing ----------------------------------------------------------------
#> We can now construct predictors and see how much the `RMSE` improves:
model_user_movie_rmse <- user_and_movie_effects_rmse(test_set, a, b)
model_user_movie_rmse
#> [1] 0.8664145

final_model_user_movie_rmse <- user_and_movie_effects_rmse(final_holdout_test, a, b)
final_model_user_movie_rmse
#> [1] 0.8665345

## Penalized Least Squares ----------------------------------
# Reference: the Textbook section "23.6 Penalized least squares"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#penalized-least-squares

#> Instead of minimizing the least squares equation, 
#> we minimize an equation that adds a penalty:

#  ∑{i,j}(y[i,j] - μ - α[i] - β[j])^2 + λ*∑{j}β[j]^2

#> The values of `β[j]` that minimize this equation are:

# β[j](λ) = 1/(λ + n[j])*∑{u=1,n[i]}(Y[i,j] - μ - α[i])
# where `n[j]` is the number of ratings made for movie `j`.

### Support functions ----------------------------------------------------------
reg_rmse <- function(test_set, b){
  test_set |> 
    left_join(data.frame(userId = as.integer(names(a)), a = a), by = "userId") |>
    left_join(data.frame(movieId = as.integer(names(b)), b = b), by = "movieId") |>
    mutate(resid = rating - clamp(mu + a + b)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> rmse()
}

### Model building -------------------------------------------------------------

#> Here we will simply compute the RMSE we for different values of `λ` 
#> to illustrate the effect:
n <- colSums(!is.na(y))
sums <- colSums(y - mu - a, na.rm = TRUE)
lambdas <- seq(0, 10, 0.1)
rmses <- sapply(lambdas, function(lambda){
  b <-  sums / (n + lambda)
  reg_rmse(test_set, b)
})

# Here is a plot of the RMSE versus `λ`:
plot(lambdas, rmses, type = "l")

min(rmses)
#> [1] 0.8659219

lambda <- lambdas[which.min(rmses)] 
lambda
#> [1] 2.6

#> Using minimal `λ`, we can compute the regularized estimates:
b_reg <- sums / (n + lambda)

### Model testing ----------------------------------------------------------------
reg_rmse(test_set, b_reg)
#> [1] 0.8659219
reg_rmse(final_holdout_test, b_reg)
#> [1] 0.8663589

## Matrix factorization --------------------------------------------------------
#> References: 
#> 1. The Textbook chapter "24  Matrix Factorization"
#> https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/matrix-factorization.html
#> 2. "Matrix Factorization Techniques for Recommendation Systems" by 
#> Yehuda Koren, Yahoo Research;
#> Robert Bell and Chris Volinsky, AT&T Labs - Research:
# https://datajobs.com/data-science-repo/Recommender-Systems-[Netflix].pdf

#> the model ignores an important source of information related to the fact 
#> that groups of movies, have similar rating patterns and groups of users have 
#> similar rating patterns as well.

# To see an example of this, we compute residuals:

# r[i,j] = y[i,j] - (μ + α[i] + β[j])
  
### Model building -------------------------------------------------------------

# Compute residuals for the model:
r <- sweep(y - mu - a, 2, b_reg)

# r_names <- colnames(r)
# head(r_names)

movie_titles <- 
  data.frame(titles = with(movie_map, title[match(colnames(r), movieId)]),
             genres = with(movie_map, genres[match(colnames(r), movieId)])) 

str(movie_titles)
head(movie_titles)

#### Principal Component Analysis (PCA) -----------------------------------
# Reference: the Textbook section "24.3 Case study: movie recommendations"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/matrix-factorization.html#connection-to-pca

#> We will rewrite the model from the previous chapter to include factors 
#> to explain similarities between movies:

# Y[i,j] = μ + α[i] + β[j] + ∑{k=[1,K]}p[i,k]q[j,k] + ε[i,j]

#> Unfortunately, we can’t fit this model with prcomp due to the missing values. 
#> We introduce the `missMDA` package that provides an approach to fit such models 
#> when matrix entries are missing, a very common occurrence 
#> in movie recommendations, through the function `imputePCA`. 
#> Also, because there are small sample sizes for several movie pairs, 
#> it is useful to regularize the `p`s. The imputePCA function also permits 
#> regularization.

#> We use the estimates for `μ`, the `α`s and `β`s calculated above, 
#> and estimate two factors (ncp = 2). 
#> We fit the model to movies rated more than 25 times, and, finally, 
#> we use regularization by setting the parameter `coeff.ridge` to the same value 
#> used to estimate the `β`s.

library(missMDA)

sum(is.na(y))

start <- start_date()
ind <- colSums(!is.na(y)) >= 25
# imputed <- imputePCA(r[,ind], ncp = 2, coeff.ridge = lambda)
end_date(start)

















