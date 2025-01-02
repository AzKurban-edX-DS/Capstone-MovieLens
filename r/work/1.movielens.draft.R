# library(caret)
# library(lubridate)

start_date <- function(){
  print(date())
  Sys.time()
}
end_date <- function(start){
  print(date())
  Sys.time() - start
}

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

## The Netflix Prize Dataset
# https://www.asc.ohio-state.edu/statistics/statgen/joul_aut2009/BigChaos.pdf

# Initial Data -----------------------------------------
np_training_set_cnt <- 100480507
np_probe_set_cnt <- 1408395  # subset of `training_set`

probe_set_ratio <- np_probe_set_cnt/np_training_set_cnt
#> [1] 0.0140166

np_qualifying_set_cnt <- 2817131

quiz_set_ratio <- 0.5
test_set_ratio <- 1 - quiz_set_ratio

np_rmse_accepted_max <- 0.8563

#-------------------------------------------------------
#> The goal of the contest is to predict the qualifying set (size: 2817131 samples) 
#> and achieve a RMSE score of at least 0.8563 on the quiz subset, 
#> to get qualifed for the Grand Prize.

#####################################################

# Inspired by:
# HarvardX: PH125.8x
# Data Science: Machine Learning
## Section 6.2: Recommendation Systems

# Textbook:
### Section 33.7. Recommendation Systems
# https://rafalab.github.io/dsbook/large-datasets.html#recommendation-systems

# Split the `edx` dataset in `train_set` & `probe_set` ------------------------

#str(edx)
# 'data.frame':	9000055 obs. of  6 variables:
#   $ userId   : int  1 1 1 1 1 1 1 1 1 1 ...
# $ movieId  : int  122 185 292 316 329 355 356 362 364 370 ...
# $ rating   : num  5 5 5 5 5 5 5 5 5 5 ...
# $ timestamp: int  838985046 838983525 838983421 838983392 838983392 838984474 838983653 838984885 838983707 838984596 ...
# $ title    : chr  "Boomerang (1992)" "Net, The (1995)" "Outbreak (1995)" "Stargate (1994)" ...
# $ genres   : chr  "Comedy|Romance" "Action|Crime|Thriller" "Action|Drama|Sci-Fi|Thriller" "Action|Adventure|Sci-Fi" ...

#str(final_holdout_test)

set.seed(2006)
probe_index <- createDataPartition(y = edx$rating, times = 1,
                                   p = probe_set_ratio, list = FALSE)
probe_set_tmp <- edx[probe_index,]
train_set <- edx[-probe_index,]
head(train_set)

#> Make sure userId and movieId in final hold-out test set 
#> are also in `train_set` set
probe_set <- probe_set_tmp |> 
  semi_join(train_set, by = "movieId") |>
  semi_join(train_set, by = "userId")

y <- select(train_set, movieId, userId, rating) |>
  pivot_wider(names_from = movieId, values_from = rating) 

rnames <- y$userId

y <- as.matrix(y[,-1])
rownames(y) <- rnames

movie_map <- train_set |> select(movieId, title) |> 
  distinct(movieId, .keep_all = TRUE)

# Calculate Naive RMSE -------------------------------------------------------
mu <- mean(train_set$rating)
mu
#> [1] 3.512465

naive_rmse <- RMSE(probe_set$rating, mu)
naive_rmse
#> [1] 1.061429

## Modeling movie effects
#-------------------------------------------------------
# Y(i,u) = μ + b(i) + ε(i,u)

movie_avgs <- train_set |> 
  group_by(movieId) |> 
  summarize(b_i = mean(rating - mu))

str(movie_avgs)
# tibble [10,673 × 2] (S3: tbl_df/tbl/data.frame)
# $ movieId: int [1:10673] 1 2 3 4 5 6 7 8 9 10 ...
# $ b_i    : num [1:10673] 0.415 -0.307 -0.363 -0.643 -0.443 ...

head(movie_avgs)
# # A tibble: 6 × 2
#   movieId    b_i
#     <int>  <dbl>
# 1       1  0.415
# 2       2 -0.307
# 3       3 -0.363
# 4       4 -0.643
# 5       5 -0.443
# 6       6  0.303

preds <- mu + probe_set |> 
  left_join(movie_avgs, by='movieId') |>
  pull(b_i)

str(preds)
head(preds)
head(probe_set$rating)

mean(preds)
#> [1] 3.512969

movie_model_rmse <- RMSE(probe_set$rating, preds)
movie_model_rmse
#> [1] 0.9442118
## Calculate user effects ------------------------------------------
# Y(i,u) = μ + b(i) + b(u) + ε(i,u)

user_means <- function(movies_fit){
  # b(u) = mean(y(i, u) - μ - b_i(i)) 
  train_set |>
    left_join(movies_fit, by='movieId') |>
    group_by(userId) |>
    summarize(b_u = mean(rating - mu - b_i))
}
movie_user_rmse <- function(movies_fit, users_fit, min_user_rates){
  predicted_ratings <- probe_set |>
    left_join(movies_fit, by='movieId') |>
    left_join(users_fit, by='userId') |>
    mutate(pred = mu + b_i + b_u) |>
    pull(pred)
  
  #head(predicted_ratings)
  
  RMSE(probe_set$rating, predicted_ratings)
}
#------------------------------------------------------------------
min_user_rates <- 100
n_bins <- 30

train_set |> 
  group_by(userId) |> 
  summarize(b_u = mean(rating)) |> 
  filter(n()>=min_user_rates) |>
  ggplot(aes(b_u)) + 
  geom_histogram(bins = n_bins, color = "black")

fit_users <- user_means(movie_avgs)
model_2_rmse <- movie_user_rmse(movie_avgs, fit_users, min_user_rates)
model_2_rmse
#> [1] 0.8673005

# Improve Movie Effect Calculation using Regularization ------------------------
# Inspired by the textbook section:
### 33.9.2 Penalized least squares
# https://rafalab.dfci.harvard.edu/dsbook/large-datasets.html#penalized-least-squares

#> Instead of minimizing the least squares equation, 
#> we minimize an equation that adds a penalty:

#  ∑{u,i}(y[u,i] - μ - b[i])^2 + λ*∑{i}b[i]^2

#> The values of `b[i]` that minimize this equation are:

# b_hat[i](λ) = 1/(λ + n[i])*∑{u=1,n[i]}(Y[u,i] - μ_hat)
# where `n[i]` is the number of ratings made for movie `i`.

# To select `λ`, we use cross validation:
fit_movies <- data.frame(movieId = as.integer(colnames(y)), 
                         mu = mu)
head(fit_movies)
#   movieId      mu
# 1     122 3.51248
# 2     185 3.51248
# 3     292 3.51248
# 4     316 3.51248
# 5     329 3.51248
# 6     355 3.51248

n <- colSums(!is.na(y))
fit_movies$n <- n

str(fit_movies)
# 'data.frame':	10673 obs. of  3 variables:
#   $ movieId: int  122 185 292 316 329 355 356 362 364 370 ...
# $ mu     : num  3.51 3.51 3.51 3.51 3.51 ...
# $ n      : num  2155 13302 14248 16823 14335 ...

head(fit_movies)
#   movieId      mu     n
# 1     122 3.51248  2155
# 2     185 3.51248 13302
# 3     292 3.51248 14248
# 5     329 3.51248 14335
# 6     355 3.51248  4777

sums <- colSums(y - mu, na.rm = TRUE)

lambdas <- seq(0, 10, 0.1)

rmses <- sapply(lambdas, function(lambda){
  b_i <-  sums / (n + lambda)
  fit_movies$b_i <- b_i
  left_join(probe_set, fit_movies, by = "movieId") |> mutate(pred = mu + b_i) |> 
    summarize(rmse = RMSE(rating, pred)) |>
    pull(rmse)
})

# We can then select the value that minimizes the RMSE:

qplot(lambdas, rmses, geom = "line")
min(rmses)
#> [1] 0.9440946

lambda <- lambdas[which.min(rmses)]
print(lambda)
#> [1] 4.2

fit_movies$b_i <- colSums(y - mu, na.rm = TRUE) / (n + lambda)

str(fit_movies)
# 'data.frame':	10673 obs. of  4 variables:
#   $ movieId: int  122 185 292 316 329 355 356 362 364 370 ...
# $ mu     : num  3.51 3.51 3.51 3.51 3.51 ...
# $ n      : num  2155 13302 14248 16823 14335 ...
# $ b_i: num  -0.6523 -0.3828 -0.0958 -0.1629 -0.1735 ...

head(fit_movies)
#   movieId      mu     n     b_i
# 1     122 3.51248  2155 -0.65227627
# 2     185 3.51248 13302 -0.38279240
# 3     292 3.51248 14248 -0.09576196
# 4     316 3.51248 16823 -0.16291808
# 5     329 3.51248 14335 -0.17346883
# 6     355 3.51248  4777 -1.02372169

## Calculate user effects for improved movie effects --------------
fit_users <- user_means(fit_movies)
model_3_rmse <- movie_user_rmse(fit_movies, fit_users, min_user_rates)
model_3_rmse
#> [1] 0.8670935

# Calculate Date Smoothed Effect -------------------------------------------------------
# Y(i,u) = μ + b(i) + b(u) + f(d(u,i)) + ε(i,u)
# with `j` a smooth function of `d(u,i)`

# library(lubridate)

date()
start <- start_date()
train_set_dm <- train_set |> 
  left_join(fit_movies, by='movieId') |>
  left_join(fit_users, by='userId') |>
  mutate(rating_residue = rating - mu - b_i - b_u) |>
  mutate(date_time = as_datetime(timestamp)) |>
  mutate(date = as_date(date_time)) 

min_date <- min(train_set_dm$date)

train_set_dm <- train_set_dm |>
  mutate(days = as.integer(date - min_date)) #|>
#arrange(date)

head(train_set_dm)
end_date(start)
date()

date()
start <- start_date()
date_global_effect <- train_set_dm |>
  group_by(days, date) |>
  summarise(de = mean(rating_residue))
end_date(start)
date()
head(date_global_effect)
sum(is.na(date_global_effect$de))

# Train model using `loess` function with default `span` & `degree` params-----
date()
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

date()
start <- start_date()
date_smoothed_effect |>
  ggplot(aes(x = days)) +
  geom_point(aes(y = de), size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(y = de_smoothed), color = "red")
end_date(start)
date()

# Re-train tuning `loess` function's `span` & `degree` params-------------------

fit_loess <- function(spans, dgr){
  fits <- sapply(spans, function(span){
    fit <- loess(de ~ days, span = span, degree = dgr, data = date_global_effect)
    fit$fitted
  })
}
predict_date_smoothed <- function(movies_fit, users_fit, date_smoothed_effect){
  preds <- probe_set |>
    mutate(date = as_date(as_datetime(timestamp))) |>
    left_join(movies_fit, by='movieId') |>
    left_join(users_fit, by='userId') |>
    left_join(date_smoothed_effect, by='date') |>
    mutate(pred = mu + b_i + b_u + de_smoothed) |>
    pull(pred)

  RMSE(preds, probe_set$rating)
}
predict_de_probe <- function(movies_fit, users_fit, fits){
  model_diu_rmses <- sapply(fits, function(smth){
    date_smoothed_effect <- as.data.frame(date_global_effect) |>
      mutate(de_smoothed = smth)
    #head(date_smoothed_effect)
    
    predict_date_smoothed(movies_fit, users_fit, date_smoothed_effect)
  })
}
#-------------------------------------------------------------------------
date_smoothed_rmse <- function(fit_movies, fit_users, spans, degree) {
  start <- start_date()
  fits <- fit_loess(spans,degree)
  end_date(start)
  
  dim(fits)
  df_fits <- as.data.frame(fits)
  #str(df_fits)
  
  start <- start_date()
  model_diu_rmses <- predict_de_probe(fit_movies, fit_users, df_fits)
  end_date(start)
  
  plot(model_diu_rmses)
  
  idx <- which.min(model_diu_rmses)
  c(spans[idx], min(model_diu_rmses))
}

degree <- c(0, 1, 2)
# 1. `degree = 0` --------------------------------------------------------------
# spans <- seq(0.0003, 0.002, 0.00001)
spans <- seq(0.0005, 0.0015, 0.00001)
ds_rmse0 <- date_smoothed_rmse(fit_movies, fit_users, spans, degree[1])
#> [1] 0.8668279
# 2. `degree = 1` --------------------------------------------------------------
#spans <- seq(0.0005, 0.002, 0.00001)
spans <- seq(0.001, 0.0014, 0.00001)
ds_rmse1 <- date_smoothed_rmse(fit_movies, fit_users, spans, degree[2])
#> [1] 0.8667133
# 3. `degree = 2` --------------------------------------------------------------
#spans <- seq(0.0003, 0.01, 0.00001)
spans <- seq(0.0007, 0.002, 0.00001)
ds_rmse2 <- date_smoothed_rmse(fit_movies, fit_users, spans, degree[3])
#> [1] 0.866782
#------------------------------

loess_rmse <- data.frame(degree = degree, 
                         span = c(ds_rmse0[1], ds_rmse1[1], ds_rmse2[1]),
                         rmse = c(ds_rmse0[2], ds_rmse1[2], ds_rmse2[2]))
loess_rmse

idx_best_rmse <- which.min(loess_rmse$rmse)

best_degree <- loess_rmse[idx_best_rmse, 1]  # 1
best_span <- loess_rmse[idx_best_rmse, 2]# 0.00108
best_rmse <- loess_rmse[idx_best_rmse, 3]
best_rmse

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

best_date_smoothed_rmse <- predict_date_smoothed(fit_movies, 
                                                 fit_users, 
                                                 date_smoothed_effect)
best_date_smoothed_rmse
#> [1] 0.8667133

# Improve the Model using Matrix factorization ------------------------
# Inspired by the textbook section:
### 33.11 Matrix factorization
# https://rafalab.dfci.harvard.edu/dsbook/large-datasets.html#matrix-factorization

# Y(i,u) = μ + b(i) + b(u) + f(d(u,i)) + ε(i,u)

# Compute residuals for the final model:

r <- sweep(y - mu, 2, fit_movies$b_i) - 
  fit_users$b_u - date_smoothed_effect$de_smoothed

colnames(r) <- with(movie_map, title[match(colnames(r), movieId)])

# Y(i,u) = μ + b(i) + b(u) + f(d(u,i)) + p(u)q(i) + ε(i,u)

# Y(i,u) = μ + b(i) + b(u) + f(d(u,i)) + p(u,1)q(1,i) + p(u,2)q(2,i) + ε(i,u)









