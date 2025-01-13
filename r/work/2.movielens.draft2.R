library(caret)
library(lubridate)
library(dplyr)

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
#---------------------------------------

## The Netflix Prize Dataset 
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
  
y <- select(train_set, movieId, userId, rating) |>
pivot_wider(names_from = movieId, values_from = rating) |>
column_to_rownames("userId") |>
as.matrix()

dim_y <- dim(y)
dim_y
#> [1] 24115 10626

movie_map <- train_set |> dplyr::select(movieId, title) |> 
  distinct(movieId, .keep_all = TRUE)
#------------------------------------

## First Model
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
#-----------------

## User effects 
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
#--------------------

## Movie Effects 
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

# Model testing ----------------------------------------------------------------
#> We can now construct predictors and see how much the `RMSE` improves:
model_user_movie_rmse <- user_and_movie_effects_rmse(test_set, a, b)
model_user_movie_rmse
#> [1] 0.8664145

final_model_user_movie_rmse <- user_and_movie_effects_rmse(final_holdout_test, a, b)
final_model_user_movie_rmse
#> [1] 0.8665345
#------------------

## Penalized Least Squares
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

min_lambda <- lambdas[which.min(rmses)] 
min_lambda
#> [1] 2.6

#> Using minimal `λ`, we can compute the regularized estimates:
b_reg <- sums / (n + min_lambda)

# Model testing ----------------------------------------------------------------
reg_rmse(test_set, b_reg)
#> [1] 0.8659219
reg_rmse(final_holdout_test, b_reg)
#> [1] 0.8663589

#------------------------------------------------------------
# sums <- colSums(y - mu, na.rm = TRUE)
# lambdas <- seq(0, 10, 0.1)
# 
# rmses <- sapply(lambdas, function(lambda){
#   b_i <-  sums / (n + lambda)
#   fit_movies$b_i <- b_i
#   left_join(probe_set, fit_movies, by = "movieId") |> mutate(pred = mu + b_i) |> 
#     summarize(rmse = RMSE(rating, pred)) |>
#     pull(rmse)
# })
# 
# # We can then select the value that minimizes the RMSE:
# qplot(lambdas, rmses, geom = "line")
# min(rmses)
# #> [1] 0.9440946
# 
# lambda <- lambdas[which.min(rmses)]
# print(lambda)
# #> [1] 4.2
# 
# fit_movies$b_i <- colSums(y - mu, na.rm = TRUE) / (n + lambda)

#str(fit_movies)
# 'data.frame':	10673 obs. of  4 variables:
#   $ movieId: int  122 185 292 316 329 355 356 362 364 370 ...
# $ mu     : num  3.51 3.51 3.51 3.51 3.51 ...
# $ n      : num  2155 13302 14248 16823 14335 ...
# $ b_i: num  -0.6523 -0.3828 -0.0958 -0.1629 -0.1735 ...

#head(fit_movies)
#   movieId      mu     n     b_i
# 1     122 3.51248  2155 -0.65227627
# 2     185 3.51248 13302 -0.38279240
# 3     292 3.51248 14248 -0.09576196
# 4     316 3.51248 16823 -0.16291808
# 5     329 3.51248 14335 -0.17346883
# 6     355 3.51248  4777 -1.02372169

## Calculate user effects for improved movie effects --------------
fit_users <- user_means(fit_movies)

model_3_rmse <- movie_user_rmse(probe_set, 
                                fit_movies, 
                                fit_users, 
                                min_user_rates)
model_3_rmse
#> [1] 0.8670935

model_3_rmse <- movie_user_rmse(final_holdout_test, 
                                fit_movies, 
                                fit_users, 
                                min_user_rates)
model_3_rmse
#> [1] 0.8652768

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
predict_date_smoothed <- function(test_set, 
                                  movies_fit, 
                                  users_fit, 
                                  date_smoothed_effect){
  preds <- test_set |>
    mutate(date = as_date(as_datetime(timestamp))) |>
    left_join(movies_fit, by='movieId') |>
    left_join(users_fit, by='userId') |>
    left_join(date_smoothed_effect, by='date') |>
    mutate(pred = mu + b_i + b_u + de_smoothed) |>
    pull(pred)

  RMSE(preds, test_set$rating)
}
predict_de_model <- function(test_set, movies_fit, users_fit, fits){
  model_diu_rmses <- sapply(fits, function(smth){
    date_smoothed_effect <- as.data.frame(date_global_effect) |>
      mutate(de_smoothed = smth)
    #head(date_smoothed_effect)
    
    predict_date_smoothed(test_set, movies_fit, users_fit, date_smoothed_effect)
  })
}
date_smoothed_rmse <- function(test_set, fit_movies, fit_users, spans, degree) {
  start <- start_date()
  fits <- fit_loess(spans,degree)
  end_date(start)
  
  dim(fits)
  df_fits <- as.data.frame(fits)
  #str(df_fits)
  
  start <- start_date()
  model_diu_rmses <- predict_de_model(test_set, fit_movies, fit_users, df_fits)
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
ds_rmse0 <- date_smoothed_rmse(probe_set, 
                               fit_movies, 
                               fit_users, 
                               spans, 
                               degree[1])
ds_rmse0
#> [1] 0.0010800 0.8668279

# 2. `degree = 1` --------------------------------------------------------------
#spans <- seq(0.0005, 0.002, 0.00001)
spans <- seq(0.001, 0.0014, 0.00001)
ds_rmse1 <- date_smoothed_rmse(probe_set, 
                               fit_movies, 
                               fit_users, 
                               spans, 
                               degree[2])
ds_rmse1
#> [1] 0.0010800 0.8667133

# 3. `degree = 2` --------------------------------------------------------------
#spans <- seq(0.0003, 0.01, 0.00001)
spans <- seq(0.0007, 0.002, 0.00001)
ds_rmse2 <- date_smoothed_rmse(probe_set, 
                               fit_movies, 
                               fit_users, 
                               spans, 
                               degree[3])
ds_rmse2
#> [1] 0.001510 0.866782
# Retrain with the best parameters figured out above ---------------------------

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

date_smoothed_rmse <- predict_date_smoothed(probe_set, 
                                            fit_movies, 
                                            fit_users, 
                                            date_smoothed_effect)
date_smoothed_rmse
#> [1] 0.8667133

final_date_smoothed_rmse <- predict_date_smoothed(final_holdout_test, 
                                                   fit_movies, 
                                                   fit_users, 
                                                   date_smoothed_effect)
final_date_smoothed_rmse
#> [1] 0.8649608

# Improve the Model using Matrix factorization ------------------------
# Inspired by the textbook section:
### 33.11 Matrix factorization
# https://rafalab.dfci.harvard.edu/dsbook/large-datasets.html#matrix-factorization

# Y(i,u) = μ + b(i) + b(u) + f(d(u,i)) + ε(i,u)

# Compute residuals for the final model:
r_m <- sweep(y - mu, 2, fit_movies$b_i)
r <- sweep(r_m, 1, fit_users$b_u)
colnames(r) <- with(movie_map, title[match(colnames(r), movieId)])
r_names <- colnames(r)
head(r_names)

godfather_idx <- str_detect(r_names, "Godfather")
sum(godfather_idx)
gdfth_nms <- r_names[godfather_idx]
length(gdfth_nms)
#> [1] 5

gdfth_nms
# [1] "Godfather, The (1972)"           "Godfather: Part II, The (1974)"  "Godfather: Part III, The (1990)" "Tokyo Godfathers (2003)"        
# [5] "3 Godfathers (1948)"           

library(gridExtra)

p12 <- qplot(r[ ,gdfth_nms[1]], 
            r[,gdfth_nms[2]], 
            xlab = gdfth_nms[1], 
            ylab = gdfth_nms[2])

p23 <- qplot(r[ ,gdfth_nms[2]], 
            r[,gdfth_nms[3]], 
            xlab = gdfth_nms[2], 
            ylab = gdfth_nms[3])

p13 <- qplot(r[ ,gdfth_nms[1]], 
           r[,gdfth_nms[3]], 
           xlab = gdfth_nms[1], 
           ylab = gdfth_nms[3])

grid.arrange(p12, p23 ,p13, ncol = 3)

p14 <- qplot(r[ ,gdfth_nms[1]], 
           r[,gdfth_nms[4]], 
           xlab = gdfth_nms[1], 
           ylab = gdfth_nms[4])

p24 <- qplot(r[ ,gdfth_nms[2]], 
            r[,gdfth_nms[4]], 
            xlab = gdfth_nms[2], 
            ylab = gdfth_nms[4])

p34 <- qplot(r[ ,gdfth_nms[3]], 
            r[,gdfth_nms[4]], 
            xlab = gdfth_nms[3], 
            ylab = gdfth_nms[4])

grid.arrange(p14, p24 ,p34, ncol = 3)

r123 <- r[, c(gdfth_nms[1], gdfth_nms[2], gdfth_nms[3])]
str(r123)

cor(r123, 
    use="pairwise.complete") |> 
  knitr::kable()
# |                                | Godfather, The (1972)| Godfather: Part II, The (1974)| Godfather: Part III, The (1990)|
# |:-------------------------------|---------------------:|------------------------------:|-------------------------------:|
# |Godfather, The (1972)           |             1.0000000|                      0.7351725|                       0.2481543|
# |Godfather: Part II, The (1974)  |             0.7351725|                      1.0000000|                       0.2448787|
# |Godfather: Part III, The (1990) |             0.2481543|                      0.2448787|                       1.0000000|
  
cor(r[, c(gdfth_nms[2], gdfth_nms[3], gdfth_nms[4])], 
    use="pairwise.complete") |> 
  knitr::kable()
# |                                | Godfather: Part II, The (1974)| Godfather: Part III, The (1990)| Tokyo Godfathers (2003)|
# |:-------------------------------|------------------------------:|-------------------------------:|-----------------------:|
# |Godfather: Part II, The (1974)  |                      1.0000000|                       0.2448787|               0.1274314|
# |Godfather: Part III, The (1990) |                      0.2448787|                       1.0000000|               0.2644005|
# |Tokyo Godfathers (2003)         |                      0.1274314|                       0.2644005|               1.0000000|

# Y(i,u) = μ + b(i) + b(u) + p(u)q(i) + ε(i,u)

# Y(i,u) = μ + b(i) + b(u) + p(u,1)q(1,i) + p(u,2)q(2,i) + ε(i,u)




# Y(i,u) = μ + b(i) + b(u) + f(d(u,i)) + p(u)q(i) + ε(i,u)

# Y(i,u) = μ + b(i) + b(u) + f(d(u,i)) + p(u,1)q(1,i) + p(u,2)q(2,i) + ε(i,u)





