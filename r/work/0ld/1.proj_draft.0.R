
library(caret)
library(lubridate)

## The Netflix Prize Dataset
# https://www.asc.ohio-state.edu/statistics/statgen/joul_aut2009/BigChaos.pdf

np_training_set_cnt <- 100480507
np_probe_set_cnt <- 1408395  # subset of `training_set`

probe_set_ratio <- np_probe_set_cnt/np_training_set_cnt
#> [1] 0.0140166

np_qualifying_set_cnt <- 2817131

quiz_set_ratio <- 0.5
test_set_ratio <- 1 - quiz_set_ratio

np_rmse_accepted_min <- 0.8563
#####################################################
# Inspired by:
# HarvardX: PH125.8x
# Data Science: Machine Learning
## Section 6.2: Recommendation Systems

# Textbook section:
### 33.7 Recommendation Systems
# https://rafalab.github.io/dsbook/large-datasets.html#recommendation-systems

str(edx)
# 'data.frame':	9000055 obs. of  6 variables:
# $ userId   : int  1 1 1 1 1 1 1 1 1 1 ...
# $ movieId  : int  122 185 292 316 329 355 356 362 364 370 ...
# $ rating   : num  5 5 5 5 5 5 5 5 5 5 ...
# $ timestamp: int  838985046 838983525 838983421 838983392 838983392 838984474 838983653 838984885 838983707 838984596 ...
# $ title    : chr  "Boomerang (1992)" "Net, The (1995)" "Outbreak (1995)" "Stargate (1994)" ...
# $ genres   : chr  "Comedy|Romance" "Action|Crime|Thriller" "Action|Drama|Sci-Fi|Thriller" "Action|Adventure|Sci-Fi" ...

str(final_holdout_test)
# 'data.frame':	999999 obs. of  6 variables:
# $ userId   : int  1 1 1 2 2 2 3 3 4 4 ...
# $ movieId  : int  231 480 586 151 858 1544 590 4995 34 432 ...
# $ rating   : num  5 5 5 3 2 3 3.5 4.5 5 3 ...
# $ timestamp: int  838983392 838983653 838984068 868246450 868245645 868245920 1136075494 1133571200 844416936 844417070 ...
# $ title    : chr  "Dumb & Dumber (1994)" "Jurassic Park (1993)" "Home Alone (1990)" "Rob Roy (1995)" ...
# $ genres   : chr  "Comedy" "Action|Adventure|Sci-Fi|Thriller" "Children|Comedy" "Action|Drama|Romance|War" ...

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

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu <- mean(train_set$rating)
mu
#> [1] 3.512465

naive_rmse <- RMSE(probe_set$rating, mu)
naive_rmse
#> [1] 1.061722

## Modeling movie effects

# Y(i,u) = μ + b(i) + ε(i,u)

# movie_mean <- function(mu){
#   train_set |> 
#     group_by(movieId) |> 
#     summarize(b_i = mean(rating - mu))
# }

#movie_avgs <- movie_mean(mu)

movie_avgs <- train_set |> 
  group_by(movieId) |> 
  summarize(b_i = mean(rating - mu))

str(movie_avgs)
head(movie_avgs)

preds <- mu + probe_set |> 
  left_join(movie_avgs, by='movieId') |>
  pull(b_i)

head(preds)
head(probe_set$rating)

mean(preds)
#> [1] 3.512969

str(probe_set)
str(preds)

# sum(is.na(preds))
#> [1] 0

# sum(is.null(preds))
# #> [1] 0
# 
# sum(is.na(probe_set$rating))
# #> [1] 0

# sum(is.na(train_set$rating))
# #> [1] 0

movie_model_rmse <- RMSE(probe_set$rating, preds)
movie_model_rmse
#> [1] 0.9442118


## Modeling user effects

# Y(i,u) = μ + b_i(i) + b_u(u) + ε(i,u)

#> We use the following code to compute the average rating for user  for those 
#> that have rated 100 or more movies and to plot the same.

min_user_rates <- 100
n_bins <- 30

train_set |> 
  group_by(userId) |> 
  summarize(b_u = mean(rating)) |> 
  filter(n()>=min_user_rates) |>
  ggplot(aes(b_u)) + 
  geom_histogram(bins = n_bins, color = "black")

# b(u) = mean(y(i, u) - μ - b_i(i)) 

# user_mean <- function(mu){
#   train_set |> 
#     left_join(movie_avgs, by='movieId') |>
#     group_by(userId) |>
#     summarize(b_u = mean(rating - mu - b_i))  
# }
# user_avgs <- user_mean(mu)

user_avgs <- train_set |>
  left_join(movie_avgs, by='movieId') |>
  group_by(userId) |>
  summarize(b_u = mean(rating - mu - b_i))

head(user_avgs)

# predict_ratings <- function(tst_set) {
#   tst_set |> 
#     left_join(movie_avgs, by='movieId') |>
#     left_join(user_avgs, by='userId') |>
#     mutate(pred = mu + b_i + b_u) |>
#     pull(pred)
# }
# predicted_ratings <- predict_ratings(probe_set)

predicted_ratings <- probe_set |>
  left_join(movie_avgs, by='movieId') |>
  left_join(user_avgs, by='userId') |>
  mutate(pred = mu + b_i + b_u) |>
  pull(pred)

head(predicted_ratings)

model_2_rmse <- RMSE(probe_set$rating, predicted_ratings)
model_2_rmse
#> [1] 0.8673005

#na_substitute <- seq(0.5, mu, 0.5)
#na_substitute

# mean(is.na(train_set$rating))
# mean(is.null(train_set$rating))
# 
# 
# na_filled_test_rmse <- sapply(na_substitute, function(s){
#   
#   trn_set <- train_set |> 
#     mutate(rating = ifelse(is.na(rating), s, rating))
#   
#   mu <- mean(trn_set$rating)
#   movie_avg <- movie_mean(mu)
#   user_avg <- user_mean(mu, movie_avg)
#   preds <- predict_ratings(probe_set, movie_avg, user_avg)
#   
#   RMSE(preds, probe_set$rating)
# })
# 
# na_filled_test_rmse

#test_RMSEs <- na_filled_test_rmse

#train_set_na_filled

## Modeling date effects

# Y(i,u) = μ + b_i(i) + b_u(u) + f(d(i,u)) + ε(i,u)

# f(d(i,u)) = y(i,u) - μ - b_i(i) - b_u(u)

library(lubridate)

# movielens |> 
#   mutate(date = floor_date(date, 
#                             unit = "day", 
#                             week_start = getOption("lubridate.week.start", 7))) |>
#   group_by(date) |>
#   summarize(rating = mean(rating)) |>
#   ggplot(aes(date, rating)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE)

# f(d(i,u)) = y(i,u) - μ - b_i(i) - b_u(u)


date()
start <- Sys.time()
train_set_dm <- train_set |> 
  left_join(movie_avgs, by='movieId') |>
  left_join(user_avgs, by='userId') |>
  mutate(rating_residue = rating - mu - b_i - b_u) |>
  mutate(date_time = as_datetime(timestamp)) |>
  mutate(date = as_date(date_time)) |>
  mutate(days = as.integer(date - min_date)) #|>
  #arrange(date)

head(train_set_dm)
Sys.time() - start
date()

min_date <- min(train_set_dm$date)

#-------------------------------------
# train_set_dm_arranged <- train_set_dm |>
#   mutate(days = as.integer(date - min_date)) |>
#   arrange(date)
# 
# head(train_set_dm_arranged)

# range(train_set$date)
# total_days <- diff(range(train_set$date))
# 
# ndays <- as.integer(total_days)
# ndays
# #> [1] 5110
# 
# min(train_set$date)
# max(train_set$date)
#-------------------------

date()
start <- Sys.time()
date_global_effect <- train_set_dm |>
  group_by(days, date) |>
  summarise(de = mean(rating_residue))
Sys.time() - start
date()
head(date_global_effect)
sum(is.na(date_global_effect$de))


#----------------------
#str(date_iu_effects)
# gropd_df [8,873,904 × 4] (S3: grouped_df/tbl_df/tbl/data.frame)

#sum(is.na(date_iu_effects))

# date_iu_effects |>
#   ggplot(aes(days, du_rr)) +
#   geom_point(size = 3, alpha = .5, color = "black")

#start <- Sys.time()
#> [1] "Tue Dec 24 02:54:55 2024"
#span <- 14 
# fit <- with(date_iu_effects, 
#             ksmooth(days, du_rr, kernel = "box", bandwidth = span))
#Sys.time() - start
# date_global_effect <- date_iu_effects |>
#   group_by(days) |>
#   summarise(de = mean(du_rr))

# date_global_effect <- days_effects |>
#   left_join(train_set_dm, by = 'days') |>
#   select(days, date, de)
# 
# head(date_global_effect)
# sum(is.na(date_global_effect$de))
#--------------------

date()
start <- Sys.time()
fit <- loess(de ~ days, data = date_global_effect)
Sys.time() - start
date()
sum(is.na(fit$fitted))
str(fit$pars)
str(fit$fitted)
#fit$pars
# fit$fitted

date_smoothed_effect <- as.data.frame(date_global_effect) |>
  mutate(de_smoothed = fit$fitted)
head(date_smoothed_effect)

date()
start <- Sys.time()
date_smoothed_effect |>
  ggplot(aes(x = days)) +
  geom_point(aes(y = de), size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(y = de_smoothed), color = "red")
Sys.time() - start
date()

#--------------------------------------------------------------
# str(as.data.frame(date_iu_effects))

# smoothed_date_effects <- as.data.frame(date_iu_effects) |>
#   mutate(diu_smth = fit$y) |>
#   filter(!is.na(diu_smth)) |>
#   group_by(days) |>
#   summarise(d_smth = mean(diu_smth))
# 
# 
# str(smoothed_date_effects)
# head(smoothed_date_effects)
# sum(is.na(smoothed_date_effects$d_smth))
# #> [1] 0

# date()
# smoothed_date_effects |>
#   ggplot(aes(x = days)) +
#   #geom_point(aes(y = du_rr), size = 3, alpha = .5, color = "grey") + 
#   geom_line(aes(y = d_smth), color = "red")
# date()
# 
# str(smth_plot)
# smth_plot$data$days
  
sum(is.na(movie_avgs$b_i))
sum(is.na(user_avgs$b_u))
  
# probe_dat <- probe_set |> 
#   mutate(date = as_date(as_datetime(timestamp))) |>
#   mutate(days = as.integer(date - min_date))
# 
# str(probe_dat)
# head(probe_dat)
# sum(is.na(probe_dat$days))
# 
# sum(is.na(smoothed_date_effects$days))
# sum(is.na(smoothed_date_effects$d_smth))
# 
# d_smth_mean <- mean(smoothed_date_effects$d_smth)
# d_smth_mean
# 
# date() 
# preds_dat <- probe_dat |> 
#   #mutate(date = as_date(as_datetime(timestamp))) |>
#   #mutate(days = as.integer(date - min_date)) |>
#   left_join(movie_avgs, by='movieId') |>
#   left_join(user_avgs, by='userId') |>
#   left_join(smoothed_date_effects, by='days') |>
#   mutate(d_smth = ifelse(is.na(d_smth), 0, d_smth))
#   #filter(!is.na(d_smth))
# 
# # str(preds_dat)
# # head(preds_dat)
# sum(is.na(preds_dat$b_i))
# sum(is.na(preds_dat$b_u))
# sum(is.na(preds_dat$d_smth))
# 
# preds <- preds_dat |>  
#   mutate(pred = mu + b_i + b_u + d_smth) |>
#   pull(pred)
# 
# sum(is.na(preds))
# 
# RMSE(probe_set$rating, preds)
# #> [1] 0.867553
# 
# spans <- seq(0.5, 1, 0.01)
#--------------------------------------------------------------

start_date <- function(){
  print(date())
  Sys.time()
}

end_date <- function(start){
  print(date())
  Sys.time() - start
}

fit_loess <- function(spans, dgr){
  fits <- sapply(spans, function(span){
    fit <- loess(de ~ days, span = span, degree = dgr, data = date_global_effect)
    fit$fitted
  })
}

predict_de_probe <- function(fits){
  model_diu_rmses <- sapply(fits, function(smth){
    date_smoothed_effect <- as.data.frame(date_global_effect) |>
      mutate(de_smoothed = smth)
    #head(date_smoothed_effect)
    
    preds <- probe_set |> 
      mutate(date = as_date(as_datetime(timestamp))) |>
      left_join(movie_avgs, by='movieId') |>
      left_join(user_avgs, by='userId') |>
      left_join(date_smoothed_effect, by='date') |>
      #mutate(diu_smth = d_iu_smth) |>
      mutate(pred = mu + b_i + b_u + de_smoothed) |>
      pull(pred)
    
    RMSE(preds, probe_set$rating)
  })
}


# spans <- seq(0.0003, 0.002, 0.00001)
spans <- seq(0.0005, 0.0015, 0.00001)

start <- start_date()
fits <- fit_loess(spans,0)
end_date(start)

dim(fits)
df_fits <- as.data.frame(fits)
#str(df_fits)

start <- start_date()
model_diu_rmses <- predict_de_probe(df_fits)
end_date(start)

model_diu_rmses
plot(model_diu_rmses)
min(model_diu_rmses)
#> [1] 0.8670417

idx <- which.min(model_diu_rmses)
idx
spans[idx]
#------------------------------
#spans <- seq(0.0003, 0.002, 0.00001)
spans <- seq(0.001, 0.0014, 0.00001)

start <- start_date()
fits <- fit_loess(spans,1)
end_date(start)

dim(fits)
df_fits <- as.data.frame(fits)
#str(df_fits)

start <- start_date()
model_diu_rmses <- predict_de_probe(df_fits)
end_date(start)

model_diu_rmses
plot(model_diu_rmses)
min(model_diu_rmses)
#> [1] 0.8669269

idx <- which.min(model_diu_rmses)
idx # 9
spans[idx]
#> [1] 0.00108
#------------------------------
# spans <- seq(0.0003, 0.01, 0.00001)
spans <- seq(0.0007, 0.002, 0.00001)

start <- start_date()
fits <- fit_loess(spans,2)
end_date(start)

dim(fits)
df_fits <- as.data.frame(fits)
#str(df_fits)

start <- start_date()
model_diu_rmses <- predict_de_probe(df_fits)
end_date(start)

model_diu_rmses
plot(model_diu_rmses)
min(model_diu_rmses)
#> [1] 0.866997

idx <- which.min(model_diu_rmses)
idx # 82
spans[idx]
#> [1] 0.00151
#------------------------------
best_degree <- 1
best_span <- 0.00108

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

preds <- probe_set |> 
  mutate(date = as_date(as_datetime(timestamp))) |>
  left_join(movie_avgs, by='movieId') |>
  left_join(user_avgs, by='userId') |>
  left_join(date_smoothed_effect, by='date') |>
  #mutate(diu_smth = d_iu_smth) |>
  mutate(pred = mu + b_i + b_u + de_smoothed) |>
  pull(pred)

RMSE(preds, probe_set$rating)
#> [1] 0.8669269



#------------------------------------------
# fit <- loess(d_iu ~ days, span = best_span, degree = 0, data = date_effects)
# #fit$fitted
# 
# smoothed_date_effects <- date_effects |> mutate(d_iu_smth = fit$fitted)
# #head(smoothed_date_effects)
# 
# preds <- probe_set |>
#   mutate(date = as_date(as_datetime(timestamp))) |>
#   left_join(movie_avgs, by='movieId') |>
#   left_join(user_avgs, by='userId') |>
#   left_join(smoothed_date_effects, by='date') |>
#   mutate(pred = mu + b_i + b_u + d_iu_smth) |>
#   pull(pred)
# 
# RMSE(preds, probe_set$rating)
# #> [1] 0.8653018


#--------------------------------------------------------------
# fit <- loess(rating ~ date_time + userId + movieId, data = train_set)
# fit


# calc_date_effects <- function(mu){
#   train_set |> 
#     left_join(movie_avgs, by='movieId') |>
#     left_join(user_avgs, by='userId') |>
#     group_by(date) |>
#     summarize(d_iu = mean(rating - mu - b_i - b_u))  
# }

# f(d(i,u)) = y(i,u) - μ - b_i(i) - b_u(u)

# train_set |>
#   ggplot(aes(date_time, rating)) +
#   geom_point(size = 3, alpha = .5, color = "black")
#----------------------------------------------------------------------
  

date_effects <- train_set |>
      left_join(movie_avgs, by='movieId') |>
      left_join(user_avgs, by='userId') |>
      group_by(date) |>
      summarize(fsmth_d_iu = mean(rating - mu - b_i - b_u))

head(date_effects)
plot()
# 
min_date <- min(date_effects$date)
# 
# date_effects <- date_effects |>
#   mutate(days = as.integer(date - min_date)) |>
#   arrange(date)
#   
# head(date_effects)
# #**********************************************************************
# fit <- loess(d_iu ~ days, degree = 1, data = date_effects)
# fit
# 
# smoothed_date_effects <- date_effects |> mutate(d_iu_smth = fit$fitted)
# head(smoothed_date_effects)
# 
# smoothed_date_effects |> 
#   ggplot(aes(date, d_iu)) +
#   geom_point(size = 3, alpha = .5, color = "grey") +
#   geom_line(aes(date, d_iu_smth), color = "red")
# 
# predict_de_ratings <- function(tst_set) {
#   tst_set |> 
#     left_join(movie_avgs, by='movieId') |>
#     left_join(user_avgs, by='userId') |>
#     left_join(date_effects, by='date') |>
#     mutate(pred = mu + b_i + b_u + d_iu_smth) |>
#     pull(pred)
# }
# 
# predicted_ratings <- predict_ratings(probe_set)
# 
# model_3_rmse <- RMSE(predicted_ratings, probe_set$rating)
# model_3_rmse
# #> [1] 0.8653039
# 
# default_span <- fit$pars$span
# #> [1] 0.75
# #************************************************************************
# 
# spans <- seq(0.5, 1, 0.01)
# spans
# 
# fits <- sapply(spans, function(span){
#   fit <- loess(d_iu ~ days, span = span, degree = 0, data = date_effects)
#   fit$fitted
# })
# 
# dim(fits)
# df_fits <- as.data.frame(fits)
# 
# model_diu_rmses <- sapply(df_fits, function(x){
#   smoothed_date_effects <- date_effects |> mutate(d_iu_smth = x)
#   head(smoothed_date_effects)
#   
#   preds <- probe_set |> 
#     mutate(date = as_date(as_datetime(timestamp))) |>
#     left_join(movie_avgs, by='movieId') |>
#     left_join(user_avgs, by='userId') |>
#     left_join(smoothed_date_effects, by='date') |>
#     #mutate(diu_smth = d_iu_smth) |>
#     mutate(pred = mu + b_i + b_u + d_iu_smth) |>
#     pull(pred)
#   
#   RMSE(preds, probe_set$rating)
# })
# 
# model_diu_rmses
# 
# plot(model_diu_rmses)
# 
# min(model_diu_rmses)
# #> [1] 0.8653018
# 
# #model_3_rmse
# #> [1] 0.8653039
#----------------------------------------------------
# 
# best_span_idx <- which.min(model_diu_rmses)
# best_span <- spans[best_span_idx]
# best_span
# 
# fit <- loess(d_iu ~ days, span = best_span, degree = 0, data = date_effects)
# #fit$fitted
# 
# smoothed_date_effects <- date_effects |> mutate(d_iu_smth = fit$fitted)
# #head(smoothed_date_effects)
# 
# preds <- probe_set |> 
#   mutate(date = as_date(as_datetime(timestamp))) |>
#   left_join(movie_avgs, by='movieId') |>
#   left_join(user_avgs, by='userId') |>
#   left_join(smoothed_date_effects, by='date') |>
#   mutate(pred = mu + b_i + b_u + d_iu_smth) |>
#   pull(pred)
# 
# RMSE(preds, probe_set$rating)
# #> [1] 0.8653018
#----------------------------------------------------

## Inspired by Textbook:
### Inspired by Section of the textbook: 33.9 Regularization
# https://rafalab.dfci.harvard.edu/dsbook/large-datasets.html#regularization
















