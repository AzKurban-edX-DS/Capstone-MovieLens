# User+Movie Effect Model Functions --------------------------------------------
train_user_movie_effect <- function(train_set, lambda = 0){
  if (is.na(lambda)) {
    stop("Function: train_user_movie_effect
`lambda` is `NA`")
  }

  train_set |>
    left_join(user_effect, by = "userId") |>
    mutate(resid = rating - (mu + a)) |> 
    filter(!is.na(resid)) |>
    group_by(movieId) |>
    summarise(b = mean_reg(resid, lambda), n = n())
}
train_user_movie_effect.cv <- function(lambda = 0){
  if (is.na(lambda)) {
    stop("Function: train_user_movie_effect.cv
`lambda` is `NA`")
  }
  
  if(lambda == 0) put_log("Function: train_user_movie_effect.cv:
Computing User+Movie Effect...")
  else put_log1("Function: train_user_movie_effect.cv:
Computing User+Movie Effect for lambda: %1...",
                lambda)
  
  start <- put_start_date()
  user_movie_effects_ls <- lapply(edx_CV, function(cv_fold_dat){
    cv_fold_dat$train_set |> train_user_movie_effect(lambda)
  })
  put_end_date(start)
  str(user_movie_effects_ls)
  put_log("Function: train_user_movie_effect.cv:
User+Movie Effect list have been computed")
  
  user_movie_effects_united <- union_cv_results(user_movie_effects_ls)
  str(user_movie_effects_united)
  # sum(is.na(user_movie_effects_united$cv_dat)) # 0 (there are no NAs in there)
  
  user_movie_effect <- user_movie_effects_united |>
    group_by(movieId) |>
    summarise(b = mean(b), n = mean(n))
  
  # sum(is.na(user_movie_effect$b)) # 0 (there are no NAs in there)
  
  #user_movie_effect <- data.frame(movieId = as.integer(names(b)), b = b)
  if(lambda == 0) put_log("Function: train_user_movie_effect.cv:
Training completed: User+Movie Effects model.")
  else put_log1("Function: train_user_movie_effect.cv:
Training completed: User+Movie Effects model for lambda: %1...",
                lambda)
  
  user_movie_effect
}
calc_user_movie_effect_RMSE <- function(test_set, um_effect){
  mse <- test_set |> calc_user_movie_effect_MSE(um_effect)
  sqrt(mse)
}
calc_user_movie_effect_MSE <- function(test_set, um_effect){
  test_set |>
    left_join(user_effect, by = "userId") |>
    left_join(um_effect, by = "movieId") |>
    mutate(resid = rating - clamp(mu + a + b)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> mse()
}
calc_user_movie_effect_RMSE.cv <- function(um_effect){
  user_movie_effects_MSE <- calc_user_movie_effect_MSE.cv(um_effect)
  um_effect_RMSE <- sqrt(user_movie_effects_MSE)
  put_log2("Function: user_movie_effects_RMSE.cv:
%1-Fold Cross Validation ultimate RMSE: %2", CVFolds_N, um_effect_RMSE)
  um_effect_RMSE
}
calc_user_movie_effect_MSE.cv <- function(um_effect){
  put_log("Function: user_movie_effects_MSE.cv:
Computing the RMSE taking into account User+Movie Effects...")
  start <- put_start_date()
  user_movie_effects_MSEs <- sapply(edx_CV, function(cv_fold_dat){
    cv_fold_dat$validation_set |> calc_user_movie_effect_MSE(um_effect)
  })
  put_end_date(start)
  
  put_log1("Function: user_movie_effects_MSE.cv:
MSE values have been plotted for the %1-Fold Cross Validation samples.", 
           CVFolds_N)
  
  mean(user_movie_effects_MSEs)
}

## Regularization --------------------------------------------------------------
regularize.test_lambda.UM_effect.cv <- function(lambda){
  if (is.na(lambda)) {
    stop("Function: regularize.test_lambda.UM_effect.cv
`lambda` is `NA`")
  }
  um_effect <- train_user_movie_effect.cv(lambda)
  calc_user_movie_effect_RMSE.cv(um_effect)
}




