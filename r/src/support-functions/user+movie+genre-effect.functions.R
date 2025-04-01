# User+Movie+Genre Effect Support Functions ------------------------------------
train_user_movie_genre_effect <- function(train_set, lambda = 0){
    train_set |>
      left_join(user_effects, by = "userId") |>
      left_join(best_lambda_user_movie_effect, by = "movieId") |>
      mutate(resid = rating - (mu + a + b)) |>
      filter(!is.na(resid)) |>
      group_by(genres) |>
      summarise(g = mean_reg(resid, lambda), n = n()) #|>
    #filter(n > min_nratings)
    
    # print(c(g_NAs = sum(is.na(genre_bias$g))))
    
    train_set |>
      left_join(genre_bias, by = "genres") |>
      left_join(user_movie_effect, by = "movieId") |>
      filter(!is.na(g)) |>
      group_by(movieId) |>
      summarise(g = mean(g, na.rm = TRUE))
}
train_user_movie_genre_effect.cv <- function(lambda = 0){
  if(is.na(lambda)) put_log("Function `train_user_movie_genre_effect.cv`:
Computing User+Movie+Genre Effect...")
  else put_log1("Function `train_user_movie_genre_effect.cv`:
Computing User+Movie+Genre Effect for lambda: %1...",
                lambda)
  
  put_log1("Function `train_user_movie_genre_effect.cv`:
Computing User+Movie+Genre Effects list for %1-Fold Cross Validation samples...", 
           CVFolds_N)
  
  start <- put_start_date()
  user_movie_genre_effects_ls <- lapply(kfold_index, function(fold_i){
    cv_fold_dat <- edx_CV[[fold_i]]
    
    put_log2("Processing User+Movie+Genre Effects for %1-Fold Cross Validation samples (Fold %2)...",
             CVFolds_N,
             fold_i)
    cv_fold_dat$train_gs_set |> train_user_movie_genre_effect(lambda)
    
    
    put_log2("User+Movie+Genre Effects have been computed for the Fold %1 
of the %2-Fold Cross Validation samples.",
             fold_i,
             CVFolds_N)
    # print(movie_genre_effects)
    movie_genre_effects
  })
  print(str(user_movie_genre_effects_ls))
  put_end_date(start)
  #> Time difference of 34.83447 secs
  put_log1("Function `train_user_movie_genre_effect.cv`:
User+Movie+Genre Effects list has been computed for %1-Fold Cross Validation samples.", 
           CVFolds_N)

  user_movie_genre_effects_united <- union_cv_results(user_movie_genre_effects_ls)
  print(str(user_movie_genre_effects_united))
  # sum(user_movie_genre_effects_united$g != 0)
  # sum(is.na(user_movie_genre_effects_united$b))
  # sum(is.na(user_movie_genre_effects_united$g))
  
  user_movie_genre_effect <- user_movie_genre_effects_united |>
    group_by(movieId) |>
    summarise(g = mean(g))
  
  if(is.na(lambda)) put_log("Function `train_user_movie_genre_effect.cv`:
Training completed: User+Movie+Genre Effects model.")
  else put_log1("Function `train_user_movie_genre_effect.cv`:
Training completed: User+Movie+Genre Effects model for lambda: %1...",
                lambda)
  
  # print(str(user_movie_genre_effect))
  user_movie_genre_effect
}
calc_user_movie_genre_effect_MSE <- function(train_set, umg_effect){
  train_set |>
    left_join(user_effects, by = "userId") |>
    left_join(best_lambda_user_movie_effect, by = "movieId") |>
    left_join(umg_effect, by = "movieId") |>
    mutate(resid = rating - clamp(mu + a + b + g)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> mse()
}
calc_user_movie_genre_effect_MSE <- function(train_set, umg_effect){
  umg_mse <- train_set |> calc_user_movie_genre_effect_MSE(umg_effect)
  sqrt(umg_mse)
}
calc_user_movie_genre_effect_MSE.cv <- function(umg_effect){
  put_log("Computing RMSEs.ResultTibble on Validation Sets...")
  start <- put_start_date()
  user_movie_genre_effects_MSEs <- sapply(edx_CV, function(cv_dat){
    cv_dat$validation_set |> calc_user_movie_genre_effect_MSE(umg_effect)
  })
  put_end_date(start)
  
  # plot(user_movie_genre_effects_MSEs)
  put_log1("MSE values have been plotted for the %1-Fold Cross Validation samples.", 
           CVFolds_N)
 
  mean(user_movie_genre_effects_MSEs)
}
calc_user_movie_genre_effect_RMSE.cv <- function(umg_effect){
  umg_effect_RMSE <- sqrt(calc_user_movie_genre_effect_MSE.cv(umg_effect))
  put_log2("%1-Fold Cross Validation ultimate RMSE: %2", 
           CVFolds_N, 
           umg_effect_RMSE)
  
  umg_effect_RMSE
}

## Regularization --------------------------------------------------------------
reg_tune_user_movie_genre_effect <- function(lambdas){
  sapply(lambdas, function(lambda){
    umg_reg_effect <- train_user_movie_genre_effect.cv(lambda)
    calc_user_movie_genre_effect_RMSE(umg_reg_effect)
  })
}
regularize.test_lambda.user_movie_genre_effect.cv <- function(lambda){
  umg_effect <- train_user_movie_genre_effect.cv(lambda)
  calc_user_movie_genre_effect_RMSE.cv(umg_effect)
}

