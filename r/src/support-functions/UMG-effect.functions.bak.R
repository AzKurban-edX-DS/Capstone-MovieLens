# User+Movie+Genre Effect Support Functions ------------------------------------
calc_genre_mean_ratings <- function(train_set) {
  train_set |> 
    mutate(genre_categories = as.factor(genres)) |>
    group_by(genre_categories) |>
    summarize(n = n(), rating_avg = mean(rating), se = sd(rating)/sqrt(n())) |>
    # filter(n > min_nratings) |>
    mutate(genres = reorder(genre_categories, rating_avg)) |>
    select(genres, rating_avg, se, n)
}
calc_genre_mean_ratings.cv <- function() {
  
  gnr_mean_rating_ls <- lapply(edx_CV, function(cv_item){
    cv_item$train_set |> 
      calc_genre_mean_ratings()
  })
  put_log1("Genre Mean Ratings list has been computed for %1-Fold Cross Validation samples.", 
           CVFolds_N)
  put(str(gnr_mean_rating_ls))

  put_log1("Computing Mean Ratings per Genre list for %1-Fold Cross Validation samples...", 
           CVFolds_N)
  gnr_mean_ratings_united <- union_cv_results(gnr_mean_rating_ls)
  str(gnr_mean_ratings_united)
  
  gnr_mean_ratings_united |>
    group_by(genres) |>
    summarise(ratings = mean(rating_avg),
              se = mean(se),
              n = mean(n)) |>
    mutate(genres = reorder(genres, ratings)) |>
    sort_by.data.frame(~ratings)
}
train_user_movie_genre_effect <- function(lambda = 0){
  if (is.na(lambda)) {
    stop("Function: train_user_movie_genre_effect
`lambda` is `NA`")
  }

  genre_bias <- edx |>
      left_join(edx.user_effect, by = "userId") |>
      left_join(rglr.UM_effect, by = "movieId") |>
      mutate(resid = rating - (mu + a + b)) |>
      # filter(!is.na(resid)) |>
      group_by(genres) |>
      summarise(g = mean_reg(resid, lambda), n = n()) #|>
    #filter(n > min_nratings)
    
    # print(c(g_NAs = sum(is.na(genre_bias$g))))
    
    edx |>
      left_join(genre_bias, by = "genres") |>
      left_join(rglr.UM_effect, by = "movieId") |>
      # filter(!is.na(g)) |>
      group_by(movieId) |>
      summarise(g = mean(g))
}
calc_user_movie_genre_effect_MSE <- function(test_set, umg_effect){
  test_set |>
    left_join(edx.user_effect, by = "userId") |>
    left_join(rglr.UM_effect, by = "movieId") |>
    left_join(umg_effect, by = "movieId") |>
    mutate(resid = rating - clamp(mu + a + b + g)) |> 
    # filter(!is.na(resid)) |>
    pull(resid) |> mse()
}
calc_user_movie_genre_effect_RMSE <- function(test_set, umg_effect){
  umg_mse <- test_set |> calc_user_movie_genre_effect_MSE(umg_effect)
  sqrt(umg_mse)
}
calc_user_movie_genre_effect_MSE.cv <- function(umg_effect){
  put_log("Computing RMSEs.ResultTibble on Validation Sets...")
  start <- put_start_date()
  user_movie_genre_effects_MSEs <- sapply(edx_CV, function(cv_dat){
    cv_dat$validation_set |> calc_user_movie_genre_effect_MSE(umg_effect)
  })
  put_end_date(start)
  
  # browser()
  plot(user_movie_genre_effects_MSEs)
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
regularize.test_lambda.UMG_effect.cv <- function(lambda){
  if (is.na(lambda)) {
    stop("Function: regularize.test_lambda.user_movie_genre_effect.cv
`lambda` is `NA`")
  }
  
  umg_effect <- train_user_movie_genre_effect(lambda)
  calc_user_movie_genre_effect_RMSE.cv(umg_effect)
}

