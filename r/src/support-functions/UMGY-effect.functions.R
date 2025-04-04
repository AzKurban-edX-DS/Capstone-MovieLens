# User+Movie+Genre+Year Effect functions ---------------------------------------

calc_date_global_effect <- function(train_set, lambda = 0){
  if(lambda == 0) put_log("Function `calc_date_global_effect`:
Computing Date Global Effect for given Train Set data...")
  else put_log1("Function `calc_date_global_effect`:
Computing Date Global Effect for lambda: %1...",
                lambda)
  dg_effect <- train_set |> 
    left_join(user_effect, by = "userId") |>
    left_join(rg.UM_effect, by = "movieId") |>
    left_join(rg.UMG_effect, by = "movieId") |>
    left_join(date_days_map, by = "timestamp") |>
    mutate(resid = rating - (mu + a + b + g)) |>
    filter(!is.na(resid)) |>
  group_by(days) |>
    summarise(de = mean_reg(resid, lambda), 
              year = mean(year))
  
  if(lambda == 0) put_log("Function `calc_date_global_effect`:
Date Global Effect has been computed.")
  else put_log1("Function `calc_date_global_effect`:
Date Global Effect has been computed for lambda: %1...",
                lambda)
  dg_effect
}
calc_date_global_effect.cv <- function(lambda = 0){
  if(lambda == 0) put_log("Function `calc_date_global_effect.cv`:
Computing Date Global Effect...")
  else put_log1("Function `calc_date_global_effect.cv`:
Computing Date Global Effect for lambda: %1...",
                lambda)
  
  put_log1("Function `calc_date_global_effect.cv`:
Computing Date Global Effect list for %1-Fold Cross Validation samples...", 
           CVFolds_N)
  start <- put_start_date()
  date_global_effect_ls <- lapply(edx_CV,  function(cv_fold_dat){
    # start <- put_start_date()
    cv_fold_dat$train_set |> calc_date_global_effect(lambda)
  })
  str(date_global_effect_ls)
  put_end_date(start)
  put_log1("Function `calc_date_global_effect.cv`:
Date Global Effect list has been computed for %1-Fold Cross Validation samples.", 
           CVFolds_N)
  
  date_global_effect_united <- union_cv_results(date_global_effect_ls)
  str(date_global_effect_united)
  
  date_global_effect <- date_global_effect_united |>
    #filter(!is.na(de)) |>
    group_by(days) |>
    summarise(de = mean(de, na.rm = TRUE), year = mean(year, na.rm = TRUE))
  
  if(lambda == 0) put_log("Function `calc_date_global_effect.cv`:
Training completed: Date Global Effects model.")
  else put_log1("Function `calc_date_global_effect.cv`:
Training completed: Date Global Effects model for lambda: %1...",
                lambda)
  
  date_global_effect
}
calc_UMGY_effect <- function(train_set, date_global_effect){
  date_global_effect |>
    group_by(year) |>
    summarise(ye = mean(de, na.rm = TRUE))
}
calc_UMGY_effect.cv <- function(cv.date_global_effect){
  cv.date_global_effect |>
    group_by(year) |>
    summarise(ye = mean(de, na.rm = TRUE))
}
train_UMGY_effect <- function(train_set, lambda = 0){
  DG_effect <- train_set |>
    calc_date_global_effect(lambda)
  
  train_set |>
    calc_UMGY_effect(DG_effect)
}
train_UMGY_effect.cv <- function(lambda = 0){
  calc_date_global_effect.cv(lambda) |> calc_UMGY_effect.cv()  
}
calc_UMGY_effect_MSE <- function(test_set, UMGY_effect){
  test_set |>
    left_join(user_effect, by = "userId") |>
    left_join(rg.UM_effect, by = "movieId") |>
    left_join(rg.UMG_effect, by = "movieId") |>
    left_join(date_days_map, by = "timestamp") |>
    left_join(UMGY_effect, by='year') |>
    mutate(resid = rating - clamp(mu + a + b + g + ye)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> mse()
}
calc_UMGY_effect_MSE.cv <- function(UMGY_effect){
  start <- put_start_date()
  UMGY_effect_MSEs <- sapply(edx_CV, function(cv_fold_dat){
    cv_fold_dat$validation_set |> calc_UMGY_effect_MSE(UMGY_effect)
  })
  put_end_date(start)
  put_log1("Function: calc_UMGY_effect_MSE.cv
Date (Year) Effect MSE values have been computed for the %1-Fold Cross Validation samples.", 
           CVFolds_N)
  
  mean(UMGY_effect_MSEs)
}
calc_UMGY_effect_RMSE <- function(test_set, UMGY_effect){
  mse <- test_set |> calc_UMGY_effect_MSE(UMGY_effect)
  sqrt(mse)
}
calc_UMGY_effect_RMSE.cv <- function(UMGY_effect){
  UMGY_effect_RMSE <- sqrt(calc_UMGY_effect_MSE.cv(UMGY_effect))
  put_log2("%1-Fold Cross Validation ultimate RMSE: %2", 
           CVFolds_N, 
           UMGY_effect_RMSE)
  
  UMGY_effect_RMSE
}

## Regularization --------------------------------------------------------------
regularize.test_lambda.UMGY_effect.cv <- function(lambda){
  UMGY_effect <- train_UMGY_effect.cv(lambda)
  calc_UMGY_effect_RMSE.cv(UMGY_effect)
}




