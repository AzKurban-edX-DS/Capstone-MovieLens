##### Support Functions --------------------------------------------------------
calc_UMGDG_Day_effect <- function(train_set, lambda = 0){
  if(lambda == 0) put_log("Function `calc_gday_effect`:
Computing Global Day Effect for given Train Set data...")
  else put_log1("Function `calc_gday_effect`:
Computing Global Day Effect for lambda: %1...",
                lambda)
  gday_effect <- train_set |> 
    left_join(user_effect, by = "userId") |>
    left_join(rg.UM_effect, by = "movieId") |>
    left_join(rg.UMG_effect, by = "movieId") |>
    left_join(date_days_map, by = "timestamp") |>
    left_join(rg.UMGY_effect, by='year') |>
    mutate(resid = rating - (mu + a + b + g + ye)) |>
    filter(!is.na(resid)) |>
    group_by(days) |>
    summarise(de = mean_reg(resid, lambda), 
              year = mean(year))
  
  if(lambda == 0) put_log("Function `calc_gday_effect`:
Global Day Effect has been computed.")
  else put_log1("Function `calc_gday_effect`:
Global Day Effect has been computed for lambda: %1...",
                lambda)
  gday_effect
}
calc_UMGDG_Day_effect.cv <- function(lambda = 0){
  if(lambda == 0) put_log("Function `calc_UMGDG_Day_effect.cv`:
Computing Global Day Effect...")
  else put_log1("Function `calc_UMGDG_Day_effect.cv`:
Computing Global Day Effect for lambda: %1...",
                lambda)
  
  put_log1("Function `calc_UMGDG_Day_effect.cv`:
Computing Global Day Effect list for %1-Fold Cross Validation samples...", 
           CVFolds_N)
  start <- put_start_date()
  gday_effect_ls <- lapply(edx_CV,  function(cv_fold_dat){
    # start <- put_start_date()
    cv_fold_dat$train_set |> calc_UMGDG_Day_effect(lambda)
  })
  str(gday_effect_ls)
  put_end_date(start)
  put_log1("Function `calc_UMGDG_Day_effect.cv`:
Global Day Effect list has been computed for %1-Fold Cross Validation samples.", 
           CVFolds_N)
  
  gday_effect_united <- union_cv_results(gday_effect_ls)
  str(gday_effect_united)
  
  gday_effect <- gday_effect_united |>
    #filter(!is.na(de)) |>
    group_by(days) |>
    summarise(de = mean(de, na.rm = TRUE), year = mean(year, na.rm = TRUE))
  
  if(lambda == 0) put_log("Function `calc_UMGDG_Day_effect.cv`:
Training completed: Global Day Effects model.")
  else put_log1("Function `calc_UMGDG_Day_effect.cv`:
Training completed: Global Day Effects model for lambda: %1...",
                lambda)
  
  gday_effect
}
loess_de <- function(day_bias_dat, degree = NA, span = NA){
  if(is.na(degree)) degree = 2
  if(is.na(span)) span = 0.75
  loess(de ~ days, span = span, degree = degree, data = day_bias_dat)
}
train_UMGY.SmoothedDay_effect.cv <- function(degree = NA, span = NA){
  fit <- cv.UMGDG_Day_effect |> loess_de(degree, span)
  cv.UMGDG_Day_effect |> mutate(de_smoothed = fit$fitted)
}
calc_UMGY.SmoothedDay_effect.MSE <- function(test_set, day_smoothed_effect) {
  test_set |>
    left_join(user_effect, by = "userId") |>
    left_join(rg.UM_effect, by = "movieId") |>
    left_join(rg.UMG_effect, by = "movieId") |>
    left_join(date_days_map, by = "timestamp") |>
    left_join(rg.UMGY_effect, by='year') |>
    left_join(day_smoothed_effect, by='days') |>
    mutate(resid = rating - clamp(mu + a + b + g + ye + de_smoothed)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> mse()
}
calc_UMGY.SmoothedDay_effect.MSE.cv <- function(day_smoothed_effect){
  put_log1("Function `calc_UMGY.SmoothedDay_effect.MSE.cv`:
Computing MSE values for the %1-Fold Cross Validation samples...", 
           CVFolds_N)
  
  start <- put_start_date()
  MSEs <- sapply(edx_CV, function(cv_fold_dat){
    cv_fold_dat$validation_set |> 
      calc_UMGY.SmoothedDay_effect.MSE(day_smoothed_effect)
  })
  put_end_date(start)
  put_log1("Function `calc_UMGY.SmoothedDay_effect.MSE.cv`:
MSE value have been computed for the %1-Fold Cross Validation samples.", 
           CVFolds_N)
  mean(MSEs)
}

day_smoothed_effect_RMSE <- function(day_smoothed_effect, degree = NA, span = NA){
  #day_smoothed_effect <- train_UMGY.SmoothedDay_effect.cv(degree, span) 

  sqrt(calc_UMGY.SmoothedDay_effect.MSE.cv(day_smoothed_effect))
}

tune_de_model_RMSEs <- function(degree, spans){
  model_diu_rmses <- sapply(spans, function(span){
    put_log2("Computing RMSE using `loess` function with the following parameters: 
degree = %1, span = %2...", 
             degree,
             span)
    
    rmse <- day_smoothed_effect_RMSE(degree, span)
    put_log2("RMSE has been computed for the `loess` function parameters: 
degree = %1, span = %2.", 
             degree,
             span)
    rmse
  })
}
date_smoothed_tuned_RMSEs <- function(degree, spans) {
  n_spans <- length(spans)
  
  start <- put_start_date()
  put_log2("Tuning the Smothed Date Effect Model for Degree = %1 (%2 spans)", 
           degree, n_spans)
  model_diu_rmses <- tune_de_model_RMSEs(degree, spans)
  put_end_date(start)
  put_log2("RMSEs.ResultTibble computed for the Smothed Date Effect Model (Degree = %1, %2 spans)", 
           degree, n_spans)
  data.frame(span = spans, rmse = model_diu_rmses)
}
get_best_RMSE <- function(span_rmses){
  idx <- which.min(span_rmses$rmse)
  rmse <- c(span_rmses$span[idx], min(span_rmses$rmse))
  names(rmse) <- c("Span", "RMSE")
  rmse
}

