# User+Movie Effect Regularization
## Open log -------------------------------------------------------------------
open_logfile(".reg-um-effect.loop_0_10_d10")

## Process lamdas in loop starting from `lambdas = seq(0, 10, 10/10` -------
loop_starter <- c(0, 4, 2)
seq_start <- loop_starter[1]
seq_end <- loop_starter[2]
range_divider <- loop_starter[3] 

best_RMSE <- Inf

um_reg_lambdas_best_results <- c(best_lambda = 0, 
                                 best_RMSE = best_RMSE)
repeat{ 
  seq_increment <- (seq_end - seq_start)/range_divider 
  
  if (seq_increment < 0.0000000000001) {
    warning("Main loop:
lambda increment is too small.")
    
    put_log2("Main loop:
Final best RMSE for `lambda = %1`: %2",
             um_reg_lambdas_best_results["best_lambda"],
             um_reg_lambdas_best_results["best_RMSE"])
    
    put(um_reg_lambdas_best_results)
    # best_lambda   best_RMSE 
    # -75.0000000   0.8578522    
    break
  }
  
  file_name_tmp <- "umgy_reg-loop_" |>
    str_c(as.character(loop_starter[1])) |>
    str_c("_") |>
    str_c(as.character(loop_starter[3])) |>
    str_c("_") |>
    str_c(as.character(range_divider)) |>
    str_c(".") |>
    str_c(as.character(seq_start)) |>
    str_c("-") |>
    str_c(as.character(seq_end)) |>
    str_c(".RData")
  
  file_path_tmp <- file.path(regularization_data_path, file_name_tmp)
  
  put_log1("Main loop:
File path generated: %1", file_path_tmp)
  
  if (file.exists(file_path_tmp)) {
    put_log1("Main loop:
Loading tuning data from file: %1...", file_path_tmp)
    
    start <- put_start_date()
    load(file_path_tmp)
    put_end_date(start)
    put_log1("Main loop:
Tuning data has been loaded from file: %1", file_path_tmp)
    
    lambdas <- um_reg_lambdas
    lambda_RMSEs <- um_reg_RMSEs
    browser()
  } else {
    lambdas <- seq(seq_start, seq_end, seq_increment)
    reg_result <- regularize.user_movie_effect(lambdas, is.cv = TRUE)
    
    um_reg_RMSEs <- reg_result$RMSEs
    um_reg_lambdas <- reg_result$lambdas
    
    put_log1("Main loop:
File NOT saved (disabled for debug purposes): %1", file_path_tmp)
    #     save(um_reg_lambdas,
    #          um_reg_RMSEs,
    #          file = file_path_tmp)
    # 
    #     put_log1("Main loop:
    # File saved: %1", file_path_tmp)
  }
  
  plot(um_reg_lambdas, um_reg_RMSEs)
  # browser()
  
  lambda_RMSEs <- um_reg_RMSEs
  lambdas <- um_reg_lambdas
  min_RMSE <- min(lambda_RMSEs)
  
  if (best_RMSE <= min_RMSE) {
    warning("Currently computed minimal RMSE not greater than the previously reached best one: ",
            best_RMSE)
    
    put_log2("Main loop:
Current minimal RMSE for `lambda = %1`: %2",
             min_RMSE,
             lambdas[which.min(lambda_RMSEs)])
    
    put_log2("Main loop:
So far reached best RMSE for `lambda = %1`: %2",
             um_reg_lambdas_best_results["best_lambda"],
             um_reg_lambdas_best_results["best_RMSE"])
    
    put(um_reg_lambdas_best_results)
    range_divider <- range_divider*2
    # browser()
    next
  }
  
  best_RMSE <- min_RMSE
  
  um_reg_lambdas_best_results <- 
    get_reg_best_params(lambdas, 
                        lambda_RMSEs)
  
  put_log2("Main loop:
Currently reached best RMSE for `lambda = %1`: %2",
           um_reg_lambdas_best_results["best_lambda"],
           um_reg_lambdas_best_results["best_RMSE"])
  
  put(um_reg_lambdas_best_results)
  
  rmses_min_ind <- which.min(lambda_RMSEs)
  seq_start_ind_tmp <- rmses_min_ind - 1
  
  if (seq_start_ind_tmp < 1) {
    seq_start_ind_tmp <- 1
    warning("`lambdas` index too small, so it assigned a value ",
            seq_start_ind)
    # browser()
  }
  
  if (seq_start_ind_tmp <= seq_start_ind) {
    range_divider <- range_divider*2
    # browser()
  }
  
  seq_start_ind <- seq_start_ind_tmp
  seq_end_ind <- rmses_min_ind + 1
  
  if (length(lambdas) < seq_end_ind) {
    warning("`seq_end_ind` index too large and will be set to `rmses_min_ind`.")
    seq_end_ind <- rmses_min_ind
    put_log1("Main loop:
Index exeeded the length of `lambdas`, so it is set to maximum possible value of %1",
             seq_end_ind)
    # browser()
  }
  
  if (seq_end_ind - seq_start_ind <= 0) {
    warning("`lambdas` sequential start index are the same or greater than end one.")
    put_log1("Main loop:
Current minimal RMSE: %1", rmse_min)
    
    put_log2("Main loop:
Reached minimal RMSE for lambda = %1: %2",
             um_reg_lambdas_best_results["best_lambda"],
             um_reg_lambdas_best_results["best_RMSE"])
    
    put(um_reg_lambdas_best_results)
    browser()
    break
  }
  
  seq_start <- lambdas[seq_start_ind]
  seq_end <- lambdas[seq_end_ind]
}

browser()
# stop("Procedure Completed")

# best_lambda   best_RMSE 
# -75.0000000   0.8578522 
## Re-train Regularized User+Movie Effect Model for the best `lambda` --------
best_user_movie_reg_lambda <- um_reg_lambdas_best_results["best_lambda"]
best_user_movie_reg_lambda

best_user_movie_reg_RMSE <- um_reg_lambdas_best_results["best_RMSE"]
print(best_user_movie_reg_RMSE)

put_log1("Re-training Regularized User+Movie Effect Model for the best `lambda`: %1...",
         best_user_movie_reg_lambda)

best_lambda_user_movie_effect <- train_user_movie_effect.cv(best_user_movie_reg_lambda)
best_lambda_user_movie_effect_RMSE <- calc_user_movie_effect_RMSE.cv(best_lambda_user_movie_effect)

put_log1("Regularized User+Movie Effect Model has been re-trained for the best `lambda`: %1.",
         best_user_movie_reg_lambda)
put_log1("The best RMSE after being regularized: %1",
         best_lambda_user_movie_effect_RMSE)

## Add a row to the RMSE Result Table for the Regularized User+Movie Effect Model --------
RMSEs <- rmses_add_row("Regularized User+Movie Effect Model", 
                       best_lambda_user_movie_effect_RMSE)
rmse_kable()


## Close Log -----------------------------------------------------------------
log_close()

