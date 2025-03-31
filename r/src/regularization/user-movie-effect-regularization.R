# User+Movie Effect Regularization
## Global Variables -------------------------------------------------------------
ume_regularization_path <- file.path(regularization_data_path, 
                                      "user-movie-effect")

## Open log -------------------------------------------------------------------
open_logfile(".reg-um-effect.loop_0_10_d10")

## Process lamdas in loop starting from `ume_reg_lambdas = seq(0, 10, 10/10` -------
loop_starter <- c(0, 4, 2)
ume_seq_start <- loop_starter[1]
ume_seq_end <- loop_starter[2]
ume_range_divider <- loop_starter[3] 
ume_max_range_divider <- 128

ume_best_RMSE <- Inf
ume_best_lambda <- 0

ume_reg_lambdas_best_results <- c(best_lambda = ume_best_lambda, 
                                 best_RMSE = ume_best_RMSE)

repeat{
  ume_seq_increment <- (ume_seq_end - ume_seq_start)/ume_range_divider 
  
  if (ume_seq_increment < 0.0000000000001) {
    warning("Main loop:
lambda increment is too small.")
    
    put_log2("Main loop:
Final best RMSE for `lambda = %1`: %2",
             ume_reg_lambdas_best_results["best_lambda"],
             ume_reg_lambdas_best_results["best_RMSE"])
    
    put(ume_reg_lambdas_best_results)
    # best_lambda   best_RMSE 
    # -75.0000000   0.8578522  
    # browser()
    break
  }
  
  test_lambdas <- seq(ume_seq_start, ume_seq_end, ume_seq_increment)

  file_name_tmp <- "umgy_reg-loop_" |>
    str_c(as.character(loop_starter[1])) |>
    str_c("_") |>
    str_c(as.character(loop_starter[3])) |>
    str_c("_") |>
    str_c(as.character(ume_range_divider)) |>
    str_c(".") |>
    str_c(as.character(ume_seq_start)) |>
    str_c("-") |>
    str_c(as.character(ume_seq_end)) |>
    str_c(".RData")
  
  file_path_tmp <- file.path(ume_regularization_path, file_name_tmp)
  
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
    
    if(length(file_path_tmp) > 0) {
      # browser()
    }
  } else {
    reg_result <- regularize.user_movie_effect(test_lambdas, 
                                               is.cv = TRUE,
                                               break_if_min = TRUE)
    ume_reg_RMSEs <- reg_result$RMSEs
    ume_reg_lambdas <- reg_result$lambdas
    
#     put_log1("Main loop:
# File NOT saved (disabled for debug purposes): %1", file_path_tmp)
        save(ume_reg_lambdas,
             ume_reg_RMSEs,
             ume_best_lambda,
             ume_best_RMSE,
             ume_seq_increment,
             ume_range_divider,
             ume_max_range_divider,
             file = file_path_tmp)

        put_log1("Main loop:
File saved: %1", file_path_tmp)
  }
  
  plot(ume_reg_lambdas, ume_reg_RMSEs)
  # browser()
  
  min_RMSE <- min(ume_reg_RMSEs)
  RMSEs_min_ind <- which.min(ume_reg_RMSEs)
  
  
  if (ume_best_RMSE <= min_RMSE) {
    warning("Currently computed minimal RMSE not greater than the previously reached best one: ",
            ume_best_RMSE)
    
    put_log2("Main loop:
Current minimal RMSE for `lambda = %1`: %2",
             min_RMSE,
             ume_reg_lambdas[which.min(ume_reg_RMSEs)])
    
    put_log2("Main loop:
So far reached best RMSE for `lambda = %1`: %2",
             ume_reg_lambdas_best_results["best_lambda"],
             ume_reg_lambdas_best_results["best_RMSE"])
    
    put(ume_reg_lambdas_best_results)
    
    if (ume_range_divider < ume_max_range_divider) {
      ume_range_divider <- ume_range_divider*2
      # browser()
    } else {
      warning("`ume_range_divider` reached its maximum allowed value: ",
              ume_max_range_divider)
      put_log1("The actual value of the `ume_range_divider` is %1",
               ume_range_divider)
      # browser()
      break
    }
  } else {
    ume_best_RMSE <- min_RMSE
    ume_best_lambda <- ume_reg_lambdas[RMSEs_min_ind]
  }

  ume_reg_lambdas_best_results <- 
    get_reg_best_params(ume_reg_lambdas, 
                        ume_reg_RMSEs)
  
  put_log2("Main loop:
Currently reached best RMSE for `lambda = %1`: %2",
           ume_reg_lambdas_best_results["best_lambda"],
           ume_reg_lambdas_best_results["best_RMSE"])
  
  put(ume_reg_lambdas_best_results)
  
  ume_seq_start_ind <- RMSEs_min_ind - 1
  
  if (ume_seq_start_ind < 1) {
    ume_seq_start_ind <- 1
    warning("`ume_reg_lambdas` index too small, so it assigned a value ",
            ume_seq_start_ind)
    # browser()
  }
  
  ume_seq_end_ind <- RMSEs_min_ind + 1
  
  if (length(ume_reg_lambdas) < ume_seq_end_ind) {
    warning("`ume_seq_end_ind` index too large and will be set to `RMSEs_min_ind`.")
    ume_seq_end_ind <- RMSEs_min_ind
    put_log1("Main loop:
Index exeeded the length of `ume_reg_lambdas`, so it is set to maximum possible value of %1",
             ume_seq_end_ind)
    # browser()
  }
  
  if (ume_seq_end_ind - ume_seq_start_ind <= 0) {
    warning("`ume_reg_lambdas` sequential start index are the same or greater than end one.")
    put_log1("Main loop:
Current minimal RMSE: %1", rmse_min)
    
    put_log2("Main loop:
Reached minimal RMSE for lambda = %1: %2",
             ume_reg_lambdas_best_results["best_lambda"],
             ume_reg_lambdas_best_results["best_RMSE"])
    
    put(ume_reg_lambdas_best_results)
    # browser()
    break
  }
  
  ume_seq_start <- ume_reg_lambdas[ume_seq_start_ind]
  ume_seq_end <- ume_reg_lambdas[ume_seq_end_ind]
}

# browser()
# stop("Procedure Completed")

# best_lambda   best_RMSE 
# -75.0000000   0.8578522 
## Re-train Regularized User+Movie Effect Model for the best `lambda` --------
best_user_movie_reg_lambda <- ume_reg_lambdas_best_results["best_lambda"]
best_user_movie_reg_lambda

best_user_movie_reg_RMSE <- ume_reg_lambdas_best_results["best_RMSE"]
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
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("Regularized User+Movie Effect Model", 
               best_lambda_user_movie_effect_RMSE)

RMSE_kable(RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble 
for the `Regularized User+Movie Effect Model`.")
## Close Log -----------------------------------------------------------------
log_close()

