## Common Helper Functions -----------------------------------------------------
# Because we know ratings can’t be below 0.5 or above 5, 
# we define the function clamp:
clamp <- function(x, min = 0.5, max = 5) pmax(pmin(x, max), min)

make_ordinal_no <- function(n){
  if(n == 1){
    "1st"
  } else if(n == 2) {
    "2nd"
  } else if(n == 3) {
    "3rd"
  } else {
    str_glue("{n}th")
  }
}

## (R)MSE-related functions ---------------------------------------------------- 

#> Let's define some helper functions that we will use in our subsequent analysis:
mse <- function(r) mean(r^2)
mse_cv <- function(r_list) {
  mses <- sapply(r_list, mse(r))
  mean(mses)
}
rmse <- function(r) sqrt(mse(r))
# rmse_cv <- function(r_list) sqrt(mse_cv(r_list))
rmse2 <- function(true_ratings, predicted_ratings) {
  rmse(true_ratings - predicted_ratings)
}

## RMSEs Result Tibble ---------------------------------------------------------
CreateRMSEs_ResultTibble <- function(){
  tibble(Method = c("Project Objective"),
         RMSE = project_objective,
         Comment = " ")
}
RMSEs.AddRow <- function(RMSEs, method, value, comment = ""){
  RMSEs |>
    add_row(Method = method,
            RMSE = value,
            Comment = comment)
}
RMSE_kable <- function(RMSEs,
                       col1width = 10,
                       col2width = 5,
                       col3width = 20){
  RMSEs |>
    kable(align='lcl', booktabs = T, padding = 5) |> 
    row_spec(0, bold = T) |>
    column_spec(column = 1, width = RSME.tibble.col_width(col1width)) |>
    column_spec(column = 2, width = RSME.tibble.col_width(col2width)) #|>
    # column_spec(column = 3, width = RSME.tibble.col_width(col3width)) 
}
RSME.tibble.col_width <- function(x){
  "%1em" |> msg.glue(x)
}
## Overall Mean Rating Model ---------------------------------------------------
naive_model_MSEs <- function(val) {
  sapply(edx_CV, function(cv_item){
    mse(cv_item$validation_set$rating - val)
  })
}
naive_model_RMSE <- function(val){
  sqrt(mean(naive_model_MSEs(val)))
}
## Regularization --------------------------------------------------------------
mean_reg <- function(vals, lambda = 0, na.rm = TRUE){
  if (is.na(lambda)) {
    stop("Function: mean_reg
`lambda` is `NA`")
  }
  
  names(lambda) <- NULL
  sums <- sum(vals, na.rm = na.rm)
  N <- ifelse(na.rm, sum(!is.na(vals)), length(vals))
  sums/(N + lambda)
}
## Model Tuning ---------------------------------------------------------
get_fine_tune.param.endpoints <- function(preset.result) {

  preset.result.idx <- get_fine_tune.param.endpoints.idx(preset.result)
  
  i <- preset.result.idx["start"]
  j <- preset.result.idx["end"]
  best.idx <- preset.result.idx["best"]
  
  c(start = preset.result$parameter.value[i], 
    end = preset.result$parameter.value[j],
    best = preset.result$parameter.value[best.idx])
}
get_fine_tune.param.endpoints.idx <- function(preset.result) {
  best_RMSE <- min(preset.result$RMSE)
  best_RMSE.idx <- which.min(preset.result$RMSE)
  # best_lambda <- preset.result$parameter.value[best_RMSE.idx]
  
  preset.result.N <- length(preset.result$RMSE)
  i <- best_RMSE.idx
  j <- i
  
  while (i > 1) {
    i <- i - 1
    
    if (preset.result$RMSE[i] > best_RMSE) {
      break
    }
  }
  
  while (j < preset.result.N) {
    j <- j + 1
    
    if (preset.result$RMSE[j] > best_RMSE) {
      break
    }
  }
  
  c(start = i, 
    end = j,
    best = best_RMSE.idx)
}
tune.model_param <- function(param_values, 
                             fn_tune.test.param_value, 
                             break.if_min = TRUE,
                             steps.beyond_min = 2){
  n <- length(param_values)
  param_vals_tmp <- numeric()
  RMSEs_tmp <- numeric()
  RMSE_min <- Inf
  i_max.beyond_RMSE_min <- Inf
  prm_val.best <- NA
  
  put_log("Function: `tune.model_param`:
param_values:")
  put(param_values)
  
  for (i in 1:n) {
    put_log1("Function: `tune.model_param`:
Iteration %1", i)
    prm_val <- param_values[i]
    put_log1("Function: `tune.model_param`:
prm_val: %1", prm_val)
    param_vals_tmp[i] <- prm_val
    
    put_log2("Function: `tune.model_param`:
param_vals_tmp[%1]: %2", i, param_vals_tmp[i])
    put_log1("Function: `tune.model_param`:
param_vals_tmp length: %1", length(param_vals_tmp))
    put(param_vals_tmp)

    RMSE_tmp <- fn_tune.test.param_value(prm_val)

    put_log1("Function: `tune.model_param`:
RMSE_tmp: %1", RMSE_tmp)
    RMSEs_tmp[i] <- RMSE_tmp
    
    put_log2("Function: `tune.model_param`:
RMSEs_tmp[%1]: %2", i, RMSEs_tmp[i])
    put_log1("Function: `tune.model_param`:
RMSEs_tmp length: %1", length(RMSEs_tmp))
    put(RMSEs_tmp)
    
    plot(param_vals_tmp[RMSEs_tmp > 0], RMSEs_tmp[RMSEs_tmp > 0])
    # browser()
    
    if(RMSE_tmp > RMSE_min){
      warning("Function: `tune.model_param`:
`RSME` reached its minimum: ", RMSE_min, "
for parameter value: ", prm_val)
      put_log2("Function: `tune.model_param`:
Current `RMSE` value is %1 related to parameter value: %2",
               RMSE_tmp,
               prm_val)
      
      if (i > i_max.beyond_RMSE_min) {
        warning("Function: `tune.model_param`:
Operation is breaked (after `RSME` reached its minimum) on the following step: ", i)
        # browser()
        break
      }
      # browser()
      next
    }

    RMSE_min <- RMSE_tmp
    prm_val.best <- prm_val
    
    if (break.if_min) {
      i_max.beyond_RMSE_min <- i + steps.beyond_min
    }
    # browser()
  }
  
  param_values.best_result <- c(param.best_value = prm_val.best, 
                                best_RMSE = RMSE_min)
  
  
  put_log1("Function: `tune.model_param`:
Completed with RMSEs_tmp length: %1", length(RMSEs_tmp))
  list(tuned.result = data.frame(RMSE = RMSEs_tmp,
                                parameter.value = param_vals_tmp),
       best_result = param_values.best_result)
}
model.tune.param_range <- function(loop_starter,
                             tune_dir_path,
                             cache_file_base_name,
                             fn_tune.test.param_value,
                             # interval_divisor.multiplier = 4,
                             max.identical.min_RMSE.count = 4,
                             endpoint.min_diff = 0, #1e-07,
                             break.if_min = TRUE,
                             steps.beyond_min = 2){

  seq_start <- loop_starter[1]
  seq_end <- loop_starter[2]
  
  prm_val.leftmost <- seq_start
  prm_val.rightmost <- seq_end
  
  RMSE.leftmost <- NA
  RMSE.rightmost <- NA
  
  interval_divisor <- loop_starter[3]
  if (interval_divisor < 4) {
    interval_divisor <- 4
  }
  # max_interval_divisor <- loop_starter[4]
  
  
  best_RMSE <- NA
  param.best_value <- 0
  
  
  param_values.best_result <- c(param.best_value = param.best_value, 
                                best_RMSE = best_RMSE)
  # Start repeat loop
  repeat{
    seq_increment <- (seq_end - seq_start)/interval_divisor 
    
    if (seq_increment < 0.0000000000001) {
      warning("Function `model.tune.param_range`:
parameter value increment is too small.")
      
      put_log2("Function `model.tune.param_range`:
Final best RMSE for `parameter value = %1`: %2",
               param_values.best_result["param.best_value"],
               param_values.best_result["best_RMSE"])
      
      put(param_values.best_result)
      # param.best_value   best_RMSE 
      # -75.0000000   0.8578522  
      # browser()
      break
    }
    
    test_param_vals <- seq(seq_start, seq_end, seq_increment)
    
    file_name_tmp <- cache_file_base_name |>
      str_c("_") |>
      str_c(as.character(loop_starter[1])) |>
      str_c("_") |>
      str_c(as.character(loop_starter[3])) |>
      str_c("_") |>
      str_c(as.character(interval_divisor)) |>
      str_c(".") |>
      str_c(as.character(seq_start)) |>
      str_c("-") |>
      str_c(as.character(seq_end)) |>
      str_c(".RData")
    
    file_path_tmp <- file.path(tune_dir_path, file_name_tmp)
    
    put_log1("Function `model.tune.param_range`:
File path generated: %1", file_path_tmp)
    
    if (file.exists(file_path_tmp)) {
      put_log1("Function `model.tune.param_range`:
Loading tuning data from file: %1...", file_path_tmp)
      
      start <- put_start_date()
      load(file_path_tmp)
      put_end_date(start)
      put_log1("Function `model.tune.param_range`:
Tuning data has been loaded from file: %1", file_path_tmp)
      
      tuned.result <- tuned_result$tuned.result
      
      if(length(file_path_tmp) > 0) {
        # browser()
      }
    } else {
      tuned_result <- tune.model_param(test_param_vals, 
                                        fn_tune.test.param_value,
                                        break.if_min,
                                        steps.beyond_min)
      
      tuned.result <- tuned_result$tuned.result
      
      
      #     put_log1("Function `model.tune.param_range`:
      # File NOT saved (disabled for debug purposes): %1", file_path_tmp)
      save(tuned_result,
           param.best_value,
           # best_RMSE,
           seq_increment,
           interval_divisor,
           file = file_path_tmp)

      put_log1("Function `model.tune.param_range`:
File saved: %1", file_path_tmp)
    }
    
    plot(tuned.result$parameter.value, tuned.result$RMSE)
    bound.idx <- get_fine_tune.param.endpoints.idx(tuned.result)
    start.idx <- bound.idx["start"]
    end.idx <- bound.idx["end"]
    best_RMSE.idx <- bound.idx["best"]
    
    prm_val.leftmost.tmp <- tuned.result$parameter.value[start.idx]
    RMSE.leftmost.tmp <- tuned.result$RMSE[start.idx]

    # tuned.result.N <- length(tuned.result$parameter.value)
    prm_val.rightmost.tmp <- tuned.result$parameter.value[end.idx]
    RMSE.rightmost.tmp <- tuned.result$RMSE[end.idx]
    
    min_RMSE <- tuned.result$RMSE[best_RMSE.idx]
    min_RMSE.prm_val <- tuned.result$parameter.value[best_RMSE.idx]

    seq_start <- prm_val.leftmost.tmp
    seq_end <- prm_val.rightmost.tmp
    
    if (is.na(best_RMSE)) {
      prm_val.leftmost <- prm_val.leftmost.tmp
      RMSE.leftmost <- RMSE.leftmost.tmp
      
      # n <- length(tuned.result$parameter.value)
      prm_val.rightmost <- prm_val.rightmost.tmp
      RMSE.rightmost <- RMSE.rightmost.tmp
      
      param.best_value <- min_RMSE.prm_val
      best_RMSE <- min_RMSE
      
      # browser()
    }
    
    # browser()

    if (RMSE.leftmost.tmp - min_RMSE >= endpoint.min_diff) {
      prm_val.leftmost <- prm_val.leftmost.tmp
      RMSE.leftmost <- RMSE.leftmost.tmp
      # browser()
    } 
    
    if (RMSE.rightmost.tmp - min_RMSE >= endpoint.min_diff) {
      prm_val.rightmost <- prm_val.rightmost.tmp
      RMSE.rightmost <- RMSE.rightmost.tmp
      # browser()
    } 
    
    if (end.idx - start.idx <= 0) {
      warning("`tuned.result$parameter.value` sequential start index are the same or greater than end one.")
      put_log1("Function `model.tune.param_range`:
Current minimal RMSE: %1", rmse_min)
      
      put_log2("Function `model.tune.param_range`:
Reached minimal RMSE for the test parameter value = %1: %2",
               param_values.best_result["param.best_value"],
               param_values.best_result["best_RMSE"])
      
      put(param_values.best_result)
      # browser()
      break
    }
    
    if (best_RMSE == min_RMSE) {
      warning("Currently computed minimal RMSE equals the previously reached best one: ",
              best_RMSE, "
Currently computed minial value is: ", min_RMSE)
      
      put_log2("Function `model.tune.param_range`:
Current minimal RMSE for `parameter value = %1`: %2",
               tuned.result$parameter.value[which.min(tuned.result$RMSE)],
               min_RMSE)
      
      put_log2("Function `model.tune.param_range`:
So far reached best RMSE for `parameter value = %1`: %2",
               param_values.best_result["param.best_value"],
               param_values.best_result["best_RMSE"])
      
      put(param_values.best_result)

      if (sum(tuned.result$RMSE[tuned.result$RMSE == min_RMSE]) >= max.identical.min_RMSE.count) {
        warning("Minimal `RMSE`identical values count reached it maximum allowed value: ",
                max.identical.min_RMSE.count)
        # browser()
        put(tuned.result$RMSE)
        
        param_values.best_result <-
          get_best_param.result(tuned.result$parameter.value,
                                tuned.result$RMSE)
        # browser()
        put_log2("Function `model.tune.param_range`:
      Reached the best RMSE for `parameter value = %1`: %2",
                 param_values.best_result["param.best_value"],
                 param_values.best_result["best_RMSE"])
        # browser()
        break
      }

      # if (interval_divisor < max_interval_divisor) {
      #   interval_divisor <- interval_divisor*interval_divisor.multiplier
      #   # browser()
      # } else {
      #   warning("`interval_divisor` reached its maximum allowed value: ",
      #           max_interval_divisor)
      #   put_log1("The actual value of the `interval_divisor` is %1",
      #            interval_divisor)
      #   # browser()
      #   #break
      # }
    } else if (best_RMSE < min_RMSE) {
      warning("Current minimal RMSE is greater than previously computed best value: ",
              best_RMSE, "
Currently computed minial value is: ", min_RMSE)
      stop("Current minimal RMSE is greater than previously computed best value: ",
           best_RMSE, "
Currently computed minial value is: ", min_RMSE)
    }

    best_RMSE <- min_RMSE
    param.best_value <- min_RMSE.prm_val

    param_values.best_result <- 
      get_best_param.result(tuned.result$parameter.value, 
                          tuned.result$RMSE)
    
    put_log2("Function `model.tune.param_range`:
Currently reached best RMSE for `parameter value = %1`: %2",
             param_values.best_result["param.best_value"],
             param_values.best_result["best_RMSE"])
    
    put(param_values.best_result)
    
    #seq_start_ind <- best_RMSE.idx - 1
    
    # if (seq_start_ind < 1) {
    #   seq_start_ind <- 1
    #   warning("`tuned.result$parameter.value` index too small, so it assigned a value ",
    #           seq_start_ind)
    #   # browser()
    # }
    
    #seq_end_ind <- best_RMSE.idx + 1
    
#     if (length(tuned.result$parameter.value) < seq_end_ind) {
#       warning("`seq_end_ind` index too large and will be set to `best_RMSE.idx`.")
#       seq_end_ind <- best_RMSE.idx
#       put_log1("Function `model.tune.param_range`:
# Index exeeded the length of `tuned.result$parameter.value`, so it is set to maximum possible value of %1",
#                seq_end_ind)
#       # browser()
#     }
    
  }
  # End repeat loop
  
  n <- length(tuned.result$parameter.value)
  
  # browser()
  parameter.value <- tuned.result$parameter.value
  result.RMSE <- tuned.result$RMSE
  
  if (result.RMSE[1] == best_RMSE) {
    parameter.value[1] <- prm_val.leftmost
    result.RMSE[1] <- RMSE.leftmost
    # browser()
  }
  if (result.RMSE[n] == best_RMSE) {
    parameter.value[n+1] <- prm_val.rightmost
    result.RMSE[n+1] <- RMSE.rightmost
    # browser()
  }
  
  # browser()
  list(best_result = param_values.best_result,
       param_values.endpoints = c(prm_val.leftmost, prm_val.rightmost, seq_increment),
       tuned.result = data.frame(parameter.value = parameter.value,
                                 RMSE = result.RMSE))
}

