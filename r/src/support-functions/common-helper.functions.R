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
RMSE_kable <- function(RMSEs){
  RMSEs |>
    kable(align='lcl', booktabs = T, padding = 5) |> 
    row_spec(0, bold = T) |>
    column_spec(column = 1, width = "35em") |>
    column_spec(column = 2, width = "10em") |>
    column_spec(column = 3, width = "50em") 
}

## Data processing functions -------------------------------
load_movielens_data_from_file <- function(file_path){
  put(sprintf("Loading MovieLens datasets from file: %s...", 
              file_path))
  start <- put_start_date()
  load(file_path)
  put_end_date(start)
  put(sprintf("MoviLens datasets have been loaded from file: %s.", 
              file_path))
  movielens_datasets
}
filter_noMore_nratings <- function(data, nratings){
  data |> 
    group_by(userId) |>
    filter(n() > nratings) |>
    ungroup()  
}
splitByUser <- function(data){
  split(1:nrow(data), data$userId)
}
mutateDateTimeAndDays <- function(data){
  data |>
    mutate(date_time = as_datetime(timestamp)) |>
    mutate(date = as_date(date_time)) |>
    mutate(days = as.integer(date - min(date)))
  
}
separateGenreRows <- function(data){
  put("Splitting dataset rows related to multiple genres...")
  start <- put_start_date()
  gs_splitted <- data |>
    separate_rows(genres, sep = "\\|")
  put("Dataset rows related to multiple genres have been splitted to have single genre per row.")
  put_end_date(start)
  gs_splitted
}
union_cv_results <- function(data_list) {
  out_dat <- data_list[[1]]
  
  for (i in 2:CVFolds_N){
    out_dat <- union(out_dat, 
                     data_list[[i]])
  }
  
  out_dat
}
sample_train_validation_sets <- function(seed){
  put_log("Function: `sample_train_validation_sets`: Sampling 20% of the `edx` data...")
  set.seed(seed)
  validation_ind <- 
    sapply(splitByUser(edx),
           function(i) sample(i, ceiling(length(i)*.2))) |> 
    unlist() |> 
    sort()
  
  put_log("Function: `sample_train_validation_sets`: 
For training our models, we will ignore the data from users 
who have provided no more than the specified number of ratings. ({min_nratings})")
  
  put_log("Function: `sample_train_validation_sets`: 
Extracting 80% of the `edx` data not used for the Validation Set, 
excluding data for users who provided no more than a specified number of ratings: {min_nratings}.")
  train_set <- edx[-validation_ind,] |>
    filter_noMore_nratings(min_nratings)
  
  put_log("Function: `sample_train_validation_sets`: Dataset created: train_set")
  put(summary(train_set))
  
  put_log("Function: `sample_train_validation_sets`: 
To make sure we don’t include movies in the Training Set that should not be there, 
we remove entries using the semi_join function from the Validation Set.")
  validation_set <- edx[validation_ind,] |> 
    semi_join(train_set, by = "movieId") |> 
    as.data.frame()
  
  put_log("Function: `sample_train_validation_sets`: Dataset created: validation_set")
  put(summary(validation_set))
  
  list(train_set = train_set, 
       validation_set = validation_set)
}
get_best_param.result <- function(param_values, rmses){
  best_pvalue_idx <- which.min(rmses)
  c(param.best_value = param_values[best_pvalue_idx], 
    best_RMSE = rmses[best_pvalue_idx])
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
  
  sums <- sum(vals, na.rm = na.rm)
  N <- ifelse(na.rm, sum(!is.na(vals)), length(vals))
  sums/(N + lambda)
}
## Model Tuning ---------------------------------------------------------
get_fine_tuning.param.endpoints <- function(preset_result) {
  best_RMSE <- min(preset_result$RMSEs)
  best_RMSE.idx <- which.min(preset_result$RMSEs)
  best_lambda <- preset_result$param_values[best_RMSE.idx]
  
  preset_result.N <- length(preset_result$RMSEs)
  i <- best_RMSE.idx
  j <- i
  
  while (i > 1) {
    i <- i - 1
    
    if (preset_result$RMSEs[i] > best_RMSE) {
      break
    }
  }
  
  while (j < preset_result.N) {
    j <- j + 1
    
    if (preset_result$RMSEs[j] > best_RMSE) {
      break
    }
  }
  
  c(start = preset_result$param_values[i], 
    end = preset_result$param_values[j])
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
  list(RMSEs = RMSEs_tmp,
       param_values = param_vals_tmp,
       best_result = param_values.best_result)
}
model.tune.param_range <- function(loop_starter,
                             tune_dir_path,
                             cache_file_base_name,
                             fn_tune.test.param_value,
                             # range_divider.multiplier = 4,
                             max.identical.min_RMSE.count = 4,
                             is.cv = TRUE,
                             endpoint.min_diff = 1e-07,
                             break.if_min = TRUE,
                             steps.beyond_min = 2){

  seq_start <- loop_starter[1]
  seq_end <- loop_starter[2]
  
  prm_val.leftmost <- seq_start
  prm_val.rightmost <- seq_end
  
  RMSE.leftmost <- NA
  RMSE.rightmost <- NA
  
  range_divider <- loop_starter[3]
  if (range_divider < 4) {
    range_divider <- 4
  }
  # max_range_divider <- loop_starter[4]
  
  
  best_RMSE <- Inf
  param.best_value <- 0
  
  
  param_values.best_result <- c(param.best_value = param.best_value, 
                                best_RMSE = best_RMSE)
  
  repeat{
    seq_increment <- (seq_end - seq_start)/range_divider 
    
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
      str_c(as.character(range_divider)) |>
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
      
      if(length(file_path_tmp) > 0) {
        # browser()
      }
    } else {
      tuning_result <- tune.model_param(test_param_vals, 
                                        fn_tune.test.param_value,
                                        break.if_min,
                                        steps.beyond_min)
      
      #     put_log1("Function `model.tune.param_range`:
      # File NOT saved (disabled for debug purposes): %1", file_path_tmp)
      save(tuning_result,
           param.best_value,
           best_RMSE,
           seq_increment,
           range_divider,
           # max_range_divider,
           file = file_path_tmp)

      put_log1("Function `model.tune.param_range`:
File saved: %1", file_path_tmp)
    }
    
    plot(tuning_result$param_values, tuning_result$RMSEs)
    # browser()
    prm_val.leftmost.tmp <- tuning_result$param_values[1]
    RMSE.leftmost.tmp <- tuning_result$RMSEs[1]

    tuning_result.N <- length(tuning_result$param_values)
    prm_val.rightmost.tmp <- tuning_result$param_values[tuning_result.N]
    RMSE.rightmost.tmp <- tuning_result$RMSEs[tuning_result.N]
    
    min_RMSE <- min(tuning_result$RMSEs)
    RMSEs_min_ind <- which.min(tuning_result$RMSEs)

    if (RMSE.leftmost.tmp - min_RMSE >= endpoint.min_diff) {
      prm_val.leftmost <- prm_val.leftmost.tmp
      RMSE.leftmost <- RMSE.leftmost.tmp
    } 
    
    if (RMSE.rightmost.tmp - min_RMSE >= endpoint.min_diff) {
      prm_val.rightmost <- prm_val.rightmost.tmp
      RMSE.rightmost <- RMSE.rightmost.tmp
    } 

    if (best_RMSE == min_RMSE) {
      warning("Currently computed minimal RMSE equals the previously reached best one: ",
              best_RMSE, "
Currently computed minial value is: ", min_RMSE)
      
      put_log2("Function `model.tune.param_range`:
Current minimal RMSE for `parameter value = %1`: %2",
               tuning_result$param_values[which.min(tuning_result$RMSEs)],
               min_RMSE)
      
      put_log2("Function `model.tune.param_range`:
So far reached best RMSE for `parameter value = %1`: %2",
               param_values.best_result["param.best_value"],
               param_values.best_result["best_RMSE"])
      
      put(param_values.best_result)

      if (sum(tuning_result$RMSEs[tuning_result$RMSEs == min_RMSE]) >= max.identical.min_RMSE.count) {
        warning("Minimal `RMSE`identical values count reached it maximum allowed value: ",
                max.identical.min_RMSE.count)
        
        put(tuning_result$RMSEs)
        
        param_values.best_result <-
          get_best_param.result(tuning_result$param_values,
                                tuning_result$RMSEs)
        
        put_log2("Function `model.tune.param_range`:
      Reached the best RMSE for `parameter value = %1`: %2",
                 param_values.best_result["param.best_value"],
                 param_values.best_result["best_RMSE"])
        break
      }

      # if (range_divider < max_range_divider) {
      #   range_divider <- range_divider*range_divider.multiplier
      #   # browser()
      # } else {
      #   warning("`range_divider` reached its maximum allowed value: ",
      #           max_range_divider)
      #   put_log1("The actual value of the `range_divider` is %1",
      #            range_divider)
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
    param.best_value <- tuning_result$param_values[RMSEs_min_ind]

    param_values.best_result <- 
      get_best_param.result(tuning_result$param_values, 
                          tuning_result$RMSEs)
    
    put_log2("Function `model.tune.param_range`:
Currently reached best RMSE for `parameter value = %1`: %2",
             param_values.best_result["param.best_value"],
             param_values.best_result["best_RMSE"])
    
    put(param_values.best_result)
    
    seq_start_ind <- RMSEs_min_ind - 1
    
    if (seq_start_ind < 1) {
      seq_start_ind <- 1
      warning("`tuning_result$param_values` index too small, so it assigned a value ",
              seq_start_ind)
      # browser()
    }
    
    seq_end_ind <- RMSEs_min_ind + 1
    
    if (length(tuning_result$param_values) < seq_end_ind) {
      warning("`seq_end_ind` index too large and will be set to `RMSEs_min_ind`.")
      seq_end_ind <- RMSEs_min_ind
      put_log1("Function `model.tune.param_range`:
Index exeeded the length of `tuning_result$param_values`, so it is set to maximum possible value of %1",
               seq_end_ind)
      # browser()
    }
    
    if (seq_end_ind - seq_start_ind <= 0) {
      warning("`tuning_result$param_values` sequential start index are the same or greater than end one.")
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
    
    seq_start <- tuning_result$param_values[seq_start_ind]
    seq_end <- tuning_result$param_values[seq_end_ind]
  }
  n <- length(tuning_result$param_values)
  tuning_result$param_values[1] <- prm_val.leftmost
  tuning_result$param_values[n+1] <- prm_val.rightmost
  
  tuning_result$RMSEs[1] <- RMSE.leftmost
  tuning_result$RMSEs[n+1] <- RMSE.rightmost
  
  # browser()
  list(best_result = param_values.best_result,
       param_values.endpoints = c(prm_val.leftmost, prm_val.rightmost),
       tuning_result = tuning_result)
}




