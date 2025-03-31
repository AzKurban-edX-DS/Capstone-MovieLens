## Logging Feature -------------------------------------------------------------
open_logfile <- function(file_name){
  log_file_name <- as.character(Sys.time()) |> 
    str_replace_all(':', '_') |> 
    str_replace(' ', 'T') |>
    str_c(file_name)
  
  log_open(file_name = log_file_name)
}
print_start_date <- function(){
  print(date())
  Sys.time()
}
put_start_date <- function(){
  put(date())
  Sys.time()
}
print_end_date <- function(start){
  print(date())
  print(Sys.time() - start)
}
put_end_date <- function(start){
  put(date())
  put(Sys.time() - start)
}

print_log <- function(msg){
  print(str_glue(msg))
}
put_log <- function(msg){
  put(str_glue(msg))
}
put_log1 <- function(msg_template, arg1){
  msg <- str_replace_all(msg_template, "%1", as.character(arg1))
  put(str_glue(msg))
}
put_log2 <- function(msg_template, arg1, arg2){
  msg <- msg_template |> 
    str_replace_all("%1", as.character(arg1)) |>
    str_replace_all("%2", as.character(arg2))
  put(str_glue(msg))
}

put_log3 <- function(msg_template, arg1, arg2, arg3){
  msg <- msg_template |> 
    str_replace_all("%1", as.character(arg1)) |>
    str_replace_all("%2", as.character(arg2)) |>
    str_replace_all("%3", as.character(arg2))
  put(str_glue(msg))
}

put_log3 <- function(msg_template, arg1, arg2, arg3, arg4){
  msg <- msg_template |> 
    str_replace_all("%1", as.character(arg1)) |>
    str_replace_all("%2", as.character(arg2)) |>
    str_replace_all("%3", as.character(arg2)) |>
    str_replace_all("%4", as.character(arg2))
  put(str_glue(msg))
}


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
         RMSE = project_objective)
}

RMSEs.AddRow <- function(RMSEs, method, value){
  RMSEs |>
    add_row(Method = method,
            RMSE = value)
}

RMSE_kable <- function(RMSEs){
  RMSEs |>
    kable(align='lrr', booktabs = T, padding = 5) |> 
    row_spec(0, bold = T) |>
    column_spec(column = 1, width = "25em")
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
get_reg_best_params <- function(lambdas, rmses){
  best_lambda_idx <- which.min(rmses)
  c(best_lambda = lambdas[best_lambda_idx], 
    best_RMSE = rmses[best_lambda_idx])
}




