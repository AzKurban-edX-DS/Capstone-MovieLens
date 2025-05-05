## Data processing functions -------------------------------
load_movielens_data_from_file <- function(file_path){
  put_log1("Loading MovieLens datasets from file: %1...", 
           file_path)
  start <- put_start_date()
  load(file_path)
  put_end_date(start)
  put_log1("MoviLens datasets have been loaded from file: %s.", 
           file_path)
  movielens_datasets
}
splitByUser <- function(data){
  split(1:nrow(data), data$userId)
}
sample_train_validation_sets <- function(data, seed){
  put_log("Function: `sample_train_validation_sets`: Sampling 20% of the `data` data...")
  set.seed(seed)
  validation_ind <- 
    sapply(splitByUser(data),
           function(i) sample(i, ceiling(length(i)*.2))) |> 
    unlist() |> 
    sort()
  
  put_log("Function: `sample_train_validation_sets`: 
Extracting 80% of the original `data` not used for the Validation Set, 
excluding data for users who provided no more than a specified number of ratings: {min_nratings}.")
  
  train_set <- data[-validation_ind,]
  
  put_log("Function: `sample_train_validation_sets`: Dataset created: train_set")
  put(summary(train_set))
  
  put_log("Function: `sample_train_validation_sets`: 
To make sure we don’t include movies in the Training Set that should not be there, 
we exclude entries using the semi_join function from the Validation Set.")
  tmp.data <- data[validation_ind,]
  
  validation_set <- tmp.data |> 
    semi_join(train_set, by = "movieId") |> 
    semi_join(train_set, by = "userId") |>
    as.data.frame()
  
  # Add rows excluded from `validation_set` into `train_set`
  tmp.excluded <- anti_join(tmp.data, validation_set)
  train_set <- rbind(train_set, tmp.excluded)
  
  put_log("Function: `sample_train_validation_sets`: Dataset created: validation_set")
  put(summary(validation_set))

  # CV train & test sets Consistency Test
  validation.left_join.Nas <- train_set |>
    mutate(tst.col = rating) |>
    select(userId, movieId, tst.col) |>
    data.consistency.test(validation_set)
  
  put_log("Function: `sample_train_validation_sets`:
Below are the data consistency verification results")
  put(validation.left_join.Nas)
  
  # Return result datassets ----------------------------------------------------    
  list(train_set = train_set, 
       validation_set = validation_set)
}
mutateDateTimeAndDays <- function(data){
  data |>
    mutate(date_time = as_datetime(timestamp)) |>
    mutate(date = as_date(date_time)) |>
    mutate(days = as.integer(date - min(date)))
  
}
splitGenreRows <- function(data){
  put("Splitting dataset rows related to multiple genres...")
  start <- put_start_date()
  gs_splitted <- data |>
    separate_rows(genres, sep = "\\|")
  put("Dataset rows related to multiple genres have been splitted to have single genre per row.")
  put_end_date(start)
  gs_splitted
}
filter_noMore_nratings <- function(data, nratings){
  data |> 
    group_by(userId) |>
    filter(n() > nratings) |>
    ungroup()  
}
## Initialize input datasets ---------------------------------------------------
make_source_datasets <- function(){
  put_log("Function: `make_source_datasets`: Creating source datasets...")
  
  put_log("Function: `make_source_datasets`: Creating Rating Matrix from `edx` dataset...")
  edx.mx <- edx |> 
    mutate(userId = factor(userId),
           movieId = factor(movieId)) |>
    select(movieId, userId, rating) |>
    pivot_wider(names_from = movieId, values_from = rating) |>
    column_to_rownames("userId") |>
    as.matrix()
  
  put_log("Function: `make_source_datasets`:
Matrix created: `edx.mx` of the following dimentions:")
  put(dim(edx.mx))

  #> To be able to map movie IDs to titles we create the following lookup table:
  movie_map <- edx |> select(movieId, title, genres) |> 
    distinct(movieId, .keep_all = TRUE)
  
  put_log("Function: `make_source_datasets`: Dataset created: movie_map")
  put(summary(movie_map))
  
  put_log("Function: `make_source_datasets`: Creating Date-Days Map dataset...")
  date_days_map <- edx |>
    mutate(date_time = as_datetime(timestamp)) |>
    mutate(date = as_date(date_time)) |>
    mutate(year = year(date_time)) |>
    mutate(days = as.integer(date - min(date))) |>
    select(timestamp, date_time, date, year, days) |>
    distinct(timestamp, .keep_all = TRUE)
  
  str(date_days_map)
  put_log("Function: `make_source_datasets`: Dataset created: date_days_map")
  put(summary(date_days_map))
  
  put_log("Function: `make_source_datasets`: 
To account for the Movie Genre Effect, we need a dataset with split rows 
for movies belonging to multiple genres.")
  edx.sgr <- splitGenreRows(edx)
  
  
  
  #> We will use K-fold cross validation as explained in 
  #> Section 29.6.1: "K-fold validation" of the Cource Textbook:
  #> https://rafalab.dfci.harvard.edu/dsbook-part-2/ml/resampling-methods.html#k-fold-cross-validation
  #> We are going to compute the following version of the MSE introducing in that section:
  
  # $$
  #   \mbox{MSE}(\lambda) \approx\frac{1}{B} \sum_{b = 1}^B \frac{1}{N}\sum_{i = 1}^N \left(\hat{y}_i^b(\lambda) - y_i^b\right)^2 
  # $$
  
  start <- put_start_date()
  edx_CV <- lapply(kfold_index,  function(fold_i){
    
    put_log1("Method `make_source_datasets`: 
Creating K-Fold Cross Validation Datasets, Fold %1", fold_i)
    
    #> We split the initial datasets into training sets, which we will use to build 
    #> and train our models, and validation sets in which we will compute the accuracy 
    #> of our predictions, the way described in the `Section 23.1.1 Movielens data`
    #> (https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#movielens-data) 
    #> of the Course Textbook.
    
    split_sets <- edx |>
      sample_train_validation_sets(fold_i*1000)
    
    train_set <- split_sets$train_set
    validation_set <- split_sets$validation_set
    
    put_log("Function: `make_source_datasets`: 
Sampling 20% from the split-row version of the `edx` dataset...")
    split_sets.gs <- edx.sgr |>
      sample_train_validation_sets(fold_i*2000)

    train.sgr <- split_sets.gs$train_set
    validation.sgr <- split_sets.gs$validation_set
    
    # put_log("Function: `make_source_datasets`: Dataset created: validation.sgr")
    # put(summary(validation.sgr))
    
    #> We will use the array representation described in `Section 17.5 of the Textbook`
    #> (https://rafalab.dfci.harvard.edu/dsbook-part-2/linear-models/treatment-effect-models.html#sec-anova), 
    #> for the training data. 
    #> To create this matrix, we use `tidyr::pivot_wider` function:
    
    # train_set <- mutate(train_set, userId = factor(userId), movieId = factor(movieId))
    # train.sgr <- mutate(train.sgr, userId = factor(userId), movieId = factor(movieId))
    
    put_log("Function: `make_source_datasets`: Creating Rating Matrix from Train Set...")
    train_mx <- train_set |> 
      mutate(userId = factor(userId),
             movieId = factor(movieId)) |>
      select(movieId, userId, rating) |>
      pivot_wider(names_from = movieId, values_from = rating) |>
      column_to_rownames("userId") |>
      as.matrix()
    
    put_log("Function: `make_source_datasets`:
Matrix created: `train_mx` of the following dimentions:")
    put(dim(train_mx))


    list(train_set = train_set,
         train_mx = train_mx,
         train.sgr = train.sgr,
         validation_set = validation_set)
  })
  put_end_date(start)
  put_log("Function: `make_source_datasets`: 
Set of K-Fold Cross Validation datasets created: edx_CV")
  
  tuning_sets <- edx |>
    sample_train_validation_sets(5423)
  
  list(edx_CV = edx_CV,
       edx.mx = edx.mx,
       tuning_sets = tuning_sets,
       movie_map = movie_map,
       date_days_map = date_days_map)
}
init_source_datasets <- function(){
  put_log("Method `init_source_datasets`: 
Initializing sourse datasets...")
  
  if(file.exists(movielens_datasets_file_path)){
    movielens_datasets <- load_movielens_data_from_file(movielens_datasets_file_path)
  } else if(file.exists(movielens_datasets_zip)) {
    put_log("Method `init_source_datasets`: 
Unzipping MovieLens data file from zip-archive: {movielens_datasets_zip}...") 
    
    start <- put_start_date()
    unzip(movielens_datasets_zip, movielens_datasets_file_path)
    
    if(!file.exists(movielens_datasets_file_path)) {
      put_log("Method `init_source_datasets`: 
File does not exists: {movielens_datasets_file}.")
      stop("Failed to unzip MovieLens data zip-archive.")
    }
    
    movielens_datasets <- load_movielens_data_from_file(movielens_datasets_file_path)
  } else {
    put_log("Method `init_source_datasets`: 
Creating datasets...")
    library(edx.capstone.movielens.data)
    put_log("Method `init_source_datasets`: 
Library attached: 'edx.capstone.movielens.data'")
    
    start <- put_start_date()
    movielens_datasets <- make_source_datasets()
    put_end_date(start)
    put("Method `init_source_datasets`: 
All required datasets have been created.")
    
    put_log("Method `init_source_datasets`: 
Saving newly created input datasets to file...")
    start <- put_start_date()
    save(movielens_datasets, file =  movielens_datasets_file_path)
    put_end_date(start)
    
    if(!file.exists(movielens_datasets_file_path)) {
      put_log("Method `init_source_datasets`: 
File was not created: {movielens_datasets_file}.")
      warning("MovieLens data was not saved to file.")
    } else {
      put_log("Method `init_source_datasets`: 
Datasets have been saved to file: {movielens_datasets_file_path}.") 
      
      
      put_log("Method `init_source_datasets`: 
Creating zip-archive: {movielens_datasets_zip}...") 
      
      zip(movielens_datasets_zip, movielens_datasets_file_path)
      
      if(!file.exists(movielens_datasets_zip)){
        put_log("Method `init_source_datasets`: 
Failed to zip file: {movielens_datasets_file_path}.")
        warning("Failed to zip MovieLens data file.")
      } else {
        put_log("Method `init_source_datasets`: 
Zip-archive created: {movielens_datasets_zip}.")
        #file.remove(movielens_datasets_file)
        
        if(file.exists(movielens_datasets_file_path)){
          put_log("Method `init_source_datasets`: 
Failed to remove file: {movielens_datasets_file_path}.")
          warning("Failed to remove MovieLens data file.")
        } else {
          put_log("Method `init_source_datasets`: 
File has been removed: {movielens_datasets_file_path}")
        }
      }
    }
  }
  movielens_datasets
}
## Data consistency validation -------------------------------------------------
data.consistency.days.test.cv <- function(data) {
  data |> data.consistency.test.cv(by.userId = FALSE,
                                   by.movieId = FALSE,
                                   by.days = TRUE)
}
data.consistency.test.cv <- function(data, 
                                     by.userId = TRUE, 
                                     by.movieId = TRUE,
                                     by.days = FALSE) {
  user.dat <- NULL
  movie.dat <- NULL
  days.dat <- NULL
  
  if (by.userId) {
    user.dat <- data |>
      group_by(userId) |>
      summarise(u = mean(tst.col))
    
    print(str(user.dat))
    print(sum(is.na(user.dat$u)))
  }
  
  if (by.movieId) {
    movie.dat <- data |>
      group_by(movieId) |>
      summarise(m = mean(tst.col))
    
    print(str(movie.dat))
    print(sum(is.na(movie.dat$m)))
  }

  if (by.days) {
    days.dat <- data |>
      group_by(days) |>
      summarise(d = mean(tst.col))
    
    print(str(days.dat))
    print(sum(is.na(days.dat$m)))
  }
  
  edx_CV.left_join.NAs(user.dat, 
                       movie.dat, 
                       days.dat)
}

edx_CV.left_join.NAs <- function(user.dat = NULL, 
                                 movie.dat = NULL,
                                 days.dat = NULL) {
  result <- sapply(edx_CV, function(cv_item){
    cv_item$validation_set |>
      left_join(date_days_map, by = "timestamp") |>
      datasets.left_join.NAs(user.dat, 
                             movie.dat,
                             days.dat)
  })
  
  t(result)
}

data.consistency.days.test <- function(data, test.dat) {
  data |> 
    data.consistency.test(test.dat |>
                            left_join(date_days_map, by = "timestamp"),
                          by.userId = FALSE,
                          by.movieId = FALSE,
                          by.days = TRUE)
}
data.consistency.test <- function(data, 
                                  test.dat,
                                  by.userId = TRUE, 
                                  by.movieId = TRUE,
                                  by.days = FALSE) {
  user.dat <- NULL
  movie.dat <- NULL
  days.dat <- NULL
  
  if (by.userId) {
    user.dat <- data |>
      group_by(userId) |>
      summarise(u = mean(tst.col))
    
    print(str(user.dat))
    print(sum(is.na(user.dat$u)))
  }
  
  if (by.movieId) {
    movie.dat <- data |>
      group_by(movieId) |>
      summarise(m = mean(tst.col))
    
    print(str(movie.dat))
    print(sum(is.na(movie.dat$m)))
  }

  if (by.days) {
    days.dat <- data |>
      group_by(days) |>
      summarise(d = mean(tst.col))
    
    print(str(days.dat))
    print(sum(is.na(days.dat$d)))
  }
  
  test.dat |>
    datasets.left_join.NAs(user.dat, 
                           movie.dat, 
                           days.dat)
}

datasets.left_join.NAs <- function(test_set, 
                                   user.dat = NULL, 
                                   movie.dat = NULL,
                                   days.dat = NULL) {
  u.NAs <- NA
  m.NAs <- NA
  d.NAs <- NA
  
  if (!is.null(user.dat)) {
    u.vals <- test_set |>
      left_join(user.dat, by = "userId") |>
      pull(u)
    
    u.NAs <- sum(is.na(u.vals))
  }
  
  
  if (!is.null(movie.dat)) {
    m.vals <- test_set |>
      left_join(movie.dat, by = "movieId") |>
      pull(m)
    
    m.NAs <- sum(is.na(m.vals))
  }
  
  if (!is.null(days.dat)) {
    d.vals <- test_set |>
      left_join(days.dat, by = "days") |>
      pull(d)
    
    d.NAs <- sum(is.na(d.vals))
  }
  
  c(user.NAs = u.NAs, 
    movie.NAs = m.NAs, 
    days.NAs = d.NAs)
}
## Model training ---------------------------------------------------
union_cv_results <- function(data_list) {
  out_dat <- data_list[[1]]
  
  for (i in 2:CVFolds_N){
    out_dat <- union(out_dat, 
                     data_list[[i]])
  }
  
  out_dat
}
get_best_param.result <- function(param_values, rmses){
  best_pvalue_idx <- which.min(rmses)
  c(param.best_value = param_values[best_pvalue_idx], 
    best_RMSE = rmses[best_pvalue_idx])
}
