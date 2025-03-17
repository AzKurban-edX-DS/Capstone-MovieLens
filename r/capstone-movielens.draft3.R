## Setup -----------------------------------------------------------------------
#> Reference: Some ideas and code snippers were used from the following GitHub repository:
#> https://github.com/AzKurban-edX-DS/harvardx-movielens


if(!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")

#> `stringr` library is already included to the `tidyverse` package,
#> there's no need to install `stringr`
# if(!require(stringr))
#   install.packages("stringr")

if(!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table))
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(gridExtra))
  install.packages("gridExtra")
if(!require("logr")) 
  install.packages("logr")

if(!require(gtools)) 
  install.packages("gtools")
if(!require(pak)) 
  install.packages("pak")
if(!require("pacman")) 
  install.packages("pacman")

# Loading the required libraries
library(dslabs)
library(tidyverse)

library(caret)
library(cowplot)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(Metrics)
library(recosystem)
library(scales)
library(stringr)
library(tibble)
library(tidyr)
library(gridExtra)
library(logr)

library(rafalib)
library(gtools)
library(pak)
library(pacman)

p_load(conflicted, latex2exp, kableExtra)

### Resolve conflicts ----------------------------------------------------------

# For functions with identical names in different packages, ensure the
# right one is chosen:
conflict_prefer("first", "dplyr", quiet = TRUE)
conflict_prefer("count", "dplyr", quiet = TRUE)
conflict_prefer("select", "dplyr", quiet = TRUE)
conflict_prefer("group_by", "dplyr", quiet = TRUE)
conflict_prefer("ungroup", "dplyr", quiet = TRUE)
conflict_prefer("summarise", "dplyr", quiet = TRUE)
conflict_prefer("summarize", "dplyr", quiet = TRUE)
conflict_prefer("distinct", "dplyr", quiet = TRUE)
conflict_prefer("top_n", "dplyr", quiet = TRUE)
conflict_prefer("arrange", "dplyr", quiet = TRUE)
conflict_prefer("mutate", "dplyr", quiet = TRUE)
conflict_prefer("semi_join", "dplyr", quiet = TRUE)
conflict_prefer("left_join", "dplyr", quiet = TRUE)
conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("slice", "dplyr", quiet = TRUE)
conflict_prefer("glimpse", "dplyr", quiet = TRUE)

conflict_prefer("pivot_wider", "tidyr", quiet = TRUE)
conflict_prefer("kable", "kableExtra", quiet = TRUE)
conflict_prefer("year", "lubridate", quiet = TRUE)

## Open log Function -----------------------------------------------------------
open_logfile <- function(file_name){
  log_file_name <- as.character(Sys.time()) |> 
    str_replace_all(':', '_') |> 
    str_replace(' ', 'T') |>
    str_c(file_name)
  
  log_open(file_name = log_file_name)
}

## Open log -----------------------------------------------------------
open_logfile(".init-project-data")

## Init Project Global Variables ----------------------------------------------
put("Set Project Objective according to Capstone course requirements")
project_objective <- 0.86490
put(project_objective)

put("Set minimum number of ratings to ignore")
min_nratings <- as.integer(100)
put(min_nratings)

## Defining helper functions --------------------------------------------------

#> Let's define some helper functions that we will use in our subsequent analysis:
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

mse <- function(r) mean(r^2, rm.na = TRUE)
mse_cv <- function(r_list) {
  mses <- sapply(r_list, mse(r))
  mean(mses)
}

rmse <- function(r) sqrt(mse(r))
rmse_cv <- function(r_list) sqrt(mse_cv(r_list))
rmse2 <- function(true_ratings, predicted_ratings) {
  rmse(true_ratings - predicted_ratings)
}

final_rmse <- function(r) sqrt(mean(r^2, rm.na = TRUE))

RMSEs <- NULL
RMSEs

rmses_add_row <- function(method, value){
  RMSEs |>
    add_row(Method = method,
            RMSE = value)
}

rmse_kable <- function(){
  RMSEs |>
    kable(align='lrr', booktabs = T, padding = 5) |> 
    row_spec(0, bold = T) |>
    column_spec(column = 1, width = "25em")
}

# Because we know ratings can’t be below 0.5 or above 5, 
# we define the function clamp:
clamp <- function(x, min = 0.5, max = 5) pmax(pmin(x, max), min)

### Data processing functions & global variables -------------------------------
data_path <- "data"
CVFolds_N <- 5
kfold_index <- seq(from = 1:CVFolds_N)

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

## Datasets ===================================================================

# To start with we have to generate two datasets derived from the MovieLens one:
#   
# *`edx`: we use it to develop and train our algorithms;
# * `final_holdout_test`:  according to the course requirements, 
#    we use it exclusively to evaluate the _**RMSE**_ of our final algorithm.
# 
# For this purpose the following package has been developed by the author of this 
# script: `edx.capstone.movielens.data`. The source code of the package is available 
# on `GitHub`(https://github.com/AzKurban-edX-DS/edx.capstone.movielens.data).
# 
# Let's install the development version of this package from the `GitHub` repository 
# and attach the correspondent library to the global environment:

if(!require(edx.capstone.movielens.data)) {
  start <- put_start_date()
  pak::pak("AzKurban-edX-DS/edx.capstone.movielens.data")
  put_end_date(start)
}

movielens_datasets_file <- "movielens-datasets.RData"
movielens_datasets_file_path <- file.path(data_path, movielens_datasets_file)
movielens_datasets_zip <- file.path(data_path, "movielens-datasets.zip")

make_source_datasets <- function(){
  put_log("Function: `make_source_datasets`: Creating source datasets...")

  put_log("Function: `make_source_datasets`: 
Dataset loaded from `edx.capstone.movielens.data` package: edx")
  put(summary(edx))

  put_log("Function: `make_source_datasets`: 
Dataset loaded from `edx.capstone.movielens.data` package: final_holdout_test")
  put(summary(final_holdout_test))
  
  #> To be able to map movie IDs to titles we create the following lookup table:
  movie_map <- edx |> select(movieId, title, genres) |> 
    distinct(movieId, .keep_all = TRUE)
  
  put_log("Function: `make_source_datasets`: Dataset created: movie_map")
  put(summary(movie_map))
  
  put_log("Function: `make_source_datasets`: Creating Date-Days Map dataset...")
  date_days_map <- edx |>
    mutate(date_time = as_datetime(timestamp)) |>
    mutate(date = as_date(date_time)) |>
    mutate(days = as.integer(date - min(date))) |>
    select(timestamp, date_time, date, days) |>
    distinct(timestamp, .keep_all = TRUE)
  
  put_log("Function: `make_source_datasets`: Dataset created: date_days_map")
  put_log(str(date_days_map))
  put(summary(date_days_map))
  

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

    put_log("Function: `make_source_datasets`: 
For each user we are going to process, we will split their ratings
into 80% for training and 20% for validation.")
    
    put_log("Function: `make_source_datasets`: Sampling 20% of the `edx` data...")
    set.seed(fold_i*1000)
    validation_ind <- 
      sapply(splitByUser(edx),
             function(i) sample(i, ceiling(length(i)*.2))) |> 
      unlist() |> 
      sort()
    
    put_log("Function: `make_source_datasets`: 
For training our models, we will ignore the data from users 
who have provided no more than the specified number of ratings. ({min_nratings})")

    put_log("Function: `make_source_datasets`: 
Extracting 80% of the `edx` data not used for the Validation Set, 
excluding data for users who provided no more than a specified number of ratings: {min_nratings}.")
    train_set <- edx[-validation_ind,] |>
      filter_noMore_nratings(min_nratings)

    put_log("Function: `make_source_datasets`: Dataset created: train_set")
    put(summary(train_set))

    put_log("Function: `make_source_datasets`: 
To make sure we don’t include movies in the Training Set that should not be there, 
we remove entries using the semi_join function from the Validation Set.")
    validation_set <- edx[validation_ind,] |> 
      semi_join(train_set, by = "movieId") |> 
      as.data.frame()
    
    put_log("Function: `make_source_datasets`: Dataset created: validation_set")
    put(summary(validation_set))
    
    put_log("Function: `make_source_datasets`: 
To account for the Movie Genre Effect, we need a dataset with split rows 
for movies belonging to multiple genres.")
    edx_split_row_genre <- separateGenreRows(edx)

    put_log("Function: `make_source_datasets`: 
Sampling 20% from the split-row version of the `edx` dataset...")
    set.seed(fold_i*2000)
    validation_gs_ind <- 
      sapply(splitByUser(edx_split_row_genre),
             function(i) sample(i, ceiling(length(i)*.2))) |> 
      unlist() |> 
      sort()
    
    put_log("Function: `make_source_datasets`:
Extracting 80% of the split-row `edx` data not used for the Validation Set, 
excluding data for users who provided no more than a specified number of ratings: {min_nratings}.")
    train_gs_set <- edx_split_row_genre[-validation_gs_ind,] |>
      filter_noMore_nratings(min_nratings)

    put_log("Function: `make_source_datasets`: Dataset created: train_gs_set")
    put(summary(train_gs_set))


    put_log("Function: `make_source_datasets`: 
To make sure we don’t include movies in the Training Set (with split rows) 
that should not be there, we remove entries using the semi_join function 
from the Validation Set.")
    validation_gs_set <- edx_split_row_genre[validation_gs_ind,] 
    validation_gs_set <- validation_gs_set |> 
      semi_join(train_gs_set, by = "movieId") |> 
      as.data.frame()
    
    put_log("Function: `make_source_datasets`: Dataset created: validation_gs_set")
    put(summary(validation_gs_set))

    #> We will use the array representation described in `Section 17.5 of the Textbook`
    #> (https://rafalab.dfci.harvard.edu/dsbook-part-2/linear-models/treatment-effect-models.html#sec-anova), 
    #> for the training data. 
    #> To create this matrix, we use `tidyr::pivot_wider` function:
    
    # train_set <- mutate(train_set, userId = factor(userId), movieId = factor(movieId))
    # train_gs_set <- mutate(train_gs_set, userId = factor(userId), movieId = factor(movieId))
    
    put_log("Function: `make_source_datasets`: Creating Rating Matrix from Train Set...")
    train_mx <- train_set |> 
      mutate(userId = factor(userId),
             movieId = factor(movieId)) |>
      select(movieId, userId, rating) |>
      pivot_wider(names_from = movieId, values_from = rating) |>
      column_to_rownames("userId") |>
      as.matrix()
    
    put_log("Function: `make_source_datasets`: Matrix created: train_mx")
    put(dim(train_mx))

    list(train_set = train_set,
         train_gs_set = train_gs_set,
         train_mx = train_mx, 
         validation_set = validation_set,
         validation_gs_set = validation_gs_set)
  })
  put_end_date(start)
  put_log("Function: `make_source_datasets`: 
Set of K-Fold Cross Validation datasets created: edx_CV")

  list(edx_CV = edx_CV,
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
    dir.create(data_path)
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

# edx <- movielens_datasets$edx
put("Dataset summary: edx")
put(summary(edx))

movielens_datasets <- init_source_datasets()

edx_CV <- movielens_datasets$edx_CV
put("Set of K-Fold Cross Validation datasets summary: edx_CV")
put(summary(edx_CV))

movie_map <- movielens_datasets$movie_map
put("Dataset summary: movie_map")
put(summary(movie_map))

date_days_map <- movielens_datasets$date_days_map
put("Dataset summary: date_days_map")
put(summary(date_days_map))

# final_holdout_test <- movielens_datasets$final_holdout_test
put("Dataset summary: final_holdout_test")
put(summary(final_holdout_test))

rm(movielens_datasets)

# rm(movielens_datasets,
#    edx_CV,
#    movie_map)
#### Close Log ---------------------------------------------------------------
log_close()
## Data Analysis ===============================================================
### Open log -------------------------------------------------------------------
open_logfile(".data-analysis")
### `edx` Dataset --------------------------------------------------------------
put("Data Analysis: Exploring `edx` Dataset...")

# Let's look into the details of the `edx` dataset:
#> First, let's note that we have 10677 different movies: 
n_movies <- n_distinct(edx$movieId)
put_log1("Total amount of movies: %1", n_movies)

# and 69878 different users in the dataset:
n_users <- n_distinct(edx$userId)
put_log1("Total amount of users: %1", n_users)

#> Also, we can see that no movies have a rating of 0. 
#> Movies are rated from 0.5 to 5.0 in 0.5 increments:

#library(dplyr)
s <- edx |> group_by(rating) |>
  summarise(n = n())

put_log("No movies have a rating of 0. 
Movies are rated from 0.5 to 5.0 in 0.5 increments.")
print(s)

#> Now, note the expressions below which confirm the fact explained in 
#> Section 23.1.1 Movielens data
#> (https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#movielens-data) 
#> of the Course Textbook that not every user rated every movie:

dim_edx <- dim(edx)
max_possible_ratings <- n_movies*n_users

put_log1("Maximum possible ratings: %1", max_possible_ratings)

put_log1("Total Rows in `edx` dataset: %1", dim_edx[1])

put_log1("Not every movie was rated: %1", max_possible_ratings > dim_edx[1])

#> We can think of a recommendation system as filling in the `NA`s in the dataset 
#> for the movies that some or all the users do not rate. 
#> A sample from the `edx` data below illustrates this idea: 

keep <- edx |> 
  count(movieId) |> 
  top_n(4, n) |> 
  pull(movieId)

tab <- edx |> 
  filter(movieId %in% keep) |> 
  filter(userId %in% c(13:20)) |> 
  select(userId, title, rating) |> 
  mutate(title = str_remove(title, ", The"),
         title = str_remove(title, ":.*")) |>
  pivot_wider(names_from = "title", values_from = "rating")

print(tab)
put_log("Conclusion: Not every user rated every movie.")

#### Movies' Popularity --------------------------------------------------------
#> Further, we can find out the movies that have the greatest number of ratings 
#> using the following code:

ordered_movie_ratings <- edx |> group_by(movieId, title) |>
  summarize(number_of_ratings = n()) |>
  arrange(desc(number_of_ratings))
print(head(ordered_movie_ratings))
put_log("Movies popularity has been analysed.")

#### Rating Distribution -------------------------------------------------------

#> The following code figure out the most given ratings in order from most to least:
ratings <- edx |>  group_by(rating) |>
  summarise(count = n()) |>
  arrange(desc(count))
print(ratings)

#> The following code allows us to summarize that in general, half-star ratings 
#> are less common than whole-star ratings (e.g., there are fewer ratings of 3.5 
#> than there are ratings of 3 or 4, etc.):
print(edx |> group_by(rating) |> summarize(count = n()))

#> We can visually see that from the following plot:
edx |>
  group_by(rating) |>
  summarize(count = n()) |>
  ggplot(aes(x = rating, y = count)) +
  geom_line() 

#> The code below demonstrates another way of visualizing the rating distribution:
edx |>
  group_by(rating) |>
  summarize(count = n()) |>
  ggplot(aes(x = rating, y = count)) +
  geom_bar(stat = "identity", fill = "#8888ff") +
  ggtitle("Rating Distribution") +
  xlab("Rating") +
  ylab("Occurrences Count") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(n.breaks = 10) +
  theme_economist() +
  theme(axis.title.x = element_text(vjust = -5, face = "bold"), 
        axis.title.y = element_text(vjust = 10, face = "bold"), 
        plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))

put_log("Completed: Rating statistics have been analyzed.")

#### Close Log ---------------------------------------------------------------
log_close()
## Methods =====================================================================
### Overall Mean Rating (Naive) Model --------------------------------------------------
# Reference: the Textbook section "23.3 A first model"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#a-first-model

#>  A model that assumes the same rating for all movies and users with all 
#>  the differences explained by random variation would look as follows:
# Y[i,j] = μ + ε[i,j]

#### Open log -------------------------------------------------------------------
open_logfile(".overall-mean-rating")
### Create an RMSE Result Table and add a first row for the Project Objective ----

# Add the RMSE value of the Naive Model to a tibble.
RMSEs <- tibble(Method = c("Project Objective"),
                RMSE = project_objective)
rmse_kable()
put("RMSE Results Table created.")
#### Compute Naive RMSE -------------------------------------------------------

# ratings_avg <- sapply(edx_CV, function(cv_item){
#   mean(cv_item$train_set$rating, na.rm = TRUE)
# })
# 
# plot(ratings_avg)
# put_log1("Overall Mean Rating Model:
# Mean Ratings plotted for %1-Fold Cross Validation samples.", CVFolds_N)
# 
# mu <- mean(ratings_avg)
mu <- mean(edx$rating, na.rm = TRUE)
put_log1("Overall Mean Rating Model:
The Overall Mean Rating is: %1", mu)
#> The Overall Mean Rating is: 3.51246520160155
rmses <- sapply(edx_CV, function(cv_item){
  sqrt(mse(cv_item$validation_set$rating - mu))
})
plot(rmses)
put_log1("Overall Mean Rating Model:
RMSE values plotted for %1-Fold Cross Validation samples.", CVFolds_N)

naive_rmse <- mean(rmses)
put_log2("Overall Mean Rating Model:
%1-Fold Cross Validation ultimate RMSE: %2", CVFolds_N, naive_rmse)
#> 5-Fold Cross Validation ultimate RMSE: 1.06186141545291

#### Ensure that this is the best RMSE value for the current model ----------------
#> If we plug in any other number, we will get a higher RMSE. 
#> Let's prove that by the following small investigation:

deviation <- seq(0, 6, 0.1) - 3
deviation

rmse_test_results <- lapply(kfold_index, function(i){
  cv_item <- edx_CV[[i]]
  rmse_val <-rmses[i] 
  rmse_values <- sapply(deviation, function(diff){
    sqrt(mse(cv_item$validation_set$rating - (mu + diff)))
  })
  print(rmse_values)
  put_log2("Overall Mean Rating Model:
RMSE values have been computed for the %1-Fold Cross-validation, %2 iteration result for the deviations from the Overall Mean Rating.",
           CVFolds_N, make_ordinal_no(i))
  
  rmse_plot <- data.frame(deviation = deviation, 
                    rmse_values = rmse_values) |> 
    ggplot(aes(deviation, rmse_values)) +
    geom_line()
  put_log2("Overall Mean Rating Model:
A plot was constructed for the %1-Fold Cross-validation, %2 iteration result for the deviations from the Overall Mean Rating.",
           CVFolds_N, make_ordinal_no(i))
  rmse_plot
})

n <- length(rmse_test_results)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(rmse_test_results, ncol = nCol))
put_log1("Overall Mean Rating Model:
%1-Fold Cross-validation results for the deviations from the Overall Mean Rating have been plotted.",
         CVFolds_N)

for (i in kfold_index) {
  dvs <- rmse_test_results[[i]]$data$deviation
  test_vals <- rmse_test_results[[i]]$data$rmse_values
  rmse_val <-rmses[i] 
  
  which_min_deviation <- dvs[which.min(test_vals)]
  min_rmse = min(test_vals)
  
  put_log2("For Validation Set %1:
Minimum MSE is achieved when the deviation from the mean is: %2",
           i, which_min_deviation)

  put_log1("Is the previously computed RMSE the best for the current model: %1",
           rmse_val == min_rmse)
  #> [1] "Is the previously computed RMSE the best for the current model: TRUE"
  writeLines("")
}

#### Add a row to the RMSE Result Table for the Overall Mean Rating Model ---------------- 
RMSEs <- rmses_add_row("Overall Mean Rating Model", naive_rmse)
rmse_kable()
put_log("Overall Mean Rating Model:
A row has been added to the RMSE Result Table for the `Simple Mean Rating Model`.")
#### Close Log ---------------------------------------------------------------
log_close()

### User Effect Model ---------------------------------------------------------- 
# Reference: the Textbook section "23.4 User effects"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#user-effects

#### Open log -------------------------------------------------------------------
open_logfile("user-effect")
put("Building User Effect Model...")
#### Model building: User Effect ----------------------------------------------
# Let's visualize the average rating for each user:
put_log("Computing Average Ratings per User (User Mean Ratings)...")
start <- put_start_date()
user_ratings_avg_ls <- lapply(edx_CV, function(cv_item){
  # print(dim(cv_item$train_mx))
  #str(cv_item$train_mx)
  user_ratings_avg <- rowMeans(cv_item$train_mx, na.rm = TRUE)
  n_ratings <- rowSums(!is.na(cv_item$train_mx))
  # print(sum(is.na(user_ratings_avg))) # 0 (there are no NAs in there)
  # print(str(user_ratings_avg))
  
  data.frame(userId = names(user_ratings_avg), 
             ratings_avg = user_ratings_avg,
             n = n_ratings)
})
put_end_date(start)
put_log1("User Average Rating list has been computed for %1-Fold Cross Validation samples.", 
         CVFolds_N)

put(str(user_ratings_avg_ls))

user_ratings_avg_united <- union_cv_results(user_ratings_avg_ls)
put(str(user_ratings_avg_united))
#sum(is.na(user_ratings_avg_united))

user_mean_ratings <- user_ratings_avg_united |>
  group_by(userId) |>
  summarise(mean_rating = mean(ratings_avg), n = mean(n))

put_log("The Average Ratings per User (User Mean Ratings) have been computed.")
put(str(user_mean_ratings))
# sum(is.na(user_mean_ratings$mean_rating))
#> [1] 0 (there are no NAs in there)

hist(user_mean_ratings$mean_rating, nclass = 30)
put_log("A histogram of the User Mean Rating distribution has been plotted.")

# we notice that there is substantial variability across users.
#>  To account for this, we can use a linear model with a treatment effect `α[i]` 
#>  for each user. The sum `μ + α[i]` can be interpreted as the typical 
#>  rating user `i` gives to movies. We can write the model as:

# Y[i,j] = μ + α[i] + ε[i,j]

#> It can be shown that the least squares estimate `α[i]` is just the average 
#> of `y[i,j] - μ` for each user. So we can compute them this way:

put_log("User Effect Model:
Computing User Effect per users ...")
user_effects <- user_mean_ratings |> 
  mutate(a = mean_rating - mu,
         userId = as.integer(userId)) |>
  select(userId, a)

put_log("A User Effect Model has been builded and trained")
put(str(user_effects))

# Plot a histogram of the user effects -----------------------------------------
par(cex = 0.7)
hist(user_effects$a, 30, xlab = TeX(r'[$\hat{alpha}_{i}$]'),
     main = TeX(r'[Histogram of $\hat{alpha}_{i}$]'))
put_log("User Effect Model:
A histogram of the User Effect distribution has been plotted.")


#> Finally, we are ready to compute the `RMSE` (additionally using the helper 
#> function `clamp` we defined above to keep predictions in the proper range):

# Computing the RMSE taking into account user effects --------------------------
put_log("Computing the RMSE taking into account user effects...")
#start <- put_start_date()
user_effect_rmses <- sapply(edx_CV, function(cv_item){
  # a <- cv_item$validation_set |>
  #   left_join(user_effects, by = "userId") |>
  #   pull(a)
  # 
  # print(sum(is.na(a)))
  # print(sum(!is.na(a)))
  
  cv_item$validation_set |>
    left_join(user_effects, by = "userId") |>
    mutate(resid = rating - clamp(mu + a)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> rmse()
})
#put_end_date(start)

plot(user_effect_rmses)
put_log1("RMSE values have been plotted for the %1-Fold Cross Validation samples.", 
         CVFolds_N)

user_effect_rmse <- mean(user_effect_rmses)
put_log2("%1-Fold Cross Validation ultimate RMSE: %2", 
         CVFolds_N, 
         user_effect_rmse)
user_effect_rmse
#> [1] 0.9689451

# Add a row to the RMSE Result Table for the User Effect Model ---------------- 
RMSEs <- rmses_add_row("User Effect Model", user_effect_rmse)
rmse_kable()
#### Close Log -----------------------------------------------------------------
log_close()
### Taking into account User+Movie Effects ------------------------------------------

# Reference: the Textbook section "23.5 Movie effects"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#movie-effects

#> We know from experience that some movies are just generally rated higher 
#> than others. We can use a linear model with a treatment effect `β[j]` 
#> for each movie, which can be interpreted as movie effect or the difference 
#> between the average ranking for movie `j` and the overall average `μ`:

# Y[i,j] = μ + α[i] + β[j] + ε[i,j]

#> We use an approximation by first computing the least square estimate `μ` and
#> α[i], and then estimating β[j] as the average of the residuals 
#> `y[i,j] - μ - α[i]`:

#### Open log -------------------------------------------------------------------
open_logfile("user+movie-effect")

#### Model building: User+Movie Effects -----------------------------------------
put_log("Computing User+Movie Effect...")
start <- put_start_date()
user_movie_effects_ls <- lapply(edx_CV, function(cv_item){
  cv_item$train_set |>
    left_join(user_effects, by = "userId") |>
    filter(!is.na(rating)) |>
    mutate(ume = rating - mu - ifelse(!is.na(a), a, 0)) |> 
    group_by(movieId) |>
    summarise(b_cv = mean(ume), n = n())
})
put_end_date(start)
put(str(user_movie_effects_ls))

user_movie_effects_united <- union_cv_results(user_movie_effects_ls)
put(str(user_movie_effects_united))
# sum(is.na(user_movie_effects_united$b_cv)) # 0 (there are no NAs in there)

user_movie_effects <- user_movie_effects_united |>
  group_by(movieId) |>
  summarise(b = mean(b_cv), n = mean(n))

put_log("User+Movie Effects have been computed")
put(str(user_movie_effects))
# sum(is.na(user_movie_effects$b)) # 0 (there are no NAs in there)

#user_movie_effects <- data.frame(movieId = as.integer(names(b)), b = b)
put_log("Completed building the User+Movie Effects model.")

#### Plot a histogram of the User+Movie Effects -----------------------------------
par(cex = 0.7)
hist(user_movie_effects$b, 30, xlab = TeX(r'[$\hat{beta}_{j}$]'),
     main = TeX(r'[Histogram of $\hat{beta}_{j}$]'))
put_log("A histogram of the User+Movie Effects distribution has been plotted.")

#### Calculate RMSEs on Validation Sets --------------------------------------------
put_log("Computing the RMSE taking into account User+Movie Effects...")
start <- put_start_date()
user_movie_effects_rmses <- sapply(edx_CV, function(cv_item){
  cv_item$validation_set |>
    left_join(user_effects, by = "userId") |>
    left_join(user_movie_effects, by = "movieId") |>
    mutate(resid = rating - clamp(mu + a + b)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> rmse()
})
put_end_date(start)

plot(user_movie_effects_rmses)
put_log1("RMSE values have been plotted for the %1-Fold Cross Validation samples.", 
         CVFolds_N)

user_movie_effects_rmse <- mean(user_movie_effects_rmses)
put_log2("%1-Fold Cross Validation ultimate RMSE: %2", CVFolds_N, user_movie_effects_rmse)
user_movie_effects_rmse
#> [1] 0.8594761

#### Add a row to the RMSE Result Table for the User+Movie Effect Model ---------- 
RMSEs <- rmses_add_row("User+Movie Effect Model", 
                       user_movie_effects_rmse)
rmse_kable()

#### Close Log -----------------------------------------------------------------
log_close()

### Accounting for Movie Genres ------------------------------------------------
#> We can slightly improve our naive model by accounting for movie genres.
#> Let's do some preliminary analysis first.

#### Open log -------------------------------------------------------------------
open_logfile("user+movie+genre-effect")

#### Data Analysis and Visualization -------------------------------------------

# Reference: the Textbook Section "23.7 Exercises" of the Chapter "23 Regularization"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#exercises

#> The `edx` dataset also has a genres column. This column includes 
#> every genre that applies to the movie 
#> (some movies fall under several genres)[@IDS2_23-7].

##### Average rating per genre -----------------------------------------------------

# Preparing data for plotting:
put_log1("Computing Genre Summary list for %1-Fold Cross Validation samples...", 
        CVFolds_N)

genres_summary_list <- lapply(edx_CV, function(cv_item){
  cv_item$train_set |> 
    mutate(genre_categories = as.factor(genres)) |>
    group_by(genre_categories) |>
    summarize(n = n(), rating_avg = mean(rating), se = sd(rating)/sqrt(n())) |>
    filter(n > min_nratings) |>
    mutate(genres = reorder(genre_categories, rating_avg)) |>
    select(genres, rating_avg, se, n)
})
put_log1("Genre Summary list has been computed for %1-Fold Cross Validation samples.", 
         CVFolds_N)

str(genres_summary_list)

# genres_summary_list_nas <- lapply(genres_summary_list, function(item){
#   c(sum(is.na(item$genres)), sum(is.na(item$rating_avg)), sum(is.na(item$se)), sum(is.na(item$n)))
# })
# genres_summary_list_nas

put_log1("Computing Average Rating per Genre list for %1-Fold Cross Validation samples...", 
         CVFolds_N)

genre_ratings_united <- union_cv_results(genres_summary_list)
str(genre_ratings_united)

# genre_ratings_united |>
#   summarise(na_test = c(sum(is.na(genres)), 
#                         sum(is.na(rating_avg)),
#                         sum(is.na(se)),
#                         sum(is.na(n))
#                         )) |>
#   pull(na_test)

genre_mean_ratings <- genre_ratings_united |>
  group_by(genres) |>
  summarise(ratings = mean(rating_avg),
            se = mean(se),
            n = mean(n)) |>
  mutate(genres = reorder(genres, ratings)) |>
  sort_by.data.frame(~ratings)

put_log1("Mean Rating per Genre list has been computed for %1-Fold Cross Validation samples.",
         CVFolds_N)

put(str(genre_mean_ratings))
#print(head(genre_mean_ratings))

put(sprintf("The worst rating is for the genre category: %s (average rating is %s)",
            genre_mean_ratings$genres[which.min(genre_mean_ratings$ratings)],
            as.character(clamp(min(genre_mean_ratings$ratings)))))

put(sprintf("The best rating is for the genre category: %s (average rating is %s)",
            genre_mean_ratings$genres[which.max(genre_mean_ratings$ratings)],
            as.character(clamp(max(genre_mean_ratings$ratings)))))

##### Genres Popularity ------------------------------------------------------------

put(sprintf("The worst popularity was for the genre category: %s (%s ratings)",
            genre_mean_ratings$genres[which.min(genre_mean_ratings$n)],
            as.character(min(genre_mean_ratings$n))))

put(sprintf("The best popularity was for the genre category: %s (%s ratings)",
            genre_mean_ratings$genres[which.max(genre_mean_ratings$n)],
            as.character(max(genre_mean_ratings$n))))

##### Genres Info Visualization ------------------------------------------------
#> For illustrative purposes, we will limit the genre information 
#> we are going to plot to movies with more than 24,000 ratings:
nratings <- 24000

###### Plot Genre Info --------------------------------------------------------------  
genre_ratings_plot_dat <- genre_mean_ratings |>
  filter(n > nratings)

dim(genre_ratings_plot_dat)
str(genre_ratings_plot_dat)
# head(genre_ratings_plot_dat)
# genre_ratings_plot_dat

# Creating plot:
genre_ratings_plot_dat |> 
  ggplot(aes(x = genres, 
             y = ratings, 
             ymin = ratings - 2*se, 
             ymax = ratings + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  ggtitle("Average rating per Genre") +
  ylab("Average rating") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

put_log1("Mean Rating per Genre list distribution filtered by ratings amount greater than %1
has been plotted.",
         nratings)

sprintf("The worst ratings were for the genre category: %s",
        genre_ratings_plot_dat$genres[which.min(genre_ratings_plot_dat$ratings)])

sprintf("The best ratings were for the genre category: %s",
        genre_ratings_plot_dat$genres[which.max(genre_ratings_plot_dat$ratings)])

####### Alternative way of visualizing a Genre Effect ----------------------------
#> Reference: Article "Movie Recommendation System using R - BEST" written by 
#> Amir Moterfaker (https://www.kaggle.com/amirmotefaker)
#> (section "Average rating for each genre")[@MRS-R-BEST]
#> https://www.kaggle.com/code/amirmotefaker/movie-recommendation-system-using-r-best/notebook#Average-rating-for-each-genre

# For better visibility, we reduce the data for plotting 
# while keeping the worst and best rating rows:
plot_ind <- odd(1:nrow(genre_ratings_plot_dat))
plot_dat <- genre_ratings_plot_dat[plot_ind,] 

plot_dat |>
  ggplot(aes(x = ratings, y = genres)) +
  ggtitle("Genre Average Rating") +
  geom_bar(stat = "identity", width = 0.6, fill = "#8888ff") +
  xlab("Average ratings") +
  ylab("Genres") +
  scale_x_continuous(labels = comma, limits = c(0.0, 5.0)) +
  theme_economist() +
  theme(plot.title = element_text(vjust = 3.5),
        axis.title.x = element_text(vjust = -5, face = "bold"),
        axis.title.y = element_text(vjust = 10, face = "bold"),
        axis.text.x = element_text(vjust = 1, hjust = 1, angle = 0),
        axis.text.y = element_text(vjust = 0.25, hjust = 1, size = 9),
        plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))

put_log1("Mean Rating per Genre list distribution filtered by ratings amount greater than %1
has been plotted alternative way.",
         nratings)

##### Genre Separated Data Analysis ------------------------------------------------

#### Including Genre effect -----------------------------------------------------
# Y[i,j] = μ + α[i] + β[j] + g[i,j]  + ε[i,j]
# where g[i,j] is a combination of genres for movie `i` rated by user `j`,
# so that g[i,j] = ∑{k=1,K}(x[i,j]^k*𝜸[k]) 
# with `x[i,j]^k = 1` if g[i,j] includes genre `k`, and `x[i,j]^k = 0` otherwise.

# mutate(userId = as.integer(userId),
#        movieId = as.integer(movieId)) |>

put_log1("Computing Genre Bias list for %1-Fold Cross Validation samples...", 
         CVFolds_N)

start <- put_start_date()
user_movie_genre_effects_ls <- lapply(edx_CV, function(cv_item){
  genre_bias <- cv_item$train_gs_set |>
    left_join(user_effects, by = "userId") |>
    left_join(user_movie_effects, by = "movieId") |>
    group_by(genres) |>
    summarise(g = mean(rating - (mu + a + b), na.rm = TRUE), n = n()) |>
    filter(n > min_nratings)
  
  # print(c(g_NAs = sum(is.na(genre_bias$g))))
  
  mg_bias <- cv_item$train_gs_set |>
    left_join(genre_bias, by = "genres") |>
    left_join(user_movie_effects, by = "movieId") |>
    filter(!is.na(b)) |>
    group_by(movieId) |>
    summarise(b = mean(b, na.rm = TRUE), g = mean(g, na.rm = TRUE))
  
  #print(c(g_NAs = sum(is.na(mg_bias$g)), b_NAs = sum(is.na(mg_bias$b))))
  mg_bias
})
put_end_date(start)

put_log1("Genre Bias list has been computed for %1-Fold Cross Validation samples.", 
         CVFolds_N)

put(str(user_movie_genre_effects_ls))
#head(user_movie_genre_effects_ls)

user_movie_genre_effects_united <- union_cv_results(user_movie_genre_effects_ls)
put(str(user_movie_genre_effects_united))
# sum(user_movie_genre_effects_united$g != 0)
# sum(is.na(user_movie_genre_effects_united$b))
# sum(is.na(user_movie_genre_effects_united$g))

# edx_cv_item <- edx_CV[[1]]$train_set

##### Compute Genre Movie Bias -----------------------------------------------------

user_movie_genre_effects <- user_movie_genre_effects_united |>
  group_by(movieId) |>
  summarise(b = mean(b), g = mean(g))

put_log("User+Movie+Genre Effects have been computed.")
str(user_movie_genre_effects)
# sum(is.na(user_movie_genre_effects$b))
#> [1] 0
# sum(is.na(user_movie_genre_effects$g))
#> [1] 0

##### Finalize User+Movie+Genre Effects ---------------------------------------------

# sum(is.na(user_movie_genre_effects$g))
# #> [1] 0
# sum(is.na(user_movie_genre_effects$g))
#> [1] 0

###### Plot a histogram of the Movie Effect distribution ----------------------------
par(cex = 0.7)
hist(user_movie_genre_effects$b, 30, xlab = TeX(r'[$\hat{beta}_{j}$]'),
     main = TeX(r'[Histogram of $\hat{beta}_{j}$]'))

put_log("A histogram of the Movie Effect (adjusted for Genre Bias) distribution has been plotted.")

###### Plot a histogram of the Genre Effect distribution ----------------------------
#par(cex = 0.7)
hist(user_movie_genre_effects$g, 30, xlab = TeX(r'[$\hat{g}_{i,j}$]'),
     main = TeX(r'[Histogram of $\hat{g}_{j}$]'))

put_log("A histogram of the Genre Effect distribution has been plotted.")

###### Compute RMSE: user+movie+genre effects ------------------------------------

put_log("Computing RMSEs on Validation Sets...")
start <- put_start_date()
user_movie_genre_effects_rmses <- sapply(edx_CV, function(cv_item){
  cv_item$validation_set |>
    left_join(user_effects, by = "userId") |>
    left_join(user_movie_genre_effects, by = "movieId") |>
    mutate(resid = rating - clamp(mu + a + b + g)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> rmse()
})
put_end_date(start)

plot(user_movie_genre_effects_rmses)
put_log1("RMSE values have been plotted for the %1-Fold Cross Validation samples.", 
         CVFolds_N)

user_movie_genre_effects_rmse <- mean(user_movie_genre_effects_rmses)
put_log2("%1-Fold Cross Validation ultimate RMSE: %2", 
         CVFolds_N, 
         user_movie_genre_effects_rmse)

user_movie_genre_effects_rmse
#> [1] 0.8594752

#### Add a row to the RMSE Result Table for the User+Movie+Genre Effect Model ---- 
RMSEs <- rmses_add_row("User+Movie+Genre Effect Model", 
                       user_movie_genre_effects_rmse)
rmse_kable()

# final_holdout_test |>
#   left_join(user_effects, by = "userId") |>
#   left_join(user_movie_genre_effects, by = "movieId") |>
#   mutate(resid = rating - clamp(mu + a + b + g)) |> 
#   filter(!is.na(resid)) |>
#   pull(resid) |> final_rmse()
#> [1] 0.8659243

#### Close Log -----------------------------------------------------------------
log_close()

### Accounting for Date Smoothed Effect ------------------------------------------
# Y[i,j] = μ + α[i] + β[j] + g[i,j]  + f(d[i,j]) + ε[i,j]

# with `f` a smooth function of `d[(i,j]`

# library(lubridate)
#### Open log -------------------------------------------------------------------
open_logfile("user+movie+genre+date-effect")

# Let's take a look at the Average rating per year:
#### Plot: Average Rating per Year ------------------------------------------------

start <- put_start_date()
put("Plotting Average Rating per Year distribution...")
put_log("Ignoring the data from users 
who have provided no more than the specified number of ratings. ({min_nratings})")

edx |>
  filter_noMore_nratings(min_nratings) |>
  mutate(year = year(as_datetime(timestamp))) |>
  group_by(year) |>
  summarize(avg = mean(rating)) |>
  ggplot(aes(x = year, y = avg)) +
  geom_bar(stat = "identity", fill = "#8888ff") + 
  ggtitle("Average rating per year") +
  xlab("Year") +
  ylab("Average rating") +
  scale_y_continuous(labels = comma) + 
  theme_economist() +
  theme(axis.title.x = element_text(vjust = -5, face = "bold"), 
        axis.title.y = element_text(vjust = 10, face = "bold"), 
        plot.margin = margin(0.7, 0.5, 1, 1.2, "cm"))

put_end_date(start)
put("Average Rating per Year distribution has been plotted.")

#### Calculate Date Smoothed Effect -----------------------------------------------
#train_set1 <- edx_CV[[1]]$train_set

put_log1("Computing Global Date Bias list for %1-Fold Cross Validation samples...", 
         CVFolds_N)

start <- put_start_date()
global_date_effects <- sapply(kfold_index,  function(fold_i){
  put(sprintf("Creating K-Fold CV Global Date Effects Set, Vector %s", fold_i))

  # start <- put_start_date()
  de_global <- edx_CV[[fold_i]]$train_set |> 
    left_join(user_effects, by = "userId") |>
    left_join(user_movie_genre_effects, by = "movieId") |>
    left_join(date_days_map, by = "timestamp") |>
    mutate(rating_residue = rating - (mu + a + b + g)) |>
    filter(!is.na(rating_residue)) |>
    group_by(days) |>
    summarise(de = mean(rating_residue, na.rm = TRUE))
  # put_end_date(start)
  
  # put("Global Date Effect Vector Structure:")
  # put(str(de_global))
  # put("Global Date Effect Vector Structure:")
  names(de_global$de) = de_global$days
  # put(str(de_global$de))
  de_global$de
})
put_end_date(start)
put_log1("Global Date Bias list has been computed for %1-Fold Cross Validation samples.", 
         CVFolds_N)

print(str(global_date_effects))

de_vector <- global_date_effects |> unlist()
str(de_vector)

date_global_effect <- 
  data.frame(days = as.integer(names(de_vector)),
             de = de_vector) |>
  group_by(days) |>
  summarise(de = mean(de, na.rm = TRUE)) |>
  sort_by.data.frame(~days)

put_log("Global Date Effect has been computed.")
print(str(date_global_effect))
# sum(is.na(date_global_effect$de))
#> [1] 0

##### Train model using `loess` function with default `span` & `degree` params----

put_log("Training model using `loess` function with default `span` & `degree` params...")
# start <- put_start_date()
fit <- loess(de ~ days, data = date_global_effect)
# put_end_date(start)
put_log("Model was trained using `loess` function with default `span` & `degree` params.")
str(fit$fitted)
# sum(is.na(fit$fitted))
#> [1] 0
put(str(fit$pars))

date_smoothed_effect <- date_global_effect |>
  mutate(de_smoothed = fit$fitted)

put_log("Date Smoothed Effect has been computed.")
put(str(date_smoothed_effect))

# Date Smoothed Effect Visualization:
start <- put_start_date()
date_smoothed_effect |>
  ggplot(aes(x = days)) +
  geom_point(aes(y = de), size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(y = de_smoothed), color = "red")
put_end_date(start)
put_log("Date Smoothed Effect has been plotted.")

# Function for RMSEs calculation of Smoothed Date Model ----------------------

date_smoothed_RMSEs <- function(date_smoothed_effect){
  put_log1("Computing RMSE values for the %1-Fold Cross Validation samples...", 
           CVFolds_N)
  
  start <- put_start_date()
  RMSEs <- sapply(kfold_index, function(fold_i){
    rmse_i <- edx_CV[[fold_i]]$validation_set |>
      left_join(user_effects, by = "userId") |>
      left_join(user_movie_genre_effects, by = "movieId") |>
      left_join(date_days_map, by = "timestamp") |>
      left_join(date_smoothed_effect, by='days') |>
      mutate(resid = rating - clamp(mu + a + b + g + de_smoothed)) |> 
      filter(!is.na(resid)) |>
      pull(resid) |> rmse()
    # put_end_date(start)
    rmse_i
  })
  put_end_date(start)
  put_log1("RMSE values have been computed for the %1-Fold Cross Validation samples.", 
           CVFolds_N)
  RMSEs
}
# Calculate RMSE for loess fitted with default parameters ----------------------

user_movie_genre_date_effects_RMSEs <- date_smoothed_RMSEs(date_smoothed_effect)
plot(user_movie_genre_date_effects_RMSEs)
put_log1("RMSE values have been plotted for the %1-Fold Cross Validation samples.", 
         CVFolds_N)

user_movie_genre_date_effects_rmse <- mean(user_movie_genre_date_effects_RMSEs)
print(user_movie_genre_date_effects_rmse)
#> [1] 0.8589684

#### Re-train tuning `loess` function's with `span` & `degree` params-----------
##### Support Functions ------------------------------------------------------------
fit_loess <- function(spans, dgr){
  n_spans <- length(spans)
  put_log2("Fitting model for degree = %1 (%2 spans)...", dgr, n_spans)
  start <- put_start_date()
  fits <- lapply(spans, function(span){
    fit <- loess(de ~ days, span = span, degree = dgr, data = date_global_effect)
  })
  put_end_date(start = start)
  put_log2("Model fitted for degree = %1 (%2 spans).", dgr, n_spans)
  fits
}
tune_de_model_rmses <- function(fits){
  model_diu_rmses <- sapply(fits, function(fit){
    put_log2("Tuning Model for degree = %1, span = %2...", 
             fit$pars$degree,
             fit$pars$span)
    
    # print(str(fit$fitted))
    date_smoothed_effect <- date_global_effect |>
      mutate(de_smoothed = fit$fitted)

    rmses <- date_smoothed_RMSEs(date_smoothed_effect)
    rmse <- mean(rmses)
    put_log2("RMSE has been computed for degree = %1, span = %2.", 
             fit$pars$degree,
             fit$pars$span)
    rmse
  })
}
date_smoothed_tuning_RMSEs <- function(spans, dgr) {
  n_spans <- length(spans)
  # dgr <- degree[1] # debug
  # put_log2("Fitting the model for degree = %1 (%2 spans)...", dgr, n_spans)
  # start <- put_start_date()
  fits <- fit_loess(spans,dgr)
  # put_end_date(start)
  # put_log2("Model has been fitted for degree = %1 (%2 spans).", dgr, n_spans)
  dim(fits)

  start <- put_start_date()
  put(sprintf("Tuning the Smothed Date Effect Model for Degree = %s (%s spans)", 
              dgr, n_spans))
  model_diu_rmses <- tune_de_model_rmses(fits)
  put(sprintf("RMSEs computed for the Smothed Date Effect Model (Degree: %s)", dgr))
  put_end_date(start)

  data.frame(span = spans, rmse = model_diu_rmses)
}
best_rmse <- function(span_rmses){
  idx <- which.min(span_rmses$rmse)
  rmse <- c(span_rmses$span[idx], min(span_rmses$rmse))
  names(rmse) <- c("Span", "RMSE")
  rmse
}

##### Tune the Global Date Smoothed Effect model -------------------------------
degree <- c(0, 1, 2)
put_log("Tuning `loess` function for degrees:")
put(degree)

###### 1. `degree = 0` ---------------------------------------------------------
put("Case 1. `degree = 0`")
# spans <- seq(0.0003, 0.002, 0.00001)
# spans <- seq(0.00100, 0.00102, 0.00001)
spans <- seq(0.0005, 0.0015, 0.00001)
put_log2("Tuning for spans %1:%2...", "0.0005", max(spans))
# spans

ds0_rmses <- date_smoothed_tuning_RMSEs(spans, 
                               degree[1])
put("Case 1. `degree = 0` RMSEs:")
str(ds0_rmses)
plot(ds0_rmses)
put_log1("RMSE values have been plotted for the %1-Fold Cross Validation samples.", 
         CVFolds_N)

ds_rmse0 <- best_rmse(ds0_rmses)
put(ds_rmse0)
#      Span      RMSE 
# 0.0008700 0.8573265  

###### 2. `degree = 1` --------------------------------------------------------------
put("Case 2. `degree = 1`")
#spans <- seq(0.0005, 0.002, 0.00001)
spans <- seq(0.001, 0.0014, 0.00001)
put_log2("Tuning for spans %1:%2...", min(spans), max(spans))

ds1_rmses <- date_smoothed_tuning_RMSEs(spans, 
                                 degree[2])
put("Case 2. `degree = 1` RMSEs:")
str(ds1_rmses)
plot(ds1_rmses)
put_log1("RMSE values have been plotted for the %1-Fold Cross Validation samples.", 
         CVFolds_N)

ds_rmse1 <- best_rmse(ds1_rmses)
put(ds_rmse1)
#      Span      RMSE 
# 0.0010000 0.8568608 
###### 3. `degree = 2` --------------------------------------------------------------
put("Case 3. `degree = 2`")
#spans <- seq(0.0003, 0.01, 0.00001)
spans <- seq(0.0007, 0.002, 0.00001)
put("Tuning for spans:")
put(spans)

ds2_rmses <- date_smoothed_tuning_RMSEs(spans, 
                                 degree[3])
put("Case 3. `degree = 2` RMSEs:")
put(str(ds2_rmses))
plot(ds2_rmses)
put_log1("RMSE values have been plotted for the %1-Fold Cross Validation samples.", 
         CVFolds_N)

ds_rmse2 <- best_rmse(ds2_rmses)
put(ds_rmse2)
#      Span      RMSE 
# 0.0013100 0.8571513

#### Retrain with the best parameters figured out above ---------------------------

loess_rmse <- data.frame(degree = degree, 
                         span = c(ds_rmse0[1], ds_rmse1[1], ds_rmse2[1]),
                         rmse = c(ds_rmse0[2], ds_rmse1[2], ds_rmse2[2]))
put(loess_rmse)

idx_best_rmse <- which.min(loess_rmse$rmse)

best_degree <- loess_rmse[idx_best_rmse, 1]  # 1
best_span <- loess_rmse[idx_best_rmse, 2]# 0.00108
best_rmse <- loess_rmse[idx_best_rmse, 3]
put(best_rmse)
#> [1] 0.8568608

put_log2("Re-training model using `loess` function with the best parameters: 
span = %1, degree = %2", best_span, best_degree)
start <- put_start_date()
fit <- loess(de ~ days, 
             degree = best_degree, 
             span = best_span, 
             data = date_global_effect)
put_end_date(start)
put_log("The model has been re-trained with the best parameters.") 

put("Best Fit:")
put(sprintf("Contains NAs: %s", sum(is.na(fit$fitted))))
str(fit$pars)
str(fit$fitted)
put(fit)

date_smoothed_effect <- as.data.frame(date_global_effect) |>
  mutate(de_smoothed = fit$fitted)
str(date_smoothed_effect)
head(date_smoothed_effect)

start <- put_start_date()
date_smoothed_effect |>
  ggplot(aes(x = days)) +
  geom_point(aes(y = de), size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(y = de_smoothed), color = "red")
put_end_date(start)
put_log("Optimized Date Smoothed Effect has been plotted.")

user_movie_genre_tuned_date_effect_RMSEs <- date_smoothed_RMSEs(date_smoothed_effect)
plot(user_movie_genre_tuned_date_effect_RMSEs)
user_movie_genre_tuned_date_effect_rmse <- mean(user_movie_genre_tuned_date_effect_RMSEs)
put(user_movie_genre_tuned_date_effect_rmse)
#> [1] 0.8568608

##### Add a row to the RMSE Result Table for the User+Movie+Genre+Date Effects Model ---- 
RMSEs <- rmses_add_row("User+Movie+Genre+Date Effects Model (tuned)", 
                       user_movie_genre_tuned_date_effect_rmse)
rmse_kable()

# start <- put_start_date()
# final_holdout_test |>
#   left_join(date_days_map, by = "timestamp") |>
#   left_join(user_effects, by = "userId") |>
#   left_join(user_movie_genre_effects, by = "movieId") |>
#   left_join(date_smoothed_effect, by='days') |>
#   mutate(resid = rating - clamp(mu + a + b + g + de_smoothed)) |>
#   filter(!is.na(resid)) |>
#   pull(resid) |> final_rmse()
# put_end_date(start)
#> [1] 0.8724055

#### Close Log -----------------------------------------------------------------
log_close()

### Utilizing Penalized least squares-------------------------------------------

# Reference: the Textbook section "23.6 Penalized least squares"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#penalized-least-squares

#> Instead of minimizing the least squares equation, 
#> we minimize an equation that adds a penalty:

#  ∑{i,j}(y[i,j] - y_hat[i,j])^2 + λ*∑{j}β[j]^2
# where y_hat[i,j] = μ + α[i] + β[j] + g[i,j] + f(d[i,j])

#> The values of `β[j]` that minimize this equation are:

# β[j](λ) = 1/(λ + n[j])*∑{u=1,n[i]}(Y[i,j] - μ - α[i])
# where `n[j]` is the number of ratings made for movie `j`.

#### Support function ----------------------------------------------------------

#> We will use the following function to calculate _RMSE_ in this section:
reg_rmse <- function(b){
  test_set |> 
    left_join(data.frame(userId = as.integer(names(a)), a = a), by = "userId") |>
    left_join(data.frame(movieId = as.integer(names(b)), b = b), by = "movieId") |>
    mutate(resid = rating - clamp(mu + a + b)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> rmse()
}

### Model building -------------------------------------------------------------

#> Here we will simply compute the RMSE we for different values of `λ`: 
n <- colSums(!is.na(y))

sums <- colSums(y - mu - a, na.rm = TRUE)
lambdas <- seq(0, 10, 0.1)

rmses <- sapply(lambdas, function(lambda){
  b <-  sums / (n + lambda)
  reg_rmse(b)
})

# Here is a plot of the RMSE versus `lambda`:
plot(lambdas, rmses, type = "l")

#> Now we can determine the minimal _RMSE_:
put(min(rmses))
#> [1] 0.8659219

#> which is achieved for the following `λ`:
lambda <- lambdas[which.min(rmses)] 
put(lambda)
#> [1] 2.6

#> Using minimal `λ`, we can compute the regularized estimates:
b_reg <- sums / (n + lambda)

#> Finally, let's verify that the penalized estimates 
#> we have just computed actually result in the minimal `RMSE` figured out above: 
reg_rmse(b_reg)
#> [1] 0.8659219


