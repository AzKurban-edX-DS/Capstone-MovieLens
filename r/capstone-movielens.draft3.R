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

### Data processing functions -------------------------------------------------
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

data_path <- "data"
CVFolds_N <- 5
kfold_index <- seq(from = 1:CVFolds_N)

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
  edx_CV <- lapply(kfold_index,  function(fi){

    put_log1("Method `make_source_datasets`: 
Creating K-Fold Cross Validation Datasets, Fold %1", fi)
    
    #> We split the initial datasets into training sets, which we will use to build 
    #> and train our models, and validation sets in which we will compute the accuracy 
    #> of our predictions, the way described in the `Section 23.1.1 Movielens data`
    #> (https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#movielens-data) 
    #> of the Course Textbook.

    put_log("Function: `make_source_datasets`: 
For each user we are going to process, we will split their ratings
into 80% for training and 20% for validation.")
    
    put_log("Function: `make_source_datasets`: Sampling 20% of the `edx` data...")
    set.seed(fi*1000)
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
    set.seed(fi*2000)
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
#### Model building: User Effect ----------------------------------------------
# Let's visualize the average rating for each user:
start <- put_start_date()
put_log("User Effect Model:
Computing Average Ratings per User (User Mean Ratings)...")
user_ratings_avg_ls <- sapply(edx_CV, function(cv_item){
  rowMeans(cv_item$train_mx, na.rm = TRUE)
})
put_end_date(start)
put_log1("User Effect Model:
User Mean Rating list has been computed for %1-Fold Cross Validation samples.", 
         CVFolds_N)
#put(str(user_ratings_avg_ls))

y <- as.matrix(user_ratings_avg_ls)
put_log1("User Effect Model:
The User Mean Rating list for the %1-Fold Cross Validation samples has been converted to a matrix.",
        CVFolds_N)
put(dim(y))
put(head(y))
#        [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]     [,9]    [,10]
# 8  3.401893 3.395869 3.367470 3.393287 3.389845 3.382100 3.409639 3.383821 3.398451 3.351119
# 10 3.764045 3.775281 3.820225 3.820225 3.898876 3.887640 3.842697 3.775281 3.876404 3.820225
# 13 3.320000 3.370000 3.290000 3.370000 3.340000 3.270000 3.270000 3.300000 3.320000 3.330000
# 18 3.479239 3.479239 3.422145 3.444637 3.420415 3.444637 3.425606 3.463668 3.422145 3.461938
# 19 3.680233 3.767442 3.726744 3.738372 3.784884 3.726744 3.790698 3.662791 3.703488 3.726744
# 30 4.446429 4.508929 4.491071 4.500000 4.491071 4.482143 4.500000 4.500000 4.508929 4.517857

user_ratings_avg = rowMeans(y, na.rm = TRUE)
put_log("User Effect Model:
The Average Ratings per User (User Mean Ratings) have been calculated.")
hist(user_ratings_avg, nclass = 30)
put_log("User Effect Model:
A histogram of the User Mean Rating distribution has been plotted.")
print(head(user_ratings_avg))
sum(is.na(user_ratings_avg))
#> [1] 0

# we notice that there is substantial variability across users.
#>  To account for this, we can use a linear model with a treatment effect `α[i]` 
#>  for each user. The sum `μ + α[i]` can be interpreted as the typical 
#>  rating user `i` gives to movies. We can write the model as:

# Y[i,j] = μ + α[i] + ε[i,j]

#> It can be shown that the least squares estimate `α[i]` is just the average 
#> of `y[i,j] - μ` for each user. So we can compute them this way:

put_log("User Effect Model:
Computing User Effect per users ...")
a <- user_ratings_avg - mu

#> Finally, we are ready to compute the `RMSE` (additionally using the helper 
#> function `clamp` we defined above to keep predictions in the proper range):
user_effects <- data.frame(userId = as.integer(names(a)), a = a)
put_log("User Effect Model:
A User Effect Model has been builded and trained")
put(str(user_effects))
print(head(user_effects))

# Plot a histogram of the user effects -----------------------------------------
par(cex = 0.7)
hist(user_effects$a, 30, xlab = TeX(r'[$\hat{alpha}_{i}$]'),
     main = TeX(r'[Histogram of $\hat{alpha}_{i}$]'))
put_log("User Effect Model:
A histogram of the User Effect distribution has been plotted.")

# Computing the RMSE taking into account user effects --------------------------
put_log("User Effect Model:
Computing the RMSE taking into account user effects...")
start <- put_start_date()
user_effect_rmses <- sapply(edx_CV, function(cv_item){
  #mse(cv_item$validation_set$rating - mu)
  cv_item$validation_set |>
    left_join(user_effects, by = "userId") |>
    mutate(resid = rating - clamp(mu + a)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> rmse()
})
plot(user_effect_rmses)
put_log1("User Effect Model:
RMSE values have been plotted for the %1-Fold Cross Validation samples.", CVFolds_N)
put_end_date(start)

user_effect_rmse <- mean(user_effect_rmses)
put_log2("User Effect Model:
%1-Fold Cross Validation ultimate RMSE: %2", CVFolds_N, user_effect_rmse)
#> [1] 0.9684293

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

### Model building: User+Movie Effects -----------------------------------------
put_log("User+Movie Effect Model:
Computing User+Movie Effect...")
start <- put_start_date()
user_movie_effects_ls <- sapply(edx_CV, function(cv_item){
  colMeans(cv_item$train_mx - user_effects$a - mu, na.rm = TRUE)
})
put_end_date(start)
put_log1("User+Movie Effect Model:
User+Movie Effect list has been computed for %1-Fold Cross Validation samples.", 
         CVFolds_N)

print(str(user_movie_effects_ls))

b <- user_movie_effects_ls |> unlist()
str(b)

user_movie_effects <- 
  data.frame(movieId = names(b),                                     
             b = b) |>
  group_by(movieId) |>
  summarise(b = mean(b, na.rm = TRUE)) |>
  mutate(movieId = as.integer(movieId))

print(str(user_movie_effects))
print(head(user_movie_effects))

user_movie_effects <- data.frame(movieId = as.integer(names(b)), b = b)
put_log("User+Movie Effect Model:
Completed building the model.")

# Plot a histogram of the User+Movie Effects -----------------------------------
par(cex = 0.7)
hist(user_movie_effects$b, 30, xlab = TeX(r'[$\hat{beta}_{j}$]'),
     main = TeX(r'[Histogram of $\hat{beta}_{j}$]'))
put_log("User+Movie Effects Model:
A histogram of the User+Movie Effect distribution has been plotted.")

# Calculate RMSEs on Validation Sets --------------------------------------------
put_log("User+Movie Effects Model:
Computing the RMSE taking into account User+Movie Effects...")
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
put_log1("User+Movie Effects Model:
RMSE values have been plotted for the %1-Fold Cross Validation samples.", CVFolds_N)

user_movie_effects_rmse <- mean(user_movie_effects_mses)
put_log2("User+Movie Effects Model:
%1-Fold Cross Validation ultimate RMSE: %2", CVFolds_N, user_movie_effects_rmse)
#> [1] 0.8621376

#### Close Log -----------------------------------------------------------------
log_close()

# Add a row to the RMSE Result Table for the User+Movie Effect Model ---------- 
RMSEs <- rmses_add_row("User+Movie Effect Model", 
                       user_movie_effects_rmse)
rmse_kable()

# final_holdout_test |>
#   left_join(user_effects, by = "userId") |>
#   left_join(user_movie_effects, by = "movieId") |>
#   mutate(resid = rating - clamp(mu + a + b)) |> 
#   filter(!is.na(resid)) |>
#   pull(resid) |> final_rmse()
#> [1] 0.8660814

### Accounting for Movie Genres ------------------------------------------------
#> We can slightly improve our naive model by accounting for movie genres.
#> Let's do some preliminary analysis first.
#### Data Analysis and Visualization -------------------------------------------

# Reference: the Textbook Section "23.7 Exercises" of the Chapter "23 Regularization"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#exercises

#> The `edx` dataset also has a genres column. This column includes 
#> every genre that applies to the movie 
#> (some movies fall under several genres)[@IDS2_23-7].

# Preparing data for plotting:

genres_summary_list <- lapply(edx_CV, function(cv_item){
  
  grp <- cv_item$train_set |> 
    mutate(genre_categories = as.factor(genres)) |>
    group_by(genre_categories) |>
    summarize(n = n(), rating_avg = mean(rating), se = sd(rating)/sqrt(n())) |>
    mutate(genres = reorder(genre_categories, rating_avg)) |>
    select(genres, rating_avg, se, n)
})

# Average rating by genre -----------------------------------------------------
genre_ratings_avg <- lapply(genres_summary_list, function(gdat){
  r_avg <- gdat$rating_avg
  names(r_avg) <- gdat$genres
  r_avg
})

str(genre_ratings_avg)
#length(genre_ratings_avg[[1]])
gr_avg_length <- sapply(genre_ratings_avg, function(x) length(x))
gr_avg_length

movie_genre_groups <- names(genre_ratings_avg[[which.max(gr_avg_length)]])
head(movie_genre_groups)
length(movie_genre_groups)

gratings_mx <- matrix(unlist(genre_ratings_avg), 
                      ncol = length(kfold_index),
                      byrow = TRUE)
dim(gratings_mx)
rownames(gratings_mx) <- movie_genre_groups
head(gratings_mx)

genre_ratings_mu <- rowMeans(gratings_mx, na.rm = TRUE)
str(genre_ratings_mu)


sprintf("The worst ratings were for the genre category: %s",
        names(genre_ratings_mu)[which.min(genre_ratings_mu)])

sprintf("The best ratings were for the genre category: %s",
        names(genre_ratings_mu)[which.max(genre_ratings_mu)])

# Genres Popularity ------------------------------------------------------------

genre_popularity <- lapply(genres_summary_list, function(gdat){
  n <- gdat$n
  names(n) <- gdat$genres
  n
})

str(genre_popularity)

genres_n <- matrix(unlist(genre_popularity), 
                   ncol = length(kfold_index),
                   byrow = TRUE)
dim(genres_n)
rownames(genres_n) <- movie_genre_groups
head(genres_n)

genres_N <- rowMeans(genres_n, na.rm = TRUE)
str(genres_N)

sprintf("The worst popularity was for the genre category: %s",
        names(genres_N)[which.min(genres_N)])

sprintf("The best popularity was for the genre category: %s",
        names(genres_N)[which.max(genres_N)])


# Genre Ratings Data Frame -----------------------------------------------------

genre_ratings_se <- lapply(genres_summary_list, function(gdat){
  gdat$se
})

str(genre_ratings_se)

genres_se_mx <- matrix(unlist(genre_ratings_se), 
                       ncol = length(kfold_index),
                       byrow = TRUE)
dim(genres_se_mx)
genres_se <- rowMeans(genres_se_mx, na.rm = TRUE)
str(genres_se)

genre_ratins_df <- data.frame(genres = names(genre_ratings_mu),
                              ratings_avg = genre_ratings_mu,
                              n = genres_N,
                              se = genres_se) |>
  sort_by.data.frame(~ratings_avg)



row.names(genre_ratins_df) <- 1:nrow(genre_ratins_df)

str(genre_ratins_df)
head(genre_ratins_df)

##### Genres Info Visualization ------------------------------------------------
#> For illustrative purposes, we will limit the genre information 
#> we are going to plot to movies with more than 24,000 ratings:
nratings <- 24000

# Plot Genre Info --------------------------------------------------------------  
genre_ratings_plot_dat <- genre_ratins_df |>
  filter(n > nratings) |>
  mutate(genres = factor(genres, levels = unique(genres)))
#mutate(genres = as.factor(genres))

dim(genre_ratings_plot_dat)
str(genre_ratings_plot_dat)
head(genre_ratings_plot_dat)
put(genre_ratings_plot_dat)


# Creating plot:
genre_ratings_plot_dat |> 
  ggplot(aes(x = genres, 
             y = ratings_avg, 
             ymin = ratings_avg - 2*se, 
             ymax = ratings_avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  ggtitle("Average rating per Genre") +
  ylab("Average rating") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

sprintf("The worst ratings were for the genre category: %s",
        genre_ratings_plot_dat$genres[which.min(genre_ratings_plot_dat$ratings_avg)])

sprintf("The best ratings were for the genre category: %s",
        genre_ratings_plot_dat$genres[which.max(genre_ratings_plot_dat$ratings_avg)])

##### Alternative way of visualizing a Genre Effect ----------------------------
#> Reference: Article "Movie Recommendation System using R - BEST" written by 
#> Amir Moterfaker (https://www.kaggle.com/amirmotefaker)
#> (section "Average rating for each genre")[@MRS-R-BEST]
#> https://www.kaggle.com/code/amirmotefaker/movie-recommendation-system-using-r-best/notebook#Average-rating-for-each-genre

# For better visibility, we reduce the data for plotting 
# while keeping the worst and best rating rows:
plot_ind <- odd(1:nrow(genre_ratings_plot_dat))
plot_dat <- genre_ratings_plot_dat[plot_ind,] 

plot_dat |>
  ggplot(aes(x = ratings_avg, y = genres)) +
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

# Genre Separated Data Analysis ------------------------------------------------


### Including Genre effect -----------------------------------------------------
# Y[i,j] = μ + α[i] + β[j] + g[i,j]  + ε[i,j]
# where g[i,j] is a combination of genres for movie `i` rated by user `j`,
# so that g[i,j] = ∑{k=1,K}(x[i,j]^k*𝜸[k]) 
# with `x[i,j]^k = 1` if g[i,j] includes genre `k`, and `x[i,j]^k = 0` otherwise.

# mutate(userId = as.integer(userId),
#        movieId = as.integer(movieId)) |>

start <- put_start_date()
user_movie_genre_effects_ls <- lapply(edx_CV, function(cv_item){
  g_bias <- cv_item$train_gs_set |>
    left_join(user_effects, by = "userId") |>
    left_join(user_movie_effects, by = "movieId") |>
    group_by(genres) |>
    summarise(g = mean(rating - (mu + a + b), na.rm = TRUE))
  
  mg_bias <- cv_item$train_gs_set |>
    left_join(g_bias, by = "genres") |>
    left_join(user_movie_effects, by = "movieId") |>
    group_by(movieId) |>
    summarise(b = mean(b, na.rm = TRUE), g = mean(g, na.rm = TRUE))
  
  mg_bias
})
put_end_date(start)
str(user_movie_genre_effects_ls)
#head(user_movie_genre_effects_ls)

# edx_cv_item <- edx_CV[[1]]$train_set

# Compute Genre Movie Bias ----------------

start <- put_start_date()
mg_bias_b <- sapply(user_movie_genre_effects_ls, function(mg_bias){
  names(mg_bias$b) <- mg_bias$movieId
  mg_bias$b
})
put_end_date(start)
str(mg_bias_b)

b <- mg_bias_b |> unlist()
str(b)

genre_movie_effects <- 
  data.frame(movieId = names(b),                                     
             b = b) |>
  group_by(movieId) |>
  summarise(b = mean(b, na.rm = TRUE)) |>
  mutate(movieId = as.integer(movieId))

str(genre_movie_effects)
sum(is.na(genre_movie_effects$b))
#> [1] 0

# Compute Genre Bias ---------------

start <- put_start_date()
mg_bias_g <- sapply(user_movie_genre_effects_ls, function(mg_bias){
  names(mg_bias$g) <- mg_bias$movieId
  mg_bias$g
})
put_end_date(start)
str(mg_bias_g)

g <- mg_bias_g |> unlist()
str(g)

genre_effects <- 
  data.frame(movieId = names(g),                                     
             g = g) |>
  group_by(movieId) |>
  summarise(g = mean(g, na.rm = TRUE)) |>
  mutate(movieId = as.integer(movieId))

str(genre_effects)
sum(is.na(genre_effects$g))
#> [1] 0

# Finalize User+Movie+Genre Effects ---------------------------------------------

user_movie_genre_effects <- genre_effects |>
  left_join(genre_movie_effects, by = "movieId") |>
  group_by(movieId) |>
  summarise(b = mean(b, na.rm = TRUE), g = mean(g, na.rm = TRUE))

str(user_movie_genre_effects)
head(user_movie_genre_effects)

sum(is.na(user_movie_genre_effects$g))
#> [1] 0
sum(is.na(user_movie_genre_effects$g))
#> [1] 0

# Plot a histogram of the User+Movie+Genre Effects (Movie Bias) ----------------
par(cex = 0.7)
hist(user_movie_genre_effects$b, 30, xlab = TeX(r'[$\hat{beta}_{j}$]'),
     main = TeX(r'[Histogram of $\hat{beta}_{j}$]'))

# Plot a histogram of the User+Movie+Genre Effects (Genre Bias) ----------------
#par(cex = 0.7)
hist(user_movie_genre_effects$g, 30, xlab = TeX(r'[$\hat{g}_{i,j}$]'),
     main = TeX(r'[Histogram of $\hat{beta}_{j}$]'))

#### Compute RMSE: user+movie+genre effects ------------------------------------

# Calculate MSEs on Validation Sets
start <- put_start_date()
user_movie_genre_effects_mses <- sapply(edx_CV, function(cv_item){
  cv_item$validation_set |>
    left_join(user_effects, by = "userId") |>
    left_join(user_movie_genre_effects, by = "movieId") |>
    mutate(resid = rating - clamp(mu + a + b + g)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> mse()
})
put_end_date(start)

plot(user_movie_genre_effects_mses)
user_movie_genre_effects_rmse <- sqrt(mean(user_movie_genre_effects_mses))

put(user_movie_genre_effects_rmse)
#> [1] 0.8619763

##### Add a row to the RMSE Result Table for the User+Movie+Genre Effect Model ---- 
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

### Accounting for Date Smoothed Effect ------------------------------------------
# Y[i,j] = μ + α[i] + β[j] + g[i,j]  + f(d[i,j]) + ε[i,j]

# with `f` a smooth function of `d[(i,j]`

# library(lubridate)

# Let's take a look at the Average rating per year:
#### Plot: Average Rating per Year ------------------------------------------------

start <- put_start_date()
put("Plotting Average Rating per Year...")
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

#### Calculate Date Smoothed Effect -----------------------------------------------
#train_set1 <- edx_CV[[1]]$train_set

start <- put_start_date()
global_date_effects <- sapply(kfold_index,  function(fi){
  put(sprintf("Creating K-Fold CV Global Date Effects Set, Vector %s", fi))

  start <- put_start_date()
  de_global <- edx_CV[[fi]]$train_set |> 
    left_join(user_effects, by = "userId") |>
    left_join(genre_movie_effects, by = "movieId") |>
    left_join(date_days_map, by = "timestamp") |>
    mutate(rating_residue = rating - (mu + a + b + g)) |>
    filter(!is.na(rating_residue)) |>
    group_by(days) |>
    summarise(de = mean(rating_residue, na.rm = TRUE))
  put_end_date(start)
  
  put("Global Date Effect Vector Structure:")
  put(str(de_global))
  
  
  put("Global Date Effect Vector Structure:")
  names(de_global$de) = de_global$days
  put(str(de_global$de))
  de_global$de
})
put_end_date(start)
put("Date Global Effect Structure list:")
put(str(global_date_effects))

de_vector <- global_date_effects |> unlist()
str(de_vector)

date_global_effect <-  data.frame(days = as.integer(names(de_vector)),
                         de = de_vector) |>
  group_by(days) |>
  summarise(de = mean(de, na.rm = TRUE)) |>
  sort_by.data.frame(~days)

put("Global Date Effect Structure:")
put(str(date_global_effect))

##### Train model using `loess` function with default `span` & `degree` params-----
start <- put_start_date()
fit <- loess(de ~ days, data = date_global_effect)
put_end_date(start)
date()
sum(is.na(fit$fitted))

put("loess function results structure (fit$pars)")
put(str(fit$pars))

put("loess function results structure (fit$fitted)")
str(fit$fitted)

date_smoothed_effect <- date_global_effect |>
  mutate(de_smoothed = fit$fitted)

put("Global Date Smoothed Effect Structure:")
put(str(date_smoothed_effect))

# Date Smoothed Effect Visualization:
start <- put_start_date()
date_smoothed_effect |>
  ggplot(aes(x = days)) +
  geom_point(aes(y = de), size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(y = de_smoothed), color = "red")
put_end_date(start)

# Function for MSEs calculation of Smoothed Date Model ----------------------
date_smoothed_mses <- function(date_smoothed_effect){
  # Calculate MSEs on Validation Sets
  start <- put_start_date()
  MSEs <- sapply(kfold_index, function(fi){
    put(sprintf("Calculating MSEs on K-Fold Cross Validation Sets, Fold %s...",
                  fi))
    start <- put_start_date()
    m_se <- edx_CV[[fi]]$validation_set |>
      left_join(user_effects, by = "userId") |>
      left_join(user_movie_genre_effects, by = "movieId") |>
      left_join(date_days_map, by = "timestamp") |>
      left_join(date_smoothed_effect, by='days') |>
      mutate(resid = rating - clamp(mu + a + b + g + de_smoothed)) |> 
      filter(!is.na(resid)) |>
      pull(resid) |> mse()
    put_end_date(start)
    m_se
  })
  put_end_date(start)
  MSEs
}
# Calculate RMSE for loess fitted with default parameters ----------------------

user_movie_genre_date_effects_mses <- date_smoothed_mses(date_smoothed_effect)
plot(user_movie_genre_date_effects_mses)
user_movie_genre_date_effects_rmse <- sqrt(mean(user_movie_genre_date_effects_mses))
put(user_movie_genre_date_effects_rmse)
#> [1] 0.8613816S

#### Re-train tuning `loess` function's with `span` & `degree` params--------------
##### Support Functions ------------------------------------------------------------
fit_loess <- function(spans, dgr){
  put(sprintf("Fitting model for %s spans, degree = %s...", length(spans), dgr))
  start <- put_start_date()
  fits <- sapply(spans, function(span){
    put(sprintf("Fitting model for span = %s, degree = %s...", span, dgr))
    start <- put_start_date()
    fit <- loess(de ~ days, span = span, degree = dgr, data = date_global_effect)
    put_end_date(start)
    put(sprintf("Model fitted for span = %s, degree = %s...", span, dgr))
    put(fit)
    fit$fitted
  })
  put_end_date(start = start)
  fits
}
tune_de_model_rmses <- function(fits){
  i <- 1
  model_diu_rmses <- sapply(fits, function(smth){
    put(sprintf("Tuning Model for Case %s", i))
    date_smoothed_effect <- date_global_effect |>
      mutate(de_smoothed = smth)

    mses <- date_smoothed_mses(date_smoothed_effect)
    rmse <- sqrt(mean(mses))
    put(sprintf("RMSE calculated for Case %s: %s", i, rmse))
    i <- i + 1
    rmse
  })
}
date_smoothed_rmses <- function(spans, dgr) {
  # dgr <- degree[1] # debug
  fits <- fit_loess(spans,dgr)
  dim(fits)
  df_fits <- as.data.frame(fits)
  #str(df_fits)
  
  start <- put_start_date()
  put(sprintf("Tuning the Smothed Date Effect Model for Degree: %s", dgr))
  model_diu_rmses <- tune_de_model_rmses(df_fits)
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

##### Tune the Global Date Smoothed Effect model -----------------------------------
degree <- c(0, 1, 2)
put("Tuning `loess` function for degrees:")
put(degree)

###### 1. `degree = 0` --------------------------------------------------------------
put("Case 1. `degree = 0`")
# spans <- seq(0.0003, 0.002, 0.00001)
spans <- seq(0.0005, 0.0015, 0.00001)
put("Tuning for spans:")
put(spans)

ds0_rmses <- date_smoothed_rmses(spans, 
                               degree[1])
put("Case 1. `degree = 0` RMSEs:")
put(str(ds0_rmses))
plot(ds0_rmses)

ds_rmse0 <- best_rmse(ds0_rmses)
put(ds_rmse0)
#   Span      RMSE 
# 0.0010900 0.8602027 

###### 2. `degree = 1` --------------------------------------------------------------
put("Case 2. `degree = 1`")
#spans <- seq(0.0005, 0.002, 0.00001)
spans <- seq(0.001, 0.0014, 0.00001)
put("Tuning for spans:")
put(spans)

ds1_rmses <- date_smoothed_rmses(spans, 
                                 degree[2])
put("Case 2. `degree = 1` RMSEs:")
put(str(ds1_rmses))
plot(ds1_rmses)

ds_rmse1 <- best_rmse(ds1_rmses)
put(ds_rmse1)
#      Span      RMSE 
# 0.0010000 0.8597244 

###### 3. `degree = 2` --------------------------------------------------------------
put("Case 3. `degree = 2`")
#spans <- seq(0.0003, 0.01, 0.00001)
spans <- seq(0.0007, 0.002, 0.00001)
put("Tuning for spans:")
put(spans)

ds2_rmses <- date_smoothed_rmses(spans, 
                                 degree[3])
put("Case 3. `degree = 2` RMSEs:")
put(str(ds2_rmses))
plot(ds2_rmses)

ds_rmse2 <- best_rmse(ds2_rmses)
put(ds_rmse2)
#      Span      RMSE 
# 0.0013000 0.8596676 

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

start <- put_start_date()
fit <- loess(de ~ days, 
             degree = best_degree, 
             span = best_span, 
             data = date_global_effect)
put_end_date(start)
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

user_movie_genre_tuned_date_effect_mses <- date_smoothed_mses(date_smoothed_effect)
plot(user_movie_genre_tuned_date_effect_mses)
user_movie_genre_tuned_date_effect_rmse <- sqrt(mean(user_movie_genre_tuned_date_effect_mses))
put(user_movie_genre_tuned_date_effect_rmse)
#> [1] 0.8596676

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

#### Closing log file ----------------------------------------------------------
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


