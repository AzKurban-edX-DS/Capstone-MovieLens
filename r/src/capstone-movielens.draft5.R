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

## Init Project Global Variables ----------------------------------------------
put("Set Project Objective according to Capstone course requirements")
project_objective <- 0.86490
put(project_objective)

put("Set minimum number of ratings to ignore")
min_nratings <- as.integer(100)
put(min_nratings)

CVFolds_N <- 5
kfold_index <- seq(from = 1:CVFolds_N)

#RMSEs.ResultTibble <- NULL

### File Paths -----------------------------------------------------------------
r_path <- "r"

src_folder <- "src"
functions_folder <- "support-functions"
regularization_folder <- "regularization"

r_src_path <- file.path(r_path, src_folder)
functions_path <- file.path(r_src_path, functions_folder)
src_regularization_path <- file.path(r_src_path, regularization_folder)

data_path <- "data"
models_folder <- "models"
models_data_path <- file.path(data_path, models_folder)
regularization_data_path <- file.path(data_path, regularization_folder)



## Defining helper functions --------------------------------------------------
common_helper_functions.file_path <- file.path(functions_path, 
                                            "common-helper.functions.R")
source(common_helper_functions.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)

## Open log -----------------------------------------------------------
open_logfile(".init-project-data")

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
    mutate(year = year(date_time)) |>
    mutate(days = as.integer(date - min(date))) |>
    select(timestamp, date_time, date, year, days) |>
    distinct(timestamp, .keep_all = TRUE)
  
  str(date_days_map)
  put_log("Function: `make_source_datasets`: Dataset created: date_days_map")
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

    split_sets <- sample_train_validation_sets(fold_i*1000)
    train_set <- split_sets$train_set
    validation_set <- split_sets$validation_set

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

  tuning_sets <- sample_train_validation_sets(2)
  
  list(edx_CV = edx_CV,
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

#> Inpired by Chapter 29. Resampling methods
#> (in particular starting from 
#> Section 29.5 "Mathematical description of resampling methods" onwards.
#> https://rafalab.dfci.harvard.edu/dsbook-part-2/ml/resampling-methods.html
edx_CV <- movielens_datasets$edx_CV
put("Set of K-Fold Cross Validation datasets summary: edx_CV")
put(summary(edx_CV))

tune.train_set <- movielens_datasets$tuning_sets$train_set
put("Train Set for tuning purposes")
put(summary(tune.train_set))

tune.test_set <- movielens_datasets$tuning_sets$validation_set
put("Test Set for tuning purposes")
put(summary(tune.test_set))

movie_map <- movielens_datasets$movie_map
put("Dataset summary: movie_map")
put(summary(movie_map))

date_days_map <- movielens_datasets$date_days_map
put("Dataset summary: date_days_map")
put(summary(date_days_map))

put("Dataset summary: final_holdout_test")
put(summary(final_holdout_test))

rm(movielens_datasets)

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
### Create an RMSE Result Tibble and add a first row for the Project Objective ----
RMSEs.ResultTibble <- CreateRMSEs_ResultTibble()
RMSE_kable(RMSEs.ResultTibble)
put("RMSE Results Tibble created.")

#### Compute Naive RMSE --------------------------------------------------------
file_name_tmp <- "overall-mean-rating.RData"
file_path_tmp <- file.path(models_data_path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading overal mean rating value from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log2("Overall mean rating data (`mu = %1`) has been loaded from file: %2",
           mu,
           file_path_tmp)
} else {
  mu <- mean(edx$rating)

  MSEs <- naive_model_MSEs(mu)
  plot(MSEs)
  put_log1("MSE values plotted for %1-Fold Cross Validation samples.", CVFolds_N)
  naive_rmse <- sqrt(mean(MSEs))
  # naive_rmse <- naive_model_RMSE(mu)
  put_log2("%1-Fold Cross Validation ultimate RMSE: %2", CVFolds_N, naive_rmse)
  #> 5-Fold Cross Validation ultimate RMSE: 1.06034335317133
  
  put_log2("Saving Overall mean rating value (`mu = %1`) to file: %1...", 
           mu,
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       naive_rmse,
       file = file_path_tmp)
  put_end_date(start)
  put_log2("Overall mean rating value (`mu = %1`) has been saved to file: %1", 
           mu,
           file_path_tmp)
}

put_log1("The Overall Mean Rating is: %1", mu)
#> The Overall Mean Rating is: 3.51246520160155
put_log1("The Naive RMSE is: %1", naive_rmse)

#### Ensure that this is the best RMSE value for the current model ----------------
#> If we plug in any other number, we will get a higher RMSE. 
#> Let's prove that by the following small investigation:

file_name_tmp <- "mu-deviation.RData"
file_path_tmp <- file.path(models_data_path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading Overall Mean Deviation data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Overall Mean Deviation  data has been loaded from file: %1", file_path_tmp)
  
} else {
  deviation <- seq(0, 6, 0.1) - 3

  start = put_start_date()
  rmse_values <- sapply(deviation, function(delta){
    naive_model_RMSE(mu + delta)
  })
  print(rmse_values)
  put_log1("RMSE values have been computed for %1 deviations from the Overall Mean Rating.",
           length(deviation))
  
  put_log1("Saving Overall Mean Rating Deviation data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       naive_rmse,
       deviation,
       rmse_values,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("Overall Mean Rating Deviation data has been saved to file: %1", 
           file_path_tmp)
} 

data.frame(deviation = deviation, 
                        rmse_values = rmse_values) |> 
  ggplot(aes(deviation, rmse_values)) +
  geom_line()

put_log("A plot was constructed for the deviations from the Overall Mean Rating.")

which_min_deviation <- deviation[which.min(rmse_values)]
min_rmse = min(rmse_values)

put_log1("Minimum RMSE is achieved when the deviation from the mean is: %1",
         which_min_deviation)

put_log1("Is the previously computed RMSE the best for the current model: %1",
         naive_rmse == min_rmse)
#> [1] "Is the previously computed RMSE the best for the current model: TRUE"
writeLines("")

#### Add a row to the RMSE Result Tibble for the Overall Mean Rating Model ------ 
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("Overall Mean Rating Model", naive_rmse)

RMSE_kable(RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble for the `Overall Mean Rating Model`.")
#### Close Log ---------------------------------------------------------------
log_close()

### User Effect Model ---------------------------------------------------------- 
# Reference: the Textbook section "23.4 User effects"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#user-effects

#### Open log -------------------------------------------------------------------
open_logfile(".user-effect")
put("Building User Effect Model...")
#### Model building: User Effect -----------------------------------------------
##### User Mean Ratings Computation --------------------------------------------
file_name_tmp <- "user-mean-ratings.RData"
file_path_tmp <- file.path(models_data_path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading User Mean Rating data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("User Mean Rating data has been loaded from file: %1", file_path_tmp)
  
} else {
  put_log("Computing Average Ratings per User (User Mean Ratings)...")
  start <- put_start_date()
  user_mean_ratings_ls <- lapply(edx_CV, function(cv_item){
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
  str(user_mean_ratings_ls)
  put_end_date(start)
  put_log1("User Average Rating list has been computed for %1-Fold Cross Validation samples.", 
           CVFolds_N)
  
  user_ratings_avg_united <- union_cv_results(user_mean_ratings_ls)
  str(user_ratings_avg_united)
  # sum(is.na(user_ratings_avg_united))
  
  user_mean_ratings <- user_ratings_avg_united |>
    group_by(userId) |>
    summarise(mean_rating = mean(ratings_avg), n = mean(n))
  
  put_log("User Mean Ratings (User Effect) have been computed.")
  str(user_mean_ratings)

  put_log1("Saving User Mean Rating data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       naive_rmse,
       deviation,
       rmse_values,
       user_mean_ratings,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User Mean Rating data has been saved to file: %1", 
           file_path_tmp)
} 

##### User Mean Ratings: Visualization ------------------------------
# Let's visualize the average rating for each user:

# sum(is.na(user_mean_ratings$mean_rating))
#> [1] 0 (there are no NAs in there)

hist(user_mean_ratings$mean_rating, nclass = 30)
put_log("A histogram of the User Mean Rating distribution has been plotted.")

##### Building User Effects Model ----------------------------------------------

# we notice that there is substantial variability across users.
#>  To account for this, we can use a linear model with a treatment effect `α[i]` 
#>  for each user. The sum `μ + α[i]` can be interpreted as the typical 
#>  rating user `i` gives to movies. We can write the model as:

# Y[i,j] = μ + α[i] + ε[i,j]

#> It can be shown that the least squares estimate `α[i]` is just the average 
#> of `y[i,j] - μ` for each user. So we can compute them this way:

file_name_tmp <- "user-effect-model.RData"
file_path_tmp <- file.path(models_data_path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading User Effect Model data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("User Effect Model data has been loaded from file: %1", file_path_tmp)
  
} else {
  put_log("Computing User Effect per users ...")
  user_effects <- user_mean_ratings |>
    mutate(userId = as.integer(userId),
           a = mean_rating - mu)
  
  put_log("A User Effect Model has been builded and trained")
  
  put_log1("Saving User Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       naive_rmse,
       user_effects,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

put(str(user_effects))

# Plot a histogram of the user effects -----------------------------------------
par(cex = 0.7)
hist(user_effects$a, 30, xlab = TeX(r'[$\hat{alpha}_{i}$]'),
     main = TeX(r'[Histogram of $\hat{alpha}_{i}$]'))
put_log("A histogram of the User Effect distribution has been plotted.")

# Computing the RMSE taking into account user effects --------------------------
#> Finally, we are ready to compute the `RMSE` (additionally using the helper 
#> function `clamp` we defined above to keep predictions in the proper range):

put_log("Computing the RMSE taking into account user effects...")
start <- put_start_date()
user_effect_mses <- sapply(edx_CV, function(cv_fold_dat){
  cv_fold_dat$validation_set |>
    left_join(user_effects, by = "userId") |>
    mutate(resid = rating - clamp(mu + a)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> mse()
})
put_end_date(start)

plot(user_effect_mses)
put_log1("RMSE values have been plotted for the %1-Fold Cross Validation samples.", 
         CVFolds_N)

user_effect_rmse <- sqrt(mean(user_effect_mses))
put_log2("%1-Fold Cross Validation ultimate RMSE: %2", 
         CVFolds_N, 
         user_effect_rmse)
user_effect_rmse
#> [1] 0.9716054

# Add a row to the RMSE Result Tibble for the User Effect Model ---------------- 
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("User Effect Model", user_effect_rmse)

RMSE_kable(RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble for the `User Effect Model`.")

#### Close Log -----------------------------------------------------------------
log_close()
### Taking into account User+Movie Effects -------------------------------------

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

#### Open log ------------------------------------------------------------------
open_logfile(".user+movie-effect")

#### Support Functions ---------------------------------------------------------
ume_functions.file_path <- file.path(functions_path, 
                                               "user+movie-effect.functions.R")
source(ume_functions.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)
#### Model building: User+Movie Effect ----------------------------------------
file_name_tmp <- "user-movie-effect.RData"
file_path_tmp <- file.path(models_data_path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading User+Movie Effect data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie Effect data has been loaded from file: %1", file_path_tmp)
  
} else {
  user_movie_effect <- train_user_movie_effect.cv()
  
  put_log1("Saving User+Movie Effect data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effects,
       user_movie_effect,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie Effect data has been saved to file: %1",
           file_path_tmp)
} 

put(str(user_movie_effect))

##### User+Movie Effects: Visualization ------------------------------
par(cex = 0.7)
hist(user_movie_effect$b, 30, xlab = TeX(r'[$\hat{beta}_{j}$)]'),
     main = TeX(r'[Histogram of $\hat{beta}_{j}$]'))
put_log("A histogram of the Mean User+Movie Effects distribution has been plotted.")

#### Calculate RMSEs.ResultTibble on Validation Sets ----------------------------------------
user_movie_effect_RMSE <- calc_user_movie_effect_RMSE.cv(user_movie_effect)
#> [1] 0.8594763

#### Add a row to the RMSE Result Tibble for the User+Movie Effect Model --------
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("User+Movie Effect Model", user_movie_effect_RMSE)

RMSE_kable(RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble for the `User+Movie Effect Model`.")

#### Close Log -----------------------------------------------------------------
log_close()
#### Regularizing User+Movie Effects (Utilizing Penalized least squares)  -----
# Reference: the Textbook section "23.6 Penalized least squares"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#penalized-least-squares

#> Instead of minimizing the least squares equation, 
#> we minimize an equation that adds a penalty:

#  ∑{i,j}(y[i,j] - y_hat[i,j])^2 + λ*∑{j}β[j]^2
# where y_hat[i,j] = μ + α[i] + β[j] + g[i,j] + ye[i,j] + f(d[i,j])

#> The values of `β[j]` that minimize this equation are:

# β[j](λ) = 1/(λ + n[j])*∑{u=1,n[i]}(Y[i,j] - μ - α[i])
# where `n[j]` is the number of ratings made for movie `j`.

##### Open log --------------------------------------------------------------------
open_logfile(".reg-um-effect.loop_0_10_d10")

##### Process User+Movie Model Regularization -------------------------------------
# ume_regularization.file_path <- file.path(src_regularization_path, 
#                                             "user-movie-effect-regularization.R")
# source(ume_regularization.file_path, 
#        catch.aborts = TRUE,
#        echo = TRUE,
#        spaced = TRUE,
#        verbose = TRUE,
#        keep.source = TRUE)

ume_regularization_path <- file.path(regularization_data_path, 
                                     "user-movie-effect")
ume_loop_starter <- c(0, 4, 2, 128)
ume_cache_file_base_name <- "ume_reg-loop"

ume_reg_lambdas_best_results <- model.regularize(ume_loop_starter,
                                                 ume_regularization_path,
                                                 ume_cache_file_base_name,
                                                 regularize.test_lambda.user_movie_effect.cv)

##### Re-train Regularized User+Movie Effect Model for the best `lambda` --------
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

##### Add a row to the RMSE Result Table for the Regularized User+Movie Effect Model --------
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("Regularized User+Movie Effect Model", 
               best_lambda_user_movie_effect_RMSE)

RMSE_kable(RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble 
for the `Regularized User+Movie Effect Model`.")
##### Close Log -----------------------------------------------------------------
log_close()


### Accounting for Movie Genres ------------------------------------------------
#> We can slightly improve our naive model by accounting for movie genres.
#> Let's do some preliminary analysis first.

#### Open log -------------------------------------------------------------------
open_logfile(".user+movie+genre-effect")

#### Data Analysis and Visualization -------------------------------------------

# Reference: the Textbook Section "23.7 Exercises" of the Chapter "23 Regularization"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#exercises

#> The `edx` dataset also has a genres column. This column includes 
#> every genre that applies to the movie 
#> (some movies fall under several genres)[@IDS2_23-7].

##### Average rating per genre -----------------------------------------------------
file_name_tmp <- "genre-average-rating.RData"
file_path_tmp <- file.path(models_data_path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading Genre Average Rating data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Genre Average Rating data has been loaded from file: %1", file_path_tmp)
  
} else {
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
  
  put_log1("Computing Average Rating per Genre list for %1-Fold Cross Validation samples...", 
           CVFolds_N)
  
  genre_ratings_united <- union_cv_results(genres_summary_list)
  str(genre_ratings_united)
  
  genre_mean_ratings <- genre_ratings_united |>
    group_by(genres) |>
    summarise(ratings = mean(rating_avg),
              se = mean(se),
              n = mean(n)) |>
    mutate(genres = reorder(genres, ratings)) |>
    sort_by.data.frame(~ratings)
  
  put_log1("Mean Rating per Genre list has been computed for %1-Fold Cross Validation samples.",
           CVFolds_N)

  put_log1("Saving Genre Average Rating data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effects,
       user_movie_effect,
       genre_mean_ratings,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("Genre Average Rating data has been saved to file: %1", 
           file_path_tmp)
} 

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

#### Support Functions ---------------------------------------------------------
umge_functions_file <- "user+movie+genre-effect.functions.R"
umge_functions.file_path <- file.path(functions_path, 
                                      umge_functions_file)
source(umge_functions.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)

#### Train User+Movie+Genre Effect Model ---------------------------------------
file_name_tmp <- "user-movie-genre-effect.RData"
file_path_tmp <- file.path(models_data_path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading User+Movie+Genre Effect Model data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre Effect Model data has been loaded from file: %1", file_path_tmp)
  
} else {
  user_movie_genre_effect <- train_user_movie_genre_effect.cv()
  
  put_log1("Saving User+Movie+Genre Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effects,
       user_movie_effect,
       genre_mean_ratings,
       user_movie_genre_effect,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

put(str(user_movie_genre_effect))

###### Plot a histogram of the Movie Genre Effect distribution -----------------
par(cex = 0.7)
hist(user_movie_genre_effect$g, 30, xlab = TeX(r'[$\hat{g}_{i,j}$]'),
     main = TeX(r'[Histogram of $\hat{g}_{i,j}$]'))

put_log("A histogram of the Movie Genre Effect distribution has been plotted.")

###### Compute RMSE: user+movie+genre effects ------------------------------------
user_movie_genre_effect_RMSE <- calc_user_movie_genre_effect_RMSE.cv(user_movie_genre_effect)
user_movie_genre_effect_RMSE
#> [1] 0.859473

#### Add a row to the RMSE Result Tibble for the User+Movie+Genre Effect Model ---- 
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("User+Movie+Genre Effect Model", user_movie_genre_effect_RMSE)
RMSE_kable(RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble for the `User+Movie+Genre Effect Model`.")

#### Close Log -----------------------------------------------------------------
log_close()

### Regularizing User+Movie+Genre Effects --------------------------------------------
##### Open log --------------------------------------------------------------------
open_logfile(".reg-umg-effect.loop_0_128_d128")

##### Process User+Movie+Genre Model Regularization -------------------------------------
# ume_regularization.file_path <- file.path(src_regularization_path, 
#                                             "user-movie-effect-regularization.R")
# source(ume_regularization.file_path, 
#        catch.aborts = TRUE,
#        echo = TRUE,
#        spaced = TRUE,
#        verbose = TRUE,
#        keep.source = TRUE)

ume_regularization_path <- file.path(regularization_data_path, 
                                     "user-movie-genre-effect")
ume_loop_starter <- c(0, 2, 2, 128)
ume_cache_file_base_name <- "ume_reg-loop"

ume_reg_lambdas_best_results <- model.regularize(ume_loop_starter,
                                                 ume_regularization_path,
                                                 ume_cache_file_base_name,
                                                 regularize.test_lambda.user_movie_genre_effect.cv)

put(ume_reg_lambdas_best_results)

##### Close Log -----------------------------------------------------------------
log_close()






##### Re-training Regularized User+Movie+Genre Effect Model for the best `lambda` value ----

best_user_movie_genre_lambda_RMSEs <- 
  data.frame(lambda = c(best_user_movie_genre_lambda5, 
                        best_user_movie_genre_lambda6),
             RMSE = c(best_user_movie_genre_reg_RMSE_lambda5, 
                      best_user_movie_genre_reg_RMSE_lambda6))
put(best_user_movie_genre_lambda_RMSEs)

best_lambda_idx <- which.min(best_user_movie_genre_lambda_RMSEs$lambda)
best_lambda_idx
best_user_movie_genre_lambda <- best_user_movie_genre_lambda_RMSEs[best_lambda_idx,]$lambda
best_user_movie_genre_lambda
best_user_movie_genre_reg_RMSE <- best_user_movie_genre_lambda_RMSEs[best_lambda_idx,]$RMSE
best_user_movie_genre_reg_RMSE

put_log1("Re-training Regularized User+Movie Effect Model for the best `lambda`: %1...",
         best_user_movie_genre_lambda)

user_movie_genre_effect_best_lambda <- train_user_movie_genre_effect(best_user_movie_genre_lambda)
user_movie_genre_effect_best_lambda_RMSE <- calc_user_movie_genre_effect_RMSE.cv(user_movie_genre_effect_best_lambda)

put_log1("Regularized User+Movie Effect Model has been re-trained for the best `lambda`: %1.",
         best_user_movie_genre_lambda)
put_log1("Is this a best RMSE? %1",
         best_user_movie_genre_reg_RMSE == user_movie_genre_effect_best_lambda_RMSE)

#### Add a row to the RMSE Result Tibble for the Regularized User+Movie+Genre Effect Model --------
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("Regularized User+Movie+Genre Effect Model", 
               best_user_movie_genre_reg_RMSE)

RMSE_kable(RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble for the `Regularized User+Movie+Genre Effect Model`.")

#### Close Log -----------------------------------------------------------------
log_close()

### Accounting for Date Effect ------------------------------------------
# Y[i,j] = μ + α[i] + β[j] + g[i,j]  + f(d[i,j]) + ε[i,j]

# with `f` a smooth function of `d[(i,j]`

# library(lubridate)
#### Open log -------------------------------------------------------------------
open_logfile(".user+movie+genre+date-effect")

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

#### Sample Train & Test sets for computing Date Effects -----------------------
file_name_tmp <- "sample-year-tune-sets.RData"
file_path_tmp <- file.path(models_data_path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading Year Bias Tuning data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Year Bias Tuning data has been loaded from file: %1", file_path_tmp)
} else {
  umgy_tune_sets <- sample_train_validation_sets(3)
  
  put_log1("Saving Year Bias Tuning data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effects,
       user_movie_effect,
       genre_mean_ratings,
       user_movie_genre_effect,
       user_movie_genre_reg_lambdas_6p6_m4p2_p2,
       user_movie_genre_reg_RMSEs_m66_42_0_2,
       umgy_tune_sets,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("Year Bias Tuning data has been saved to file: %1", 
           file_path_tmp)
} 

umgy_train_set <- umgy_tune_sets$train_set
str(umgy_train_set)
umgy_test_set <- umgy_tune_sets$validation_set
str(umgy_test_set)

#### Support Functions ---------------------------------------------------------
calc_date_global_effect <- function(train_set, lambda = 0){
  if(is.na(lambda)) put_log("Computing Date Global Effect for given Train Set data...")
  else put_log1("Computing Date Global Effect for lambda: %1...",
                lambda)
  dg_effect <- train_set |> 
    left_join(user_effects, by = "userId") |>
    left_join(best_lambda_user_movie_effect, by = "movieId") |>
    left_join(user_movie_genre_effect_best_lambda, by = "movieId") |>
    left_join(date_days_map, by = "timestamp") |>
    mutate(resid = rating - (mu + a + b + g)) |>
    filter(!is.na(resid)) |>
    filter(!is.infinite(de))
    group_by(days) |>
    summarise(de = mean_reg(resid, lambda), 
              year = mean(year))
    
    if(is.na(lambda)) put_log("Date Global Effect has been computed.")
    else put_log1("Date Global Effect has been computed for lambda: %1...",
                  lambda)
    dg_effect
}
  
  
calc_date_global_effect.cv <- function(lambda = 0){
  if(is.na(lambda)) put_log("Computing Date Global Effect...")
  else put_log1("Computing Date Global Effect for lambda: %1...",
                lambda)
  
  put_log1("Computing Date Global Effect list for %1-Fold Cross Validation samples...", 
           CVFolds_N)
  start <- put_start_date()
  date_global_effect_ls <- lapply(edx_CV,  function(cv_fold_dat){
    # start <- put_start_date()
    cv_fold_dat$train_set |> calc_date_global_effect(lambda)
  })
  str(date_global_effect_ls)
  put_end_date(start)
  put_log1("Date Global Effect list has been computed for %1-Fold Cross Validation samples.", 
           CVFolds_N)
  
  date_global_effect_united <- union_cv_results(date_global_effect_ls)
  str(date_global_effect_united)
  
  date_global_effect <- date_global_effect_united |>
    #filter(!is.na(de)) |>
    filter(!is.infinite(de)) |>
    group_by(days) |>
    summarise(de = mean(de, na.rm = TRUE), year = mean(year, na.rm = TRUE))
  
  if(is.na(lambda)) put_log("Training completed: Date Global Effects model.")
  else put_log1("Training completed: Date Global Effects model for lambda: %1...",
                lambda)
  
  date_global_effect
}

train_date_year_effect <- function(train_set, lambda = 0){
  train_set |>
    calc_date_global_effect(lambda) |>
    group_by(year) |>
    summarise(ye = mean(de, na.rm = TRUE))
}
train_date_year_effect.cv <- function(lambda = 0){
  calc_date_global_effect.cv(lambda) |>
    group_by(year) |>
    summarise(ye = mean(de, na.rm = TRUE))
}
calc_date_year_effect_MSE <- function(test_set, dy_effect){
  test_set |>
    left_join(user_effects, by = "userId") |>
    left_join(best_lambda_user_movie_effect, by = "movieId") |>
    left_join(user_movie_genre_effect_best_lambda, by = "movieId") |>
    left_join(date_days_map, by = "timestamp") |>
    left_join(dy_effect, by='year') |>
    mutate(resid = rating - clamp(mu + a + b + g + ye)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> mse()
}
calc_date_year_effect_RMSE.cv <- function(dy_effect){
  start <- put_start_date()
  date_year_effect_MSEs <- sapply(edx_CV, function(cv_fold_dat){
    cv_fold_dat$validation_set |> calc_date_year_effect_MSE(dy_effect)
  })
  put_end_date(start)
  put_log1("Function: calc_date_year_effect_RMSE.cv
Date (Year) Effect MSE values have been computed for the %1-Fold Cross Validation samples.", 
           CVFolds_N)
  
  sqrt(mean(date_year_effect_MSEs))
}
calc_date_year_effect_RMSE <- function(test_set, dy_effect){
  mse <- test_set |> calc_date_year_effect_MSE(dy_effect)
  sqrt(mse)
}
reg_tune_user_movie_genre_year_effect <- function(lambdas){
  n <- length(lambdas)
  lambdas_tmp <- numeric(n)
  rmses_tmp <- numeric(n)
  put_log("Function: reg_tune_user_movie_genre_year_effect
lambdas:")
  print(lambdas)
  
  for (i in 1:n) {
    put_log1("Function: reg_tune_user_movie_genre_year_effect
Iteration %1", i)
    lambda <- lambdas[i]
    put_log1("Function: reg_tune_user_movie_genre_year_effect
lambda: %1", lambda)
    lambdas_tmp[i] <- lambda
    
    put_log2("Function: reg_tune_user_movie_genre_year_effect
lambdas_tmp[%1]: %2", i, lambdas_tmp[i])
    put_log1("Function: reg_tune_user_movie_genre_year_effect
lambdas_tmp length: %1", length(lambdas_tmp))
print(lambdas_tmp)

    umgy_reg_effect <- umgy_train_set |> train_date_year_effect(lambda)
    
    rmse_tmp <- umgy_test_set |> 
      calc_date_year_effect_RMSE(umgy_reg_effect)
    
    put_log1("Function: reg_tune_user_movie_genre_year_effect
rmse_tmp: %1", rmse_tmp)
    rmses_tmp[i] <- rmse_tmp
    
    put_log2("Function: reg_tune_user_movie_genre_year_effect
rmses_tmp[%1]: %2", i, rmses_tmp[i])
    put_log1("Function: reg_tune_user_movie_genre_year_effect
rmses_tmp length: %1", length(rmses_tmp))
    print(rmses_tmp)
    
    plot(lambdas_tmp[rmses_tmp > 0], rmses_tmp[rmses_tmp > 0])
  }
  
  rmses_tmp
}

##### Training Date (Year) Effect Model ----------------------------------------
file_name_tmp <- "date-global-and-year-effects.RData"
file_path_tmp <- file.path(models_data_path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading Year Effect data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Year Effect data has been loaded from file: %1", file_path_tmp)
  
} else {
  date_global_effect <- calc_date_global_effect.cv()
  str(date_global_effect)
  
  date_year_effect <- train_date_year_effect.cv()
  str(date_year_effect)

  put_log1("Saving Year Effect data has been saved to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effects,
       user_movie_effect,
       genre_mean_ratings,
       user_movie_genre_effect,
       user_movie_genre_reg_lambdas_6p6_m4p2_p2,
       user_movie_genre_reg_RMSEs_m66_42_0_2,
       umgy_tune_sets,
       date_global_effect,
       date_year_effect,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("Year Effect data has been saved to file: %1", 
           file_path_tmp)
} 

##### Compute Date (Year) Effect Model RMSE ------------------------------------
date_year_effect_RMSE <- calc_date_year_effect_RMSE.cv(date_year_effect)
date_year_effect_RMSE
#> [1] 0.8590795
##### Add a row to the RMSE Result Tibble for the User+Movie+Genre+Date (Year) Effect Model ---- 
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("User+Movie+Genre+Year Effect Model", 
               date_year_effect_RMSE)

RMSE_kable(RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble for the `User+Movie+Genre+Year Effect Model`.")

#### Close Log -----------------------------------------------------------------
log_close()


### Regularizing User+Movie+Genre+Year Effects ---------------------------------
#### Open log -------------------------------------------------------------------
open_logfile(".tuning-umgy-effect-lambda_m1000_0_100")

# Let's take a look at the Average rating per year:

# lamdas = seq(-1000, 0, 100) --------------------------------------------------
umgy_reg_RMSEs_m1000_0_100_file <- 
  file.path(data_path,"umgy_reg_RMSEs_m1000_0_100.RData")

rm(umgy_reg_RMSEs_m1000_0_100)

lambdas_m1000_0_100 <- seq(-1000, 0, 100)
file_path_tmp <- umgy_reg_RMSEs_m1000_0_100_file
RMSEs_tmp <- numeric()

if (file.exists(file_path_tmp)) {
  put_log("Loading tuning data from file...")
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log("Tuning data has been loaded from file.")
  numeric()
} else {
  RMSEs_tmp <- reg_tune_user_movie_genre_year_effect(lambdas_m1000_0_100)
}

if (length(RMSEs_tmp) > 1) {
  umgy_reg_RMSEs_m1000_0_100 <- RMSEs_tmp
  
  save(lambdas_m1000_0_100,
       umgy_reg_RMSEs_m1000_0_100,
       file = file_path_tmp)
}
rm(file_path_tmp)

plot(lambdas_m1000_0_100, umgy_reg_RMSEs_m1000_0_100)

lambdas_m1000_0_100_best_results <- 
  get_reg_best_params(lambdas_m1000_0_100, 
                      umgy_reg_RMSEs_m1000_0_100)

# Plot a histogram of the RMSE distribution 
#par(cex = 0.7)
# hist_RMSEs <- hist(umgy_reg_RMSEs_m1000_0_100, 30, xlab = "RMSE",
#                    main = TeX(r'[Histogram of RMSE]'))

put_log("A histogram of the RMSE distribution has been plotted.")

n_max_rmse <- hist_RMSEs$breaks[which.max(hist_RMSEs$density)]
n_max_rmse

lambdas_m1000_0_100_best_results
# best_lambda    best_RMSE 
# -100.0000000    0.8580053 

best_rmse <- min(umgy_reg_RMSEs_m1000_0_100)
best_rmse
#> [1] 0.8580053

# rmse_threshold <- best_rmse + (n_max_rmse - best_rmse)/2
# rmse_threshold
# 
# RMSE_localMin_ind <- umgy_reg_RMSEs_m1000_0_100 < rmse_threshold
# RMSE_localMin_ind
# 
# RMSE_localMin <- umgy_reg_RMSEs_m1000_0_100[RMSE_localMin_ind]
# RMSE_localMin
# 
# lambda_localMin <- lambdas_m1000_0_100[RMSE_localMin_ind]
# lambda_localMin

#### Close Log -----------------------------------------------------------------
log_close()



#### Open log -------------------------------------------------------------------
open_logfile(".tuning-umg-year-effect-in-loop_m1000_0_100")

# lamdas in loop starting from results of `lambdas = seq(-1000, 0, 100)` -------

lambdas <- lambdas_m1000_0_100
lambda_rmses <- umgy_reg_RMSEs_m1000_0_100 

repeat{ 
  rmses_min_ind <- which.min(lambda_rmses)
  rmse_min <- min(lambda_rmses)
  print(rmse_min)
  
  if(sum(lambda_rmses > rmse_min) == 0){
    put_log1("Reached best RMSE: %1", rmse_min)
    break
  }
  
  seq_start_ind <- rmses_min_ind - 1
  
  if (seq_start_ind < 1) {
    seq_start_ind <- 1
    warning("`lambdas` index too small, so it assigned a value %1.",
            seq_start_ind)
  }
  
  seq_end_ind <- rmses_min_ind + 1
  
  if (length(lambdas) < seq_end_ind) {
    warning("Index too large.")
    seq_end_ind <- rmses_min_ind
    put_log1("Index exeeded the length of `lambdas`, 
            so it is set to maximum possible value of %1",
            seq_end_ind)
  }
  
  if (seq_end_ind - seq_start_ind == 0) {
    put_log1("Reached best RMSE: %1", rmse_min)
    
    put_log2("Final lambda (%1) best RMSE: %2",
             lambdas_best_results["best_lambda"],
             lambdas_best_results["best_RMSE"])
    
    put(lambdas_best_results)
    break
  }
  
  lambda_rmses <- numeric()
  seq_start <- lambdas[seq_start_ind]
  seq_end <- lambdas[seq_end_ind]
  seq_increment <- (seq_end - seq_start)/32 
  
  if (seq_increment < 0.0000000000001) {
    warning("lambda increment is too small.")
    
    put_log2("Final lambda (%1) best RMSE: %2",
             lambdas_best_results["best_lambda"],
             lambdas_best_results["best_RMSE"])
    
    # Final lambda (-75) best RMSE: 0.857852155782141

    put(lambdas_best_results)
    # best_lambda   best_RMSE 
    # -75.0000000   0.8578522 
    break
  }
  
  lambdas <- seq(seq_start, seq_end, seq_increment)
  
  file_name_tmp <- "umgy_reg-loop-from-m1000-0-100_" |>
    str_c(as.character(seq_start)) |>
    str_c("-") |>
    str_c(as.character(seq_end)) |>
    str_c("-") |>
    str_c(as.character(seq_increment)) |>
    str_c(".RData")
  
  file_path_tmp <- file.path(regularization_data_path, file_name_tmp)
  
  put_log1("File path generated: %1", file_path_tmp)
  
  if (file.exists(file_path_tmp)) {
    put_log1("Loading tuning data from file: %1...", file_path_tmp)
    start <- put_start_date()
    load(file_path_tmp)
    put_end_date(start)
    put_log1("Tuning data has been loaded from file: %1", file_path_tmp)
    lambdas <- umgy_reg_lambdas
    lambda_rmses <- umgy_reg_RMSEs
  } else {
    lambda_rmses <- reg_tune_user_movie_genre_year_effect(lambdas)
    umgy_reg_RMSEs <- lambda_rmses
    umgy_reg_lambdas <- lambdas
    
    save(umgy_reg_lambdas,
         umgy_reg_RMSEs,
         file = file_path_tmp)
    put_log1("File saved: %1", file_path_tmp)
  }
  
  plot(lambdas, lambda_rmses)
  
  lambdas_best_results <- 
    get_reg_best_params(lambdas, 
                        lambda_rmses)
  put_log2("Current lambda (%1) best RMSE: %2",
           lambdas_best_results["best_lambda"],
           lambdas_best_results["best_RMSE"])
  
  put(lambdas_best_results)
  
  # Plot a histogram of the RMSE distribution 
  #par(cex = 0.7)
  # hist_RMSEs <- hist(umgy_reg_RMSEs_0_1000_100, 30, xlab = "RMSE",
  #                    main = TeX(r'[Histogram of RMSE]'))
  
  # put_log("A histogram of the RMSE distribution has been plotted.")
  # 
  # n_max_rmse <- hist_RMSEs$breaks[which.max(hist_RMSEs$density)]
  # n_max_rmse
}

# stop("Procedure Completed")

# best_lambda   best_RMSE 
# -75.0000000   0.8578522 


#### Close Log -----------------------------------------------------------------
log_close()

#### Open log -------------------------------------------------------------------
open_logfile(".tuning-umgy-effect-lambda_m1000_0_100")

# Let's take a look at the Average rating per year:

# lamdas = seq(-1000, 0, 100) --------------------------------------------------
umgy_reg_RMSEs_m1000_0_100_file <- 
  file.path(data_path,"umgy_reg_RMSEs_m1000_0_100.RData")

rm(umgy_reg_RMSEs_m1000_0_100)

lambdas_m1000_0_100 <- seq(-1000, 0, 100)
file_path_tmp <- umgy_reg_RMSEs_m1000_0_100_file
RMSEs_tmp <- numeric()

if (file.exists(file_path_tmp)) {
  put_log("Loading tuning data from file...")
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log("Tuning data has been loaded from file.")
  numeric()
} else {
  RMSEs_tmp <- reg_tune_user_movie_genre_year_effect(lambdas_m1000_0_100)
}

if (length(RMSEs_tmp) > 1) {
  umgy_reg_RMSEs_m1000_0_100 <- RMSEs_tmp
  
  save(lambdas_m1000_0_100,
       umgy_reg_RMSEs_m1000_0_100,
       file = file_path_tmp)
}
rm(file_path_tmp)

plot(lambdas_m1000_0_100, umgy_reg_RMSEs_m1000_0_100)

lambdas_m1000_0_100_best_results <- 
  get_reg_best_params(lambdas_m1000_0_100, 
                      umgy_reg_RMSEs_m1000_0_100)

# Plot a histogram of the RMSE distribution 
#par(cex = 0.7)
# hist_RMSEs <- hist(umgy_reg_RMSEs_m1000_0_100, 30, xlab = "RMSE",
#                    main = TeX(r'[Histogram of RMSE]'))

put_log("A histogram of the RMSE distribution has been plotted.")

n_max_rmse <- hist_RMSEs$breaks[which.max(hist_RMSEs$density)]
n_max_rmse

lambdas_m1000_0_100_best_results
# best_lambda    best_RMSE 
# -100.0000000    0.8580053 

best_rmse <- min(umgy_reg_RMSEs_m1000_0_100)
best_rmse
#> [1] 0.8580053

# rmse_threshold <- best_rmse + (n_max_rmse - best_rmse)/2
# rmse_threshold
# 
# RMSE_localMin_ind <- umgy_reg_RMSEs_m1000_0_100 < rmse_threshold
# RMSE_localMin_ind
# 
# RMSE_localMin <- umgy_reg_RMSEs_m1000_0_100[RMSE_localMin_ind]
# RMSE_localMin
# 
# lambda_localMin <- lambdas_m1000_0_100[RMSE_localMin_ind]
# lambda_localMin

#### Close Log -----------------------------------------------------------------
log_close()



#### Open log -------------------------------------------------------------------
open_logfile(".tuning-umg-year-effect-experiments")


# lamdas = seq(-10, 0, 0.1) ------------------------------------------------------------

umgy_reg_RMSEs_m10_0_0_1_file <- file.path(data_path,"umgy_reg_RMSEs_m10_0_0_1.RData")
lambdas_m10_0_0_1 <- seq(-10, 0, 0.1)
rm(umgy_reg_RMSEs_m10_0_0_1)

file_path_tmp <- umgy_reg_RMSEs_m10_0_0_1_file

# if (file.exists(file_path_tmp)) {
#   put_log1("Loading tuning data from file: %1...", file_path_tmp)
#   start <- put_start_date()
#   load(file_path_tmp)
#   put_end_date(start)
#   put_log1("Tuning data has been loaded from file: %1", file_path_tmp)
#   lambdas <- umgy_reg_lambdas
#   lambda_rmses <- umgy_reg_RMSEs
# } else {
#   lambda_rmses <- reg_tune_user_movie_genre_year_effect(lambdas)
#   umgy_reg_RMSEs <- lambda_rmses
#   umgy_reg_lambdas <- lambdas
#   
#   save(umgy_reg_lambdas,
#        umgy_reg_RMSEs,
#        file = file_path_tmp)
#   put_log1("File saved: %1", file_path_tmp)
# }


RMSEs_tmp <- reqularize_umgy_effect(lambdas_m10_0_0_1,
                                    umgy_reg_RMSEs_m10_0_0_1_file)

if (length(RMSEs_tmp) > 1) {
  umgy_reg_RMSEs_m10_0_0_1 <- RMSEs_tmp

    save(lambdas_m10_0_0_1,
       umgy_reg_RMSEs_m10_0_0_1,
       file = umgy_reg_RMSEs_m10_0_0_1_file)
}

plot(lambdas_m10_0_0_1, umgy_reg_RMSEs_m10_0_0_1)

lambdas_m10_0_0_1_best_results <- 
  get_reg_best_params(lambdas_m10_0_0_1, 
                      umgy_reg_RMSEs_m10_0_0_1)


# Plot a histogram of the RMSE distribution 
#par(cex = 0.7)
hist_RMSEs <- hist(umgy_reg_RMSEs_m10_0_0_1, 30, xlab = "RMSE",
     main = TeX(r'[Histogram of RMSE]'))

put_log("A histogram of the RMSE distribution has been plotted.")

n_max_rmse <- hist_RMSEs$breaks[which.max(hist_RMSEs$density)]
n_max_rmse

lambdas_m10_0_0_1_best_results
# best_lambda   best_RMSE 
#  -8.1000000   0.8579123 

best_rmse <- min(umgy_reg_RMSEs_m10_0_0_1)
best_rmse
rmse_threshold <- best_rmse + (n_max_rmse - best_rmse)/2
rmse_threshold

RMSE_localMin_ind <- umgy_reg_RMSEs_m10_0_0_1 < rmse_threshold
RMSE_localMin_ind

RMSE_localMin <- umgy_reg_RMSEs_m10_0_0_1[RMSE_localMin_ind]
RMSE_localMin

lambda_localMin <- lambdas_m10_0_0_1[RMSE_localMin_ind]
lambda_localMin

# lamdas = seq(0, 5, 0.1) ------------------------------------------------------------
umgy_reg_RMSEs_0_5_p1_file <- file.path(data_path,"umgy_reg_RMSEs_0_5_p1.RData")
lambdas_0_5_p1 <- seq(0, 5, 0.1)
rm(umgy_reg_RMSEs_0_5_p1)

RMSEs_tmp <- reqularize_umgy_effect(lambdas_0_5_p1,
                                    umgy_reg_RMSEs_0_5_p1_file)

if (length(RMSEs_tmp) > 1) {
  umgy_reg_RMSEs_0_5_p1 <- RMSEs_tmp

    save(lambdas_0_5_p1,
       umgy_reg_RMSEs_0_5_p1,
       file = umgy_reg_RMSEs_0_5_p1_file)
}

plot(lambdas_0_5_p1, umgy_reg_RMSEs_0_5_p1)

lambdas_0_5_p1_best_results <- 
  get_reg_best_params(lambdas_0_5_p1, 
                      umgy_reg_RMSEs_0_5_p1)


# Plot a histogram of the RMSE distribution 
#par(cex = 0.7)
# hist_RMSEs <- hist(umgy_reg_RMSEs_0_5_p1, 30, xlab = "RMSE",
#      main = TeX(r'[Histogram of RMSE]'))

put_log("A histogram of the RMSE distribution has been plotted.")

n_max_rmse <- hist_RMSEs$breaks[which.max(hist_RMSEs$density)]
n_max_rmse

lambdas_0_5_p1_best_results
# best_lambda   best_RMSE 
#    5.000000    0.858092

best_rmse <- min(umgy_reg_RMSEs_0_5_p1)
best_rmse
rmse_threshold <- best_rmse + (n_max_rmse - best_rmse)/2
rmse_threshold

RMSE_localMin_ind <- umgy_reg_RMSEs_0_5_p1 < rmse_threshold
RMSE_localMin_ind

RMSE_localMin <- umgy_reg_RMSEs_0_5_p1[RMSE_localMin_ind]
RMSE_localMin

lambda_localMin <- lambdas_0_5_p1[RMSE_localMin_ind]
lambda_localMin

# lamdas = seq(4, 20, 0.2) ------------------------------------------------------------
umgy_reg_RMSEs_4_20_p2_file <- file.path(data_path,"umgy_reg_RMSEs_4_20_p2.RData")
lambdas_4_20_p2 <- seq(4, 20, 0.2)
rm(umgy_reg_RMSEs_4_20_p2)

RMSEs_tmp <- reqularize_umgy_effect(lambdas_4_20_p2,
                                    umgy_reg_RMSEs_4_20_p2_file)

if (length(RMSEs_tmp) > 1) {
  umgy_reg_RMSEs_4_20_p2 <- RMSEs_tmp

    save(lambdas_4_20_p2,
       umgy_reg_RMSEs_4_20_p2,
       file = umgy_reg_RMSEs_4_20_p2_file)
}

plot(lambdas_4_20_p2, umgy_reg_RMSEs_4_20_p2)

lambdas_4_20_p2_best_results <- 
  get_reg_best_params(lambdas_4_20_p2, 
                      umgy_reg_RMSEs_4_20_p2)


# Plot a histogram of the RMSE distribution 
#par(cex = 0.7)
# hist_RMSEs <- hist(umgy_reg_RMSEs_4_20_p2, 30, xlab = "RMSE",
#      main = TeX(r'[Histogram of RMSE]'))

put_log("A histogram of the RMSE distribution has been plotted.")

n_max_rmse <- hist_RMSEs$breaks[which.max(hist_RMSEs$density)]
n_max_rmse

lambdas_4_20_p2_best_results
# best_lambda   best_RMSE 
#   20.000000    0.858021 

best_rmse <- min(umgy_reg_RMSEs_4_20_p2)
best_rmse
rmse_threshold <- best_rmse + (n_max_rmse - best_rmse)/2
rmse_threshold

RMSE_localMin_ind <- umgy_reg_RMSEs_4_20_p2 < rmse_threshold
RMSE_localMin_ind

RMSE_localMin <- umgy_reg_RMSEs_4_20_p2[RMSE_localMin_ind]
RMSE_localMin

lambda_localMin <- lambdas_4_20_p2[RMSE_localMin_ind]
lambda_localMin

# lamdas = seq(16, 40, 2) ------------------------------------------------------------

umgy_reg_RMSEs_16_40_2_file <- file.path(data_path,"umgy_reg_RMSEs_16_40_2.RData")
lambdas_16_40_2 <- seq(16, 40, 2)
rm(umgy_reg_RMSEs_16_40_2)

RMSEs_tmp <- reqularize_umgy_effect(lambdas_16_40_2,
                                    umgy_reg_RMSEs_16_40_2_file)

if (length(RMSEs_tmp) > 1) {
  umgy_reg_RMSEs_16_40_2 <- RMSEs_tmp
  
  save(lambdas_16_40_2,
       umgy_reg_RMSEs_16_40_2,
       file = umgy_reg_RMSEs_16_40_2_file)
}

plot(lambdas_16_40_2, umgy_reg_RMSEs_16_40_2)

lambdas_16_40_2_best_results <- 
  get_reg_best_params(lambdas_16_40_2, 
                      umgy_reg_RMSEs_16_40_2)


# Plot a histogram of the RMSE distribution 
#par(cex = 0.7)
# hist_RMSEs <- hist(umgy_reg_RMSEs_16_40_2, 30, xlab = "RMSE",
#                    main = TeX(r'[Histogram of RMSE]'))

put_log("A histogram of the RMSE distribution has been plotted.")

n_max_rmse <- hist_RMSEs$breaks[which.max(hist_RMSEs$density)]
n_max_rmse

lambdas_16_40_2_best_results
# best_lambda   best_RMSE 
#  -8.1000000   0.8579123 

best_rmse <- min(umgy_reg_RMSEs_16_40_2)
best_rmse
rmse_threshold <- best_rmse + (n_max_rmse - best_rmse)/2
rmse_threshold

RMSE_localMin_ind <- umgy_reg_RMSEs_16_40_2 < rmse_threshold
RMSE_localMin_ind

RMSE_localMin <- umgy_reg_RMSEs_16_40_2[RMSE_localMin_ind]
RMSE_localMin

lambda_localMin <- lambdas_16_40_2[RMSE_localMin_ind]
lambda_localMin



# lamdas = seq(36, 100, 2) ------------------------------------------------------------
umgy_reg_RMSEs_36_100_2_file <- file.path(data_path,"umgy_reg_RMSEs_36_100_2.RData")
rm(umgy_reg_RMSEs_36_100_2)
lambdas_36_100_2 <- seq(36, 100, 2)

RMSEs_tmp <- reqularize_umgy_effect(lambdas_36_100_2,
                                    umgy_reg_RMSEs_36_100_2_file)

if (length(RMSEs_tmp) > 1) {
  umgy_reg_RMSEs_36_100_2 <- RMSEs_tmp
  
  save(lambdas_36_100_2,
       umgy_reg_RMSEs_36_100_2,
       file = umgy_reg_RMSEs_36_100_2_file)
}

plot(lambdas_36_100_2, umgy_reg_RMSEs_36_100_2)

lambdas_36_100_2_best_results <- 
  get_reg_best_params(lambdas_36_100_2, 
                      umgy_reg_RMSEs_36_100_2)


# Plot a histogram of the RMSE distribution 
#par(cex = 0.7)
# hist_RMSEs <- hist(umgy_reg_RMSEs_36_100_2, 30, xlab = "RMSE",
#                    main = TeX(r'[Histogram of RMSE]'))

put_log("A histogram of the RMSE distribution has been plotted.")

n_max_rmse <- hist_RMSEs$breaks[which.max(hist_RMSEs$density)]
n_max_rmse

lambdas_36_100_2_best_results
# best_lambda   best_RMSE 
#  -8.1000000   0.8579123 

best_rmse <- min(umgy_reg_RMSEs_36_100_2)
best_rmse
rmse_threshold <- best_rmse + (n_max_rmse - best_rmse)/2
rmse_threshold

RMSE_localMin_ind <- umgy_reg_RMSEs_36_100_2 < rmse_threshold
RMSE_localMin_ind

RMSE_localMin <- umgy_reg_RMSEs_36_100_2[RMSE_localMin_ind]
RMSE_localMin

lambda_localMin <- lambdas_36_100_2[RMSE_localMin_ind]
lambda_localMin



#### Close Log -----------------------------------------------------------------
log_close()

#### Open log -------------------------------------------------------------------
open_logfile(".tuning-umgy-effect-lambda_0_1000_100")

# Let's take a look at the Average rating per year:


# lamdas = seq(0, 1000, 100) ------------------------------------------------------------
umgy_reg_RMSEs_0_1000_100_file <- 
  file.path(data_path,"umgy_reg_RMSEs_0_1000_100.RData")

rm(umgy_reg_RMSEs_0_1000_100)

lambdas_0_1000_100 <- seq(0, 1000, 100)
file_path_tmp <- umgy_reg_RMSEs_0_1000_100_file
RMSEs_tmp <- numeric()

if (file.exists(file_path_tmp)) {
  put_log("Loading tuning data from file...")
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log("Tuning data has been loaded from file.")
  numeric()
} else {
  RMSEs_tmp <- reg_tune_user_movie_genre_year_effect(lambdas_0_1000_100)
}

if (length(RMSEs_tmp) > 1) {
  umgy_reg_RMSEs_0_1000_100 <- RMSEs_tmp
  
  save(lambdas_0_1000_100,
       umgy_reg_RMSEs_0_1000_100,
       file = file_path_tmp)
}
rm(file_path_tmp)

plot(lambdas_0_1000_100, umgy_reg_RMSEs_0_1000_100)

lambdas_0_1000_100_best_results <- 
  get_reg_best_params(lambdas_0_1000_100, 
                      umgy_reg_RMSEs_0_1000_100)

# Plot a histogram of the RMSE distribution 
#par(cex = 0.7)
# hist_RMSEs <- hist(umgy_reg_RMSEs_0_1000_100, 30, xlab = "RMSE",
#                    main = TeX(r'[Histogram of RMSE]'))

put_log("A histogram of the RMSE distribution has been plotted.")

n_max_rmse <- hist_RMSEs$breaks[which.max(hist_RMSEs$density)]
n_max_rmse

lambdas_0_1000_100_best_results
# best_lambda   best_RMSE 
#  200.000000    0.857932 

best_rmse <- min(umgy_reg_RMSEs_0_1000_100)
best_rmse

# rmse_threshold <- best_rmse + (n_max_rmse - best_rmse)/2
# rmse_threshold
# 
# RMSE_localMin_ind <- umgy_reg_RMSEs_0_1000_100 < rmse_threshold
# RMSE_localMin_ind
# 
# RMSE_localMin <- umgy_reg_RMSEs_0_1000_100[RMSE_localMin_ind]
# RMSE_localMin
# 
# lambda_localMin <- lambdas_0_1000_100[RMSE_localMin_ind]
# lambda_localMin


#### Open log -------------------------------------------------------------------
open_logfile(".tuning-umg-year-effect-in-loop_0_1000_100")

# lamdas in loop starting from results of `lambdas = seq(0, 1000, 100)` ----------

lambdas <- lambdas_0_1000_100
lambda_rmses <- umgy_reg_RMSEs_0_1000_100 

repeat{ 
  rmses_min_ind <- which.min(lambda_rmses)
  rmse_min <- min(lambda_rmses)
  print(rmse_min)
  
  if(sum(lambda_rmses > rmse_min) == 0){
    put_log1("Reached best RMSE: %1", rmse_min)
    break
  }

  seq_start_ind <- rmses_min_ind - 1
  
  if (seq_start_ind < 1) {
    seq_start_ind <- 1
    warning("`lambdas` index too small, so it assigned a value %1.",
            seq_start_ind)
  }
  
  seq_end_ind <- rmses_min_ind + 1
  
  if (length(lambdas) < seq_end_ind) {
    warning("Index too large.")
    seq_end_ind <- rmses_min_ind
    put_log1("Index exeeded the length of `lambdas`, 
            so it is set to maximum possible value of %1",
            seq_end_ind)
  }
  
  if (seq_end_ind - seq_start_ind == 0) {
    put_log1("Reached best RMSE: %1", rmse_min)

    put_log2("Final lambda (%1) best RMSE: %2",
             lambdas_best_results["best_lambda"],
             lambdas_best_results["best_RMSE"])
    
    put(lambdas_best_results)
    break
  }

  lambda_rmses <- numeric()
  seq_start <- lambdas[seq_start_ind]
  seq_end <- lambdas[seq_end_ind]
  seq_increment <- (seq_end - seq_start)/32 
  
  if (seq_increment < 0.0000000000001) {
    warning("lambda increment is too small.")
    
    put_log2("Final lambda (%1) best RMSE: %2",
             lambdas_best_results["best_lambda"],
             lambdas_best_results["best_RMSE"])

    # Final lambda (170.425100252312) best RMSE: 0.857931162318245    
    
    put(lambdas_best_results)
    # best_lambda   best_RMSE 
    # 170.4251003   0.8579312 
    
    break
  }
  
  lambdas <- seq(seq_start, seq_end, seq_increment)
  
  file_name_tmp <- "umgy_reg-loop-from-0_1000_100_" |>
    str_c(as.character(seq_start)) |>
    str_c("-") |>
    str_c(as.character(seq_end)) |>
    str_c("-") |>
    str_c(as.character(seq_increment)) |>
    str_c(".RData")
  
  file_path_tmp <- file.path(regularization_data_path, file_name_tmp)
  
  put_log1("File path generated: %1", file_path_tmp)
  
  if (file.exists(file_path_tmp)) {
    put_log1("Loading tuning data from file: %1...", file_path_tmp)
    start <- put_start_date()
    load(file_path_tmp)
    put_end_date(start)
    put_log1("Tuning data has been loaded from file: %1", file_path_tmp)
    lambdas <- umgy_reg_lambdas
    lambda_rmses <- umgy_reg_RMSEs
  } else {
    lambda_rmses <- reg_tune_user_movie_genre_year_effect(lambdas)
    umgy_reg_RMSEs <- lambda_rmses
    umgy_reg_lambdas <- lambdas
    
    save(umgy_reg_lambdas,
         umgy_reg_RMSEs,
         file = file_path_tmp)
    put_log1("File saved: %1", file_path_tmp)
  }
  
  plot(lambdas, lambda_rmses)
  
  lambdas_best_results <- 
    get_reg_best_params(lambdas, 
                        lambda_rmses)
  put_log2("Current lambda (%1) best RMSE: %2",
           lambdas_best_results["best_lambda"],
           lambdas_best_results["best_RMSE"])
  
  put(lambdas_best_results)
  
  # Plot a histogram of the RMSE distribution 
  #par(cex = 0.7)
  # hist_RMSEs <- hist(umgy_reg_RMSEs_0_1000_100, 30, xlab = "RMSE",
  #                    main = TeX(r'[Histogram of RMSE]'))
  
  # put_log("A histogram of the RMSE distribution has been plotted.")
  # 
  # n_max_rmse <- hist_RMSEs$breaks[which.max(hist_RMSEs$density)]
  # n_max_rmse
}

# stop("Procedure Completed")

# best_lambda   best_RMSE 
# 170.4251003   0.8579312 


#### Close Log -----------------------------------------------------------------
log_close()
#### Open log -------------------------------------------------------------------
open_logfile(".tuning-umgy-effect-loop_m120_200_d64")

# lamdas in loop starting from r`lambdas = seq(-300, 400, (400+300)/140)` -------
loop_starter <- c(-300,0,400)
lambdas <- loop_starter
lambda_rmses <- c(0,-1,0)
range_divider <- 140 

repeat{ 
  rmses_min_ind <- which.min(lambda_rmses)
  rmse_min <- min(lambda_rmses)
  print(rmse_min)
  
  if (rmse_min >= 0) {
    put_log2("Current lambda (%1) minimal RMSE: %2",
             lambdas[rmses_min_ind],
             rmse_min)
  }
  
  if(sum(lambda_rmses > rmse_min) == 0){
    put_log1("Reached best RMSE: %1", rmse_min)
    break
  }
  
  seq_start_ind <- rmses_min_ind - 1
  
  if (seq_start_ind < 1) {
    seq_start_ind <- 1
    warning("`lambdas` index too small, so it assigned a value %1.",
            seq_start_ind)
  }
  
  seq_end_ind <- rmses_min_ind + 1
  
  if (length(lambdas) < seq_end_ind) {
    warning("Index too large.")
    seq_end_ind <- rmses_min_ind
    put_log1("Index exeeded the length of `lambdas`, 
            so it is set to maximum possible value of %1",
            seq_end_ind)
  }
  
  if (seq_end_ind - seq_start_ind == 0) {
    put_log1("Reached best RMSE: %1", rmse_min)
    
    put_log2("Final lambda (%1) best RMSE: %2",
             lambdas_best_results["best_lambda"],
             lambdas_best_results["best_RMSE"])
    
    put(lambdas_best_results)
    break
  }
  
  lambda_rmses <- numeric()
  seq_start <- lambdas[seq_start_ind]
  seq_end <- lambdas[seq_end_ind]
  seq_increment <- (seq_end - seq_start)/range_divider 
  
  if (seq_increment < 0.0000000000001) {
    warning("lambda increment is too small.")
    
    put_log2("Final lambda (%1) best RMSE: %2",
             lambdas_best_results["best_lambda"],
             lambdas_best_results["best_RMSE"])
    
    # Final lambda (-75) best RMSE: 0.857852155782141    
    
    put(lambdas_best_results)
    # best_lambda   best_RMSE 
    # -75.0000000   0.8578522    
    break
  }
  
  lambdas <- seq(seq_start, seq_end, seq_increment)
  
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
  
  put_log1("File path generated: %1", file_path_tmp)
  
  if (file.exists(file_path_tmp)) {
    put_log1("Loading tuning data from file: %1...", file_path_tmp)
    start <- put_start_date()
    load(file_path_tmp)
    put_end_date(start)
    put_log1("Tuning data has been loaded from file: %1", file_path_tmp)
    lambdas <- umgy_reg_lambdas
    lambda_rmses <- umgy_reg_RMSEs
  } else {
    lambda_rmses <- reg_tune_user_movie_genre_year_effect(lambdas)
    umgy_reg_RMSEs <- lambda_rmses
    umgy_reg_lambdas <- lambdas
    
    save(umgy_reg_lambdas,
         umgy_reg_RMSEs,
         file = file_path_tmp)
    put_log1("File saved: %1", file_path_tmp)
  }
  
  plot(umgy_reg_lambdas, umgy_reg_RMSEs)
  
  lambdas_best_results <- 
    get_reg_best_params(lambdas, 
                        lambda_rmses)
  put_log2("Current lambda (%1) best RMSE: %2",
           lambdas_best_results["best_lambda"],
           lambdas_best_results["best_RMSE"])
  
  put(lambdas_best_results)
  
  # Plot a histogram of the RMSE distribution 
  #par(cex = 0.7)
  # hist_RMSEs <- hist(umgy_reg_RMSEs_0_1000_100, 30, xlab = "RMSE",
  #                    main = TeX(r'[Histogram of RMSE]'))
  
  # put_log("A histogram of the RMSE distribution has been plotted.")
  # 
  # n_max_rmse <- hist_RMSEs$breaks[which.max(hist_RMSEs$density)]
  # n_max_rmse
}

# stop("Procedure Completed")

# best_lambda   best_RMSE 
# -75.0000000   0.8578522 

#### Close Log -----------------------------------------------------------------
log_close()


#### Open log -------------------------------------------------------------------
open_logfile(".continue-tuning-umg-date-effect")

# First minimum (lambda = -9) --------------------------------------------------
# lamdas = seq(-9.2, -8.8, 0.01) ------------------------------------------------------------
umgy_reg_RMSEs_m92_m88_p01_file <- file.path(data_path,"umgy_reg_RMSEs_m92_m88_p01.RData")
lambdas_m92_m88_p01 <- seq(-9.2, -8.8, 0.01)

RMSEs_tmp <- reqularize_umgy_effect(lambdas_m92_m88_p01,
                                    umgy_reg_RMSEs_m92_m88_p01_file)

if (length(RMSEs_tmp) > 1) {
  umgy_reg_RMSEs_m92_m88_p01 <- RMSEs_tmp
  
  save(lambdas_m92_m88_p01,
       umgy_reg_RMSEs_m92_m88_p01,
       file = umgy_reg_RMSEs_m92_m88_p01_file)
}

plot(lambdas_m92_m88_p01, umgy_reg_RMSEs_m92_m88_p01)

lambdas_m92_m88_p01_best_results <- 
  get_reg_best_params(lambdas_m92_m88_p01, 
                      umgy_reg_RMSEs_m92_m88_p01)

lambdas_m92_m88_p01_best_results
# best_lambda   best_RMSE 
#  -9.0000000   0.8488281 

# lambdas = seq(-9.04, -8.9, 0.001) ------------------------------------------------------------
umgy_reg_RMSEs_m904_m89_p001_file <- file.path(data_path,"umgy_reg_RMSEs_m904_m89_p001.RData")
lambdas_m904_m89_p001 <- seq(-9.04, -8.9, 0.001)

RMSEs_tmp <- reqularize_umgy_effect(lambdas_m904_m89_p001,
                                    umgy_reg_RMSEs_m904_m89_p001_file)

if (length(RMSEs_tmp) > 1) {
  umgy_reg_RMSEs_m904_m89_p001 <- RMSEs_tmp
  
  save(lambdas_m904_m89_p001,
       umgy_reg_RMSEs_m904_m89_p001,
       file = umgy_reg_RMSEs_m904_m89_p001_file)
}

plot(lambdas_m904_m89_p001, umgy_reg_RMSEs_m904_m89_p001)

lambdas_m904_m89_p001_best_results <- 
  get_reg_best_params(lambdas_m904_m89_p001, 
                      umgy_reg_RMSEs_m904_m89_p001)

lambdas_m904_m89_p001_best_results


# lambdas = seq(-9.012, -8.98, 0.0001) ------------------------------------------------------------
umgy_reg_RMSEs_m9012_m898_p0001_file <- file.path(data_path,"umgy_reg_RMSEs_m9012_m898_p0001.RData")
lambdas_m9012_m898_p0001 <- seq(-9.012, -8.98, 0.0001)

RMSEs_tmp <- reqularize_umgy_effect(lambdas_m9012_m898_p0001,
                                    umgy_reg_RMSEs_m9012_m898_p0001_file)

if (length(RMSEs_tmp) > 1) {
  umgy_reg_RMSEs_m9012_m898_p0001 <- RMSEs_tmp
  
  save(lambdas_m9012_m898_p0001,
       umgy_reg_RMSEs_m9012_m898_p0001,
       file = umgy_reg_RMSEs_m9012_m898_p0001_file)
}

plot(lambdas_m9012_m898_p0001, umgy_reg_RMSEs_m9012_m898_p0001)

lambdas_m9012_m898_p0001_best_results <- 
  get_reg_best_params(lambdas_m9012_m898_p0001, 
                      umgy_reg_RMSEs_m9012_m898_p0001)

lambdas_m9012_m898_p0001_best_results

# lambdas = seq(-9.002, -8.998, 0.00001) ------------------------------------------------------------
umgy_reg_RMSEs_m9002_m8998_p00001_file <- 
  file.path(data_path,"umgy_reg_RMSEs_m9002_m8998_p00001.RData")

lambdas_m9002_m8998_p00001 <- seq(-9.002, -8.998, 0.00001)

RMSEs_tmp <- reqularize_umgy_effect(lambdas_m9002_m8998_p00001,
                                    umgy_reg_RMSEs_m9002_m8998_p00001_file)

if (length(RMSEs_tmp) > 1) {
  umgy_reg_RMSEs_m9002_m8998_p00001 <- RMSEs_tmp
  
  save(lambdas_m9002_m8998_p00001,
       umgy_reg_RMSEs_m9002_m8998_p00001,
       file = umgy_reg_RMSEs_m9002_m8998_p00001_file)
}

plot(lambdas_m9002_m8998_p00001, umgy_reg_RMSEs_m9002_m8998_p00001)

lambdas_m9002_m8998_p00001_best_results <- 
  get_reg_best_params(lambdas_m9002_m8998_p00001, 
                      umgy_reg_RMSEs_m9002_m8998_p00001)

lambdas_m9002_m8998_p00001_best_results
# best_lambda   best_RMSE 
#  -9.0000000   0.8488281 

# lambdas = seq(-9., -8., 0.0000) ------------------------------------------------------------
umgy_reg_RMSEs_m92_m88_p001_file <- file.path(data_path,"umgy_reg_RMSEs_m92_m88_p001.RData")
lambdas_m92_m88_p001 <- seq(-9.1, -8.9, 0.0001)

RMSEs_tmp <- reqularize_umgy_effect(lambdas_m92_m88_p001,
                                    umgy_reg_RMSEs_m92_m88_p001_file)

if (length(RMSEs_tmp) > 1) {
  umgy_reg_RMSEs_m92_m88_p001 <- RMSEs_tmp
  
  save(lambdas_m92_m88_p001,
       umgy_reg_RMSEs_m92_m88_p001,
       file = umgy_reg_RMSEs_m92_m88_p001_file)
}

plot(lambdas_m92_m88_p001, umgy_reg_RMSEs_m92_m88_p001)

lambdas_m92_m88_p001_best_results <- 
  get_reg_best_params(lambdas_m92_m88_p001, 
                      umgy_reg_RMSEs_m92_m88_p001)

lambdas_m92_m88_p001_best_results









#------------------------------------------------------------
# lambdas <- seq(-9.0000000000005, -8.9999999999995, 0.0000000000001) ----------
umgy_reg_RMSEs_m9p12_05_m8p12_95_p12_01_file <- 
  file.path(data_path,"umgy_reg_RMSEs_m9p12_05_m8p12_95_p12_01.RData")

lambdas_m9p12_05_m8p12_95_p12_01 <- 
  seq(-9.0000000000005, -8.9999999999995, 0.0000000000001)
# seq(-9.000,000,000,000,5, -8.999,999,999,999,5, 0.000,000,000,000,1)

file_path_tmp <- umgy_reg_RMSEs_m9p12_05_m8p12_95_p12_01_file

if (file.exists(file_path_tmp)) {
  put_log1("Loading tuning data from file: %1...", file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Tuning data has been loaded from file: %1", file_path_tmp)
  # lambdas <- umgy_reg_lambdas
  # lambda_rmses <- umgy_reg_RMSEs
} else {
  umgy_reg_RMSEs_m9p12_05_m8p12_95_p12_01 <- 
    reg_tune_user_movie_genre_year_effect(lambdas_m9p12_05_m8p12_95_p12_01)

  save(lambdas_m9p12_05_m8p12_95_p12_01,
       umgy_reg_RMSEs_m9p12_05_m8p12_95_p12_01,
       file = file_path_tmp)
  
  put_log1("File saved: %1", file_path_tmp)
}

plot(lambdas_m9p12_05_m8p12_95_p12_01, umgy_reg_RMSEs_m9p12_05_m8p12_95_p12_01)

lambdas_m9p12_05_m8p12_95_p12_01_best_results <- 
  get_reg_best_params(lambdas_m9p12_05_m8p12_95_p12_01, 
                      umgy_reg_RMSEs_m9p12_05_m8p12_95_p12_01)

lambdas_m9p12_05_m8p12_95_p12_01_best_results
# best_lambda   best_RMSE 
#  -9.0000000   0.8582678 

# Second minimum (lambda = -6) ---------------------------------------------------

# Third minimum (lambda = -5) ---------------------------------------------------

# 4th minimum (lambda = -4) ---------------------------------------------------

##### Re-training Regularized User+Movie+Year Effect Model for the `lambda = -9` ----
best_umgy_effect_lambda <- 
  lambdas_m9p12_05_m8p12_95_p12_01_best_results["best_lambda"]

put_log1("Re-training Regularized User+Movie Effect Model for the best `lambda`: %1...",
         best_umgy_effect_lambda)

# umgy_effect_best_lambda <- train_date_year_effect.cv(best_umgy_effect_lambda)

best_date_global_effect <- 
  calc_date_global_effect.cv(best_umgy_effect_lambda)

str(best_date_global_effect)
sum(is.na(best_date_global_effect$de))
#> [1] 0
sum(is.infinite(best_date_global_effect$de))
#> [1] 0

umgy_effect_best_lambda <- best_date_global_effect |>
  group_by(year) |>
  summarise(ye = mean(de, na.rm = TRUE))

str(umgy_effect_best_lambda)
umgy_effect_best_lambda

umgy_effect_best_lambda_RMSE <- calc_date_year_effect_RMSE.cv(umgy_effect_best_lambda)
umgy_effect_best_lambda_RMSE
#> [1] 0.8500172
#> [1] 0.8592063

put_log1("Regularized User+Movie Effect Model has been re-trained for the best `lambda`: %1.",
         best_umgy_effect_lambda)

##### Add a row to the RMSE Result Tibble for the User+Movie+Genre+Date (Year) Effects Model ---- 
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("Regularized User+Movie+Genre+Year Effect Model", 
               umgy_effect_best_lambda_RMSE)

RMSE_kable(RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble 
for the `Regularized User+Movie+Genre+Year Effect Model`.")

##### Compute Date Day Effects -------------------------------------------------
file_name_tmp <- "umg-year-day-effect.RData"
file_path_tmp <- file.path(models_data_path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading User+Movie+Year+Day Effect data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Year+Day Effect data has been loaded from file: %1", 
           file_path_tmp)
} else {
  year_day_effects <- date_global_effect |>
    left_join(umgy_effect_best_lambda, by = "year") |>
    mutate(de = de -   ye)
  
  put_log1("Saving User+Movie+Year+Day Effect data to file: %1", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effects,
       user_movie_effect,
       genre_mean_ratings,
       user_movie_genre_effect,
       user_movie_genre_reg_lambdas_6p6_m4p2_p2,
       user_movie_genre_reg_RMSEs_m66_42_0_2,
       umgy_tune_sets,
       date_global_effect,
       date_year_effect,
       year_day_effects,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Year+Day Effect data has been saved to file: %1", 
           file_path_tmp)
} 
put(str(year_day_effects))

##### Date Effect Computation Support Functions -----------------------
loess_de <- function(train_dat, degree = NA, span = NA){
  if(is.na(degree)) degree = 2
  if(is.na(span)) span = 0.75
  loess(de ~ days, span = span, degree = degree, data = train_dat)
}
compute_day_smoothed_effect <- function(degree = NA, span = NA){
  fit <- year_day_effects |> loess_de(degree, span)
  year_day_effects |> mutate(de_smoothed = fit$fitted)
}
day_smoothed_MSEs <- function(day_smoothed_effect){
  put_log1("Computing MSE values for the %1-Fold Cross Validation samples...", 
           CVFolds_N)
  
  start <- put_start_date()
  MSEs <- sapply(edx_CV, function(cv_fold_dat){
    cv_fold_dat$validation_set |>
      left_join(user_effects, by = "userId") |>
      left_join(user_movie_effect, by = "movieId") |>
      left_join(user_movie_genre_effect, by = "movieId") |>
      left_join(date_days_map, by = "timestamp") |>
      #left_join(date_year_effect, by='year') |>
      left_join(day_smoothed_effect, by='days') |>
      mutate(resid = rating - clamp(mu + a + b + g + ye + de_smoothed)) |> 
      filter(!is.na(resid)) |>
      pull(resid) |> mse()
  })
  put_end_date(start)
  put_log1("MSE values have been computed for the %1-Fold Cross Validation samples.", 
           CVFolds_N)
  MSEs
}
day_smoothed_effect_RMSE <- function(degree = NA, span = NA){
  day_smoothed_effect <- compute_day_smoothed_effect(degree, span) 
  MSEs <- day_smoothed_MSEs(day_smoothed_effect)
  sqrt(mean(MSEs))
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
best_rmse <- function(span_rmses){
  idx <- which.min(span_rmses$rmse)
  rmse <- c(span_rmses$span[idx], min(span_rmses$rmse))
  names(rmse) <- c("Span", "RMSE")
  rmse
}

##### Train model using `loess` function with default `span` & `degree` params----
put_log1("Training User+Movie+Genre+Day Smoothed Effect Model using `loess` function 
with default `span` & `degree` parameters for %1-Fold Cross Validation samples...",
CVFolds_N)

file_name_tmp <- "umg-year-day-smoothed-effect.RData"
file_path_tmp <- file.path(models_data_path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading User+Movie+Year+Day Smoothed Effect data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Year+Day Smoothed Effect data has been loaded from file: %1", 
           file_path_tmp)
  
} else {
  start <- put_start_date()
  day_smoothed_effect <- compute_day_smoothed_effect()
  str(day_smoothed_effect)
  put_end_date(start)
  put_log1("User+Movie+Genre+Date Effect Model has been trained
using `loess` function with default `span` & `degree` parameters
for the %1-Fold Cross Validation samples.",
          CVFolds_N)
  
  start <- put_start_date()
  save(mu,
       user_effects,
       user_movie_effect,
       genre_mean_ratings,
       user_movie_genre_effect,
       user_movie_genre_reg_lambdas_6p6_m4p2_p2,
       user_movie_genre_reg_RMSEs_m66_42_0_2,
       umgy_tune_sets,
       date_global_effect,
       date_year_effect,
       year_day_effects,
       day_smoothed_effect,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Year+Day Smoothed Effect data has been saved to file: %1", 
           file_path_tmp)
} 


##### Date Smoothed Effect Visualization ----------------------------------
# mean_day_smoothed_effect <- compute_mean_dse(day_smoothed_effect_ls)
# str(mean_day_smoothed_effect)

day_smoothed_effect |>
  ggplot(aes(x = days)) +
  geom_point(aes(y = de), size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(y = de_smoothed), color = "red")

put_log("Date Smoothed Effect has been plotted 
for the `loess` function fitted with default parameters.")

# Calculate RMSE for the `loess` function fitted with default parameters -------
start <- put_start_date()
day_smoothed_effects_RMSE <- day_smoothed_effect_RMSE()
put_end_date(start)
put_log1("RMSE value has been computed using `loess` function 
with default (degree & span) parameters for the %1-Fold Cross Validation samples.",
         CVFolds_N)

print(day_smoothed_effects_RMSE)
#> [1] 0.859081

#### Re-train tuning `loess` function's with `span` & `degree` params-----------
##### Tune the Global/Day Smoothed Effect model -------------------------------
degree <- c(0, 1, 2)
put_log("Tuning `loess` function for degrees:")
put(degree)

###### 1. `degree = 0` ---------------------------------------------------------
put("Case 1. `degree = 0`")
file_name_tmp <- "day-smoothed-loess-degree0.RData"
file_path_tmp <- file.path(models_data_path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading data for `loess` function with parameter `degree = 0` from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Data for `loess` function with parameter `degree = 0` has been loaded from file: %1", 
           file_path_tmp)
} else {
  # spans <- seq(0.0003, 0.002, 0.00001)
  # spans <- seq(0.00100, 0.00102, 0.00001)
  degree0_spans <- seq(0.0005, 0.0015, 0.00001)
  
  put_log2("Tuning for degree0_spans %1:%2...", "0.0005", max(degree0_spans))
  degree0_tuned_RMSEs <- date_smoothed_tuned_RMSEs(degree[1], degree0_spans)
  degree0_best_RMSE <- best_rmse(degree0_tuned_RMSEs)
  
  put_log1("Saving data for `loess` function with parameter `degree = 0` to file: %1", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effects,
       user_movie_effect,
       genre_mean_ratings,
       user_movie_genre_effect,
       user_movie_genre_reg_lambdas_6p6_m4p2_p2,
       user_movie_genre_reg_RMSEs_m66_42_0_2,
       umgy_tune_sets,
       date_global_effect,
       date_year_effect,
       year_day_effects,
       day_smoothed_effect,
       degree,
       degree0_spans,
       degree0_tuned_RMSEs,
       degree0_best_RMSE,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("Data for `loess` function with parameter `degree = 0` has been saved to file: %1", 
           file_path_tmp)
} 

put("Case 1. `degree = 0` RMSEs.ResultTibble:")
put(str(degree0_tuned_RMSEs))

plot(degree0_spans, degree0_tuned_RMSEs)
put_log1("RMSE values have been plotted for the %1-Fold Cross Validation samples.", 
         CVFolds_N)

put_log("The best RMSE for `degree = 0`:")
put(degree0_best_RMSE)
#      Span      RMSE 
# 0.0008700 0.8573269 

###### 2. `degree = 1` --------------------------------------------------------------
put("Case 2. `degree = 1`")
file_name_tmp <- "day-smoothed-loess-degree1.RData"
file_path_tmp <- file.path(models_data_path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading data for `loess` function with parameter `degree = 1` from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Data for `loess` function with parameter `degree = 1 has been loaded from file: %1", 
           file_path_tmp)
} else {
  #degree1_spans <- seq(0.0005, 0.002, 0.00001)
  degree1_spans <- seq(0.0005, 0.00135, 0.00001)
  put_log2("Tuning for degree1_spans %1:%2...", min(degree1_spans), max(degree1_spans))
  degree1_tuned_RMSEs <- date_smoothed_tuned_RMSEs(degree[2], degree1_spans)
  
  degree1_best_RMSE <- best_rmse(degree1_tuned_RMSEs)

  put_log1("Saving data for `loess` function with parameter `degree = 1` to file: %1", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effects,
       user_movie_effect,
       genre_mean_ratings,
       user_movie_genre_effect,
       user_movie_genre_reg_lambdas_6p6_m4p2_p2,
       user_movie_genre_reg_RMSEs_m66_42_0_2,
       umgy_tune_sets,
       date_global_effect,
       date_year_effect,
       year_day_effects,
       day_smoothed_effect,
       degree,
       degree0_spans,
       degree0_tuned_RMSEs,
       degree0_best_RMSE,
       degree1_spans,
       degree1_tuned_RMSEs,
       degree1_best_RMSE,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("Data for `loess` function with parameter `degree = 1` has been saved to file: %1", 
           file_path_tmp)
} 
put("Case 2. `degree = 1` RMSEs.ResultTibble:")
put(str(degree1_tuned_RMSEs))

plot(degree1_spans, degree1_tuned_RMSEs)
put_log1("RMSE values have been plotted for the %1-Fold Cross Validation samples.", 
         CVFolds_N)

put_log("The best RMSE for `degree = 1`:")
put(degree1_best_RMSE)
#      Span      RMSE 
# 0.0008700 0.8568612

###### 3. `degree = 2` --------------------------------------------------------------
put("Case 3. `degree = 2`")
file_name_tmp <- "day-smoothed-loess-degree2.RData"
file_path_tmp <- file.path(models_data_path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading  data for `loess` function with parameter `degree = 2` from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Data for `loess` function with parameter `degree = 2` has been loaded from file: %1", 
           file_path_tmp)
} else {
  #degree2_spans <- seq(0.0003, 0.01, 0.00001)
  degree2_spans <- seq(0.0007, 0.002, 0.00001)
  put_log2("Tuning for spans %1:%2...", min(degree2_spans), max(degree2_spans))
  degree2_tuned_RMSEs <- date_smoothed_tuned_RMSEs(degree[3], degree2_spans)
  
  degree2_best_RMSE <- best_rmse(degree2_tuned_RMSEs)

  start <- put_start_date()
  save(mu,
       user_effects,
       user_movie_effect,
       genre_mean_ratings,
       user_movie_genre_effect,
       user_movie_genre_reg_lambdas_6p6_m4p2_p2,
       user_movie_genre_reg_RMSEs_m66_42_0_2,
       umgy_tune_sets,
       date_global_effect,
       date_year_effect,
       year_day_effects,
       day_smoothed_effect,
       degree,
       degree0_spans,
       degree0_tuned_RMSEs,
       degree0_best_RMSE,
       degree1_spans,
       degree1_tuned_RMSEs,
       degree1_best_RMSE,
       degree2_spans,
       degree2_tuned_RMSEs,
       degree2_best_RMSE,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("Data for `loess` function with parameter `degree = 2` has been saved to file: %1", 
           file_path_tmp)
} 
put("Case 3. `degree = 2` RMSEs.ResultTibble:")
str(degree2_tuned_RMSEs)

plot(degree2_spans, degree2_tuned_RMSEs)
put_log1("RMSE values have been plotted for the %1-Fold Cross Validation samples.", 
         CVFolds_N)

put_log("The best RMSE for `degree = 2`:")
put(degree2_best_RMSE)
#      Span      RMSE 
# 0.0013100 0.8571522

#### Retrain with the best parameters figured out above ------------------------
file_name_tmp <- "day-smoothed-tuned.RData"
file_path_tmp <- file.path(models_data_path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading Tuned Day Smoothed Effect data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Tuned Day Smoothed Effect data has been loaded from file: %1", 
           file_path_tmp)
} else {
  # The Best Parameters and RMSE Value 
  loess_rmse <- data.frame(degree = degree, 
                           span = c(degree0_best_RMSE[1], degree1_best_RMSE[1], degree2_best_RMSE[1]),
                           rmse = c(degree0_best_RMSE[2], degree1_best_RMSE[2], degree2_best_RMSE[2]))
  put(loess_rmse)
  
  idx_best_rmse <- which.min(loess_rmse$rmse)
  
  day_loess_best_degree <- loess_rmse[idx_best_rmse, 1]  # 1
  put_log1("The Best Degree: %1", day_loess_best_degree)
  day_loess_best_degree
  #> [1] 1
  
  day_loess_best_span <- loess_rmse[idx_best_rmse, 2]# 0.00108
  put_log1("The Best Span: %1", day_loess_best_span)
  day_loess_best_span
  #> [1] 0.00087
  
  day_loess_best_RMSE <- loess_rmse[idx_best_rmse, 3]
  put_log1("The Best RMSE: %1",day_loess_best_RMSE)
  day_loess_best_RMSE
  #> [1] 0.8568619
  
  put_log2("Re-training model using `loess` function with the best parameters: 
span = %1, degree = %2", day_loess_best_span, day_loess_best_degree)
  start <- put_start_date()
  best_day_smoothed_effect <- compute_day_smoothed_effect(day_loess_best_degree, day_loess_best_span)
  str(best_day_smoothed_effect)
  put_end_date(start)
  put_log2("The model has been re-trained using `loess` function with the best parameters: 
span = %1, degree = %2", day_loess_best_span, day_loess_best_degree)
  
  put_log1("Saving Tuned Day Smoothed Effect data to file: %1", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effects,
       user_movie_effect,
       genre_mean_ratings,
       user_movie_genre_effect,
       user_movie_genre_reg_lambdas_6p6_m4p2_p2,
       user_movie_genre_reg_RMSEs_m66_42_0_2,
       umgy_tune_sets,
       date_global_effect,
       date_year_effect,
       year_day_effects,
       day_smoothed_effect,
       degree,
       degree0_spans,
       degree0_tuned_RMSEs,
       degree0_best_RMSE,
       degree1_spans,
       degree1_tuned_RMSEs,
       degree1_best_RMSE,
       degree2_spans,
       degree2_tuned_RMSEs,
       degree2_best_RMSE,
       day_loess_best_degree,
       day_loess_best_span,
       day_loess_best_RMSE,
       best_day_smoothed_effect,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("Tuned Day Smoothed Effect data has been saved to file: %1", 
           file_path_tmp)
} 

##### The Best Date Smoothed Effect Visualization ----------------------------------
best_day_smoothed_effect |>
  ggplot(aes(x = days)) +
  geom_point(aes(y = de), size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(y = de_smoothed), color = "red")

put_log1("Optimized Mean Date Smoothed Effect has been plotted for the %1-Fold Cross Validation samples.",
         CVFolds_N)
# Calculate RMSE for `loess` function fitted with the best parameters ----------
start <- put_start_date()
user_movie_genre_tuned_date_effect_RMSE <- day_smoothed_effect_RMSE(day_loess_best_degree, day_loess_best_span)
put_end_date(start)
put_log3("RMSE value has been computed using `loess` function 
with the best parameters for the %1-Fold Cross Validation samples:
degree = %2;
span = %3.",
        CVFolds_N,
        day_loess_best_degree,
        day_loess_best_span)

print(user_movie_genre_tuned_date_effect_RMSE)
#> [1] 0.8568612

##### Add a row to the RMSE Result Tibble for the User+Movie+Genre+Date Effects Model ---- 
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("User+Movie+Genre+Date Effect Model (tuned)", 
               user_movie_genre_tuned_date_effect_RMSE)

RMSE_kable(RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble for the tuned `User+Movie+Genre+Date Effect Model`.")

# start <- put_start_date()
# final_holdout_test |>
#   left_join(date_days_map, by = "timestamp") |>
#   left_join(user_effects, by = "userId") |>
#   left_join(mean_user_movie_genre_bias, by = "movieId") |>
#   left_join(day_smoothed_effect, by='days') |>
#   mutate(resid = rating - clamp(mu + a + b + g + de_smoothed)) |>
#   filter(!is.na(resid)) |>
#   pull(resid) |> final_rmse()
# put_end_date(start)
#> [1] 0.8724055

#### Close Log -----------------------------------------------------------------
log_close()

