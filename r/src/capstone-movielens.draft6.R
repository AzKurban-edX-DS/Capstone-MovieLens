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

## Logging Helper functions -----------------------------------------------------
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

msg.set_arg <- function(msg_template, arg, arg.name = "%1") {
  msg_template |> 
    str_replace_all(arg.name, as.character(arg))
}
msg.glue <- function(msg_template, arg, arg.name = "%1"){
  msg_template |>
    msg.set_arg(arg, arg.name) |>
    str_glue()
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
    str_replace_all("%2", as.character(arg2)) |>
    str_glue()
  
  put(msg)
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
### Open log file for `Initialize Source File Paths` Feature -------------------
open_logfile(".src.file-paths")
## Source File Paths -----------------------------------------------------------
r.path <- "r"
dir.create(r.path)
put_log1("Directory path has been created: %1", r.path)

src.folder <- "src"
support_functions.folder <- "support-functions"

r.src.path <- file.path(r.path, src.folder)
dir.create(r.src.path)
put_log1("Directory path has been created: %1", r.src.path)

support_functions.path <- file.path(r.src.path, support_functions.folder)
dir.create(support_functions.path)
put_log1("Directory path has been created: %1", support_functions.path)
common_helper_functions.file_path <- file.path(support_functions.path, 
                                            "common-helper.functions.R")
## External Common Helper functions -------------------------------------
source(common_helper_functions.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)
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

### Data File Paths ------------------------------------------------------------
data.path <- "data"
dir.create(data.path)
put_log1("Directory path has been created: %1", data.path)

regularization.folder <- "regularization"
models.folder <- "models"
model_tuning.folder <- "model-tuning"
fine_tuning.folder <- "fine-tuning"

data.models.path <- file.path(data.path, models.folder)
dir.create(data.models.path)
put_log1("Directory path has been created: %1", data.models.path)

data.regularization.path <- file.path(data.path, regularization.folder)
dir.create(data.regularization.path)
put_log1("Directory path has been created: %1", data.regularization.path)

data.model_tuning.path <- file.path(data.path, model_tuning.folder)
dir.create(data.model_tuning.path)
put_log1("Directory path has been created: %1", data.model_tuning.path)

# src.regularization.path <- file.path(r.src.path, regularization.folder)
# dir.create(src.regularization.path)
# put_log1("Directory path has been created: %1", src.regularization.path)

#### Close Log ---------------------------------------------------------------
log_close()
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
movielens_datasets_file_path <- file.path(data.path, movielens_datasets_file)
movielens_datasets_zip <- file.path(data.path, "movielens-datasets.zip")

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
file_name_tmp <- "1.mu.RData"
file_path_tmp <- file.path(data.models.path, file_name_tmp)

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

file_name_tmp <- "2.mu-deviation.RData"
file_path_tmp <- file.path(data.models.path, file_name_tmp)

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
  deviation.RMSE <- sapply(deviation, function(delta){
    naive_model_RMSE(mu + delta)
  })
  print(deviation.RMSE)
  put_log1("RMSE values have been computed for %1 deviations from the Overall Mean Rating.",
           length(deviation))
  
  put_log1("Saving Overall Mean Rating Deviation data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       naive_rmse,
       deviation,
       deviation.RMSE,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("Overall Mean Rating Deviation data has been saved to file: %1", 
           file_path_tmp)
} 

data.frame(deviation = deviation, 
           deviation.RMSE = deviation.RMSE) |> 
  tuning.plot(title = "RMSE as a function of Deviation from the Overall Mean Rating",
              xname = "deviation", 
              yname = "deviation.RMSE", 
              xlabel = "deviation", 
              ylabel = "RMSE")



put_log("A plot was constructed for the deviations from the Overall Mean Rating.")

which_min_deviation <- deviation[which.min(deviation.RMSE)]
min_rmse = min(deviation.RMSE)

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
file_name_tmp <- "3.user-mean-ratings.RData"
file_path_tmp <- file.path(data.models.path, file_name_tmp)

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

##### Building User Effect Model ----------------------------------------------

# we notice that there is substantial variability across users.
#>  To account for this, we can use a linear model with a treatment effect `α[i]` 
#>  for each user. The sum `μ + α[i]` can be interpreted as the typical 
#>  rating user `i` gives to movies. We can write the model as:

# Y[i,j] = μ + α[i] + ε[i,j]

#> It can be shown that the least squares estimate `α[i]` is just the average 
#> of `y[i,j] - μ` for each user. So we can compute them this way:

file_name_tmp <- "4.user-effect-model.RData"
file_path_tmp <- file.path(data.models.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading User Effect Model data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("User Effect Model data has been loaded from file: %1", file_path_tmp)
  
} else {
  put_log("Computing User Effect per users ...")
  user_effect <- user_mean_ratings |>
    mutate(userId = as.integer(userId),
           a = mean_rating - mu)
  
  put_log("A User Effect Model has been builded and trained")
  
  put_log1("Saving User Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       naive_rmse,
       user_effect,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

put(str(user_effect))

# Plot a histogram of the user effects -----------------------------------------
par(cex = 0.7)
hist(user_effect$a, 30, xlab = TeX(r'[$\hat{alpha}_{i}$]'),
     main = TeX(r'[Histogram of $\hat{alpha}_{i}$]'))
put_log("A histogram of the User Effect distribution has been plotted.")

# Computing the RMSE taking into account user effects --------------------------
#> Finally, we are ready to compute the `RMSE` (additionally using the helper 
#> function `clamp` we defined above to keep predictions in the proper range):

put_log("Computing the RMSE taking into account user effects...")
start <- put_start_date()
user_effect_mses <- sapply(edx_CV, function(cv_fold_dat){
  cv_fold_dat$validation_set |>
    left_join(user_effect, by = "userId") |>
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

#### Support Functions ---------------------------------------------------------
UM_effect.functions.file_path <- file.path(support_functions.path, 
                                               "UM-effect.functions.R")
source(UM_effect.functions.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)
#### Open log ------------------------------------------------------------------
# User+Movie Effect log: 
open_logfile(".UM-effect")
#### Model building: User+Movie Effect ----------------------------------------
file_name_tmp <- "5.cv.UM-effect.RData"
file_path_tmp <- file.path(data.models.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading User+Movie Effect data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie Effect data has been loaded from file: %1", file_path_tmp)
  
} else {
  UM_effect <- train_user_movie_effect.cv()
  
  put_log1("Saving User+Movie Effect data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       UM_effect,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie Effect data has been saved to file: %1",
           file_path_tmp)
} 

put(str(UM_effect))
#### User+Movie Effects: Visualization ------------------------------
par(cex = 0.7)
hist(UM_effect$b, 30, xlab = TeX(r'[$\hat{beta}_{j}$)]'),
     main = TeX(r'[Histogram of $\hat{beta}_{j}$]'))
put_log("A histogram of the Mean User+Movie Effects distribution has been plotted.")

#### Calculate RMSEs.ResultTibble on Validation Sets ---------------------------
user_movie_effect_RMSE <- calc_user_movie_effect_RMSE.cv(UM_effect)
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

##### Open log for `Preliminary setting-up of lambda range` feature -----------
open_logfile(".rg.UM-effect.pre-set-lambdas")
##### UM Effect Regularization Directory Paths ------------------------------------------
UM_effect.regularization.path <- file.path(data.regularization.path, 
                                           "1.UM-effect")
dir.create(UM_effect.regularization.path)
put_log1("Directory path has been created: %1", UM_effect.regularization.path)

UM_effect.rg.fine_tuning.path <- file.path(UM_effect.regularization.path, 
                                           fine_tuning.folder)
dir.create(UM_effect.rg.fine_tuning.path)
put_log1("Directory path has been created: %1", UM_effect.rg.fine_tuning.path)

##### Process Preliminary setting-up of lambda range --------------------------
file_name_tmp <- "1.UME.rg.pre-tune.RData" # UME stands for `User+Movie Effect`
file_path_tmp <- file.path(UM_effect.regularization.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading preliminary regularization set-up data for User+Movie Effect from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Preliminary regularization set-up data for User+Movie Effect has been loaded from file: %1", 
           file_path_tmp)
} else {
  put_log1("Preliminary setting-up of `lambda`s range for %1-Fold Cross Validation samples...",
CVFolds_N)
  
  start <- put_start_date()
  lambdas <- seq(0, 1, 0.1)
  cv.UME.preset.result <- 
    tune.model_param(lambdas, regularize.test_lambda.UM_effect.cv)
  put_end_date(start)
  put_log1("Preliminary regularization set-up of `lambda`s range for the User+Movie Effect has been completed.
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
  put_log1("Saving User+Movie Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       #cv.UM_effect,
       cv.UME.preset.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

plot(cv.UME.preset.result$param_values,
     cv.UME.preset.result$RMSEs)
##### Close Log -----------------------------------------------------------------
log_close()
##### Open log ------------------------------------------------------------------
open_logfile(".UME.rg.fine-tuning")
##### Fine-tuning for `lambda` parameters value ---------------------------------- 
endpoints <- 
  get_fine_tuning.param.endpoints(cv.UME.preset.result)

UM_effect.loop_starter <- c(endpoints["start"], 
                            endpoints["end"], 
                            8)
UM_effect.loop_starter
#> [1] 0.3 0.5 8.0

UM_effect.cache_file_base_name <- "UME.rg.fine-tuning"

UM_effect.reg_lambdas_best_results <- 
  model.tune.param_range(UM_effect.loop_starter,
                         UM_effect.rg.fine_tuning.path,
                         UM_effect.cache_file_base_name,
                         regularize.test_lambda.UM_effect.cv)

##### Close Log -----------------------------------------------------------------
log_close()
##### Open log --------------------------------------------------------------------
open_logfile(".rg.UME.final-tuning")
##### Final Tuning with refined lambda range ----------------------------------
file_name_tmp <- "2.UME.rg.final-tune.RData" # UME stands for `User+Movie Effect`
file_path_tmp <- file.path(UM_effect.regularization.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading final-tuned data for User+Movie Effect from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Final-tuned data for User+Movie Effect has been loaded from file: %1", 
           file_path_tmp)
} else {
  put_log1("Final-tuning UM Effect Model for %1-Fold Cross Validation samples...",
           CVFolds_N)
  
  fine_tuning.result <- UM_effect.reg_lambdas_best_results
  seq_start <- fine_tuning.result$param_values.endpoints[1]
  seq_end <- fine_tuning.result$param_values.endpoints[2]
  seq_step <- (seq_end - seq_start)/64  

  lambdas <- seq(seq_start, seq_end, seq_step)
  
  start <- put_start_date()
  cv.UME.final_tuned.result <- 
    tune.model_param(lambdas, regularize.test_lambda.UM_effect.cv)
  put_end_date(start)
  put_log1("Final-tuning of User+Movie Effect has been completed.
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
  plot(cv.UME.final_tuned.result$param_values,
       cv.UME.final_tuned.result$RMSEs)
  
  put_log1("Saving Final-tuning User+Movie Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       #cv.UM_effect,
       cv.UME.final_tuned.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

##### Close Log -----------------------------------------------------------------
log_close()
##### Open log ------------------------------------------------------------------
open_logfile(".UME.rg.re-train.best-lambda")
##### Re-train Regularized User+Movie Effect Model for the best `lambda` -------
file_name_tmp <- "3.UME.rg.re-train.best-lambda.RData"
file_path_tmp <- file.path(UM_effect.regularization.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading User+Movie Effect data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie Effect data has been loaded from file: %1", file_path_tmp)
  
} else {
  
  best_result <- cv.UME.final_tuned.result$best_result
  best_user_movie_reg_lambda <- best_result["param.best_value"]
  best_user_movie_reg_lambda
  
  best_user_movie_reg_RMSE <- best_result["best_RMSE"]
  print(best_user_movie_reg_RMSE)
  
  put_log1("Re-training Regularized User+Movie Effect Model for the best `lambda`: %1...",
           best_user_movie_reg_lambda)
  
  rg.UM_effect <- train_user_movie_effect.cv(best_user_movie_reg_lambda)
  rg.UM_effect.RMSE <- calc_user_movie_effect_RMSE.cv(rg.UM_effect)
  
  put_log1("Is this the best RMSE? %1",
           best_user_movie_reg_RMSE == rg.UM_effect.RMSE)
  
  put_log1("Regularized User+Movie Effect Model has been re-trained for the best `lambda`: %1.",
           best_user_movie_reg_lambda)
  put_log1("The best RMSE after being regularized: %1",
           rg.UM_effect.RMSE)
  
  put_log1("Saving Regularized User+Movie Effect data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       rg.UM_effect.RMSE,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie Effect data has been saved to file: %1",
           file_path_tmp)
} 
##### Add a row to the RMSE Result Table for the Regularized User+Movie Effect Model --------
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("Regularized User+Movie Effect Model", 
               rg.UM_effect.RMSE)
RMSE_kable(RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble 
for the `Regularized User+Movie Effect Model`.")
##### Close Log -----------------------------------------------------------------
log_close()
### Accounting for Movie Genres ------------------------------------------------
#> We can slightly improve our naive model by accounting for movie genres.
#> Let's do some preliminary analysis first.
#### Open logging file for the feature: Building User+Movie+Genre Effect Model----
open_logfile(".UMG-effect")
#### Support Functions ---------------------------------------------------------
umge_functions_file <- "UMG-effect.functions.R"
umge_functions.file_path <- file.path(support_functions.path, 
                                      umge_functions_file)
source(umge_functions.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)

#### Data Analysis and Visualization -------------------------------------------
# Reference: the Textbook Section "23.7 Exercises" of the Chapter "23 Regularization"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#exercises
#> The `edx` dataset also has a genres column. This column includes 
#> every genre that applies to the movie 
#> (some movies fall under several genres)[@IDS2_23-7].
##### Computing Genre Mean Ratings ---------------------------------------------
file_name_tmp <- "7.genre-mean-ratings.RData"
file_path_tmp <- file.path(data.models.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading Genre Average Rating data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Genre Mean Ratings data has been loaded from file: %1", file_path_tmp)
  
} else {
  # Preparing data for plotting:
  put_log1("Computing Genre Mean Ratings for %1-Fold Cross Validation samples...", 
           CVFolds_N)
  gnr_mean_ratings.cv <- calc_genre_mean_ratings.cv()
  put_log1("Mean Ratings per Genre list has been computed for %1-Fold Cross Validation samples.",
           CVFolds_N)

  put_log1("Saving Genre Mean Ratings data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       gnr_mean_ratings.cv,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("Genre Average Rating data has been saved to file: %1", 
           file_path_tmp)
} 
put_log("Genre Mean Rating data structure:")
put(str(gnr_mean_ratings.cv))

put_log2("The worst rating is for the genre category: %1 (average rating is %2)",
            gnr_mean_ratings.cv$genres[which.min(gnr_mean_ratings.cv$ratings)],
            as.character(clamp(min(gnr_mean_ratings.cv$ratings))))

put_log2("The best rating is for the genre category: %1 (average rating is %2)",
            gnr_mean_ratings.cv$genres[which.max(gnr_mean_ratings.cv$ratings)],
            as.character(clamp(max(gnr_mean_ratings.cv$ratings))))

##### Genres Popularity ------------------------------------------------------------

put_log2("The worst popularity was for the genre category: %1 (%2 ratings)",
            gnr_mean_ratings.cv$genres[which.min(gnr_mean_ratings.cv$n)],
            as.character(min(gnr_mean_ratings.cv$n)))

put_log2("The best popularity was for the genre category: %1 (%2 ratings)",
            gnr_mean_ratings.cv$genres[which.max(gnr_mean_ratings.cv$n)],
            as.character(max(gnr_mean_ratings.cv$n)))

##### Genres Info Visualization ------------------------------------------------
#> For illustrative purposes, we will limit the genre information 
#> we are going to plot to movies with more than 24,000 ratings:
nratings <- 24000

###### Plot Genre Info --------------------------------------------------------------  
genre_ratings_plot_dat <- gnr_mean_ratings.cv |>
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

put_log1("The worst ratings were for the genre category: %1",
        genre_ratings_plot_dat$genres[which.min(genre_ratings_plot_dat$ratings)])

put_log1("The best ratings were for the genre category: %1",
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
#### Including Separated Genre effect -----------------------------------------------------
# Y[i,j] = μ + α[i] + β[j] + g[i,j]  + ε[i,j]
# where g[i,j] is a combination of genres for movie `i` rated by user `j`,
# so that g[i,j] = ∑{k=1,K}(x[i,j]^k*𝜸[k]) 
# with `x[i,j]^k = 1` if g[i,j] includes genre `k`, and `x[i,j]^k = 0` otherwise.

# mutate(userId = as.integer(userId),
#        movieId = as.integer(movieId)) |>

#### Train User+Movie+Genre Effect Model ---------------------------------------
file_name_tmp <- "8.UMG-effect.RData"
file_path_tmp <- file.path(data.models.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading User+Movie+Genre Effect Model data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre Effect Model data has been loaded from file: %1", file_path_tmp)
  
} else {
  UMG_effect <- train_user_movie_genre_effect.cv()
  
  put_log1("Saving User+Movie+Genre Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       gnr_mean_ratings.cv,
       UMG_effect,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

put_log("User+Movie+Genre Effect Model data structure:")
put(str(UMG_effect))

###### Plot a histogram of the Movie Genre Effect distribution -----------------
par(cex = 0.7)
hist(UMG_effect$g, 30, xlab = TeX(r'[$\hat{g}_{i,j}$]'),
     main = TeX(r'[Histogram of $\hat{g}_{i,j}$]'))

put_log("A histogram of the Movie Genre Effect distribution has been plotted.")

###### Compute RMSE: User+Movie+Genre effects ------------------------------------
UMG_effect.RMSE <- calc_user_movie_genre_effect_RMSE.cv(UMG_effect)
UMG_effect.RMSE
#> [1] 0.859473

#### Add a row to the RMSE Result Tibble for the User+Movie+Genre Effect Model ---- 
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("User+Movie+Genre Effect Model", UMG_effect.RMSE)
RMSE_kable(RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble for the `User+Movie+Genre Effect Model`.")

#### Close Log -----------------------------------------------------------------
log_close()

### Regularizing User+Movie+Genre Effects --------------------------------------
##### Open log file for `Preliminary setting-up of lambda range` feature -------
open_logfile(".rg.UM-effect.pre-set-lambdas")
##### UMG Effect Regularization Directory Paths --------------------------------
UMG_effect.regularization.path <- file.path(data.regularization.path, 
                                           "2.UMG-effect")
dir.create(UMG_effect.regularization.path)
put_log1("Directory path has been created: %1", UMG_effect.regularization.path)

UMG_effect.rg.fine_tuning.path <- file.path(UMG_effect.regularization.path, 
                                           fine_tuning.folder)
dir.create(UMG_effect.rg.fine_tuning.path)
put_log1("Directory path has been created: %1", UMG_effect.rg.fine_tuning.path)
##### Process Preliminary setting-up of lambda range ---------------------------
file_name_tmp <- "1.UMGE.rg.pre-tune.RData" # UMGE stands for `User+Movie+Genre Effect`
file_path_tmp <- file.path(UMG_effect.regularization.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading preliminary regularization set-up data for User+Movie+Genre Effect from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Preliminary regularization set-up data for User+Movie+Genre Effect has been loaded from file: %1", 
           file_path_tmp)
} else {
  put_log1("Preliminary setting-up of `lambda`s range for %1-Fold Cross Validation samples...",
           CVFolds_N)
  
  start <- put_start_date()
  lambdas <- seq(0, 1, 0.1)
  cv.UMGE.preset.result <- 
    tune.model_param(lambdas, regularize.test_lambda.UMG_effect.cv)
  put_end_date(start)
  put_log1("Preliminary regularization set-up of `lambda`s range for the User+Movie+Genre Effect has been completed.
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
  put_log1("Saving User+Movie+Genre Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       cv.UMGE.preset.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

plot(cv.UMGE.preset.result$param_values,
     cv.UMGE.preset.result$RMSEs)

##### Close Log -----------------------------------------------------------------
log_close()
##### Open log file for `User+Movie+Genre Effect Regularization (Fine-Tuning)` feature ----
open_logfile(".UMGE.rg.fine-tuning")
##### Fine-tuning for `lambda` parameters value -------------------------------- 
endpoints <- 
  get_fine_tuning.param.endpoints(cv.UMGE.preset.result)

UMG_effect.loop_starter <- c(endpoints["start"], 
                            endpoints["end"], 
                            8)
UMG_effect.loop_starter
#> [1] 0.4   0.6   8.0

UMG_effect.cache_file_base_name <- "UMGE.rg.fine-tuning"

UMG_effect.reg_lambdas_best_results <- 
  model.tune.param_range(UMG_effect.loop_starter,
                         UMG_effect.rg.fine_tuning.path,
                         UMG_effect.cache_file_base_name,
                         regularize.test_lambda.UMG_effect.cv)

put_log("Fine-tuning stage of the User+Movie+Genre Effect Model Regularization 
has ended up with with the following results:")
put(UMG_effect.reg_lambdas_best_results)
#### Close Log -----------------------------------------------------------------
log_close()
##### Open log for the `Final-tuning` stage of the  `User+Movie+Genre Effect Regularization` feature ----
open_logfile(".rg.UMGE.final-tuning")
##### Final Tuning with refined lambda range ----------------------------------
file_name_tmp <- "2.UMGE.rg.final-tune.RData" # UMGE stands for `User+Movie+Genre Effect`
file_path_tmp <- file.path(UMG_effect.regularization.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading final-tuned data for User+Movie+Genre Effect from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Final-tuned data for User+Movie+Genre Effect has been loaded from file: %1", 
           file_path_tmp)
} else {
  put_log1("Final-tuning UMG Effect Model for %1-Fold Cross Validation samples...",
           CVFolds_N)
  
  fine_tuning.result <- UMG_effect.reg_lambdas_best_results
  seq_start <- fine_tuning.result$param_values.endpoints[1]
  seq_end <- fine_tuning.result$param_values.endpoints[2]
  seq_step <- (seq_end - seq_start)/64  
  
  lambdas <- seq(seq_start, seq_end, seq_step)
  
  start <- put_start_date()
  cv.UMGE.final_tuned.result <- 
    tune.model_param(lambdas, regularize.test_lambda.UMG_effect.cv)
  put_end_date(start)
  put_log1("Final-tuning of User+Movie+Genre Effect has been completed.
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
  plot(cv.UMGE.final_tuned.result$param_values,
       cv.UMGE.final_tuned.result$RMSEs)
  
  put_log1("Saving Final-tuning User+Movie+Genre Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       cv.UMGE.final_tuned.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

##### Close Log -----------------------------------------------------------------
log_close()
##### Open log for `Re-train Regularized User+Movie+Genre Effect Model` feature for the best `lambda` value----
open_logfile(".UMGE.rg.re-train.best-lambda")
##### Re-train `Regularized User+Movie+Genre Effect Model` for the best `lambda` value ----
file_name_tmp <- "3.UMGE.rg.re-train.best-lambda.RData"
file_path_tmp <- file.path(UMG_effect.regularization.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading Regularized User+Movie+Genre Effect Model data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Regularized User+Movie+Genre Effect Model data has been loaded from file: %1", 
           file_path_tmp)
  
} else {
  rg.UMGE.final_tuned.best_lambda <- cv.UMGE.final_tuned.result$best_result["param.best_value"]
  rg.UMGE.final_tuned.best_RMSE <- cv.UMGE.final_tuned.result$best_result["best_RMSE"]
  
  put_log1("Re-training Regularized User+Movie+Genre Effect Model for the best `lambda`: %1...",
           rg.UMGE.final_tuned.best_lambda)
  
  rg.UMG_effect <- train_user_movie_genre_effect.cv(rg.UMGE.final_tuned.best_lambda)
  rg.UMG_effect.RMSE <- calc_user_movie_genre_effect_RMSE.cv(rg.UMG_effect)
  
  put_log2("Regularized User+Movie+Genre Effect RMSE has been computed for the best `lambda = %1`: %2.",
           rg.UMGE.final_tuned.best_lambda,
           rg.UMG_effect.RMSE)
  put_log1("Is this a best RMSE? %1",
           rg.UMGE.final_tuned.best_RMSE == rg.UMG_effect.RMSE)
  
  
  put_log1("Saving User+Movie+Genre Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       rg.UMG_effect,
       rg.UMG_effect.RMSE,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

#### Add a row to the RMSE Result Tibble for the Regularized User+Movie+Genre Effect Model --------
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("Regularized User+Movie+Genre Effect Model", 
               rg.UMG_effect.RMSE)

RMSE_kable(RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble for the `Regularized User+Movie+Genre Effect Model`.")

#### Close Log -----------------------------------------------------------------
log_close()

### Accounting for User+Movie+Genre+Year Effect --------------------------------
#### Open log file for the feature: `Building  User+Movie+Genre+Year Effect Model`----
open_logfile(".user+movie+genre+year-effect")

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

#### Support Functions ---------------------------------------------------------
cv.UMGY_effect.functions_file <- "UMGY-effect.functions.R"
cv.UMGY_effect.functions.file_path <- file.path(support_functions.path, 
                                      cv.UMGY_effect.functions_file)
source(cv.UMGY_effect.functions.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)


#### Training User+Movie+Genre+Year Effect Model ----------------------------------------
file_name_tmp <- "9.UMGY-effect.RData"
file_path_tmp <- file.path(data.models.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading User+Movie+Genre+Year Effect data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre+Year Effect data has been loaded from file: %1", file_path_tmp)
  
} else {
  put_log("Computing User+Movie+Genre+Year Effect...")
  cv.UMGY_effect <- train_UMGY_effect.cv()
  put_log("User+Movie+Genre+Year Effect has been computed.")
  put(summary(cv.UMGY_effect))

  put_log1("Saving User+Movie+Genre+Year Effect data has been saved to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       rg.UMG_effect,
       cv.UMGY_effect,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre+Year Effect data has been saved to file: %1", 
           file_path_tmp)
} 

#### Compute User+Movie+Genre+Year Effect Model RMSE ------------------------------------
cv.UMGY_effect.RMSE <- calc_UMGY_effect_RMSE.cv(cv.UMGY_effect)
cv.UMGY_effect.RMSE
#> [1] 0.8590795
#### Add a row to the RMSE Result Tibble for the User+Movie+Genre+Year Effect Model ---- 
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("User+Movie+Genre+Year Effect Model", 
               cv.UMGY_effect.RMSE)

RMSE_kable(RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble for the `User+Movie+Genre+Year Effect Model`.")

#### Close Log -----------------------------------------------------------------
log_close()

#### Regularizing User+Movie+Genre+Year Effect ---------------------------------
##### Open log file for `Preliminary setting-up of lambda range` feature -------
open_logfile(".rg.UMGY-effect.pre-set-lambdas")
##### UMGY Effect Regularization Directory Paths --------------------------------
UMGYE.regularization.path <- file.path(data.regularization.path, 
                                            "3.UMGY-effect")
dir.create(UMGYE.regularization.path)
put_log1("Directory path has been created for `User+Movie+Genre+Year Effect Model` data: %1", 
         UMGYE.regularization.path)

UMGYE.rg.fine_tuning.path <- file.path(UMGYE.regularization.path, 
                                            fine_tuning.folder)
dir.create(UMGYE.rg.fine_tuning.path)
put_log1("Directory path has been created: %1", UMGYE.rg.fine_tuning.path)
##### Process Preliminary setting-up of lambda range ---------------------------
file_name_tmp <- "1.UMGYE.rg.pre-tune.RData" # UMGE stands for `User+Movie+Genre+Year Effect`
file_path_tmp <- file.path(UMGYE.regularization.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading preliminary regularization set-up data for User+Movie+Genre+Year Effect from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Preliminary regularization set-up data for User+Movie+Genre+Year Effect has been loaded from file: %1", 
           file_path_tmp)
} else {
  put_log1("Preliminary setting-up of `lambda`s range for %1-Fold Cross Validation samples...",
           CVFolds_N)
  
  start <- put_start_date()
  lambdas <- seq(0, 256, 16)
  cv.UMGYE.preset.result <- 
    tune.model_param(lambdas, regularize.test_lambda.UMGY_effect.cv)
  put_end_date(start)
  put_log1("Preliminary regularization set-up of `lambda`s range for the User+Movie+Genre+Year Effect has been completed.
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
  put_log1("Saving User+Movie+Genre+Year Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       rg.UMG_effect,
       #cv.UMGY_effect,       
       cv.UMGYE.preset.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre+Year Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

plot(cv.UMGYE.preset.result$param_values,
     cv.UMGYE.preset.result$RMSEs)

put_log("Preliminary regularization set-up of `lambda`s range for the User+Movie+Genre+Year Effect 
has resulted as follows:")
put(cv.UMGYE.preset.result$best_result)

##### Close Log -----------------------------------------------------------------
log_close()
##### Open log file for Fine-Tuning Stage of the `User+Movie+Genre+Year Effect Regularization` feature ----
open_logfile(".UMGYE.rg.fine-tuning")
##### Fine-tuning for the `lambda` parameter values range ----------------------- 
endpoints <- 
  get_fine_tuning.param.endpoints(cv.UMGYE.preset.result)

UMGYE.loop_starter <- c(endpoints["start"], 
                             endpoints["end"], 
                             8)
UMGYE.loop_starter
#> [1] 

UMGYE.cache_file_base_name <- "UMGE.rg.fine-tuning"

UMGYE.rg.fine_tuning.result <- 
  model.tune.param_range(UMGYE.loop_starter,
                         UMGYE.rg.fine_tuning.path,
                         UMGYE.cache_file_base_name,
                         regularize.test_lambda.UMGY_effect.cv)

put_log("Fine-tuning stage of the User+Movie+Genre+Year Effect Model Regularization 
has ended up with with the following results:")
put(UMGYE.rg.fine_tuning.result)
#### Close Log -----------------------------------------------------------------
log_close()
##### Open log for the `Final-tuning` Stage -------------------------------------
open_logfile(".rg.UMGYE.final-tuning")
##### Final Tuning with refined lambda range ----------------------------------
file_name_tmp <- "2.UMGYE.rg.final-tune.RData" # UMGE stands for `User+Movie+Genre+Year Effect`
file_path_tmp <- file.path(UMGYE.regularization.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading final-tuned data for User+Movie+Genre+Year Effect from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Final-tuned data for User+Movie+Genre+Year Effect has been loaded from file: %1", 
           file_path_tmp)
} else {
  put_log1("Final-tuning UMGY Effect Model for %1-Fold Cross Validation samples...",
           CVFolds_N)
  
  fine_tuning.result <- UMGYE.rg.fine_tuning.result
  seq_start <- fine_tuning.result$param_values.endpoints[1]
  seq_end <- fine_tuning.result$param_values.endpoints[2]
  seq_step <- (seq_end - seq_start)/64  
  
  lambdas <- seq(seq_start, seq_end, seq_step)
  
  start <- put_start_date()
  cv.UMGYE.final_tuned.result <- 
    tune.model_param(lambdas, regularize.test_lambda.UMGY_effect.cv)
  put_end_date(start)
  put_log1("Final-tuning of User+Movie+Genre+Year Effect has been completed.
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
  plot(cv.UMGYE.final_tuned.result$param_values,
       cv.UMGYE.final_tuned.result$RMSEs)
  
  put_log1("Saving Final-tuning User+Movie+Genre+Year Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       rg.UMG_effect,
       cv.UMGYE.final_tuned.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre+Year Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

put_log("Final-tuning Stage of regularization `User+Movie+Genre+Year Effect` Model 
has resulted as follows:")
put(cv.UMGYE.final_tuned.result)

##### Close Log -----------------------------------------------------------------
log_close()
##### Open log file for re-training Regularized Model for the best `lambda` value----
open_logfile(".UMGYE.rg.re-train.best-lambda")
#### Re-train Regularized User+Movie+Genre+Year Effect Model for the best `lambda` value ----
file_name_tmp <- "3.UMGYE.rg.re-train.best-lambda.RData"
file_path_tmp <- file.path(UMGYE.regularization.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading Regularized User+Movie+Genre+Year Effect Model data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Regularized User+Movie+Genre+Year Effect Model data has been loaded from file: %1", 
           file_path_tmp)
} else {
  rg.UMGYE.final_tuned.best_lambda <- cv.UMGYE.final_tuned.result$best_result["param.best_value"]
  rg.UMGYE.final_tuned.best_RMSE <- cv.UMGYE.final_tuned.result$best_result["best_RMSE"]
  
  put_log1("Re-training Regularized User+Movie+Genre+Year Effect Model for the best `lambda`: %1...",
           rg.UMGYE.final_tuned.best_lambda)
  
  rg.UMGY_effect <- train_UMGY_effect.cv(rg.UMGYE.final_tuned.best_lambda)
  rg.UMGY_effect.RMSE <- calc_UMGY_effect_RMSE.cv(rg.UMGY_effect)
  
  put_log2("Regularized User+Movie+Genre+Year Effect RMSE has been computed for the best `lambda = %1`: %2.",
           rg.UMGYE.final_tuned.best_lambda,
           rg.UMGY_effect.RMSE)
  put_log1("Is this a best RMSE? %1",
           rg.UMGYE.final_tuned.best_RMSE == rg.UMGY_effect.RMSE)
  
  
  put_log1("Saving User+Movie+Genre+Year Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       rg.UMG_effect,
       rg.UMGY_effect,
       rg.UMGY_effect.RMSE,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre+Year Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

#### Add a row to the RMSE Result Tibble for the Regularized User+Movie+Genre+Year Effects Model ---- 
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("Regularized User+Movie+Genre+Year Effect Model", 
               rg.UMGY_effect.RMSE)

RMSE_kable(RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble 
for the `Regularized User+Movie+Genre+Year Effect Model`.")

#### Close Log -----------------------------------------------------------------
log_close()

### Accounting for User+Movie+Genre+Year+(Smoothed)Day Effect ---------------------------------
# Y[i,j] = μ + α[i] + β[j] + g[i,j] yr[i,j]  + f(d[i,j]) + ε[i,j]

# with `f` a smooth function of `d[(i,j]`
#### Support Functions --------------------------------------------------------
cv.UMGYDE.default_params.functions_file <- "UMGYD-effect.functions.R"
cv.UMGYDE.default_params.functions.file_path <- file.path(support_functions.path, 
                                                cv.UMGYDE.default_params.functions_file)

source(cv.UMGYDE.default_params.functions.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)

#### Open log file for training the model using `loess` function with default parameters ----
open_logfile(".UMGYDE.loess.default-params")
###### Model Tuning Data File Paths --------------------------------------------
UMGYDE.tuning_folder <- "UMGYD-effect"

UMGYDE.tuning.data.path <- 
  file.path(data.model_tuning.path, UMGYDE.tuning_folder)

dir.create(UMGYDE.tuning.data.path)
put_log1("Directory path has been created for tuning 
the `User+Movie+Genre+Year+(Smoothed)Day Effect Model` data: %1", 
         UMGYDE.tuning.data.path)

degree <- c(0, 1, 2)
put_log("Tuning `loess` function for degrees:")
put(degree)

UMGYDE.tuning.degree_param_folders <- c("degree0", 
                                        "degree1", 
                                        "degree2")

UMGYDE.tuning.degree0.data.path <- 
  file.path(UMGYDE.tuning.data.path,  
            UMGYDE.tuning.degree_param_folders[1])

dir.create(UMGYDE.tuning.degree0.data.path)
put_log1("Directory path has been created for tuning 
the `User+Movie+Genre+Year+(Smoothed)Day Effect Model` data
using `loess` function with parameter `degree = 0`: %1", 
UMGYDE.tuning.degree0.data.path)

UMGYDE.tuning.degree1.data.path <- 
  file.path(UMGYDE.tuning.data.path,       
            UMGYDE.tuning.degree_param_folders[2])

dir.create(UMGYDE.tuning.degree1.data.path)
put_log1("Directory path has been created for tuning 
the `User+Movie+Genre+Year+(Smoothed)Day Effect Model` data
using `loess` function with parameter `degree = 1`: %1", 
UMGYDE.tuning.degree1.data.path)

UMGYDE.tuning.degree2.data.path <- 
  file.path(UMGYDE.tuning.data.path,       
            UMGYDE.tuning.degree_param_folders[3])

dir.create(UMGYDE.tuning.degree2.data.path)
put_log1("Directory path has been created for tuning 
the `User+Movie+Genre+Year+(Smoothed)Day Effect Model` data
using `loess` function with parameter `degree = 2`: %1", 
UMGYDE.tuning.degree2.data.path)

UMGYDE.fine_tune.degree0.data.path <- 
  file.path(UMGYDE.tuning.degree0.data.path, 
            fine_tuning.folder)

dir.create(UMGYDE.fine_tune.degree0.data.path)
put_log1("Directory path has been created for fine-tuning 
the `User+Movie+Genre+Year+(Smoothed)Day Effect Model` data
using `loess` function with parameter `degree = 0`: %1", 
UMGYDE.fine_tune.degree0.data.path)

UMGYDE.fine_tune.degree1.data.path <- 
  file.path(UMGYDE.tuning.degree1.data.path, 
            fine_tuning.folder)

dir.create(UMGYDE.fine_tune.degree1.data.path)
put_log1("Directory path has been created for fine-tuning 
the `User+Movie+Genre+Year+(Smoothed)Day Effect Model` data
using `loess` function with parameter `degree = 1`: %1", 
UMGYDE.fine_tune.degree1.data.path)

UMGYDE.fine_tune.degree2.data.path <- 
  file.path(UMGYDE.tuning.degree2.data.path, 
            fine_tuning.folder)

dir.create(UMGYDE.fine_tune.degree2.data.path)
put_log1("Directory path has been created for fine-tuning 
the `User+Movie+Genre+Year+(Smoothed)Day Effect Model` data
using `loess` function with parameter `degree = 2`: %1", 
UMGYDE.fine_tune.degree2.data.path)

#### Train model using `loess` function with default `span` & `degree` params----
file_name_tmp <- "10.cv.UMGYDE.loess.default-params.RData"
file_path_tmp <- file.path(data.models.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading General Day Effect Model data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("General Day Effect Model data has been loaded from file: %1", 
           file_path_tmp)
} else {
  put_log1("Training User+Movie+Genre+Year+(Smoothed)Day Effect Model using `loess` function 
with default `span` & `degree` parameters for %1-Fold Cross Validation samples...",
CVFolds_N)
  
  start <- put_start_date()
  cv.UMGYDE.default_params <- train_UMGY_SmoothedDay_effect.cv()
  put_end_date(start)
  put_log1("User+Movie+Genre+Date Effect Model has been trained
using `loess` function with default `span` & `degree` parameters
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  put(str(cv.UMGYDE.default_params))
  
  put_log1("Saving General Day Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       rg.UMG_effect,
       rg.UMGY_effect,
       cv.UMGYDE.default_params,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("General Day Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

put(str(cv.UMGYDE.default_params))

#### User+Movie+Genre+Year+Day-Smoothed Effect Model Visualization ----------------------------------
# mean_day_smoothed_effect <- compute_mean_dse(day_smoothed_effect_ls)
# str(mean_day_smoothed_effect)

cv.UMGYDE.default_params |>
  ggplot(aes(x = days)) +
  geom_point(aes(y = de), size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(y = de_smoothed), color = "red")

put_log("Date Smoothed Effect has been plotted 
for the `loess` function fitted with default parameters.")

#### Calculate RMSE for the `loess` function fitted with default parameters -------
start <- put_start_date()
cv.UMGYDE.default_params.RMSE <- cv.UMGYDE.default_params |>
  calc_UMGY_SmoothedDay_effect.RMSE.cv()
put_end_date(start)
put_log2("RMSE value has been computed using `loess` function 
with default (degree & span) parameters for the %1-Fold Cross Validation samples: %2.",
         CVFolds_N,
         cv.UMGYDE.default_params.RMSE)

print(cv.UMGYDE.default_params.RMSE)
#> [1] 0.8588864


##### Add a row to the RMSE Result Tibble for the User+Movie+Genre+Date Effects Model ---- 
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("User+Movie+Genre+Year+(Smoothed)Day Effect Model", 
               cv.UMGYDE.default_params.RMSE,
               comment = "Computed using `loess` function with default `degree` & `span` parameters.")

RMSE_kable(RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble 
for the tuned `User+Movie+Genre+Year+(Smoothed)Day Effect Model`.")
#### Close Log -----------------------------------------------------------------
log_close()
#### Tune the model using `loess` with `span` & `degree` params ---------------
##### 1. `degree = 0` ---------------------------------------------------------
###### Open log file for tuning the model using `loess` function with parameter `degree = 0`----
open_logfile(".UMGYDE.loess.degree0.pre-tuning")
put("Case 1. `degree = 0`")
###### Preliminary setting-up of spans range ----------------------------------
file_name_tmp <- "1.pre-tune.degree0.UMGYD-effect.RData"
file_path_tmp <- file.path(UMGYDE.tuning.degree0.data.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading Preliminary set-up data for tuning UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 0` from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Preliminary set-up data for tuning UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 0` has been loaded from file: %1", 
           file_path_tmp)
} else {
  put_log1("Preliminary setting-up of spans range for tuning UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 0` for %1-Fold Cross Validation samples...",
CVFolds_N)
  
  start <- put_start_date()
  spans <- seq(0.0005, 1, 0.001)
  cv.UMGYDE.degree0.pretune_results <- 
    tune.model_param(spans, train_UMGY_SmoothedDay_effect.RMSE.cv.degree0)
  put_end_date(start)
  put_log1("Preliminary set-up data for tuning UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 0` has been computed 
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
  put_log1("Saving UMGY+(Smoothed)Day Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       rg.UMG_effect,
       rg.UMGY_effect,
       cv.UMGYDE.degree0.pretune_results,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("UMGY+(Smoothed)Day Effect Model data has been saved to file: %1", 
           file_path_tmp)
}

cv.UMGYDE.degree0.pretune_results$tuned.result |>
  tuning.plot(title = "Preliminary set-up for tuning UMGY+(Smoothed)Day Effect Model using `loess` with parameter `degree = 0`",
              xname = "parameter.value", 
              yname = "RMSE", 
              xlabel = "spans", 
              ylabel = "RMSE")

put_log("Preliminary tuning stage of the UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 0` has ended up with with the following results:")
put(cv.UMGYDE.degree0.pretune_results$best_result)
# param.best_value        best_RMSE 
#          0.00150          0.85762 
###### Close Log -----------------------------------------------------------------
log_close()

###### Open log file for fine-tuning the model using `loess` function with parameter `degree = 0`----
open_logfile(".UMGYD-effect.loess.degree0.fine-tuning")
###### Fine-tuning of a span parameter value for `degree = 0` ------------------ 
UMGYDE.fine_tune.degree0.loop_starter <- 
  c(cv.UMGYDE.degree0.pretune_results$tuned.result$parameter.value[1], 
    cv.UMGYDE.degree0.pretune_results$tuned.result$parameter.value[3], 
    8)
# loop_starter <- c(0.0005, 0.0025, 8)
UMGYDE.fine_tune.degree0.cache_file.base_name <- "UMGYDE.degree0.tuning-span"

UMGYDE.fine_tune.result.degree0 <- 
  model.tune.param_range(UMGYDE.fine_tune.degree0.loop_starter,
                         UMGYDE.fine_tune.degree0.data.path,
                         UMGYDE.fine_tune.degree0.cache_file.base_name,
                         train_UMGY_SmoothedDay_effect.RMSE.cv.degree0)

UMGYDE.fine_tune.result.degree0$tuned.result |>
  tuning.plot.right_detailed(title = "Fine-tuned UMGY+(Smoothed)Day Model with `loess` parameter: `degree` = 0", 
                             title.right = "Right Part of the Chart Above (zoomed in)",
                             shift = 5,
                             xname = "parameter.value", 
                             yname = "RMSE", 
                             xlabel1 = "spans", 
                             ylabel1 = "RMSE")

put_log("Fine-tuning stage of the UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 0` has ended up with with the following results:")
put(UMGYDE.fine_tune.result.degree0)
#      Span      RMSE 
# 0.0008700 0.8573253   

###### Close Log ---------------------------------------------------------------
log_close()
##### Open log file for final-tuning of the model with `degree = 0` and refined `span` range ----
open_logfile(".UMGYDE.final-tuning.degree0")
##### Final tuning of the model with `degree = 0` and refined span range -------
file_name_tmp <- "2.UMGYDE.final-tune.degree0.RData" # UME stands for `User+Movie Effect`
file_path_tmp <- file.path(UMGYDE.tuning.degree0.data.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading saved data for the final-tuned UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 0` from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("The data for the final-tune UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 0` has been loaded from file: %1", 
           file_path_tmp)
} else {
  put_log1("Final-tuning of the UMGY+(Smoothed)Day Effect Model for %1-Fold Cross Validation samples...",
           CVFolds_N)
  
  fine_tuning.result <- UMGYDE.fine_tune.result.degree0
  seq_start <- fine_tuning.result$param_values.endpoints[1]
  print(seq_start)
  seq_end <- fine_tuning.result$param_values.endpoints[2]
  print(seq_end)
  seq_step <- fine_tuning.result$param_values.endpoints[3]  
  print(seq_step)
  
  spans <- seq(seq_start, seq_end, seq_step)
  print(spans)
  
  start <- put_start_date()
  UMGYDE.final_tune.degree0.result <- 
    tune.model_param(spans, train_UMGY_SmoothedDay_effect.RMSE.cv.degree0)
  put_end_date(start)
  put_log1("Final-tuning of the UMGY+(Smoothed)Day Effect Model has been completed
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
put_log1("Saving the final-tuned UMGY+(Smoothed)Day Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       rg.UMG_effect,
       rg.UMGY_effect,
       UMGYDE.final_tune.degree0.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("The final-tuned UMGY+(Smoothed)Day Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

# plot(UMGYDE.final_tune.degree0.result$param_values,
#      UMGYDE.final_tune.degree0.result$RMSEs)

UMGYDE.final_tune.degree0.result$tuned.result |>
  tuning.plot.right_detailed(title = "Finaljy tuned UMGY+(Smoothed)Day Model with `loess` parameter: `degree` = 0", 
                             title.right = "Right Part of the Chart Above (zoomed in)",
                             shift = 5,
                             xname = "parameter.value", 
                             yname = "RMSE", 
                             xlabel1 = "spans", 
                             ylabel1 = "RMSE")

put_log("Final-tuning stage of the UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 0` has ended up with with the following results:")
put(UMGYDE.final_tune.degree0.result)

##### Add a row to the RMSE Result Tibble for the User+Movie+Genre+Date Effects Model ---- 
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("Tuned User+Movie+Genre+Year+(Smoothed)Day Effect Model", 
               UMGYDE.final_tune.degree0.result$best_result["best_RMSE"],
               comment = "Computed using function call: `loess(degree = 0, span = %1)`" |>
  msg.glue(UMGYDE.final_tune.degree0.result$best_result["param.best_value"]))

RMSE_kable(RMSEs.ResultTibble)
##### Close Log -----------------------------------------------------------------
log_close()
###### 2. `degree = 1` --------------------------------------------------------------
###### Open log file for tuning the model using `loess` function with parameter `degree = 1`----
open_logfile(".UMGYDE.loess.degree1.pre-tuning")
put("Case 2. `degree = 1`")
###### Preliminary setting-up of spans range ----------------------------------
file_name_tmp <- "1.pre-tune.degree1.UMGYD-effect.RData"
file_path_tmp <- file.path(UMGYDE.tuning.degree1.data.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading Preliminary set-up data for tuning UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 1` from file: %1...", 
file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Preliminary set-up data for tuning UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 1` has been loaded from file: %1", 
file_path_tmp)
} else {
  put_log1("Preliminary setting-up of spans range for tuning UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 1` for %1-Fold Cross Validation samples...",
CVFolds_N)
  
  start <- put_start_date()
  spans <- seq(0.0005, 1, 0.001)
  cv.UMGYDE.degree1.pretune.result <- 
    tune.model_param(spans, train_UMGY_SmoothedDay_effect.RMSE.cv.degree1)
  put_end_date(start)
  put_log1("Preliminary set-up data for tuning UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 1` has been computed 
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
  plot(cv.UMGYDE.degree1.pretune.result$param_values,
       cv.UMGYDE.degree1.pretune.result$RMSEs)
  
  put_log1("Saving UMGY+(Smoothed)Day Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       rg.UMG_effect,
       rg.UMGY_effect,
       cv.UMGYDE.degree1.pretune.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("UMGY+(Smoothed)Day Effect Model data has been saved to file: %1", 
           file_path_tmp)
}

put_log("Preliminary tuning stage of the UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 1` has ended up with with the following results:")
put(cv.UMGYDE.degree1.pretune.result$best_result)
# param.best_value        best_RMSE 
#        0.0015000        0.8576205 

###### Close Log -----------------------------------------------------------------
log_close()

###### Open log file for fine-tuning the model using `loess` function with parameter `degree = 1`----
open_logfile(".UMGYD-effect.loess.degree1.fine-tuning")
###### Fine-tuning of a span parameter value for `degree = 1` ------------------ 
UMGYDE.fine_tune.degree1.loop_starter <- c(cv.UMGYDE.degree1.pretune.result$param_values[1], 
                                           cv.UMGYDE.degree1.pretune.result$param_values[3], 
                                           8)
# loop_starter <- c(0.0005, 0.0025, 8)
UMGYDE.fine_tune.degree1.cache_file.base_name <- "UMGYDE.degree1.tuning-span"

UMGYDE.fine_tune.result.degree1 <- 
  model.tune.param_range(UMGYDE.fine_tune.degree1.loop_starter,
                         UMGYDE.fine_tune.degree1.data.path,
                         UMGYDE.fine_tune.degree1.cache_file.base_name,
                         train_UMGY_SmoothedDay_effect.RMSE.cv.degree1)

put_log("Fine-tuning stage of the UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 1` has ended up with with the following results:")
put(UMGYDE.fine_tune.result.degree1)
# param.best_value        best_RMSE 
#        0.0009375        0.8568602 
###### Close Log ---------------------------------------------------------------
log_close()
##### Open log file for final-tuning of the model with refined `span` range ----
open_logfile(".UMGYDE.final-tuning.degree1")
##### Final tuning of the model with `degree = 1` and refined span range -------
file_name_tmp <- "2.UMGYDE.final-tune.degree1.RData" # UME stands for `User+Movie Effect`
file_path_tmp <- file.path(UMGYDE.tuning.degree1.data.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading saved data for the final-tuned UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 1` from file: %1...", 
file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("The data for the final-tune UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 1` has been loaded from file: %1", 
file_path_tmp)
} else {
  put_log1("Final-tuning of the UMGY+(Smoothed)Day Effect Model for %1-Fold Cross Validation samples...",
           CVFolds_N)
  
  fine_tuning.result <- UMGYDE.fine_tune.result.degree1
  seq_start <- fine_tuning.result$param_values.endpoints[1]
  seq_end <- fine_tuning.result$param_values.endpoints[2]
  seq_step <- (seq_end - seq_start)/64  
  
  spans <- seq(seq_start, seq_end, seq_step)
  
  start <- put_start_date()
  UMGYDE.final_tune.degree1.result <- 
    tune.model_param(spans, train_UMGY_SmoothedDay_effect.RMSE.cv.degree1)
  put_end_date(start)
  put_log1("Final-tuning of the UMGY+(Smoothed)Day Effect Model has been completed
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
  plot(UMGYDE.final_tune.degree1.result$param_values,
       UMGYDE.final_tune.degree1.result$RMSEs)
  
  put_log1("Saving the final-tuned UMGY+(Smoothed)Day Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       rg.UMG_effect,
       rg.UMGY_effect,
       UMGYDE.final_tune.degree1.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("The final-tuned UMGY+(Smoothed)Day Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

put_log("Final-tuning stage of the UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 1` has ended up with with the following results:")
put(UMGYDE.final_tune.degree1.result)
# param.best_value        best_RMSE 
#      0.001079102      0.856860200 
##### Close Log -----------------------------------------------------------------
log_close()
##### Open log file for re-training of the model with the best `span` value ----
open_logfile(".UMGYDE.re-train.degree1.best_span")
#### Re-train the model for `degree = 1` and the best `span` value ----
file_name_tmp <- "3.UMGYE.re-train.degree1.best-span.RData"
file_path_tmp <- file.path(UMGYDE.tuning.degree1.data.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading saved data for the re-trained UMGY+(Smoothed)Day Effect Model
using `loess` function with the best `span` value and `degree = 1` parameters from file: %1...", 
file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("The data of the re-trained UMGY+(Smoothed)Day Effect Model
using `loess` function with the best `span` value and `degree = 1` parameters 
has been loaded from file: %1", 
file_path_tmp)
} else {
  UMGYDE.tuned.degree1.best_span <- UMGYDE.final_tune.degree1.result$best_result["param.best_value"]
  final_tuned.best_RMSE <- UMGYDE.final_tune.degree1.result$best_result["best_RMSE"]
  
  put_log1("Re-training UMGY+(Smoothed)Day Effect Model 
for the best `span` value and `degree = 1` parameters: %1...",
UMGYDE.tuned.degree1.best_span)
  
  tuned.degree1.best_span.UMGYD_effect <- 
    train_UMGY_SmoothedDay_effect.cv(degree = 1,
                                     span = UMGYDE.tuned.degree1.best_span)
  
  tuned.degree1.best_span.UMGYDE.RMSE <- 
    calc_UMGY_SmoothedDay_effect.RMSE.cv(tuned.degree1.best_span.UMGYD_effect)
  
  put_log2("RMSE has been computed for UMGY+(Smoothed)Day Effect 
for the best `span` value (%1) and `degree = 1` parameters: %2.",
UMGYDE.tuned.degree1.best_span,
tuned.degree1.best_span.UMGYDE.RMSE)
  put_log1("Is this a best RMSE? %1",
           final_tuned.best_RMSE == tuned.degree1.best_span.UMGYDE.RMSE)
  
  
  put_log1("Saving User+Movie+Genre+Year Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       rg.UMG_effect,
       rg.UMGY_effect,
       UMGYDE.final_tune.degree1.result,
       tuned.degree1.best_span.UMGYD_effect,
       UMGYDE.tuned.degree1.best_span,
       tuned.degree1.best_span.UMGYDE.RMSE,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("Data for the tuned UMGY+(Smoothed)Day Effect Model 
with the best `span` value and `degree = 1` parameters has been saved to file: %1", 
file_path_tmp)
}

put_log2("Completed re-training UMGY+(Smoothed)Day Effect Model
for `degree = 1` and `span = %1` (the best span value) parameters
with the following result RMSE: %2...",
UMGYDE.tuned.degree1.best_span,
tuned.degree1.best_span.UMGYDE.RMSE)

##### Add a row to the RMSE Result Tibble for the User+Movie+Genre+Date Effects Model ---- 
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("Tuned User+Movie+Genre+Year+(Smoothed)Day Effect Model", 
               tuned.degree1.best_span.UMGYDE.RMSE,
               comment = "Computed using function call: `loess(degree = 1, span = %1)`" |>
                 msg.glue(UMGYDE.tuned.degree1.best_span))

RMSE_kable(RMSEs.ResultTibble)
##### Close Log -----------------------------------------------------------------
log_close()
###### 3. `degree = 2` --------------------------------------------------------------
###### Open log file for tuning the model using `loess` function with parameter `degree = 2`----
open_logfile(".UMGYDE.loess.degree2.pre-tuning")
put("Case 2. `degree = 2`")
###### Preliminary setting-up of spans range ----------------------------------
file_name_tmp <- "1.pre-tune.degree2.UMGYD-effect.RData"
file_path_tmp <- file.path(UMGYDE.tuning.degree2.data.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading Preliminary set-up data for tuning UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 2` from file: %1...", 
file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Preliminary set-up data for tuning UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 2` has been loaded from file: %1", 
file_path_tmp)
} else {
  put_log1("Preliminary setting-up of spans range for tuning UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 2` for %1-Fold Cross Validation samples...",
CVFolds_N)
  
  start <- put_start_date()
  spans <- seq(0.0005, 1, 0.001)
  cv.UMGYDE.degree2.pretune.result <- 
    tune.model_param(spans, train_UMGY_SmoothedDay_effect.RMSE.cv.degree2)
  put_end_date(start)
  put_log1("Preliminary set-up data for tuning UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 2` has been computed 
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
  plot(cv.UMGYDE.degree2.pretune.result$param_values,
       cv.UMGYDE.degree2.pretune.result$RMSEs)
  
  put_log1("Saving UMGY+(Smoothed)Day Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       rg.UMG_effect,
       rg.UMGY_effect,
       cv.UMGYDE.degree2.pretune.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("UMGY+(Smoothed)Day Effect Model data has been saved to file: %1", 
           file_path_tmp)
}

put_log("Preliminary tuning stage of the UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 2` has ended up with with the following results:")
put(cv.UMGYDE.degree2.pretune.result$best_result)
# param.best_value        best_RMSE 
#        0.0015000        0.8571511 
###### Close Log -----------------------------------------------------------------
log_close()

###### Open log file for fine-tuning the model using `loess` function with parameter `degree = 2`----
open_logfile(".UMGYD-effect.loess.degree2.fine-tuning")
###### Fine-tuning of a span parameter value for `degree = 2` ------------------ 
UMGYDE.fine_tune.degree2.loop_starter <- c(cv.UMGYDE.degree2.pretune.result$param_values[1], 
                                           cv.UMGYDE.degree2.pretune.result$param_values[3], 
                                           8)
# loop_starter <- c(0.0005, 0.0025, 8)
UMGYDE.fine_tune.degree2.cache_file.base_name <- "UMGYDE.degree2.tuning-span"

UMGYDE.fine_tune.result.degree2 <- 
  model.tune.param_range(UMGYDE.fine_tune.degree2.loop_starter,
                         UMGYDE.fine_tune.degree2.data.path,
                         UMGYDE.fine_tune.degree2.cache_file.base_name,
                         train_UMGY_SmoothedDay_effect.RMSE.cv.degree2)

put_log("Fine-tuning stage of the UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 2` has ended up with with the following results:")
put(UMGYDE.fine_tune.result.degree2)
# $best_result
# param.best_value        best_RMSE 
#        0.0013750        0.8571511 
###### Close Log ---------------------------------------------------------------
log_close()
##### Open log file for final-tuning of the model with refined `span` range ----
open_logfile(".UMGYDE.final-tuning.degree2")
##### Final tuning of the model with `degree = 2` and refined span range -------
file_name_tmp <- "2.UMGYDE.final-tune.degree2.RData" # UME stands for `User+Movie Effect`
file_path_tmp <- file.path(UMGYDE.tuning.degree2.data.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading saved data for the final-tuned UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 2` from file: %1...", 
file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("The data for the final-tune UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 2` has been loaded from file: %1", 
file_path_tmp)
} else {
  put_log1("Final-tuning of the UMGY+(Smoothed)Day Effect Model for %1-Fold Cross Validation samples...",
           CVFolds_N)
  
  fine_tuning.result <- UMGYDE.fine_tune.result.degree2
  seq_start <- fine_tuning.result$param_values.endpoints[1]
  seq_end <- fine_tuning.result$param_values.endpoints[2]
  seq_step <- (seq_end - seq_start)/64  
  
  spans <- seq(seq_start, seq_end, seq_step)
  
  start <- put_start_date()
  UMGYDE.final_tune.degree2.result <- 
    tune.model_param(spans, train_UMGY_SmoothedDay_effect.RMSE.cv.degree2)
  put_end_date(start)
  put_log1("Final-tuning of the UMGY+(Smoothed)Day Effect Model has been completed
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
  plot(UMGYDE.final_tune.degree2.result$param_values,
       UMGYDE.final_tune.degree2.result$RMSEs)
  
  put_log1("Saving the final-tuned UMGY+(Smoothed)Day Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       rg.UMG_effect,
       rg.UMGY_effect,
       UMGYDE.final_tune.degree2.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("The final-tuned UMGY+(Smoothed)Day Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

put_log("Final-tuning stage of the UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 2` has ended up with with the following results:")
put(UMGYDE.final_tune.degree2.result)
# $best_result
# param.best_value        best_RMSE 
#      0.001516602      0.857151071 
##### Close Log -----------------------------------------------------------------
log_close()
##### Open log file for re-training of the model with the best `span` value ----
open_logfile(".UMGYDE.re-train.degree2.best_span")
#### Re-train the model for `degree = 2` and the best `span` value ----
file_name_tmp <- "3.UMGYE.re-train.degree2.best-span.RData"
file_path_tmp <- file.path(UMGYDE.tuning.degree2.data.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading saved data for the re-trained UMGY+(Smoothed)Day Effect Model
using `loess` function with the best `span` value and `degree = 2` parameters from file: %1...", 
file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("The data of the re-trained UMGY+(Smoothed)Day Effect Model
using `loess` function with the best `span` value and `degree = 2` parameters 
has been loaded from file: %1", 
file_path_tmp)
} else {
  UMGYDE.tuned.degree2.best_span <- UMGYDE.final_tune.degree2.result$best_result["param.best_value"]
  final_tuned.best_RMSE <- UMGYDE.final_tune.degree2.result$best_result["best_RMSE"]
  
  put_log1("Re-training UMGY+(Smoothed)Day Effect Model 
for the best `span` value and `degree = 2` parameters: %1...",
UMGYDE.tuned.degree2.best_span)
  
  tuned.degree2.best_span.UMGYD_effect <- 
    train_UMGY_SmoothedDay_effect.cv(degree = 2,
                                     span = UMGYDE.tuned.degree2.best_span)
  
  tuned.degree2.best_span.UMGYDE.RMSE <- 
    calc_UMGY_SmoothedDay_effect.RMSE.cv(tuned.degree2.best_span.UMGYD_effect)
  
  put_log2("RMSE has been computed for UMGY+(Smoothed)Day Effect 
for the best `span` value (%1) and `degree = 2` parameters: %2.",
UMGYDE.tuned.degree2.best_span,
tuned.degree2.best_span.UMGYDE.RMSE)
  put_log1("Is this a best RMSE? %1",
           final_tuned.best_RMSE == tuned.degree2.best_span.UMGYDE.RMSE)
  
  
  put_log1("Saving User+Movie+Genre+Year Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       rg.UMG_effect,
       rg.UMGY_effect,
       UMGYDE.final_tune.degree2.result,
       tuned.degree2.best_span.UMGYD_effect,
       UMGYDE.tuned.degree2.best_span,
       tuned.degree2.best_span.UMGYDE.RMSE,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("Data for the tuned UMGY+(Smoothed)Day Effect Model 
with the best `span` value and `degree = 2` parameters has been saved to file: %1", 
file_path_tmp)
}

put_log2("Completed re-training UMGY+(Smoothed)Day Effect Model
for `degree = 2` and `span = %1` (the best span value) parameters
with the following result RMSE: %2...",
UMGYDE.tuned.degree2.best_span,
tuned.degree2.best_span.UMGYDE.RMSE)

##### Add a row to the RMSE Result Tibble for the User+Movie+Genre+Date Effects Model ---- 
RMSEs.ResultTibble <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("Tuned User+Movie+Genre+Year+(Smoothed)Day Effect Model", 
               tuned.degree2.best_span.UMGYDE.RMSE,
               comment = "Computed using function call: `loess(degree = 2, span = %1)`" |>
                 msg.glue(UMGYDE.tuned.degree2.best_span))

RMSE_kable(RMSEs.ResultTibble)
##### Close Log -----------------------------------------------------------------
log_close()

#### Retrain with the best parameters figured out above ------------------------
file_name_tmp <- "4.UMGYE.re-train.loess.best-params.RData"
file_path_tmp <- file.path(UMGYDE.tuning.data.path, file_name_tmp)

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
                           span = c(cv.UMGYSD.tuned.dgr0.best_RMSE[1], degree1_best_RMSE[1], degree2_best_RMSE[1]),
                           rmse = c(cv.UMGYSD.tuned.dgr0.best_RMSE[2], degree1_best_RMSE[2], degree2_best_RMSE[2]))
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
  best_day_smoothed_effect <- train_UMGY_SmoothedDay_effect.cv(day_loess_best_degree, day_loess_best_span)
  str(best_day_smoothed_effect)
  put_end_date(start)
  put_log2("The model has been re-trained using `loess` function with the best parameters: 
span = %1, degree = %2", day_loess_best_span, day_loess_best_degree)
  
  put_log1("Saving Tuned Day Smoothed Effect data to file: %1", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       UMG_effect,
       gnr_mean_ratings.cv,
       UMG_effect,
       user_movie_genre_reg_lambdas_6p6_m4p2_p2,
       user_movie_genre_reg_RMSEs_m66_42_0_2,
       umgy_tune_sets,
       # cv.UMGDG_effect,
       # cv.UMGY_effect._effect,
       cv.UMGDG_Day_effect,
       # cv.UMGYDE.default_params,
       degree,
       degree0.spans,
       cv.UMGYSD.tuned.dgr0.RMSEs,
       cv.UMGYSD.tuned.dgr0.best_RMSE,
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
  RMSEs.AddRow("Tuned User+Movie+Genre+Year+(Smoothed)Day Effect Model", 
               user_movie_genre_tuned_date_effect_RMSE)

RMSE_kable(RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble 
for the tuned `User+Movie+Genre+Year+(Smoothed)Day Effect Model`.")

# start <- put_start_date()
# final_holdout_test |>
#   left_join(date_days_map, by = "timestamp") |>
#   left_join(user_effect, by = "userId") |>
#   left_join(mean_user_movie_genre_bias, by = "movieId") |>
#   left_join(# cv.UMGYDE.default_params, by='days') |>
#   mutate(resid = rating - clamp(mu + a + b + g + de_smoothed)) |>
#   filter(!is.na(resid)) |>
#   pull(resid) |> final_rmse()
# put_end_date(start)
#> [1] 0.8724055

#### Close Log -----------------------------------------------------------------
log_close()

