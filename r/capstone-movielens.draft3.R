## Setup -----------------------------------------------------------------------
#> Reference: Some ideas and code snippers were used from the following GitHub repository:
#> https://github.com/AzKurban-edX-DS/harvardx-movielens


if(!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table))
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(gridExtra))
  install.packages("gridExtra")

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

library(rafalib)
library(gtools)
library(pak)
library(pacman)

p_load(conflicted, latex2exp, kableExtra)

# For functions with identical names in different packages, ensure the
# right one is chosen:
conflict_prefer("first", "dplyr")
conflict_prefer("count", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("kable", "kableExtra")
conflict_prefer("year", "lubridate")

# Project Objective according to Capstone course requirements:
project_objective <- 0.86490

### Defining helper functions --------------------------------------------------

#> Let's define some helper functions that we will use in our subsequent analysis:
start_date <- function(){
  print(date())
  Sys.time()
}
end_date <- function(start){
  print(date())
  print(Sys.time() - start)
}

mse <- function(r) mean(r^2, rm.na = TRUE)
mse_cv <- function(r_list) {
  mses <- sapply(r_list, mse(r))
  mean(mses)
}

rmse <- function(r) sqrt(mse(r))
rmse_cv <- function(r_list) sqrt(mse_cv(r_list))
rmse <- function(true_ratings, predicted_ratings) {
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
  start <- start_date()
  pak::pak("AzKurban-edX-DS/edx.capstone.movielens.data")
  end_date(start)
}

kfold_index <- seq(from = 1:10)

load_movielens_data_from_file <- function(file_path){
  print(sprintf("Loading MovieLens datasets from file: %s...", 
                file_path))
  start <- start_date()
  load(movielens_datasets_file)
  end_date(start)
  print(sprintf("MoviLens datasets have been loaded from file: %s.", 
                file_path))
}

make_source_datasets <- function(){
  edx <- edx.capstone.movielens.data::edx
  final_holdout_test <- edx.capstone.movielens.data::final_holdout_test
  
  print("Dataset loaded from `edx.capstone.movielens.data` package: edx")
  print(summary(edx))

  print("Dataset loaded from `edx.capstone.movielens.data` package: final_holdout_test")
  print(summary(final_holdout_test))
  
  #> To be able to map movie IDs to titles we create the following lookup table:
  movie_map <- edx |> select(movieId, title, genres) |> 
    distinct(movieId, .keep_all = TRUE)
  
  print("Dataset created: movie_map")
  print(summary(movie_map))
  
  # We ignore the data for users who have not provided at least 100 ratings:
  edx100 <- edx |> 
    group_by(userId) |>
    filter(n() >= 100) |>
    ungroup() |>
    mutate(date_time = as_datetime(timestamp)) |>
    mutate(date = as_date(date_time)) |>
    mutate(days = as.integer(date - min(date)))

  print("Dataset created: edx100")
  print(summary(edx100))
  print(edx100 |> summarize(n_distinct(userId), n_distinct(movieId)))
  
  #> To account for movie genre effect we will need in dataset with splitted rows
  #> for movies that belong to multiple genres:
  start <- start_date()
  edx100_GS <- edx100 |>
    separate_rows(genres, sep = "\\|")
  end_date(start)
  
  print("Dataset created: edx100_GS")
  print(str(edx100_GS))
  print(head(edx100_GS))
  print(summary(edx100_GS))

  #> We split the initial datasets into training sets, which we will use to build 
  #> and train our models, and validation sets in which we will compute the accuracy 
  #> of our predictions, the way described in the `Section 23.1.1 Movielens data`
  #> (https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#movielens-data) 
  #> of the Course Textbook:
  
  #> For each user we are going to process, we will split their ratings 
  #> into 80% for training and 20% for testing:

  edx100_indexes <- split(1:nrow(edx100), edx100$userId)
  edx100_GS_indexes <- split(1:nrow(edx100_GS), edx100_GS$userId)

  #> We will also use K-fold cross validation as explained in 
  #> Section 29.6.1: "K-fold validation" of the Cource Textbook:
  #> https://rafalab.dfci.harvard.edu/dsbook-part-2/ml/resampling-methods.html#k-fold-cross-validation
  #> We are going to compute the following version of the MSE introducing in that section:
  
  # $$
  #   \mbox{MSE}(\lambda) \approx\frac{1}{B} \sum_{b = 1}^B \frac{1}{N}\sum_{i = 1}^N \left(\hat{y}_i^b(\lambda) - y_i^b\right)^2 
  # $$

  start <- start_date()
  edx100_CV <- lapply(kfold_index,  function(fi){
    print(sprintf("Creating K-Fold Cross Validation Datasets, Fold %s", fi))
    set.seed(fi*1000)
    validation_ind <- sapply(edx100_indexes, 
                             function(i) sample(i, ceiling(length(i)*.2))) |> 
      unlist() |> 
      sort()
    
    validation_set <- edx100[validation_ind,]
    train_set <- edx100[-validation_ind,]

    print("Dataset created: train_set")
    print(summary(train_set))

    set.seed(fi*2000)
    validation_gs_ind <- sapply(edx100_GS_indexes, 
                                function(i) sample(i, ceiling(length(i)*.2))) |> 
      unlist() |> 
      sort()
    
    validation_gs_set <- edx100_GS[validation_gs_ind,] 
    train_gs_set <- edx100_GS[-validation_gs_ind,]

    print("Dataset created: train_gs_set")
    print(summary(train_gs_set))

    # To make sure we don’t include movies in the training set that should not be 
    # there, we remove entries using the semi_join function:
    validation_set <- validation_set |> 
      semi_join(train_set, by = "movieId") |> 
      as.data.frame()
    
    print("Dataset created: validation_set")
    print(summary(validation_set))

    validation_gs_set <- validation_gs_set |> 
      semi_join(train_gs_set, by = "movieId") |> 
      as.data.frame()
    
    print("Dataset created: validation_gs_set")
    print(summary(validation_gs_set))

    #> We will use the array representation described in `Section 17.5 of the Textbook`
    #> (https://rafalab.dfci.harvard.edu/dsbook-part-2/linear-models/treatment-effect-models.html#sec-anova), 
    #> for the training data. 
    #> To create this matrix, we use `tidyr::pivot_wider` function:
    
    # train_set <- mutate(train_set, userId = factor(userId), movieId = factor(movieId))
    # train_gs_set <- mutate(train_gs_set, userId = factor(userId), movieId = factor(movieId))
    
    train_mx <- train_set |> 
      mutate(userId = factor(userId),
             movieId = factor(movieId)) |>
      select(movieId, userId, rating) |>
      pivot_wider(names_from = movieId, values_from = rating) |>
      column_to_rownames("userId") |>
      as.matrix()
    
    print("Matrix created: train_mx")
    print(dim(train_mx))

    list(train_set = train_set,
         train_gs_set = train_gs_set,
         train_mx = train_mx, 
         validation_set = validation_set,
         validation_gs_set = validation_gs_set)
  })
  end_date(start)
  print("Set of K-Fold Cross Validation datasets created: edx100_CV")

  list(edx = edx,
       edx100 = edx100,
       edx100_GS = edx100_GS,
       edx100_CV = edx100_CV,
       movie_map = movie_map,
       final_holdout_test = final_holdout_test)
}

data_path <- "data"
movielens_datasets_file <- file.path(data_path, "movielens-datasets.RData")
movielens_datasets_zip <- file.path(data_path, "movielens-datasets.zip")

if(file.exists(movielens_datasets_file)){
  load_movielens_data_from_file(movielens_datasets_file)
} else if(file.exists(movielens_datasets_zip)) {
  print(sprintf("Unzipping MovieLens data file from zip-archive: %s...", 
                movielens_datasets_zip))
  start <- start_date()
  unzip(movielens_datasets_zip, movielens_datasets_file, exdir = data_path)
  
  if(!file.exists(movielens_datasets_file)) {
    print(sprintf("File does not exists: %s:", movielens_datasets_file))
    stop("Failed to unzip MovieLens data zip-archive.")
  }
  
  load_movielens_data_from_file(movielens_datasets_file)
} else {
  print("Creating datasets...")
  library(edx.capstone.movielens.data)
  print("Library attached: 'edx.capstone.movielens.data'")

  start <- start_date()
  movielens_datasets <- make_source_datasets()
  end_date(start)
  print("All required datasets have been created.")
  
  print("Saving newly created input datasets to file...")
  start <- start_date()
  dir.create(data_path)
  save(movielens_datasets, file =  movielens_datasets_file)
  end_date(start)
  
  if(!file.exists(movielens_datasets_file)) {
    print(sprintf("File was not created: %s.", movielens_datasets_file))
    warning("MovieLens data was not saved to file.")
  } else {
    print(sprintf("Datasets have been saved to file: %s.", 
                  movielens_datasets_file))
    print(sprintf("Creating zip-archive: %s...", 
                  movielens_datasets_zip))
    
    zip(movielens_datasets_zip, movielens_datasets_file)
    
    if(!file.exists(movielens_datasets_zip)){
      print(sprintf("Failed to zip file: %s.", movielens_datasets_file))
      warning("Failed to zip MovieLens data file.")
    } else {
      print(sprintf("Zip-archive created: %s.", movielens_datasets_zip))
      #file.remove(movielens_datasets_file)
      
      if(file.exists(movielens_datasets_file)){
        print(sprintf("Failed to remove file: %s.", movielens_datasets_file))
        warning("Failed to remove MovieLens data file.")
      } else {
        print(sprintf("File has been removed: %s.", movielens_datasets_file))
      }
    }
  }
}

edx <- movielens_datasets$edx
print("Dataset summary: edx")
print(summary(edx))

edx100 <- movielens_datasets$edx100
print("Dataset summary: edx100")
print(summary(edx100))


edx100_GS <- movielens_datasets$edx100_GS
print("Dataset summary: edx100_GS")
print(summary(edx100_GS))

edx100_CV <- movielens_datasets$edx100_CV
print("Set of K-Fold Cross Validation datasets summary: edx100_CV")
print(summary(edx100_CV))

movie_map <- movielens_datasets$movie_map
print("Dataset summary: movie_map")
print(summary(movie_map))

final_holdout_test <- movielens_datasets$final_holdout_test
print("Dataset summary: final_holdout_test")
print(summary(final_holdout_test))

## Data Analysis ===============================================================
### `edx` Dataset --------------------------------------------------------------

# Let's look into the details of the `edx` dataset:
#> First, let's note that we have 10677 different movies: 
n_movies <- n_distinct(edx$movieId)
print(n_movies)

# and 69878 different users in the dataset:
n_users <- n_distinct(edx$userId)
print(n_users)

#> Also, we can see that no movies have a rating of 0. 
#> Movies are rated from 0.5 to 5.0 in 0.5 increments:

#library(dplyr)
s <- edx |> group_by(rating) |>
  summarise(n = n())
print(s)

#> Now, note the expressions below which confirm the fact explained in 
#> Section 23.1.1 Movielens data
#> (https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#movielens-data) 
#> of the Course Textbook that not every user rated every movie:

max_possible_ratings <- n_movies*n_users
sprintf("Maximum possible ratings: %s", max_possible_ratings)
sprintf("Rows in `edx` dataset: %s", dim_edx[1])
sprintf("Not every movie was rated: %s", max_possible_ratings > dim_edx[1])

#> We can think of a recommendation system as filling in the `NA`s in the dataset 
#> for the movies that some or all the users do not rate. 
#> A sample from the `edx` data below illustrates this idea: 

keep <- edx |> 
  dplyr::count(movieId) |> 
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


#### Movies' Popularity --------------------------------------------------------
#> Further, we can find out the movies that have the greatest number of ratings 
#> using the following code:

ordered_movie_ratings <- edx |> group_by(movieId, title) |>
  summarize(number_of_ratings = n()) |>
  arrange(desc(number_of_ratings))
print(head(ordered_movie_ratings))

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

## Methods ==========================================================
# Create an RMSE Result Table and add a first row for the Project Objective ----

# Add the RMSE value of the Naive Model to a tibble.
RMSEs <- tibble(Method = c("Project Objective"),
                RMSE = project_objective)
rmse_kable()

### Naive Model ----------------------------------------------------------------
# Reference: the Textbook section "23.3 A first model"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#a-first-model

#>  A model that assumes the same rating for all movies and users with all 
#>  the differences explained by random variation would look as follows:
# Y[i,j] = μ + ε[i,j]

### Naive RMSE -------------------------------------------------------
#sum(is.na(edx100$rating))
#> [1] 0

ratings_avg <- sapply(edx100_CV, function(cv_item){
  mean(cv_item$train_set$rating, na.rm = TRUE)
})
plot(ratings_avg)

mu <- mean(ratings_avg)
print(mu)
#> [1] 3.472081

# If we predict all unknown ratings with `μ`, we obtain the following RMSE: 
# naive_rmse <- rmse(test_set$rating - mu)
#> [1] 1.05508

mses <- sapply(edx100_CV, function(cv_item){
  mse(cv_item$validation_set$rating - mu)
})

plot(mses)

naive_rmse <- sqrt(mean(mses))
print(naive_rmse)
#> [1] 1.055951

# Ensure that this is the best RMSE value for the current model ----------------
#> If we plug in any other number, we will get a higher RMSE. 
#> Let's prove that by the following small investigation:


deviation <- seq(0, 6, 0.1) - 3
deviation

mse_test_results <- lapply(kfold_index, function(i){
  cv_item <- edx100_CV[[i]]
  mse_val <-mses[i] 
  mse_values <- sapply(deviation, function(diff){
    mse(cv_item$validation_set$rating - mu + diff)
  })
  
  data.frame(deviation = deviation, 
                    mse_values = mse_values) |> 
    ggplot(aes(deviation, mse_values)) +
    geom_line()
})

n <- length(mse_test_results)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(mse_test_results, ncol = nCol))

#str(mse_test_results[[1]]$data)

for (i in kfold_index) {
  dvs <- mse_test_results[[i]]$data$deviation
  test_vals <- mse_test_results[[i]]$data$mse_values
  mse_val <-mses[i] 
  
  which_min_deviation <- dvs[which.min(test_vals)]
  min_mse = min(test_vals)
  
  validation_head <- sprintf("For Validation Set %s:", i)
  print(validation_head)
  
  print(sprintf("Minimum MSE is achieved when the deviation from the mean is: %s", 
          which_min_deviation))
  #> [1] "Minimum RMSE is achieved when the deviation from the mean is: 0"
  
  print(sprintf("Is the previously computed RMSE the best for the current model: %s",
          mse_val == min_mse))
  #> [1] "Is the previously computed RMSE the best for the current model: TRUE"
  writeLines("")
}

# Add a row to the RMSE Result Table for the first Naive Model ---------------- 
RMSEs <- rmses_add_row("Simple Mean Rating Model", naive_rmse)
rmse_kable()

### Taking into account User effects ------------------------------------------- 
# Reference: the Textbook section "23.4 User effects"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#user-effects

#### Model building: User Effects ----------------------------------------------

# Let's visualize the average rating for each user:
start <- start_date()
user_ratings_avg_ls <- sapply(edx100_CV, function(cv_item){
  rowMeans(cv_item$train_mx, na.rm = TRUE)
})
str(user_ratings_avg_ls)

y <- as.matrix(user_ratings_avg_ls)
end_date(start)

dim(y)
head(y)
#        [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]     [,9]    [,10]
# 8  3.401893 3.395869 3.367470 3.393287 3.389845 3.382100 3.409639 3.383821 3.398451 3.351119
# 10 3.764045 3.775281 3.820225 3.820225 3.898876 3.887640 3.842697 3.775281 3.876404 3.820225
# 13 3.320000 3.370000 3.290000 3.370000 3.340000 3.270000 3.270000 3.300000 3.320000 3.330000
# 18 3.479239 3.479239 3.422145 3.444637 3.420415 3.444637 3.425606 3.463668 3.422145 3.461938
# 19 3.680233 3.767442 3.726744 3.738372 3.784884 3.726744 3.790698 3.662791 3.703488 3.726744
# 30 4.446429 4.508929 4.491071 4.500000 4.491071 4.482143 4.500000 4.500000 4.508929 4.517857
user_ratings_avg = rowMeans(y, na.rm = TRUE)
hist(user_ratings_avg, nclass = 30)
head(user_ratings_avg)
sum(is.na(user_ratings_avg))
#> [1] 0

# we notice that there is substantial variability across users.
#>  To account for this, we can use a linear model with a treatment effect `α[i]` 
#>  for each user. The sum `μ + α[i]` can be interpreted as the typical 
#>  rating user `i` gives to movies. We can write the model as:

# Y[i,j] = μ + α[i] + ε[i,j]

#> It can be shown that the least squares estimate `α[i]` is just the average 
#> of `y[i,j] - μ` for each user. So we can compute them this way:
a <- user_ratings_avg - mu

#> Finally, we are ready to compute the `RMSE` (additionally using the helper 
#> function `clamp` we defined above to keep predictions in the proper range):
user_effects <- data.frame(userId = as.integer(names(a)), a = a)
str(user_effects)
head(user_effects)

# Plot a histogram of the user effects
par(cex = 0.7)
hist(user_effects$a, 30, xlab = TeX(r'[$\hat{alpha}_{i}$]'),
     main = TeX(r'[Histogram of $\hat{alpha}_{i}$]'))

# Compute the RMSE taking into account user effects:
start <- start_date()
user_effect_mses <- sapply(edx100_CV, function(cv_item){
  #mse(cv_item$validation_set$rating - mu)
  cv_item$validation_set |>
    left_join(user_effects, by = "userId") |>
    mutate(resid = rating - clamp(mu + a)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> mse()
})
end_date(start)

plot(user_effect_mses)

user_effect_rmse <- sqrt(mean(user_effect_mses))
print(user_effect_rmse)
#> [1] 0.9684293

# Add a row to the RMSE Result Table for the User Effect Model ---------------- 
RMSEs <- rmses_add_row("User Effect Model", user_effect_rmse)
rmse_kable()

### Taking into account Movie effect ------------------------------------------

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

### Model building: User+Movie Effect -----------------------------------------

str(user_effects)

sapply(edx100_CV, function(cv_item){
  dim(cv_item$train_mx)
})

start <- start_date()


user_movie_effects_ls <- sapply(edx100_CV, function(cv_item){
  colMeans(cv_item$train_mx - user_effects$a - mu, na.rm = TRUE)
})
end_date(start)
str(user_movie_effects_ls)

b <- user_movie_effects_ls |> unlist()
str(b)

user_movie_effects <- 
  data.frame(movieId = names(b),                                     
             b = b) |>
  group_by(movieId) |>
  summarise(b = mean(b, na.rm = TRUE)) |>
  mutate(movieId = as.integer(movieId))

str(user_movie_effects)
head(user_movie_effects)

user_movie_effects <- data.frame(movieId = as.integer(names(b)), b = b)

# Plot a histogram of the User+Movie Effect -----------------------------------
par(cex = 0.7)
hist(user_movie_effects$b, 30, xlab = TeX(r'[$\hat{beta}_{j}$]'),
     main = TeX(r'[Histogram of $\hat{beta}_{j}$]'))

# Calculate MSEs on Validation Sets --------------------------------------------
start <- start_date()
user_movie_effects_mses <- sapply(edx100_CV, function(cv_item){
  cv_item$validation_set |>
    left_join(user_effects, by = "userId") |>
    left_join(user_movie_effects, by = "movieId") |>
    mutate(resid = rating - clamp(mu + a + b)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> mse()
})
end_date(start)

plot(user_movie_effects_mses)
user_movie_effects_rmse <- sqrt(mean(user_movie_effects_mses))

print(user_movie_effects_rmse)
#> [1] 0.8621376

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

genres_summary_list <- lapply(edx100_CV, function(cv_item){
  
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
print(genre_ratings_plot_dat)


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

start <- start_date()
user_movie_genre_effects_ls <- lapply(edx100_CV, function(cv_item){
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
end_date(start)
str(user_movie_genre_effects_ls)
#head(user_movie_genre_effects_ls)

# edx_cv_item <- edx100_CV[[1]]$train_set

# Compute Genre Movie Bias ----------------

start <- start_date()
mg_bias_b <- sapply(user_movie_genre_effects_ls, function(mg_bias){
  names(mg_bias$b) <- mg_bias$movieId
  mg_bias$b
})
end_date(start)
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

start <- start_date()
mg_bias_g <- sapply(user_movie_genre_effects_ls, function(mg_bias){
  names(mg_bias$g) <- mg_bias$movieId
  mg_bias$g
})
end_date(start)
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
start <- start_date()
user_movie_genre_effects_mses <- sapply(edx100_CV, function(cv_item){
  cv_item$validation_set |>
    left_join(user_effects, by = "userId") |>
    left_join(user_movie_genre_effects, by = "movieId") |>
    mutate(resid = rating - clamp(mu + a + b + g)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> mse()
})
end_date(start)

plot(user_movie_genre_effects_mses)
user_movie_genre_effects_rmse <- sqrt(mean(user_movie_genre_effects_mses))

print(user_movie_genre_effects_rmse)
#> [1] 0.8619763

# Add a row to the RMSE Result Table for the User+Movie+Genre Effect Model ---- 
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

# Accounting for Date Smoothed Effect ------------------------------------------
# Y[i,j] = μ + α[i] + β[j] + g[i,j]  + f(d(i,j)) + ε[i,j]

# with `j` a smooth function of `d(u,i)`

# library(lubridate)

# Let's take a look at the Average rating per year:
# Plot: Average Rating per Year ------------------------------------------------
start <- start_date()
edx100 |> 
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

end_date(start)

# Calculate Date Smoothed Effect -----------------------------------------------
train_set1 <- edx100_CV[[1]]$train_set

start <- start_date()
train_set_dm <- train_set1 |> 
  left_join(user_effects, by = "userId") |>
  left_join(genre_movie_effects, by = "movieId") |>
  mutate(rating_residue = rating - (mu + a + b + g)) |>
  filter(!is.na(rating_residue)) #|>
  # mutate(date_time = as_datetime(timestamp)) |>
  # mutate(date = as_date(date_time)) |>
  # mutate(days = as.integer(date - min(date)))

end_date(start)

str(train_set_dm)
min_date <- min(train_set_dm$date)
print(min_date)

start <- start_date()
date_global_effect <- train_set_dm |>
  group_by(days, date) |>
  summarise(de = mean(rating_residue))
end_date(start)

head(date_global_effect)
sum(is.na(date_global_effect$de))
head(date_global_effect)

# Train model using `loess` function with default `span` & `degree` params-----
start <- start_date()
fit <- loess(de ~ days, data = date_global_effect)
end_date(start)
date()
sum(is.na(fit$fitted))
str(fit$pars)
str(fit$fitted)

date_smoothed_effect <- as.data.frame(date_global_effect) |>
  mutate(de_smoothed = fit$fitted)
head(date_smoothed_effect)

start <- start_date()
date_smoothed_effect |>
  ggplot(aes(x = days)) +
  geom_point(aes(y = de), size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(y = de_smoothed), color = "red")
end_date(start)

# Re-train tuning `loess` function's `span` & `degree` params-------------------
fit_loess <- function(spans, dgr){
  fits <- sapply(spans, function(span){
    fit <- loess(de ~ days, span = span, degree = dgr, data = date_global_effect)
    fit$fitted
  })
}
predict_date_smoothed <- function(date_smoothed_effect){
  preds <- test_set |>
    mutate(date = as_date(as_datetime(timestamp))) |>
    left_join(data.frame(userId = as.integer(names(a)), a = a), by = "userId") |>
    left_join(data.frame(movieId = as.integer(names(b)), b = b_reg), by = "movieId") |>
    left_join(date_smoothed_effect, by='date') |>
    mutate(pred = mu + a + b + de_smoothed) |>
    pull(pred)
  
  RMSE(preds, test_set$rating)
}
predict_de_model <- function(fits){
  model_diu_rmses <- sapply(fits, function(smth){
    date_smoothed_effect <- as.data.frame(date_global_effect) |>
      mutate(de_smoothed = smth)

    predict_date_smoothed(date_smoothed_effect)
  })
}
date_smoothed_rmse <- function(spans, degree) {
  start <- start_date()
  fits <- fit_loess(spans,degree)
  end_date(start)
  
  dim(fits)
  df_fits <- as.data.frame(fits)
  #str(df_fits)
  
  start <- start_date()
  model_diu_rmses <- predict_de_model(df_fits)
  end_date(start)
  
  plot(model_diu_rmses)
  
  idx <- which.min(model_diu_rmses)
  c(spans[idx], min(model_diu_rmses))
}
#-------------------------------------------------------------------------

degree <- c(0, 1, 2)
# 1. `degree = 0` --------------------------------------------------------------
# spans <- seq(0.0003, 0.002, 0.00001)
spans <- seq(0.0005, 0.0015, 0.00001)
ds_rmse0 <- date_smoothed_rmse(spans, 
                               degree[1])
ds_rmse0
#> [1] 0.0010900 0.8644363

# 2. `degree = 1` --------------------------------------------------------------
#spans <- seq(0.0005, 0.002, 0.00001)
spans <- seq(0.001, 0.0014, 0.00001)
ds_rmse1 <- date_smoothed_rmse(spans, 
                               degree[2])
ds_rmse1
#> [1] 0.001000 0.863969

# 3. `degree = 2` --------------------------------------------------------------
#spans <- seq(0.0003, 0.01, 0.00001)
spans <- seq(0.0007, 0.002, 0.00001)
ds_rmse2 <- date_smoothed_rmse(spans, 
                               degree[3])
ds_rmse2
#> [1] 0.0013000 0.8638975

# Retrain with the best parameters figured out above ---------------------------

loess_rmse <- data.frame(degree = degree, 
                         span = c(ds_rmse0[1], ds_rmse1[1], ds_rmse2[1]),
                         rmse = c(ds_rmse0[2], ds_rmse1[2], ds_rmse2[2]))
print(loess_rmse)

idx_best_rmse <- which.min(loess_rmse$rmse)

best_degree <- loess_rmse[idx_best_rmse, 1]  # 1
best_span <- loess_rmse[idx_best_rmse, 2]# 0.00108
best_rmse <- loess_rmse[idx_best_rmse, 3]
print(best_rmse)

start <- start_date()
fit <- loess(de ~ days, 
             degree = best_degree, 
             span = best_span, 
             data = date_global_effect)
end_date(start)
sum(is.na(fit$fitted))
str(fit$pars)
str(fit$fitted)
#fit$pars
# fit$fitted

date_smoothed_effect <- as.data.frame(date_global_effect) |>
  mutate(de_smoothed = fit$fitted)
head(date_smoothed_effect)

start <- start_date()
date_smoothed_effect |>
  ggplot(aes(x = days)) +
  geom_point(aes(y = de), size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(y = de_smoothed), color = "red")
end_date(start)

date_smoothed_rmse <- predict_date_smoothed(date_smoothed_effect)
print(date_smoothed_rmse)
#> [1] 0.8638975

# Add a row to the RMSE Result Table for the User+Movie+Genre Effects Model ---- 
RMSEs <- rmses_add_row("Accounted for User+Movie+Genre+Date Effects", 
                       date_smoothed_rmse)
rmse_kable()

# final_holdout_test |>
#   left_join(user_effects, by = "userId") |>
#   left_join(user_movie_genre_effects, by = "movieId") |>
#   mutate(resid = rating - clamp(mu + a + b + g)) |> 
#   filter(!is.na(resid)) |>
#   pull(resid) |> final_rmse()
#> [1] 0.8659243


# preds <- final_holdout_test |>
#   mutate(date = as_date(as_datetime(timestamp))) |>
#   left_join(data.frame(userId = as.integer(names(a)), a = a), by = "userId") |>
#   left_join(data.frame(movieId = as.integer(names(b)), b = b_reg), by = "movieId") |>
#   left_join(date_smoothed_effect, by='date') |>
#   mutate(pred = mu + a + b + de_smoothed) |>
#   pull(pred)
# 
# final_test_set <- final_holdout_test[!is.na(preds),]
# final_preds <- preds[!is.na(preds)]
# 
# RMSE(final_preds, final_test_set$rating)
#> [1] 0.8641795

### Utilizing Penalized least squares-------------------------------------------

# Reference: the Textbook section "23.6 Penalized least squares"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#penalized-least-squares

#> Instead of minimizing the least squares equation, 
#> we minimize an equation that adds a penalty:

#  ∑{i,j}(y[i,j] - μ - α[i] - β[j])^2 + λ*∑{j}β[j]^2

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
print(min(rmses))
#> [1] 0.8659219

#> which is achieved for the following `λ`:
lambda <- lambdas[which.min(rmses)] 
print(lambda)
#> [1] 2.6

#> Using minimal `λ`, we can compute the regularized estimates:
b_reg <- sums / (n + lambda)

#> Finally, let's verify that the penalized estimates 
#> we have just computed actually result in the minimal `RMSE` figured out above: 
reg_rmse(b_reg)
#> [1] 0.8659219


