# Init Project Environment -----------------------------------------------------
## Source File Paths -----------------------------------------------------------
r.path <- "r"
src.folder <- "src"
support_scripts.folder <- "support-scripts"
support_functions.folder <- "support-functions"

r.src.path <- file.path(r.path, src.folder)
support_scripts.path <- file.path(r.src.path, support_scripts.folder)
support_functions.path <- file.path(r.src.path, support_functions.folder)
## Setup -----------------------------------------------------------------------
setup_script.file_path <- file.path(support_scripts.path, "setup.R")

source(setup_script.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)
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

### Close Log ---------------------------------------------------------------
log_close()
# Methods =====================================================================
## Overall Mean Rating (Naive) Model --------------------------------------------------
# Reference: the Textbook section "23.3 A first model"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#a-first-model

#>  A model that assumes the same rating for all movies and users with all 
#>  the differences explained by random variation would look as follows:
# Y[i,j] = μ + ε[i,j]

### Open log -------------------------------------------------------------------
open_logfile(".overall-mean-rating")
### Create an RMSE Result Tibble  ----------------------------------------------
# Create the table and add a first row for the Project Objective
RMSEs.ResultTibble <- CreateRMSEs_ResultTibble()
RMSE_kable(RMSEs.ResultTibble)
put("RMSE Results Tibble created.")

### Model Building: OMR Model --------------------------------------------------
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

  mu.MSEs <- naive_model_MSEs(mu)
  # plot(MSEs)

  data.frame(fold_No = 1:5, MSE = mu.MSEs) |>
    data.plot(title = "MSE resuls of the 5-fold Cross Validation performed for the Overall Mean Rating Model",
                xname = "fold_No", 
                yname = "MSE")
  
  
  put_log1("MSE values plotted for %1-Fold Cross Validation samples.", CVFolds_N)
  mu.RMSE <- sqrt(mean(MSEs))
  # mu.RMSE <- naive_model_RMSE(mu)
  put_log2("%1-Fold Cross Validation ultimate RMSE: %2", CVFolds_N, mu.RMSE)
  #> 5-Fold Cross Validation ultimate RMSE: 1.06034335317133
  
  put_log2("Saving Overall mean rating value (`mu = %1`) to file: %1...", 
           mu,
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       mu.RMSE,
       file = file_path_tmp)
  put_end_date(start)
  put_log2("Overall mean rating value (`mu = %1`) has been saved to file: %1", 
           mu,
           file_path_tmp)
}

put_log1("The Overall Mean Rating is: %1", mu)
#> The Overall Mean Rating is: 3.51246520160155
put_log1("The Naive RMSE is: %1", mu.RMSE)

### OMR Value Is the Best for the Current Model --------------------------------
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
       mu.RMSE,
       deviation,
       deviation.RMSE,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("Overall Mean Rating Deviation data has been saved to file: %1", 
           file_path_tmp)
} 

which_min_deviation <- deviation[which.min(deviation.RMSE)]
min_rmse = min(deviation.RMSE)

put_log1("Minimum RMSE is achieved when the deviation from the mean is: %1",
         which_min_deviation)

put_log1("Is the previously computed RMSE the best for the current model: %1",
         mu.RMSE == min_rmse)
#> [1] "Is the previously computed RMSE the best for the current model: TRUE"
writeLines("")

#### Plot dependency of RMSEs vs Overal Mean Rating Deviation -----------------  
data.frame(delta = deviation, 
           delta.RMSE = deviation.RMSE) |> 
data.plot(title = TeX(r'[RMSE as a function of deviation ($\delta$) from the Overall Mean Rating ($\hat{mu}$)]'),
              xname = "delta", 
              yname = "delta.RMSE", 
              xlabel = TeX(r'[$\delta$]'), 
              ylabel = "RMSE")

put_log("A plot was constructed for the deviations from the Overall Mean Rating.")

### Add a row to the RMSE Result Tibble ---------------------------------------- 
RMSEs.ResultTibble.OMR <- RMSEs.ResultTibble |> 
  RMSEs.AddRow("OMR Model", 
               mu.RMSE,
               comment = "Overall Mean Rating (OMR) Model")

RMSE_kable(RMSEs.ResultTibble.OMR)
put_log("A row has been added to the RMSE Result Tibble for the `Overall Mean Rating Model`.")
### Close Log ---------------------------------------------------------------
log_close()

## User Effect (UE) Model ------------------------------------------------------ 
# Reference: the Textbook section "23.4 User effects"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#user-effects

### Open log -------------------------------------------------------------------
open_logfile(".user-effect")
put("Building User Effect Model...")
### UE Model building ----------------------------------------------------------
#### User Mean Ratings Computation --------------------------------------------
file_name_tmp <- "3.edx.user-mean-ratings.RData"
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
  user.mean_ratings <- rowMeans(edx.mx, na.rm = TRUE)
  user_ratings.n <- rowSums(!is.na(edx.mx))
  
  
  edx.user_mean_ratings <- 
    data.frame(userId = names(user.mean_ratings), 
               mean_rating = user.mean_ratings,
               n = user_ratings.n)
  
  put_log("User Mean Ratings have been computed.")
  str(edx.user_mean_ratings)
  sum(is.na(edx.user_mean_ratings$mean_rating))
  
  put_log1("Saving User Mean Rating data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       edx.user_mean_ratings,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User Mean Rating data has been saved to file: %1", 
           file_path_tmp)
} 

#### User Mean Ratings: Visualization ------------------------------
# Let's visualize the average rating for each user:

# sum(is.na(edx.user_mean_ratings$mean_rating))
#> [1] 0 (there are no NAs in there)

hist(edx.user_mean_ratings$mean_rating, nclass = 30)
put_log("A histogram of the User Mean Rating distribution has been plotted.")

#### User Effect Computation ----------------------------------------------

# we notice that there is substantial variability across users.
#>  To account for this, we can use a linear model with a treatment effect `α[i]` 
#>  for each user. The sum `μ + α[i]` can be interpreted as the typical 
#>  rating user `i` gives to movies. We can write the model as:

# Y[i,j] = μ + α[i] + ε[i,j]

#> It can be shown that the least squares estimate `α[i]` is just the average 
#> of `y[i,j] - μ` for each user. So we can compute them this way:

file_name_tmp <- "4.edx.User-effect.RData"
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
  edx.user_effect <- edx.user_mean_ratings |>
    mutate(userId = as.integer(userId),
           a = mean_rating - mu)
  
  str(edx.user_effect)
  sum(is.na(edx.user_effect$a))
  
  put_log("A User Effect Model has been builded and trained")

  put_log1("Saving User Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       edx.user_effect,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

put_log("Below is a User Effect data structure:")
put(str(edx.user_effect))
sum(is.na(edx.user_effect$a))

##### User Effect data integrity test------------------------------------------
UE.tst <- edx.user_effect |>
  mutate(tst.col = a) |>
  select(userId, tst.col)

UE.test.left_join.Nas <- UE.tst |>
  data.consistency.test(tune.test_set, by.movieId = FALSE)

put_log("Below are the User Effect consistency test results")
put(UE.test.left_join.Nas)

stopifnot(UE.test.left_join.Nas["user.NAs"] == 0)

cv.UE.test.left_join.Nas <- UE.tst |>
  data.consistency.test.cv(by.movieId = FALSE)

put_log("Below are the User Effect consistency test results")
put(cv.UE.test.left_join.Nas)
stopifnot(colSums(cv.UE.test.left_join.Nas)["user.NAs"] == 0)

##### Plot a histogram of the user effects ------------------------------------
par(cex = 0.7)
hist(edx.user_effect$a, 30, xlab = TeX(r'[$\hat{alpha}_{i}$]'),
     main = TeX(r'[Histogram of $\hat{alpha}_{i}$]'))
put_log("A histogram of the User Effect distribution has been plotted.")

### Compute RMSE for User Effect Model ------------------------------------
#> Finally, we are ready to compute the `RMSE` (additionally using the helper 
#> function `clamp` we defined above to keep predictions in the proper range):

put_log("Computing the RMSE taking into account user effects...")
start <- put_start_date()
edx.user_effect.MSEs <- sapply(edx_CV, function(cv_fold_dat){
  cv_fold_dat$validation_set |>
    left_join(edx.user_effect, by = "userId") |>
    mutate(resid = rating - clamp(mu + a)) |> 
    pull(resid) |> mse()
})
put_end_date(start)

data.frame(fold_No = 1:5, MSE = edx.user_effect.MSEs) |>
  data.plot(title = "MSE resuls of the 5-fold CV method applied to the User Effect Model",
            xname = "fold_No", 
            yname = "MSE")


put_log1("RMSE values have been plotted for the %1-Fold Cross Validation samples.", 
         CVFolds_N)

edx.user_effect.RMSE <- sqrt(mean(edx.user_effect.MSEs))
put_log2("%1-Fold Cross Validation ultimate RMSE: %2", 
         CVFolds_N, 
         edx.user_effect.RMSE)
edx.user_effect.RMSE
#> [1] 0.9716054

### Add a row to the RMSE Result Tibble for the User Effect Model -----------
RMSEs.ResultTibble.UE <- RMSEs.ResultTibble.OMR |> 
  RMSEs.AddRow("UE Model", 
               edx.user_effect.RMSE,
               comment = "User Effect (UE) Model")

RMSE_kable(RMSEs.ResultTibble.UE)
put_log("A row has been added to the RMSE Result Tibble for the `User Effect Model`.")

### Close Log -----------------------------------------------------------------
log_close()
## User+Movie Effect (UME) Model -----------------------------------------------
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

### Support Functions ---------------------------------------------------------
UM_effect.functions.file_path <- file.path(support_functions.path, 
                                               "UM-effect.functions.R")
source(UM_effect.functions.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)
### Movie Effect Analysis-------------------------------------------------------
#### Open log ------------------------------------------------------------------
open_logfile(".UME-analysis")
#### Movies' Popularity --------------------------------------------------------
#> We can find out the movies that have the greatest number of ratings 
#> using the following code:

edx.ordered_movie_ratings <- edx |> group_by(movieId, title) |>
  summarize(number_of_ratings = n()) |>
  arrange(desc(number_of_ratings))
print(head(edx.ordered_movie_ratings))
put_log("Movies popularity has been analysed.")

#### Rating Distribution -------------------------------------------------------

#> The following code figure out the most given edx.rating_groups in order from most to least:
edx.rating_groups <- edx |>  group_by(rating) |>
  summarise(count = n()) |>
  arrange(desc(count))
print(edx.rating_groups)

#> The following code allows us to summarize that in general, half-star edx.rating_groups 
#> are less common than whole-star edx.rating_groups (e.g., there are fewer edx.rating_groups of 3.5 
#> than there are edx.rating_groups of 3 or 4, etc.):
print(edx |> group_by(rating) |> summarize(count = n()))

#> We can visually see that from the following plot:
edx.rating_groups |>
  ggplot(aes(x = rating, y = count)) +
  geom_line() 

#> The code below cited from article:
#> https://www.kaggle.com/code/amirmotefaker/movie-recommendation-system-using-r-best/notebook#Rating-distribution-plot
#> demonstrates the more sophisticated way of visualizing the rating distribution:
edx.rating_groups |>
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
#### Close Log -----------------------------------------------------------------
log_close()
### UME Model Building ---------------------------------------------------------
#### Open log ------------------------------------------------------------------
open_logfile(".UME.model-building")
#### Movie Effect Computation ---------------------------------------------------
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
  cv.UM_effect <- train_user_movie_effect.cv()
  
  put_log1("Saving User+Movie Effect data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       edx.user_effect,
       cv.UM_effect,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie Effect data has been saved to file: %1",
           file_path_tmp)
} 

put(str(cv.UM_effect))

#### User+Movie Effect data integrity test------------------------------------
UME.tst <- cv.UM_effect |>
  mutate(tst.col = b) |>
  select(movieId, tst.col)

cv.UME.test.left_join.Nas <- UME.tst |>
  data.consistency.test.cv(by.userId = FALSE)

put_log("Below are the User+Movie Effect consistency test results")
put(cv.UME.test.left_join.Nas)
#      user.NAs movie.NAs
# [1,]       NA         0
# [2,]       NA         0
# [3,]       NA         0
# [4,]       NA         0
# [5,]       NA         0

stopifnot(colSums(cv.UME.test.left_join.Nas)["movie.NAs"] == 0)
#### User+Movie Effects: Visualization ------------------------------
par(cex = 0.7)
hist(cv.UM_effect$b, 30, xlab = TeX(r'[$\hat{beta}_{j}$)]'),
     main = TeX(r'[Histogram of $\hat{beta}_{j}$]'))
put_log("A histogram of the Mean User+Movie Effects distribution has been plotted.")

#### Compute RMSE for UME Model -------------------------------------------------
cv.UM_effect.RMSE <- calc_user_movie_effect_RMSE.cv(cv.UM_effect)
#> [1] 0.8732081
#### Add a row to the RMSE Result Tibble for UME Model -------------------------
RMSEs.ResultTibble.UME <- RMSEs.ResultTibble.UE |> 
  RMSEs.AddRow("UME Model", 
               cv.UM_effect.RMSE,
               comment = "User+Movie Effect (UME) Model")

RMSE_kable(RMSEs.ResultTibble.UME)
put_log("A row has been added to the RMSE Result Tibble for the `User+Movie Effect Model`.")

#### Close Log -----------------------------------------------------------------
log_close()
### Regularizing User+Movie Effect Model  -----
# Reference: the Textbook section "23.6 Penalized least squares"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#penalized-least-squares

#> Instead of minimizing the least squares equation, 
#> we minimize an equation that adds a penalty:

#  ∑{i,j}(y[i,j] - y_hat[i,j])^2 + λ*∑{j}β[j]^2
# where y_hat[i,j] = μ + α[i] + β[j] + g[i,j] + ye[i,j] + f(d[i,j])

#> The values of `β[j]` that minimize this equation are:

# β[j](λ) = 1/(λ + n[j])*∑{u=1,n[i]}(Y[i,j] - μ - α[i])
# where `n[j]` is the number of ratings made for movie `j`.

#### Open log for `Pre-configuration` step -------------------------------------
open_logfile(".rglr.UM-effect.pre-set-lambdas")
#### UM Effect Regularization Directory Paths ------------------------------------------
UME.regularization.path <- file.path(data.regularization.path, 
                                           "1.UM-effect")
dir.create(UME.regularization.path)
put_log1("Directory path has been created: %1", UME.regularization.path)

UME.rglr.fine_tune.cache.path <- file.path(UME.regularization.path, 
                                           fine_tune.cache.folder)
dir.create(UME.rglr.fine_tune.cache.path)
put_log1("Directory path has been created: %1", UME.rglr.fine_tune.cache.path)

#### UME Model Regularization: Pre-configuration ------------------------------
file_name_tmp <- "1.UME.rglr.pre-set.RData" # UME stands for `User+Movie Effect`
file_path_tmp <- file.path(UME.regularization.path, file_name_tmp)

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
  put_log1("Preliminary regularization set-up of `lambda`s range for the UME Model has been completed
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
  put_log1("Saving User+Movie Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       edx.user_effect,
       cv.UME.preset.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

put_log("Preliminary regularization set-up of `lambda`s range for the User+Movie Effect 
has resulted as follows:")
put(cv.UME.preset.result$best_result)

#### Plot (rough) dependency of RMSEs vs lambdas ------------------------------  
cv.UME.preset.result$tuned.result |>
  data.plot(title = TeX(r'[UME Model Regularization: $\lambda$ Range Pre-configuration]'),
              xname = "parameter.value", 
              yname = "RMSE", 
              xlabel = TeX(r'[$\lambda$]'), 
              ylabel = "RMSE")

#### Close Log -----------------------------------------------------------------
log_close()
#### Open log file for `Fine-tuning` step --------------------------------------
open_logfile(".UME.rg.fine-tuning")
#### Fine-tuning Step of the Regularization Method for the User+Movie Model ---- 
endpoints <- 
  get_fine_tune.param.endpoints(cv.UME.preset.result$tuned.result)

UM_effect.loop_starter <- c(endpoints["start"], 
                            endpoints["end"], 
                            8)
UM_effect.loop_starter
#> [1] 0.3 0.5 8.0

UME.rglr.fine_tune.cache.base_name <- "UME.rglr.fine-tune"

UME.rglr.fine_tune.results <- 
  model.tune.param_range(UM_effect.loop_starter,
                         UME.rglr.fine_tune.cache.path,
                         UME.rglr.fine_tune.cache.base_name,
                         regularize.test_lambda.UM_effect.cv)
                         #endpoint.min_diff = 1e-07/4)

UME.rglr.fine_tune.RMSE.best <- UME.rglr.fine_tune.results$best_result["best_RMSE"]

put_log("Fine-tuning stage of the User+Movie Effect Model Regularization 
has ended up with with the following results:")
put(UME.rglr.fine_tune.results$best_result)
# param.best_value        best_RMSE 
#        0.3874500        0.8732057 

##### Plot (fine-tuned) dependency of RMSEs vs lambdas -------------------------  

UME.rglr.fine_tune.results$tuned.result |>
  data.plot(title = "UME Model Regularization: Fine-tuned result",
              xname = "parameter.value",
              yname = "RMSE",
              xlabel = TeX(r'[$\lambda$]'),
              ylabel = str_glue("Deviation from the best RMSE value (",
                                as.character(round(UME.rglr.fine_tune.RMSE.best, digits = 7)), 
                                ")"),
              normalize = TRUE)

#### Close Log -----------------------------------------------------------------
log_close()
#### Open log file for re-training Regularized Model ---------------------------
open_logfile(".UME.rg.re-train.best-lambda")
#### Re-training Regularized Model for the best `lambda` -------
file_name_tmp <- "2.UME.rglr.re-train.best-lambda.RData"
file_path_tmp <- file.path(UME.regularization.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading User+Movie Effect data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie Effect data has been loaded from file: %1", file_path_tmp)
  
} else {
  
  best_result <- UME.rglr.fine_tune.results$best_result
  UME.rglr.best_lambda <- best_result["param.best_value"]
  UME.rglr.best_lambda
  
  UME.rglr.best_RMSE <- best_result["best_RMSE"]
  print(UME.rglr.best_RMSE)
# best_RMSE 
# 0.8732057   
  
  put_log1("Re-training Regularized User+Movie Effect Model for the best `lambda`: %1...",
           UME.rglr.best_lambda)
  
  rglr.UM_effect <- train_user_movie_effect(edx, UME.rglr.best_lambda)
  str(rglr.UM_effect)
  sum(is.na(rglr.UM_effect))
  #> [1] 0
  
  put_log1("Saving Regularized User+Movie Effect data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       edx.user_effect,
       rglr.UM_effect,
       UME.rglr.best_lambda,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie Effect data has been saved to file: %1",
           file_path_tmp)
}
##### Regularized User+Movie Effect data integrity test------------------------
UME.tst <- rglr.UM_effect |>
  mutate(tst.col = b) |>
  select(movieId, tst.col)

rglr.UME.test.left_join.Nas <- UME.tst |>
  data.consistency.test.cv(by.userId = FALSE)

put_log("Below are the User+Movie Effect consistency test results")
put(rglr.UME.test.left_join.Nas)
#      user.NAs movie.NAs
# [1,]       NA         0
# [2,]       NA         0
# [3,]       NA         0
# [4,]       NA         0
# [5,]       NA         0

stopifnot(colSums(rglr.UME.test.left_join.Nas)["movie.NAs"] == 0)
#### Calculate RMSE for Regularized User+Movie Model --------------------------
UME.rglr.retrain.RMSE <- calc_user_movie_effect_RMSE.cv(rglr.UM_effect)
#> [1] 0.872973

put_log1("Regularized User+Movie Effect Model has been re-trained for the best `lambda`: %1.",
         UME.rglr.best_lambda)
put_log1("The best RMSE after being regularized: %1",
         UME.rglr.retrain.RMSE)
#### Add a row to the RMSE Result Table for the Regularized User+Movie Effect Model --------
RMSEs.ResultTibble.rglr.UME <- RMSEs.ResultTibble.UME |> 
  RMSEs.AddRow("Regularized UME Model", 
               UME.rglr.retrain.RMSE,
               comment = "Computed for `lambda` = %1" |>
                 msg.glue(UME.rglr.best_lambda))
RMSE_kable(RMSEs.ResultTibble.rglr.UME)
put_log("A row has been added to the RMSE Result Tibble 
for the `Regularized User+Movie Effect Model`.")
#### Close Log -----------------------------------------------------------------
log_close()
## User+Movie+Genre Effect (UMGE) Model ---------------------------------------
#> We can slightly improve our naive model by accounting for movie genres.
#> Let's do some preliminary analysis first.
### Support Functions ---------------------------------------------------------
umge_functions_file <- "UMG-effect.functions.R"
# umge_functions_file <- "UMG-effect.functions.azb001.R"
umge_functions.file_path <- file.path(support_functions.path, 
                                      umge_functions_file)
source(umge_functions.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)

### Data Analysis and Visualization -------------------------------------------
# Reference: the Textbook Section "23.7 Exercises" of the Chapter "23 Regularization"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#exercises
#> The `edx` dataset also has a genres column. This column includes 
#> every genre that applies to the movie 
#> (some movies fall under several genres)[@IDS2_23-7].
#### Open log ------------------------------------------------------------------
open_logfile(".UMGE.data-analysis")
#### Computing Genre Mean Ratings ---------------------------------------------
file_name_tmp <- "6.cv.genre-mean-ratings.RData"
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
       edx.user_effect,
       rglr.UM_effect,
       gnr_mean_ratings.cv,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("Genre Average Rating data has been saved to file: %1", 
           file_path_tmp)
} 
put_log("Genre Mean Rating data structure:")
put(str(gnr_mean_ratings.cv))
sum(is.na(gnr_mean_ratings.cv$ratings))
# [1] 0
sum(is.na(gnr_mean_ratings.cv$se))
# [1] 0

put_log2("The worst rating is for the genre category: %1 (average rating is %2)",
            gnr_mean_ratings.cv$genres[which.min(gnr_mean_ratings.cv$ratings)],
            as.character(clamp(min(gnr_mean_ratings.cv$ratings))))

put_log2("The best rating is for the genre category: %1 (average rating is %2)",
            gnr_mean_ratings.cv$genres[which.max(gnr_mean_ratings.cv$ratings)],
            as.character(clamp(max(gnr_mean_ratings.cv$ratings))))

#### Genres Popularity ------------------------------------------------------------

put_log2("The worst popularity was for the genre category: %1 (%2 ratings)",
            gnr_mean_ratings.cv$genres[which.min(gnr_mean_ratings.cv$n)],
            as.character(min(gnr_mean_ratings.cv$n)))

put_log2("The best popularity was for the genre category: %1 (%2 ratings)",
            gnr_mean_ratings.cv$genres[which.max(gnr_mean_ratings.cv$n)],
            as.character(max(gnr_mean_ratings.cv$n)))

#### Genres Info Visualization ------------------------------------------------
#> For illustrative purposes, we will limit the genre information 
#> we are going to plot to movies with more than 24,000 ratings:
nratings <- 24000

##### Plot Genre Info --------------------------------------------------------------  
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

##### Visualization:Alternative way --------------------------------------------
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
#### Close Log -----------------------------------------------------------------
log_close()

### UMGE Model Building --------------------------------------------------------
# Y[i,j] = μ + α[i] + β[j] + g[i,j]  + ε[i,j]
# where g[i,j] is a combination of genres for movie `i` rated by user `j`,
# so that g[i,j] = ∑{k=1,K}(x[i,j]^k*𝜸[k]) 
# with `x[i,j]^k = 1` if g[i,j] includes genre `k`, and `x[i,j]^k = 0` otherwise.

# mutate(userId = as.integer(userId),
#        movieId = as.integer(movieId)) |>
#### Open log-------------------------------------------------------------------
open_logfile(".UMGE-model.train")

#### UMG Effect Computation ----------------------------------------------------
file_name_tmp <- "7.cv.UMG-effect.RData"
file_path_tmp <- file.path(data.models.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading User+Movie+Genre Effect Model data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre Effect Model data has been loaded from file: %1", file_path_tmp)
  
} else {
  cv.UMG_effect <- train_user_movie_genre_effect.cv()
  str(cv.UMG_effect)
  sum(is.na(cv.UMG_effect$g))
  #> [1] 0
  
  put_log1("Saving User+Movie+Genre Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       edx.user_effect,
       rglr.UM_effect,
       gnr_mean_ratings.cv,
       cv.UMG_effect,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

put_log("User+Movie+Genre Effect Model data structure:")
put(str(cv.UMG_effect))

##### Plot a Histogram: UMG Effect Distribution --------------------------------
par(cex = 0.7)
hist(cv.UMG_effect$g, 30, xlab = TeX(r'[$\hat{g}_{i,j}$]'),
     main = TeX(r'[Histogram of $\hat{g}_{i,j}$]'))

put_log("A histogram of the Movie Genre Effect distribution has been plotted.")

#### Compute UMGE Model RMSE ---------------------------------------------------
cv.UMG_effect.RMSE <- calc_user_movie_genre_effect_RMSE.cv(cv.UMG_effect)
cv.UMG_effect.RMSE
#> [1] 0.859473

#### Add a row to the RMSE Result Tibble for the User+Movie+Genre Effect Model ---- 
RMSEs.ResultTibble.UMGE <- RMSEs.ResultTibble.rglr.UME |> 
  RMSEs.AddRow("UMGE Model", 
               cv.UMG_effect.RMSE,
               comment = "User+Movie+Genre Effect (UMGE) Model")

RMSE_kable(RMSEs.ResultTibble.UMGE)
put_log("A row has been added to the RMSE Result Tibble for the `User+Movie+Genre Effect Model`.")

#### Close Log -----------------------------------------------------------------
log_close()

### UMGE Model Regularization --------------------------------------------------
#### Open log for `Preliminary setting-up of lambda range` feature -------------
open_logfile(".rglr.UMG-effect.pre-set-lambdas")
#### UMG Effect Regularization Directory Paths --------------------------------
UMGE.regularization.path <- file.path(data.regularization.path, 
                                           "2.UMG-effect")
dir.create(UMGE.regularization.path)
put_log1("Directory path has been created: %1", UMGE.regularization.path)

UMGE.rglr.fine_tune.cache.path <- file.path(UMGE.regularization.path, 
                                           fine_tune.cache.folder)
dir.create(UMGE.rglr.fine_tune.cache.path)
put_log1("Directory path has been created: %1", UMGE.rglr.fine_tune.cache.path)
#### UMGE Model Regularization: Pre-configuration ----------------------------
file_name_tmp <- "1.UMGE.rglr.pre-set.RData" # UMGE stands for `User+Movie+Genre Effect`
file_path_tmp <- file.path(UMGE.regularization.path, file_name_tmp)

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
  lambdas <- seq(0, 0.2, 0.01)
  cv.UMGE.preset.result <- 
    tune.model_param(lambdas, regularize.test_lambda.UMG_effect.cv)
  put_end_date(start)
  put_log1("Preliminary regularization set-up of `lambda`s range for the UMGE Model has been completed
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
  put_log1("Saving User+Movie+Genre Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       edx.user_effect,
       rglr.UM_effect,
       cv.UMGE.preset.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

put_log("Preliminary regularization set-up of `lambda`s range for the User+Movie+Genre Effect 
has resulted as follows:")
put(cv.UMGE.preset.result$best_result)

##### Plot (rough) dependency of RMSEs vs lambdas -----------------------------  
cv.UMGE.preset.result$tuned.result |>
  data.plot(title = TeX(r'[UMGE Model Regularization: $\lambda$ Range Pre-configuration]'),
              xname = "parameter.value", 
              yname = "RMSE", 
              xlabel = TeX(r'[$\lambda$]'), 
              ylabel = str_glue("Deviation from the best RMSE value (",
                                as.character(round(cv.UMGE.preset.result$best_result["best_RMSE"], 
                                                   digits = 7)),
                                ")"),
              normalize = TRUE)

#### Close Log -----------------------------------------------------------------
log_close()
#### Open log file for Regularization (Fine-Tuning)` feature -------------------
open_logfile(".UMGE.rglr.fine-tuning")
#### UMGE Model Regularization: Fine-tuning ----------------------------------- 
endpoints <- 
  get_fine_tune.param.endpoints(cv.UMGE.preset.result$tuned.result)

UMG_effect.loop_starter <- c(endpoints["start"], 
                            endpoints["end"], 
                            8)
UMG_effect.loop_starter
#> [1] 0.0   0.1   8.0

UMGE.rglr.fine_tune.cache.base_name <- "UMGE.rglr.fine-tuning"

UMGE.rglr.fine_tune.results <- 
  model.tune.param_range(UMG_effect.loop_starter,
                         UMGE.rglr.fine_tune.cache.path,
                         UMGE.rglr.fine_tune.cache.base_name,
                         regularize.test_lambda.UMG_effect.cv)

UMGE.rglr.fine_tune.RMSE.best <- UMGE.rglr.fine_tune.results$best_result["best_RMSE"]
# best_RMSE 
#  0.872973 
 
put_log("Fine-tuning stage of the User+Movie+Genre Effect Model Regularization 
has ended up with with the following results:")
put(UMGE.rglr.fine_tune.results$best_result)
# param.best_value        best_RMSE 
#       0.03554688       0.87297303 

##### Plot (fine-tuned) dependency of RMSEs vs lambdas ------------------------  
UMGE.rglr.fine_tune.results$tuned.result |>
  data.plot(title = "UMGE Model Regularization: Fine-tuned result",
              xname = "parameter.value",
              yname = "RMSE",
              xlabel = TeX(r'[$\lambda$]'),
              ylabel = str_glue("Deviation from the best RMSE value (",
                                as.character(round(UMGE.rglr.fine_tune.RMSE.best, digits = 7)),
                                ")"),
              normalize = TRUE)

#### Close Log -----------------------------------------------------------------
log_close()
#### Open log for re-training Regularized UMGE Model for the best `lambda` value ----
open_logfile(".UMGE.rglr.re-train.best-lambda")
#### Re-train `Regularized UMGE Model` for the best `lambda` value ------------
file_name_tmp <- "2.UMGE.rglr.re-train.best-lambda.RData"
file_path_tmp <- file.path(UMGE.regularization.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading Regularized User+Movie+Genre Effect Model data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Regularized User+Movie+Genre Effect Model data has been loaded from file: %1", 
           file_path_tmp)
  
} else {
  best_result <- UMGE.rglr.fine_tune.results$best_result
  # param.best_value        best_RMSE 
  #     0.03554688       0.87297303 
  
  UMGE.rglr.best_lambda <- best_result["param.best_value"]
  UMGE.rglr.best_RMSE <- best_result["best_RMSE"]
  
  put_log1("Re-training Regularized User+Movie+Genre Effect Model for the best `lambda`: %1...",
           UMGE.rglr.best_lambda)
  
  rglr.UMG_effect <- edx.sgr |> train_user_movie_genre_effect(UMGE.rglr.best_lambda)
  rglr.UMG_effect.RMSE <- calc_user_movie_genre_effect_RMSE.cv(rglr.UMG_effect)
  
  put_log2("Regularized User+Movie+Genre Effect RMSE has been computed for the best `lambda = %1`: %2.",
           UMGE.rglr.best_lambda,
           rglr.UMG_effect.RMSE)
  put_log1("Is this a best RMSE? %1",
           UMGE.rglr.best_RMSE == rglr.UMG_effect.RMSE)
  
  
  put_log1("Saving User+Movie+Genre Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       edx.user_effect,
       rglr.UM_effect,
       rglr.UMG_effect,
       UMGE.rglr.best_lambda,
       rglr.UMG_effect.RMSE,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre Effect Model data has been saved to file: %1", 
           file_path_tmp)
  put_log1("Completed re-training regularized UMGE Model for the best `lambda`: %1...",
           UMGE.rglr.best_lambda)
} 

#### Add a row to the RMSE Result Tibble for the Regularized User+Movie+Genre Effect Model ----
RMSEs.ResultTibble.rglr.UMGE <- RMSEs.ResultTibble.UMGE |> 
  RMSEs.AddRow("Regularized UMGE Model", 
               rglr.UMG_effect.RMSE,
               comment = "Computed for `lambda` = %1" |>
                 msg.glue(UMGE.rglr.best_lambda))

RMSE_kable(RMSEs.ResultTibble.rglr.UMGE)
put_log("A row has been added to the RMSE Result Tibble for the Regularized UMGE Model`.")

#### Close Log ----------------------------------------------------------------
log_close()
## User+Movie+Genre+Year Effect (UMGYE) Model ----------------------------------
### Open log file for UMGYE Model Building -------------------------------------
open_logfile(".UMGYE.model-building")
### Plot: Average Rating per Year ------------------------------------------------
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

### Support Functions ---------------------------------------------------------
cv.UMGY_effect.functions_file <- "UMGY-effect.functions.R"
cv.UMGY_effect.functions.file_path <- file.path(support_functions.path, 
                                      cv.UMGY_effect.functions_file)
source(cv.UMGY_effect.functions.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)
### UMGYE Model Building -------------------------------------------------------
file_name_tmp <- "8.cv.UMGY-effect.RData"
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
       edx.user_effect,
       rglr.UM_effect,
       rglr.UMG_effect,
       cv.UMGY_effect,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre+Year Effect data has been saved to file: %1", 
           file_path_tmp)
} 
#### Compute User+Movie+Genre+Year Effect Model RMSE ---------------------------
cv.UMGY_effect.RMSE <- calc_UMGY_effect_RMSE.cv(cv.UMGY_effect)
cv.UMGY_effect.RMSE
#> [1] 0.8590795
#### Add a row to the RMSE Result Tibble for the User+Movie+Genre+Year Effect Model ---- 
RMSEs.ResultTibble.UMGYE <- RMSEs.ResultTibble.rglr.UMGE |> 
  RMSEs.AddRow("UMGYE Model", 
               cv.UMGY_effect.RMSE,
               comment = "User+Movie+Genre+Year Effect (UMGYE) Model")

RMSE_kable(RMSEs.ResultTibble.UMGYE)
put_log("A row has been added to the RMSE Result Tibble for the `User+Movie+Genre+Year Effect Model`.")

### Close Log -----------------------------------------------------------------
log_close()

### UMGYE Model Regularization -------------------------------------------------
#### Open log for Pre-configuration step ----------------------------------
open_logfile(".rglr.UMGY-effect.pre-set-lambdas")
#### UMGYE Model Regularization Directory Paths --------------------------------
UMGYE.regularization.path <- file.path(data.regularization.path, 
                                            "3.UMGY-effect")
dir.create(UMGYE.regularization.path)
put_log1("Directory path has been created for `User+Movie+Genre+Year Effect Model` data: %1", 
         UMGYE.regularization.path)

UMGYE.rglr.fine_tune.cache.path <- file.path(UMGYE.regularization.path, 
                                            fine_tune.cache.folder)
dir.create(UMGYE.rglr.fine_tune.cache.path)
put_log1("Directory path has been created: %1", UMGYE.rglr.fine_tune.cache.path)
#### UMGYE Model Regularization: Pre-configuration ---------------------------
file_name_tmp <- "1.UMGYE.rglr.pre-set.RData" # UMGE stands for `User+Movie+Genre+Year Effect`
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
  lambdas <- seq(0, 512, 32)
  cv.UMGYE.preset.result <- 
    tune.model_param(lambdas, 
                     regularize.test_lambda.UMGY_effect.cv,
                     steps.beyond_min = 16)
  put_end_date(start)
  put_log1("Preliminary regularization set-up of `lambda`s range for the UMGYE Model has been completed
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
  put_log1("Saving User+Movie+Genre+Year Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       edx.user_effect,
       rglr.UM_effect,
       rglr.UMG_effect,
       cv.UMGYE.preset.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre+Year Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

put_log("Preliminary regularization set-up of `lambda`s range for the User+Movie+Genre+Year Effect 
has resulted as follows:")
put(cv.UMGYE.preset.result$best_result)

##### Plot (rough) dependency of RMSEs vs lambdas -----------------------------  
cv.UMGYE.preset.result$tuned.result |>
  data.plot(title = TeX(r'[UMGYE Model Regularization: $\lambda$ Range Pre-configuration]'),
              xname = "parameter.value", 
              yname = "RMSE", 
              xlabel = TeX(r'[$\lambda$]'), 
              ylabel = "RMSE")

#### Close Log -----------------------------------------------------------------
log_close()
#### Open log for UMGYE Model Regularization:Fine-tunig ------------------------
open_logfile(".UMGYE.rglr.fine-tuning")
#### UMGYE Model Regularization: Fine-tuning ----------------------- 
endpoints <- 
  get_fine_tune.param.endpoints(cv.UMGYE.preset.result$tuned.result)

UMGYE.loop_starter <- c(endpoints["start"], 
                        endpoints["end"], 
                        8)
UMGYE.loop_starter
#> [1] 

cache.base_name <- "UMGYE.rglr.fine-tuning"

UMGYE.rglr.fine_tune.results <- 
  model.tune.param_range(UMGYE.loop_starter,
                         UMGYE.rglr.fine_tune.cache.path,
                         cache.base_name,
                         regularize.test_lambda.UMGY_effect.cv)

UMGYE.rglr.fine_tune.RMSE.best <- UMGYE.rglr.fine_tune.results$best_result["best_RMSE"]

put_log("Fine-tuning stage of the User+Movie+Genre+Year Effect Model Regularization 
has ended up with with the following results:")
put(UMGYE.rglr.fine_tune.results$best_result)

##### Plot (fine-tuned) dependency of RMSEs vs lambdas -----------------------------  
UMGYE.rglr.fine_tune.results$tuned.result |>
  data.plot(title = "UMGYE Model Regularization: Fine-tuned result",
              xname = "parameter.value",
              yname = "RMSE",
              xlabel = TeX(r'[$\lambda$]'),
              ylabel = str_glue("Deviation from the best RMSE value (",
                                as.character(round(UMGYE.rglr.fine_tune.RMSE.best, digits = 7)),
                                ")"),
              normalize = TRUE)

#### Close Log -----------------------------------------------------------------
log_close()
### Open log for Re-training Regularized Model for the best `lambda` value------
open_logfile(".UMGYE.rg.re-train.best-lambda")
### Re-train Regularized UMGYE Model for the best `lambda` ---------------------
file_name_tmp <- "2.UMGYE.rglr.re-train.best-lambda.RData"
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
  best_result <- UMGYE.rglr.fine_tune.results$best_result
  UMGYE.rglr.best_lambda <- best_result["param.best_value"]
  UMGYE.rglr.best_RMSE <- best_result["best_RMSE"]
  
  put_log1("Re-training Regularized User+Movie+Genre+Year Effect Model for the best `lambda`: %1...",
           UMGYE.rglr.best_lambda)
  
  rglr.UMGY_effect <- edx |> train_UMGY_effect(UMGYE.rglr.best_lambda)
  rglr.UMGY_effect.RMSE <- calc_UMGY_effect_RMSE.cv(rglr.UMGY_effect)
  
  put_log2("Regularized User+Movie+Genre+Year Effect RMSE has been computed for the best `lambda = %1`: %2.",
           UMGYE.rglr.best_lambda,
           rglr.UMGY_effect.RMSE)
  put_log1("Is this a best RMSE? %1",
           UMGYE.rglr.best_RMSE == rglr.UMGY_effect.RMSE)
  
  put_log1("Saving User+Movie+Genre+Year Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       edx.user_effect,
       rglr.UM_effect,
       rglr.UMG_effect,
       rglr.UMGY_effect,
       UMGYE.rglr.best_lambda,
       rglr.UMGY_effect.RMSE,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre+Year Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

### Add a row to the RMSE Result Tibble for the Regularized User+Movie+Genre+Year Effects Model ---- 
RMSEs.ResultTibble.rglr.UMGYE <- RMSEs.ResultTibble.UMGYE |> 
  RMSEs.AddRow("Regularized UMGYE Model", 
               rglr.UMGY_effect.RMSE,
               comment = "Computed for `lambda` = %1" |>
                 msg.glue(UMGYE.rglr.best_lambda))

RMSE_kable(RMSEs.ResultTibble.rglr.UMGYE)
put_log("A row has been added to the RMSE Result Tibble 
for the `Regularized User+Movie+Genre+Year Effect Model`.")

### Close Log -----------------------------------------------------------------
log_close()

## User+Movie+Genre+Year+Day Effect (UMGYDE) Model -----------------------------
# Y[i,j] = μ + α[i] + β[j] + g[i,j] yr[i,j]  + f(d[i,j]) + ε[i,j]

# with `f` a smooth function of `d[i,j]`
### Support Functions --------------------------------------------------------
cv.UMGYDE.default_params.functions_file <- "UMGYD-effect.functions.R"
cv.UMGYDE.default_params.functions.file_path <- file.path(support_functions.path, 
                                                cv.UMGYDE.default_params.functions_file)

source(cv.UMGYDE.default_params.functions.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)

### Open log for training with default parameters ------------------------------
open_logfile(".UMGYDE.loess.default-params")
### Model Tuning Data File Paths -----------------------------------------------
UMGYDE.tuning_folder <- "UMGYD-effect"

UMGYDE.tuning.data.path <- 
  file.path(data.model_tune.path, UMGYDE.tuning_folder)

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
            fine_tune.cache.folder)

dir.create(UMGYDE.fine_tune.degree0.data.path)
put_log1("Directory path has been created for fine-tuning 
the `User+Movie+Genre+Year+(Smoothed)Day Effect Model` data
using `loess` function with parameter `degree = 0`: %1", 
UMGYDE.fine_tune.degree0.data.path)

UMGYDE.fine_tune.degree1.data.path <- 
  file.path(UMGYDE.tuning.degree1.data.path, 
            fine_tune.cache.folder)

dir.create(UMGYDE.fine_tune.degree1.data.path)
put_log1("Directory path has been created for fine-tuning 
the `User+Movie+Genre+Year+(Smoothed)Day Effect Model` data
using `loess` function with parameter `degree = 1`: %1", 
UMGYDE.fine_tune.degree1.data.path)

UMGYDE.fine_tune.degree2.data.path <- 
  file.path(UMGYDE.tuning.degree2.data.path, 
            fine_tune.cache.folder)

dir.create(UMGYDE.fine_tune.degree2.data.path)
put_log1("Directory path has been created for fine-tuning 
the `User+Movie+Genre+Year+(Smoothed)Day Effect Model` data
using `loess` function with parameter `degree = 2`: %1", 
UMGYDE.fine_tune.degree2.data.path)

### UMGYDE Model (UMGYDEM) Building --------------------------------------------
#### UMGYDEM Training with default parameters ----------------------------------
file_name_tmp <- "9.cv.UMGYDE.loess.default-params.RData"
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
  put_log1("Training UMGYD Effect Model using `loess` function 
with default `span` & `degree` parameters for %1-Fold Cross Validation samples...",
CVFolds_N)
  
  start <- put_start_date()
  cv.UMGYDE.default_params <- train_UMGY_SmoothedDay_effect.cv()
  put_end_date(start)
  put_log1("UMGYD Effect Model has been trained
using `loess` function with default `span` & `degree` parameters
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  put(str(cv.UMGYDE.default_params))
  
  put_log1("Saving UMGYD Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       edx.user_effect,
       rglr.UM_effect,
       rglr.UMG_effect,
       rglr.UMGY_effect,
       cv.UMGYDE.default_params,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("General Day Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

put(str(cv.UMGYDE.default_params))

#### UMGYD Effect Visualization ------------------------------------------------
cv.UMGYDE.default_params |>
  ggplot(aes(x = days)) +
  geom_point(aes(y = de), size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(y = de_smoothed), color = "red")

put_log("Date Smoothed Effect has been plotted 
for the `loess` function fitted with default parameters.")

#### UMGYDE Model (default params): RMSE Calculation ---------------------------
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
#### UMGYDE Model RMSE Result Tibble: Add a row (default params) --------------- 
RMSEs.ResultTibble.UMGYDE <- RMSEs.ResultTibble.rglr.UMGYE |> 
  RMSEs.AddRow("UMGYDE (Default) Model", 
               cv.UMGYDE.default_params.RMSE,
               comment = "User+Movie+Genre+Year+Day Effect (UMGYDE) Model 
computed using `stats::loess` function with `degree=1` & `span=0.75` parameter values.")

RMSE_kable(RMSEs.ResultTibble.UMGYDE)
put_log("A row has been added to the RMSE Result Tibble 
for the tuned `User+Movie+Genre+Year+(Smoothed)Day Effect Model`.")
### Close Log -----------------------------------------------------------------
log_close()
### UMGYDE Model Tuning by `span` & `degree` parameters ------------------------
#### UMGYDE Model Tuning: Step 1 (`degree = 0`) --------------------------------
##### Open log for UMGYDE Model Tuning: Pre-configuration (`degree = 0`) -------
open_logfile(".UMGYDE.loess.degree0.pre-tuning")
put("Case 1. `degree = 0`")
##### UMGYDE Model Tuning: Pre-configuration (`degree = 0`) --------------------
file_name_tmp <- "1.lss.UMGYDE.pre-set.degree0.RData"
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
  lss.UMGYDE.preset.degree0.result <- 
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
       edx.user_effect,
       rglr.UM_effect,
       rglr.UMG_effect,
       rglr.UMGY_effect,
       lss.UMGYDE.preset.degree0.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("UMGY+(Smoothed)Day Effect Model data has been saved to file: %1", 
           file_path_tmp)
}

put_log("Preliminary tuning stage of the UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 0` has ended up with with the following results:")
put(lss.UMGYDE.preset.degree0.result$best_result)
# param.best_value        best_RMSE 
#          0.00150          0.85762 

###### UMGYDE (Rough) Tuned Model Plot (`degree = 0`) --------------------------
plt.title = "Preliminary set-up for tuning UMGY+(Smoothed)Day Effect Model using `loess` with parameter `degree = 0`"

lss.UMGYDE.preset.degree0.result$tuned.result |>
  data.plot(plt.title,
              xname = "parameter.value", 
              yname = "RMSE", 
              xlabel = "spans", 
              ylabel = "RMSE")
rm(plt.title)

##### Close Log -----------------------------------------------------------------
log_close()

##### Open log for UMGYDE Model Fine-tuning (`degree = 0`) -----------------------
open_logfile(".lss.UMGYDE.fine-tune.degree0")
##### UMGYDE Model Fine-tuning (`degree = 0`) ---------------------------------- 
lss.fine_tune.loop_starter <- 
  c(lss.UMGYDE.preset.degree0.result$tuned.result$parameter.value[1], 
    lss.UMGYDE.preset.degree0.result$tuned.result$parameter.value[3], 
    8)
# loop_starter <- c(0.0005, 0.0025, 8)
cache_file.base_name <- "UMGYDE.degree0.tuning-span"

lss.UMGYDE.fine_tune.degree0.result <- 
  model.tune.param_range(lss.fine_tune.loop_starter,
                         UMGYDE.fine_tune.degree0.data.path,
                         cache_file.base_name,
                         train_UMGY_SmoothedDay_effect.RMSE.cv.degree0)

put_log("Fine-tuning stage of the UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 0` has ended up with with the following results:")
put(lss.UMGYDE.fine_tune.degree0.result$best_result)
# param.best_value        best_RMSE 
#      0.000890625      0.857325313 

lss.UMGYDE.fine_tune.degree0.result.best_span <- 
  lss.UMGYDE.fine_tune.degree0.result$best_result["param.best_value"]

lss.UMGYDE.fine_tune.degree0.result.best_RMSE <- 
  lss.UMGYDE.fine_tune.degree0.result$best_result["best_RMSE"]

###### UMGYDE Fine-tuned Model Plot: `RMSEs` vs `spans` (`degree = 0`) ---------  
plt.title = "Fine-tuned UMGYDE Model with `loess` parameter: `degree` = 0"

lss.UMGYDE.fine_tune.degree0.result$tuned.result |>
  data.plot(plt.title, 
              xname = "parameter.value", 
              yname = "RMSE", 
              xlabel = "spans", 
              ylabel = "RMSE")
rm(plt.title)

##### UMGYDE Tuned Model RMSE Result Tibble: Add a row (`degree = 0`) ----------- 
RMSEs.ResultTibble.UMGYDE0 <- RMSEs.ResultTibble.UMGYDE |> 
  RMSEs.AddRow("Tuned UMGYDE.d0 Model", 
               lss.UMGYDE.fine_tune.degree0.result.best_RMSE,
               comment = "UMGYDE Model computed using function call: `loess(degree = 0, span = %1)`" |>
                 msg.glue(lss.UMGYDE.fine_tune.degree0.result.best_span))

RMSE_kable(RMSEs.ResultTibble.UMGYDE0)
##### Close Log ---------------------------------------------------------------
log_close()
#### UMGYDE Model Tuning: Step 2 (`degree = 1`) --------------------------------
##### Open log for UMGYDE Model Tuning: Pre-configuration (`degree = 1`) -------
open_logfile(".UMGYDE.loess.degree1.pre-tuning")
put("Case 2. `degree = 1`")
##### UMGYDE Model Tuning: Pre-configuration (`degree = 1`) --------------------
file_name_tmp <- "1.lss.UMGYDE.pre-set.degree1.RData"
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
  lss.UMGYDE.preset.degree1.result <- 
    tune.model_param(spans, train_UMGY_SmoothedDay_effect.RMSE.cv.degree1)
  put_end_date(start)
  put_log1("Preliminary set-up data for tuning UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 1` has been computed 
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
  put_log1("Saving UMGY+(Smoothed)Day Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       edx.user_effect,
       rglr.UM_effect,
       rglr.UMG_effect,
       rglr.UMGY_effect,
       lss.UMGYDE.preset.degree1.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("UMGY+(Smoothed)Day Effect Model data has been saved to file: %1", 
           file_path_tmp)
}

put_log("Preliminary tuning stage of the UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 1` has ended up with with the following results:")
put(lss.UMGYDE.preset.degree1.result$best_result)
# param.best_value        best_RMSE 
#        0.0015000        0.8576205 

###### UMGYDE (Rough) Tuned Model Plot (`degree = 1`) --------------------------
lss.UMGYDE.preset.degree1.result$tuned.result |>
  data.plot(title = "Preliminary set-up for tuning UMGY+(Smoothed)Day Effect Model using `loess` with parameter `degree = 1`",
              xname = "parameter.value", 
              yname = "RMSE", 
              xlabel = "spans", 
              ylabel = "RMSE")

##### Close Log -----------------------------------------------------------------
log_close()

##### Open log for UMGYDE Model Fine-tuning (`degree = 1`) ---------------------
open_logfile(".lss.UMGYDE.fine-tune.degree1")
##### UMGYDE Model Fine-tuning (`degree = 1`) ---------------------------------- 
lss.fine_tune.loop_starter <- 
  c(lss.UMGYDE.preset.degree1.result$tuned.result$parameter.value[1], 
    lss.UMGYDE.preset.degree1.result$tuned.result$parameter.value[3], 
    8)
# loop_starter <- c(0.0005, 0.0025, 8)
cache_file.base_name <- "UMGYDE.degree1.tuning-span"

lss.UMGYDE.fine_tune.degree1.result <- 
  model.tune.param_range(lss.fine_tune.loop_starter,
                         UMGYDE.fine_tune.degree1.data.path,
                         cache_file.base_name,
                         train_UMGY_SmoothedDay_effect.RMSE.cv.degree1)

put_log("Fine-tuning stage of the UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 1` has ended up with with the following results:")
put(lss.UMGYDE.fine_tune.degree1.result$best_result)
# param.best_value        best_RMSE 
#      0.000890625      0.856860200 

lss.UMGYDE.fine_tune.degree1.result.best_span <- 
  lss.UMGYDE.fine_tune.degree1.result$best_result["param.best_value"]

lss.UMGYDE.fine_tune.degree1.result.best_RMSE <- 
  lss.UMGYDE.fine_tune.degree1.result$best_result["best_RMSE"]

###### UMGYDE Fine-tuned Model Plot: `RMSEs` vs `spans` (`degree` = 1) ---------  
lss.UMGYDE.fine_tune.degree1.result$tuned.result |>
  data.plot(title = "Fine-tuned UMGYDE Model with `loess` parameter: `degree = 1`", 
                             xname = "parameter.value", 
                             yname = "RMSE", 
                             xlabel = "spans", 
                             ylabel = "RMSE")

##### UMGYDE Tuned Model RMSE Result Tibble: Add a row (`degree = 1`) ---------- 
RMSEs.ResultTibble.UMGYDE1 <- RMSEs.ResultTibble.UMGYDE0 |> 
  RMSEs.AddRow("Tuned UMGYDE.d1 Model", 
               lss.UMGYDE.fine_tune.degree1.result.best_RMSE,
               comment = "UMGYDE Model computed using function call: `loess(degree = 1, span = %1)`" |>
                 msg.glue(lss.UMGYDE.fine_tune.degree1.result.best_span))

RMSE_kable(RMSEs.ResultTibble.UMGYDE1)

##### Close Log ---------------------------------------------------------------
log_close()
#### UMGYDE Model Tuning: Step 3 (`degree = 2`) --------------------------------
##### Open log for UMGYDE Model Tuning: Pre-configuration (`degree = 2`) -------
open_logfile(".UMGYDE.loess.degree2.pre-tuning")
put("Case 3. `degree = 2`")
##### UMGYDE Model Tuning: Pre-configuration (`degree = 2`) --------------------
file_name_tmp <- "1.lss.UMGYDE.pre-set.degree2.RData"
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
  lss.UMGYDE.preset.degree2.result <- 
    tune.model_param(spans, train_UMGY_SmoothedDay_effect.RMSE.cv.degree2)
  put_end_date(start)
  put_log1("Preliminary set-up data for tuning UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 2` has been computed 
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
  put_log1("Saving UMGY+(Smoothed)Day Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       edx.user_effect,
       rglr.UM_effect,
       rglr.UMG_effect,
       rglr.UMGY_effect,
       lss.UMGYDE.preset.degree2.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("UMGY+(Smoothed)Day Effect Model data has been saved to file: %1", 
           file_path_tmp)
}

put_log("Preliminary tuning stage of the UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 2` has ended up with with the following results:")
put(lss.UMGYDE.preset.degree2.result$best_result)
# param.best_value        best_RMSE 
#          0.00150          0.85762 

###### UMGYDE (Rough) Tuned Model Plot (`degree = 2`) --------------------------
lss.UMGYDE.preset.degree2.result$tuned.result |>
  data.plot(title = "Preliminary set-up for tuning UMGY+(Smoothed)Day Effect Model using `loess` with parameter `degree = 2`",
              xname = "parameter.value", 
              yname = "RMSE", 
              xlabel = "spans", 
              ylabel = "RMSE")

##### Close Log -----------------------------------------------------------------
log_close()

##### Open log for UMGYDE Model Fine-tuning (`degree = 2`) ---------------------
open_logfile(".lss.UMGYDE.fine-tune.degree2")
##### UMGYDE Model Fine-tuning (`degree = 2`) ---------------------------------- 
lss.fine_tune.loop_starter <- 
  c(lss.UMGYDE.preset.degree2.result$tuned.result$parameter.value[1], 
    lss.UMGYDE.preset.degree2.result$tuned.result$parameter.value[3], 
    8)
# loop_starter <- c(0.0005, 0.0025, 8)
cache_file.base_name <- "UMGYDE.degree2.tuning-span"

lss.UMGYDE.fine_tune.degree2.result <- 
  model.tune.param_range(lss.fine_tune.loop_starter,
                         UMGYDE.fine_tune.degree2.data.path,
                         cache_file.base_name,
                         train_UMGY_SmoothedDay_effect.RMSE.cv.degree2)

put_log("Fine-tuning stage of the UMGY+(Smoothed)Day Effect Model
using `loess` function with parameter `degree = 2` has ended up with with the following results:")
put(lss.UMGYDE.fine_tune.degree2.result$best_result)
# param.best_value        best_RMSE 
#      0.001328125      0.857151072 

lss.UMGYDE.fine_tune.degree2.result.best_span <- 
  lss.UMGYDE.fine_tune.degree2.result$best_result["param.best_value"]

lss.UMGYDE.fine_tune.degree2.result.best_RMSE <- 
  lss.UMGYDE.fine_tune.degree2.result$best_result["best_RMSE"]

###### UMGYDE Fine-tuned Model Plot: `RMSEs` vs `spans` (`degree` = 2) ---------  
lss.UMGYDE.fine_tune.degree2.result$tuned.result |>
  data.plot.left_detailed(title = "Fine-tuned UMGYDE Model with `loess` parameter: `degree = 2`", 
                             title.left = "Left Part of the Chart Above (Zoomed in)",
                             left.n = 8,
                             xname = "parameter.value", 
                             yname = "RMSE", 
                             xlabel1 = "spans", 
                             ylabel1 = "RMSE")

##### UMGYDE Tuned Model RMSE Result Tibble: Add a row (`degree = 2`) ----------- 
RMSEs.ResultTibble.UMGYDE2 <- RMSEs.ResultTibble.UMGYDE1 |> 
  RMSEs.AddRow("Tuned UMGYDE.d2 Model", 
               lss.UMGYDE.fine_tune.degree2.result.best_RMSE,
               comment = "UMGYDE Model computed using function call: `loess(degree = 2, span = %1)`" |>
                 msg.glue(lss.UMGYDE.fine_tune.degree2.result.best_span))

RMSE_kable(RMSEs.ResultTibble.UMGYDE2)

#### Close Log -----------------------------------------------------------------
log_close()

####  Open log for UMGYDE Tuned Model: Re-training with the Best Rarams --------
open_logfile(".lss.UMGYDE.re-train.best_degree&span")
#### UMGYDE Tuned Model: Retraining with the best params -----------------------
file_name_tmp <- "1.lss.UMGYE.re-train.best-params.RData"
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
  lss.best_results <- data.frame(degree = degree, 
                           span = c(lss.UMGYDE.fine_tune.degree0.result.best_span,
                                    lss.UMGYDE.fine_tune.degree1.result.best_span,
                                    lss.UMGYDE.fine_tune.degree2.result.best_span),
                           
                           RMSE = c(lss.UMGYDE.fine_tune.degree0.result.best_RMSE, 
                                    lss.UMGYDE.fine_tune.degree1.result.best_RMSE,
                                    lss.UMGYDE.fine_tune.degree2.result.best_RMSE))
  put(lss.best_results)
  
  lss.best_RMSE.idx <- which.min(lss.best_results$RMSE)
  
  
  lss.UMGYDE.best_params <- 
    c(degree =  lss.best_results[lss.best_RMSE.idx, "degree"],  # 1
      span = lss.best_results[lss.best_RMSE.idx, "span"], # 0.00087,
      RMSE = lss.best_results[lss.best_RMSE.idx, "RMSE"]) # 0.8568619

  lss.best_degree <- lss.UMGYDE.best_params["degree"]
  put_log1("The Best Degree: %1", lss.best_degree)
  lss.best_degree
  #> [1] 1
  
  lss.best_span <- lss.UMGYDE.best_params["span"]
  put_log1("The Best Span: %1", lss.best_span)
  lss.best_span
  #> [1] 0.00087
  
  lss.best_RMSE <- lss.UMGYDE.best_params["RMSE"]
  put_log1("The Best RMSE: %1",lss.best_RMSE)
  lss.best_RMSE
  #> [1] 0.8568619

  put_log2("Re-training model using `loess` function with the best parameters: 
span = %1, degree = %2", lss.best_span, lss.best_degree)
  start <- put_start_date()
  lss.UMGYD_effect <- edx |> 
    train_UMGY_SmoothedDay_effect(lss.best_degree, lss.best_span)
  
  str(lss.UMGYD_effect)
  put_end_date(start)
  put_log2("The model has been re-trained using `loess` function with the best parameters: 
span = %1, degree = %2", lss.best_span, lss.best_degree)
  
  put_log1("Saving Tuned Day Smoothed Effect data to file: %1", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       edx.user_effect,
       rglr.UM_effect,
       rglr.UMG_effect,
       rglr.UMGY_effect,
       lss.UMGYD_effect,
       lss.UMGYDE.best_params,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("Tuned (using `loess` function call) UMGYDE Model data has been saved to file: %1", 
           file_path_tmp)
}

##### Tuned UMGYD Effect data integrity test------------------------
tuned.UMGYDE.tst <- lss.UMGYD_effect |>
  mutate(tst.col = de_smoothed) |>
  select(days, tst.col)

tuned.UMGYDE.test.left_join.Nas <- tuned.UMGYDE.tst |>
  data.consistency.days.test.cv()

put_log("Below are the User+Movie Effect consistency test results")
put(tuned.UMGYDE.test.left_join.Nas)
#      user.NAs movie.NAs
# [1,]       NA         0
# [2,]       NA         0
# [3,]       NA         0
# [4,]       NA         0
# [5,]       NA         0

stopifnot(colSums(tuned.UMGYDE.test.left_join.Nas)["days.NAs"] == 0)
##### The Best Date Smoothed Effect Visualization ----------------------------------
lss.UMGYD_effect |>
  ggplot(aes(x = days)) +
  geom_point(aes(y = de), size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(y = de_smoothed), color = "red")

put_log1("Optimized Mean Date Smoothed Effect has been plotted for the %1-Fold Cross Validation samples.",
         CVFolds_N)
#### UMGYDE Model (best params): RMSE Calculation ------------------------------

lss.best_degree <- lss.UMGYDE.best_params["degree"]
lss.best_span <- lss.UMGYDE.best_params["span"]
lss.best_RMSE <- lss.UMGYDE.best_params["RMSE"]

start <- put_start_date()
lss.UMGYD_effect.RMSE <- calc_UMGY_SmoothedDay_effect.RMSE.cv(lss.UMGYD_effect)
put_end_date(start)
put_log3("RMSE value has been computed using `loess` function 
with the best parameters for the %1-Fold Cross Validation samples:
degree = %2;
span = %3.",
        CVFolds_N,
        lss.best_degree,
        lss.best_span)

put_log1("Is this a best RMSE? %1",
         lss.best_RMSE == lss.UMGYD_effect.RMSE)

print(lss.UMGYD_effect.RMSE)
#> [1] 0.8568612

##### UMGYDE Tuned Model RMSE Result Tibble: Add a row (best params) ----------- 
RMSEs.ResultTibble.UMGYDE.tuned <- RMSEs.ResultTibble.UMGYDE2 |> 
  RMSEs.AddRow("Tuned UMGYDE Best Model", 
               lss.UMGYD_effect.RMSE,
               comment = "UMGYDE Model computed using `loess` function call with the best degree & span values.")

RMSE_kable(RMSEs.ResultTibble.UMGYDE.tuned)



put_log("A row has been added to the RMSE Result Tibble 
for the tuned `User+Movie+Genre+Year+(Smoothed)Day Effect Model`
using `loess` function call with the best degree & span values.")

#### Close Log -----------------------------------------------------------------
log_close()
### UMGDYE Model Regularization ------------------------------------------------
#### Open log for Pre-configuration step ----------------------------------
open_logfile(".rglr.UMGYD-effect.pre-set-lambdas")
#### UMGYDE Model Regularization Directory Paths -------------------------------
UMGYDE.regularization.path <- file.path(data.regularization.path, 
                                       "4.UMGYD-effect")
dir.create(UMGYDE.regularization.path)
put_log1("Directory path has been created for `User+Movie+Genre+Year+(Smoothed)Day Effect Model` data: %1", 
         UMGYDE.regularization.path)

UMGYDE.rglr.fine_tune.cache.path <- file.path(UMGYDE.regularization.path, 
                                             fine_tune.cache.folder)
dir.create(UMGYDE.rglr.fine_tune.cache.path)
put_log1("Directory path has been created: %1", UMGYDE.rglr.fine_tune.cache.path)
#### UMGYDE Model Regularization: Pre-configuration --------------------------
file_name_tmp <- "1.UMGYDE.rglr.pre-set.RData" # UMGE stands for `User+Movie+Genre+Year+(Smoothed)Day Effect`
file_path_tmp <- file.path(UMGYDE.regularization.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading preliminary regularization set-up data for User+Movie+Genre+Year+(Smoothed)Day Effect from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Preliminary regularization set-up data for User+Movie+Genre+Year+(Smoothed)Day Effect has been loaded from file: %1", 
           file_path_tmp)
} else {
  put_log1("Preliminary setting-up of `lambda`s range for %1-Fold Cross Validation samples...",
           CVFolds_N)

  start <- put_start_date()
  lambdas <- seq(0, 256, 16)
  cv.UMGYDE.preset.result <- 
    tune.model_param(lambdas, regularize.test_lambda.UMGYD_effect.cv)
  put_end_date(start)
  put_log1("Preliminary regularization set-up of `lambda`s range for the UMGYDE Model has been completed
for the %1-Fold Cross Validation samples.",
CVFolds_N)
  
  put_log1("Saving User+Movie+Genre+Year+(Smoothed)Day Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       edx.user_effect,
       rglr.UM_effect,
       rglr.UMG_effect,
       rglr.UMGY_effect,
       cv.UMGYDE.preset.result,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre+Year+(Smoothed)Day Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

put_log("Preliminary regularization set-up of `lambda`s range for the User+Movie+Genre+Year+(Smoothed)Day Effect 
has resulted as follows:")
put(cv.UMGYDE.preset.result$best_result)

##### Plot (rough) dependency of RMSEs vs lambdas -----------------------------  
cv.UMGYDE.preset.result$tuned.result |>
  data.plot(title = TeX(r'[UMGYDE Model Regularization: $\lambda$ Range Pre-configuration]'),
              xname = "parameter.value", 
              yname = "RMSE", 
              xlabel = TeX(r'[$\lambda$]'), 
              ylabel = "RMSE")

#### Close Log -----------------------------------------------------------------
log_close()
#### Open log for UMGYDE Model Regularization:Fine-tunig -----------------------
open_logfile(".UMGYDE.rglr.fine-tuning")
#### UMGYDE Model Regularization: Fine-tuning ----------------------------------- 
endpoints <- 
  get_fine_tune.param.endpoints(cv.UMGYDE.preset.result$tuned.result)

UMGYDE.loop_starter <- c(endpoints["start"], 
                        endpoints["end"], 
                        8)
UMGYDE.loop_starter
# start   end       
#     0    32     8 

cache.base_name <- "UMGYDE.rglr.fine-tuning"

UMGYDE.rglr.fine_tune.results <- 
  model.tune.param_range(UMGYDE.loop_starter,
                         UMGYDE.rglr.fine_tune.cache.path,
                         cache.base_name,
                         regularize.test_lambda.UMGYD_effect.cv)

put_log("Fine-tuning stage of the User+Movie+Genre+Year+(Smoothed)Day Effect Model Regularization 
has ended up with with the following results:")
put(UMGYDE.rglr.fine_tune.results$best_result)

UMGYDE.rglr.fine_tune.RMSE.best <- UMGYDE.rglr.fine_tune.results$best_result["best_RMSE"]

##### Plot (fine-tuned) dependency of RMSEs vs lambdas ------------------------  
UMGYDE.rglr.fine_tune.results$tuned.result |>
  data.plot(title = "UMGYDE Model Regularization: Fine-tuned result",
              xname = "parameter.value",
              yname = "RMSE",
              xlabel = TeX(r'[$\lambda$]'),
              ylabel = str_glue("Deviation from the best RMSE value (",
                                as.character(round(UMGYDE.rglr.fine_tune.RMSE.best, digits = 7)),
                                ")"),
              normalize = TRUE)
#### Close Log -----------------------------------------------------------------
log_close()
####  Open log for Regularized Model: Re-training with the Best Rarams ---------
open_logfile(".UMGYDE.rg.re-train.best-lambda")
#### UMGYDE Regularized Model: Retraining with the best params -----------------
file_name_tmp <- "2.UMGYDE.rglr.re-train.best-lambda.RData"
file_path_tmp <- file.path(UMGYDE.regularization.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading Regularized User+Movie+Genre+Year+(Smoothed)Day Effect Model data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Regularized User+Movie+Genre+Year+(Smoothed)Day Effect Model data has been loaded from file: %1", 
           file_path_tmp)
} else {
  best_result <- UMGYDE.rglr.fine_tune.results$best_result
  UMGYDE.rglr.best_lambda <- best_result["param.best_value"]
  UMGYDE.rglr.best_RMSE <- best_result["best_RMSE"]
  
  put_log1("Re-training Regularized User+Movie+Genre+Year+(Smoothed)Day Effect Model for the best `lambda`: %1...",
           UMGYDE.rglr.best_lambda)
  
  rglr.UMGYD_effect <- edx |> 
    regularize.train_UMGYD_effect(UMGYDE.rglr.best_lambda)
  
  str(rglr.UMGYD_effect)
  rglr.UMGYD_effect.RMSE <- calc_UMGY_SmoothedDay_effect.RMSE.cv(rglr.UMGYD_effect)
  #> [1] 0.8568333
  
  put_log2("Regularized User+Movie+Genre+Year+(Smoothed)Day Effect RMSE has been computed for the best `lambda = %1`: %2.",
           UMGYDE.rglr.best_lambda,
           rglr.UMGYD_effect.RMSE)
  put_log1("Is this a best RMSE? %1",
           UMGYDE.rglr.best_RMSE == rglr.UMGYD_effect.RMSE)
  
  
  put_log1("Saving User+Movie+Genre+Year+(Smoothed)Day Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       edx.user_effect,
       rglr.UM_effect,
       rglr.UMG_effect,
       rglr.UMGY_effect,
       rglr.UMGYD_effect,
       rglr.UMGYD_effect.RMSE,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre+Year+(Smoothed)Day Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 

##### Regularized UMGYD Effect data integrity test------------------------
rglr.UMGYDE.tst <- rglr.UMGYD_effect |>
  mutate(tst.col = de_smoothed) |>
  select(days, tst.col)

rglr.UMGYDE.test.left_join.Nas <- rglr.UMGYDE.tst |>
  data.consistency.days.test.cv()

put_log("Below are the User+Movie Effect consistency test results")
put(rglr.UMGYDE.test.left_join.Nas)
#      user.NAs movie.NAs days.NAs
# [1,]       NA        NA        0
# [2,]       NA        NA        0
# [3,]       NA        NA        0
# [4,]       NA        NA        0
# [5,]       NA        NA        0

stopifnot(colSums(rglr.UMGYDE.test.left_join.Nas)["days.NAs"] == 0)
#### UMGYDE Regularized Model RMSE Result Tibble: Add a row -------------------- 
RMSEs.ResultTibble.UMGYDE.rglr.tuned <- RMSEs.ResultTibble.UMGYDE.tuned |> 
  RMSEs.AddRow("Regularized UMGYDE Model", 
               rglr.UMGYD_effect.RMSE,
               comment = "The best tuned and regularized UMGYDE Model.")

RMSE_kable(RMSEs.ResultTibble.UMGYDE.rglr.tuned)

put_log("A row has been added to the RMSE Result Tibble 
for the `Regularized User+Movie+Genre+Year+(Smoothed)Day Effect Model`.")
#### Close Log -----------------------------------------------------------------
log_close()

### UMGYDE Model: Final Holdout Test  ------------------------------------------

final.UMGYDE.predicted <- final_holdout_test |>
  UMGY_SmoothedDay_effect.predict(rglr.UMGYD_effect)

str(final.UMGYDE.predicted)
sum(is.na(final.UMGYDE.predicted$predicted))

# calc_UMGY_SmoothedDay_effect.RMSE(final_holdout_test, rglr.UMGYD_effect)
# #> [1] 0.902012

#### UMGYDE Model Final Holdout Test data integrity validation------------------
final.predicted.tst <- final.UMGYDE.predicted |>
  mutate(tst.col = predicted) |>
  select(userId, movieId, tst.col)

final.predicted.left_join.Nas <- final.predicted.tst |>
  data.consistency.test(final_holdout_test)

put_log("Below are the User+Movie Effect consistency test results")
put(final.predicted.left_join.Nas)
 # user.NAs movie.NAs  days.NAs 
 #        0         0        NA 
        
stopifnot(final.predicted.left_join.Nas["user.NAs"] == 0 &&
            final.predicted.left_join.Nas["movie.NAs"] == 0)

#### Compute UMGYDE Model Final Holdout Test RMSE ------------------------------
final.UMGYDE.predicted.RMSE <- rmse2(final_holdout_test$rating,
                                     final.UMGYDE.predicted$predicted)
final.UMGYDE.predicted.RMSE
#> [1] 0.8804351

#### UMGYDE Model Final Holdout Test RMSE Result Tibble: Add a row ------------- 
final.RMSEs.ResultTibble.UMGYDE.rglr.tuned <- RMSEs.ResultTibble.UMGYDE.rglr.tuned |> 
  RMSEs.AddRow("Best UMGYDE Model (Final Test)", 
               final.UMGYDE.predicted.RMSE,
               comment = "Final Holdout Test of the best tuned and regularized UMGYDE Model.")

RMSE_kable(final.RMSEs.ResultTibble.UMGYDE.rglr.tuned)

put_log("A row has been added to the RMSE Result Tibble 
for the `Final Holdout Test of the User+Movie+Genre+Year+(Smoothed)Day Effect Model`.")
#### Close Log -----------------------------------------------------------------
log_close()

## Matrix Factorization (MF) ---------------------------------------------------
#> Reference: 
#> recosystem: Recommender System Using Parallel Matrix Factorization
#> https://cran.r-project.org/web/packages/recosystem/vignettes/introduction.html

#>
#> https://www.r-bloggers.com/2016/07/recosystem-recommender-system-using-parallel-matrix-factorization/
#> https://zhangyk8.github.io/teaching/file_spring2018/Improving_regularized_singular_value_decomposition_for_collaborative_filtering.pdf
#> https://www.csie.ntu.edu.tw/~cjlin/papers/libmf/mf_adaptive_pakdd.pdf

### Open log for MF ------------------------------------------------------------
open_logfile(".matrix-factorization")
### MF: Helper Functions ------------------------------------------------------
MF.functions.file <- "MF.functions.R"
MF.functions.file_path <- file.path(support_functions.path, 
                                    MF.functions.file)
source(MF.functions.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)

### Perform the Matrix Factorization & Final Test ----------------------------
# library(recosystem)

file_name_tmp <- "10.matrix-factorization.RData"
file_path_tmp <- file.path(data.models.path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading overal mean rating value from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Overall mean rating data has been loaded from file: %1",
           file_path_tmp)
} else {
  mf.edx.residual <- mf.residual.dataframe(edx)
  str(mf.edx.residual)
  sum(is.na(mf.edx.residual))
  #> [1] 0
  
  set.seed(5430)
  mf.edx.residual.reco <- with(mf.edx.residual, 
                               data_memory(user_index = userId, 
                                           item_index = movieId,
                                           rating = rsdl))

  final_holdout_test.reco <- with(final_holdout_test, 
                          data_memory(user_index = userId, 
                                      item_index = movieId, 
                                      rating = rating))

  reco <- Reco()
  
  reco.tuned <- reco$tune(mf.edx.residual.reco, opts = list(dim = c(10, 20, 30),
                                                # costp_l2 = c(0.01, 0.1),
                                                # costq_l2 = c(0.01, 0.1),
                                                # costp_l1 = 0,
                                                # costq_l1 = 0,
                                                lrate    = c(0.1, 0.2),
                                                nthread  = 4,
                                                niter    = 10,
                                                verbose  = TRUE))
  
  reco$train(mf.edx.residual.reco, opts = c(reco.tuned$min,
                                       niter = 20, 
                                       nthread = 4)) 
  
  mf.reco.residual <- reco$predict(final_holdout_test.reco, out_memory())
  str(mf.reco.residual)
  sum(is.na(mf.reco.residual))
  
  mf.predicted_ratings <- 
    clamp(final.UMGYDE.predicted$predicted + mf.reco.residual)
  
  str(mf.predicted_ratings)
  sum(is.na(mf.predicted_ratings))

  put_log1("Saving User+Movie+Genre+Year+(Smoothed)Day Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mf.edx.residual,
       mf.edx.residual.reco,
       final_holdout_test.reco,
       reco.tuned,
       mf.reco.residual,
       final.UMGYDE.predicted,
       mf.predicted_ratings,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("Matrix Factorization Method data has been saved to file: %1", 
           file_path_tmp)

}
#### Compute Final Holdout Test RMSE -------------------------------------------
final_holdout_test.RMSE <- rmse2(final_holdout_test$rating,
                                     mf.predicted_ratings)
final_holdout_test.RMSE
#> [1] 0.7875645

### Final Holdout Test RMSE Result Tibble: Add a row  -------------------------- 
final.MF.RMSEs.ResultTibble <- final.RMSEs.ResultTibble.UMGYDE.rglr.tuned |> 
  RMSEs.AddRow("MF (Final Test)", 
               final_holdout_test.RMSE,
               comment = "Matrix Factorization of the Best Model Residuals, Final Holdout Test")

RMSE_kable(final.MF.RMSEs.ResultTibble)
put_log("A row has been added to the RMSE Result Tibble 
for the `Final Holdout Test of the User+Movie+Genre+Year+(Smoothed)Day Effect Model`.")
### Total Results Tibbe --------------------------------------------------------
total.RMSEs.ResultTibble <- RMSEs.AddDiffColumn(final.MF.RMSEs.ResultTibble)
RMSE.Total_kable(total.RMSEs.ResultTibble)
### Close Log -----------------------------------------------------------------
log_close()

