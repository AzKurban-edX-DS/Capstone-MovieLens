# Init

## Define Logging Functions ----------------------------------------------------
log_func_script.file_path <- file.path(support_functions.path, "logging-functions.R")

source(log_func_script.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)

## Open log file for `Loading Source Datasets` feature -------------------------
open_logfile(".source-datasets")

## Load Source Datasets from Specially Designed Package ------------------------
if(!require(edx.capstone.movielens.data)) {
  start <- put_start_date()
  pak::pak("AzKurban-edX-DS/edx.capstone.movielens.data")
  put_end_date(start)
}

put_log("Dataset loaded from `edx.capstone.movielens.data` package: edx")
put(str(edx))
sum(is.na(edx$rating))
#> [1] 0
put(summary(edx))

put_log1("Dataset loaded from `edx.capstone.movielens.data` package: final_holdout_test:
%1", str(final_holdout_test))
sum(is.na(final_holdout_test$rating))
#> [1] 0
put(summary(final_holdout_test))
### Close Log ---------------------------------------------------------------
log_close()

### Open log file for `Initialize Source File Paths` Feature -------------------
open_logfile(".src.file-paths")
### External Common Helper functions -------------------------------------
common_helper_functions.file_path <- file.path(support_functions.path,
                                               "common-helper.functions.R")
source(common_helper_functions.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)
### Init Project Global Variables ----------------------------------------------
put("Set Project Objective according to Capstone course requirements")
project_objective <- 0.86490
put(project_objective)

put("Set minimum number of ratings to ignore")
min_nratings <- as.integer(100)
put(min_nratings)

CVFolds_N <- 5
kfold_index <- seq(from = 1:CVFolds_N)

#RMSEs.ResultTibble <- NULL

#### Data File Paths ------------------------------------------------------------
data.path <- "data"
dir.create(data.path)
put_log1("Directory path has been created: %1", data.path)

movielens_datasets_file <- "movielens-datasets.RData"
movielens_datasets_file_path <- file.path(data.path, movielens_datasets_file)
movielens_datasets_zip <- file.path(data.path, "movielens-datasets.zip")

regularization.cache.folder <- "regularization"
models.cache.folder <- "models"
model_tune.cache.folder <- "model-tune"
fine_tune.cache.folder <- "fine-tune"

data.models.path <- file.path(data.path, models.cache.folder)
dir.create(data.models.path)
put_log1("Directory path has been created: %1", data.models.path)

data.regularization.path <- file.path(data.path, regularization.cache.folder)
dir.create(data.regularization.path)
put_log1("Directory path has been created: %1", data.regularization.path)

data.model_tune.path <- file.path(data.models.path, model_tune.cache.folder)
dir.create(data.model_tune.path)
put_log1("Directory path has been created: %1", data.model_tune.path)

# src.regularization.path <- file.path(r.src.path, regularization.cache.folder)
# dir.create(src.regularization.path)
# put_log1("Directory path has been created: %1", src.regularization.path)

### Close Log ---------------------------------------------------------------
log_close()
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

### Open log: `Init Project Data Log`-------------------------------------------
open_logfile(".init-project-data")
### Support Functions ---------------------------------------------------------
Data.Helper.functions.file_path <- file.path(support_functions.path, 
                                             "data.helper.functions.R")
source(Data.Helper.functions.file_path, 
       catch.aborts = TRUE,
       echo = TRUE,
       spaced = TRUE,
       verbose = TRUE,
       keep.source = TRUE)

### `edx` & `final_holdout_test` Input Datasts Consistency Test ----------------
final_test.ljoin.NAs <- edx |>
  mutate(tst.col = rating) |>
  select(userId, movieId, tst.col) |>
  data.consistency.test(final_holdout_test)

put_log("Below are the`edx` & `final_holdout_test` consistency test results:")
put(final_test.ljoin.NAs)
# user.NAs movie.NAs 
#        0         0 
### Initialize Input Datasets --------------------------------------------------
movielens_datasets <- init_source_datasets()

#> Inpired by Chapter 29. Resampling methods
#> (in particular starting from 
#> Section 29.5 "Mathematical description of resampling methods" onwards.
#> https://rafalab.dfci.harvard.edu/dsbook-part-2/ml/resampling-methods.html
edx.mx <- movielens_datasets$edx.mx
put_log("`edx` data initialized as matrix")
put(str(edx.mx))

edx.sgr <- movielens_datasets$edx.sgr
put_log("`edx` data initialized as matrix")
put(str(edx.sgr))

edx_CV <- movielens_datasets$edx_CV
put("Set of K-Fold Cross Validation datasets summary: edx_CV")
put(summary(edx_CV))

tune.train_set <- movielens_datasets$tuning_sets$train_set
put("Train Set for tuning purposes")
put(summary(tune.train_set))

tune.test_set <- movielens_datasets$tuning_sets$validation_set
put("Test Set for tuning purposes")
put(summary(tune.test_set))

#### Tuning Datasts Consistency Test -----------------------------------------------
tune.test.ljoin.NAs <- tune.train_set |>
  mutate(tst.col = rating) |>
  select(userId, movieId, tst.col) |>
  data.consistency.test(tune.test_set)

put_log("Below are the tuning datasets consistency test results:") 
put(tune.test.ljoin.NAs)
# user.NAs movie.NAs 
#        0         0 
### Initialize Data Maps -------------------------------------------------------

movie_map <- movielens_datasets$movie_map
put("Dataset summary: movie_map")
put(summary(movie_map))

date_days_map <- movielens_datasets$date_days_map
put("Dataset summary: date_days_map")
put(summary(date_days_map))

put("Dataset summary: final_holdout_test")
put(summary(final_holdout_test))

rm(movielens_datasets)

### Close Log ---------------------------------------------------------------
log_close()

