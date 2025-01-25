# if(!require(tidyverse)) 
#   install.packages("tidyverse", repos = "http://cran.us.r-project.org")
# if(!require(caret)) 
#   install.packages("caret", repos = "http://cran.us.r-project.org")
# if(!require(data.table)) 
#   install.packages("data.table", repos = "http://cran.us.r-project.org")

# setwd(".../Capstone-MovieLens/r/work")

# Loading the required libraries
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

## The Netflix Prize Dataset -------------------------------------------------
# https://www.asc.ohio-state.edu/statistics/statgen/joul_aut2009/BigChaos.pdf

#> The goal of the contest is to predict the qualifying set (size: 2817131 samples) 
#> and achieve a RMSE score of at least 0.8563 on the quiz subset, 
#> to get qualifed for the Grand Prize.

### Initial Data ---------------------------------------------------------------
np_training_set_cnt <- 100480507
np_probe_set_cnt <- 1408395  # subset of `training_set`

probe_set_ratio <- np_probe_set_cnt/np_training_set_cnt
#> [1] 0.0140166

np_qualifying_set_cnt <- 2817131

quiz_set_ratio <- 0.5
test_set_ratio <- 1 - quiz_set_ratio

np_rmse_accepted_max <- 0.8563

#####################################################

# Inspired by:
# HarvardX: PH125.8x
# Data Science: Machine Learning, Section 6.2: Recommendation Systems
#> The proposed solution below uses information (including citates) from the folowing textbook:
#> "Introduction to Data Science" written by Rafael A. Irizarry:
#> https://rafalab.dfci.harvard.edu/dsbook-part-2/ 

# Reference: the Textbook section: 23.1 Case study: recommendation systems
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#sec-recommendation-systems

## Split the `edx` dataset in `train_set` & `test_set` ------------------------

#str(edx)
# 'data.frame':	9000055 obs. of  6 variables:
# $ userId   : int  1 1 1 1 1 1 1 1 1 1 ...
# $ movieId  : int  122 185 292 316 329 355 356 362 364 370 ...
# $ rating   : num  5 5 5 5 5 5 5 5 5 5 ...
# $ timestamp: int  838985046 838983525 838983421 838983392 838983392 838984474 838983653 838984885 838983707 838984596 ...
# $ title    : chr  "Boomerang (1992)" "Net, The (1995)" "Outbreak (1995)" "Stargate (1994)" ...
# $ genres   : chr  "Comedy|Romance" "Action|Crime|Thriller" "Action|Drama|Sci-Fi|Thriller" "Action|Adventure|Sci-Fi" ...

#str(final_holdout_test)

### Prepare train & test datasets ----------------------------------------------

#> Let's see the number of unique users that provided ratings 
#> and how many unique movies were rated:
edx |> summarize(n_distinct(userId), n_distinct(movieId))
#   n_distinct(userId) n_distinct(movieId)
# 1              69878               10677

#> Let's ignore the data for users who have not provided at least 100 ratings:
edx100 <- edx |> 
  group_by(userId) |>
  filter(n() >= 100) |>
  ungroup()

edx100 |> summarize(n_distinct(userId), n_distinct(movieId))
# `     n_distinct(userId)` `n_distinct(movieId)`
#                    <int>                 <int>
#   1                24115                 10665

#> For each one of these users, we will split their ratings into 80% for training 
#> and 20% for testing:

set.seed(2006)
indexes <- split(1:nrow(edx100), edx100$userId)
test_ind <- sapply(indexes, function(i) sample(i, ceiling(length(i)*.2))) |> 
  unlist() |>
  sort()

test_set <- edx100[test_ind,] 
train_set <- edx100[-test_ind,]

#> To make sure we don’t include movies in the training set that should not be 
#> there, we remove entries using the semi_join function:
test_set <- test_set |> semi_join(train_set, by = "movieId") |>
  as.data.frame()

train_set <- mutate(train_set, userId = factor(userId), movieId = factor(movieId))

dim(train_set)
#> [1] 5539951       6

class(train_set)
#> [1] "tbl_df"     "tbl"        "data.frame"

str(train_set)
# tibble [5,539,951 × 6] (S3: tbl_df/tbl/data.frame)
# $ userId   : Factor w/ 24115 levels "8","10","13",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ movieId  : Factor w/ 10631 levels "1","2","3","4",..: 2 5 16 19 22 34 36 39 50 66 ...
# $ rating   : num [1:5539951] 2.5 3 3 3.5 2.5 3 3.5 2.5 5 2.5 ...
# $ timestamp: int [1:5539951] 1115858432 1116550582 1115859664 1115859653 1111545739 1116547009 1116547602 1116549031 1115859656 1115858883 ...
# $ title    : chr [1:5539951] "Jumanji (1995)" "Father of the Bride Part II (1995)" "Casino (1995)" "Ace Ventura: When Nature Calls (1995)" ...
# $ genres   : chr [1:5539951] "Adventure|Children|Fantasy" "Comedy" "Crime|Drama" "Comedy" ...

head(train_set)
# A tibble: 6 × 6
#   userId movieId rating  timestamp title                                 genres                             
#   <fct>  <fct>    <dbl>      <int> <chr>                                 <chr>                              
# 1 8      2          2.5 1115858432 Jumanji (1995)                        Adventure|Children|Fantasy         
# 2 8      5          3   1116550582 Father of the Bride Part II (1995)    Comedy                             
# 3 8      16         3   1115859664 Casino (1995)                         Crime|Drama                        
# 4 8      19         3.5 1115859653 Ace Ventura: When Nature Calls (1995) Comedy                             
# 5 8      22         2.5 1111545739 Copycat (1995)                        Crime|Drama|Horror|Mystery|Thriller
# 6 8      34         3   1116547009 Babe (1995)                           Children|Comedy|Drama|Fantasy      

summary(train_set)
 #     userId           movieId            rating        timestamp            title              genres         
 # 59269  :   5292   356    :  12720   Min.   :0.500   Min.   :8.248e+08   Length:5539951     Length:5539951    
 # 67385  :   5088   593    :  12506   1st Qu.:3.000   1st Qu.:9.657e+08   Class :character   Class :character  
 # 14463  :   3718   480    :  12482   Median :3.500   Median :1.059e+09   Mode  :character   Mode  :character  
 # 68259  :   3228   296    :  12479   Mean   :3.472   Mean   :1.051e+09                                        
 # 27468  :   3218   260    :  12157   3rd Qu.:4.000   3rd Qu.:1.135e+09                                        
 # 19635  :   3016   1196   :  11591   Max.   :5.000   Max.   :1.231e+09                                        
 # (Other):5516391   (Other):5466016                                                               


train_set |>
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


#> We will use the array representation described in `Section 17.5`, 
#> for the training data: we denote ranking for movie `j` by user `i`as `y[i,j]`. 
#> To create this matrix, we use pivot_wider:
  
y <- dplyr::select(train_set, movieId, userId, rating) |>
pivot_wider(names_from = movieId, values_from = rating) |>
column_to_rownames("userId") |>
as.matrix()

dim_y <- dim(y)
dim_y
#> [1] 24115 10626

movie_map <- train_set |> dplyr::select(movieId, title, genres) |> 
  distinct(movieId, .keep_all = TRUE)

str(movie_map)
head(movie_map)



## First Model -----------------------------------------------------------------
# Reference: the Textbook section "23.3 A first model"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#a-first-model

#>  A model that assumes the same rating for all movies and users with all 
#>  the differences explained by random variation would look as follows:
# Y[i,j] = μ + ε[i,j]

### Naive RMSE -------------------------------------------------------
mu <- mean(train_set$rating)
mu
#> [1] 3.471931

naive_rmse <- RMSE(test_set$rating, mu)
naive_rmse
#> [1] 1.062162

# str(final_holdout_test)
# head(final_holdout_test)

final_naive_rmse <- RMSE(final_holdout_test$rating, mu)
final_naive_rmse
#> [1] 1.061958

## User effects ---------------------------------------------------------------- 
# Reference: the Textbook section "23.4 User effects"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#user-effects

# If we visualize the average rating for each user:
hist(rowMeans(y, na.rm = TRUE), nclass = 30)

# we notice that there is substantial variability across users.
#>  To account for this, we can use a linear model with a treatment effect `α[i]` 
#>  for each user. The sum `μ + α[i]` can be interpreted as the typical 
#>  rating user `i` gives to movies. We can write the model as:

# Y[i,j] = μ + α[i] + ε[i,j]

### Support functions ---------------------------------------------------------

#> Because we know ratings can’t be below 0.5 or above 5, 
#> we define the function clamp:
clamp <- function(x, min = 0.5, max = 5) pmax(pmin(x, max), min)

# to keep predictions in that range and then compute the RMSE:
user_effects_rmse <- function(test_set, a){
  test_set |> 
    left_join(data.frame(userId = as.integer(names(a)), a = a), by = "userId") |>
    mutate(resid = rating - clamp(mu + a)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> rmse()
}

### Model training ------------------------------------------------------
#min_user_rates <- 100
n_bins <- 30

train_set |> 
  group_by(userId) |> 
  summarize(user_ratings_avg = mean(rating)) |> 
  ggplot(aes(user_ratings_avg)) + 
  geom_histogram(bins = n_bins, color = "black")

#> We can show that the least squares estimate `α[i]` is just the average 
#> of `y[i,j] - μ` for each user. So we can compute them this way:
  
a <- rowMeans(y - mu, na.rm = TRUE)

### Model testing --------------------------------------------------------------
model_user_rmse <- user_effects_rmse(test_set, a)
model_user_rmse
#> [1] 0.9718791

final_model_user_rmse <- user_effects_rmse(final_holdout_test, a)
final_model_user_rmse
#> [1] 0.9720994

## Movie Effects ---------------------------------------------------- 
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

### Model building -------------------------------------------------------------
b <- colMeans(y - mu - a, na.rm = TRUE)

### Support functions ---------------------------------------------------------

# to keep predictions in that range and then compute the RMSE:
user_and_movie_effects_rmse <- function(test_set, a, b){
    test_set |> 
    left_join(data.frame(userId = as.integer(names(a)), a = a), by = "userId") |>
    left_join(data.frame(movieId = as.integer(names(b)), b = b), by = "movieId") |>
    mutate(resid = rating - clamp(mu + a + b)) |>  
    filter(!is.na(resid)) |>
    pull(resid) |> rmse()
}

### Model testing ----------------------------------------------------------------
#> We can now construct predictors and see how much the `RMSE` improves:
model_user_movie_rmse <- user_and_movie_effects_rmse(test_set, a, b)
model_user_movie_rmse
#> [1] 0.8664145

final_model_user_movie_rmse <- user_and_movie_effects_rmse(final_holdout_test, a, b)
final_model_user_movie_rmse
#> [1] 0.8665345

## Penalized Least Squares ----------------------------------
# Reference: the Textbook section "23.6 Penalized least squares"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/regularization.html#penalized-least-squares

#> Instead of minimizing the least squares equation, 
#> we minimize an equation that adds a penalty:

#  ∑{i,j}(y[i,j] - μ - α[i] - β[j])^2 + λ*∑{j}β[j]^2

#> The values of `β[j]` that minimize this equation are:

# β[j](λ) = 1/(λ + n[j])*∑{u=1,n[i]}(Y[i,j] - μ - α[i])
# where `n[j]` is the number of ratings made for movie `j`.

### Support functions ----------------------------------------------------------
reg_rmse <- function(test_set, b){
  test_set |> 
    left_join(data.frame(userId = as.integer(names(a)), a = a), by = "userId") |>
    left_join(data.frame(movieId = as.integer(names(b)), b = b), by = "movieId") |>
    mutate(resid = rating - clamp(mu + a + b)) |> 
    filter(!is.na(resid)) |>
    pull(resid) |> rmse()
}

### Model building -------------------------------------------------------------

#> Here we will simply compute the RMSE we for different values of `λ` 
#> to illustrate the effect:
n <- colSums(!is.na(y))
sums <- colSums(y - mu - a, na.rm = TRUE)
lambdas <- seq(0, 10, 0.1)
rmses <- sapply(lambdas, function(lambda){
  b <-  sums / (n + lambda)
  reg_rmse(test_set, b)
})

# Here is a plot of the RMSE versus `λ`:
plot(lambdas, rmses, type = "l")

min(rmses)
#> [1] 0.8659219

lambda <- lambdas[which.min(rmses)] 
lambda
#> [1] 2.6

#> Using minimal `λ`, we can compute the regularized estimates:
b_reg <- sums / (n + lambda)

### Model testing ----------------------------------------------------------------
reg_rmse(test_set, b_reg)
#> [1] 0.8659219
reg_rmse(final_holdout_test, b_reg)
#> [1] 0.8663589

## Matrix factorization --------------------------------------------------------

#> So far the model ignores an important source of information related to the fact 
#> that groups of movies, have similar rating patterns and groups of users have 
#> similar rating patterns as well.

# To see an example of this, we compute residuals [X]:

# r[i,j] = y[i,j] - (μ + α[i] + β[j])
  
### Model building -------------------------------------------------------------

# Compute residuals for the model:
r <- sweep(y - mu - a, 2, b_reg)

# r_names <- colnames(r)
# head(r_names)

movie_titles <- 
  data.frame(titles = with(movie_map, title[match(colnames(r), movieId)]),
             genres = with(movie_map, genres[match(colnames(r), movieId)])) 

str(movie_titles)
head(movie_titles)

#### Romance movies sample ------------------
romance_movie_titles_idx <- str_detect(movie_titles$genres, "Romance")

# scent_of_woman_idx <- romance_movie_titles_idx &
#   str_detect(movie_titles$titles, "Scent of a Woman")
# 
# sum(scent_of_woman_idx)
# #> [1] 0

you_ve_got_mail_idx <- romance_movie_titles_idx &
  str_detect(movie_titles$titles, "You've Got Mail")

sum(you_ve_got_mail_idx)
#> [1] 1

sleepless_in_seattle_idx <- romance_movie_titles_idx &
  str_detect(movie_titles$titles, "Sleepless in Seattle")

sum(sleepless_in_seattle_idx)
#> [1] 1

head(movie_titles[romance_movie_titles_idx,])
#                                titles                  genres
# 8                     Clueless (1995)          Comedy|Romance
# 11         Vampire in Brooklyn (1995)          Comedy|Romance
# 14                   Desperado (1995) Action|Romance|Thriller
# 22              Before Sunrise (1995)           Drama|Romance
# 37 Four Weddings and a Funeral (1994)          Comedy|Romance
# 40               Reality Bites (1994)    Comedy|Drama|Romance

#movie_titles[str_detect(movie_titles$titles, "Vampire in Brooklyn"),]
#                        titles         genres
# 11 Vampire in Brooklyn (1995) Comedy|Romance

look_who_is_talking_now_idx <- romance_movie_titles_idx &
  str_detect(movie_titles$titles, "Look Who's Talking Now")

sum(look_who_is_talking_now_idx)
#> [1] 1

romance_movie_idx <- 
  you_ve_got_mail_idx |
  sleepless_in_seattle_idx | 
  look_who_is_talking_now_idx

sum(romance_movie_idx)
#> [1] 3

movie_titles[romance_movie_idx,]
#                             titles                  genres
# 754    Sleepless in Seattle (1993)    Comedy|Drama|Romance
# 818         You've Got Mail (1998)          Comedy|Romance
# 2981 Look Who's Talking Now (1993) Children|Comedy|Romance

romance_titles <- movie_titles[romance_movie_idx,1]
romance_titles
# [1] "Sleepless in Seattle (1993)"   "You've Got Mail (1998)"        "Look Who's Talking Now (1993)"

romance_idx <- which(romance_movie_idx)
romance_idx
#> [1]   754 818 2981

##### Plot Romance Sample --------------------------------------------------------- 
library(gridExtra)

prm12 <- qplot(r[,romance_idx[1]], 
               r[,romance_idx[2]], 
               xlab = romance_titles[1], 
               ylab = romance_titles[2])

prm23 <- qplot(r[,romance_idx[2]], 
               r[,romance_idx[3]], 
               xlab = romance_titles[2], 
               ylab = romance_titles[3])

prm13 <- qplot(r[,romance_idx[1]], 
               r[,romance_idx[3]], 
               xlab = romance_titles[1], 
               ylab = romance_titles[3])

grid.arrange(prm12, prm23 ,prm13, ncol = 3)

#### Mob movies sample ---------------------------
crime_movie_titles_idx <- str_detect(movie_titles$genres, "Crime")

movie_titles[str_detect(movie_titles$titles, "Goodfellas"),]
#                titles      genres
# 610 Goodfellas (1990) Crime|Drama

goodfellars_idx <- crime_movie_titles_idx &
  str_detect(movie_titles$titles, "Goodfellas")

sum(goodfellars_idx)
#> [1] 1

godfather_idx <- crime_movie_titles_idx &
  str_detect(movie_titles$titles, "Godfather")

sum(godfather_idx)
#> [1] 3

mob_sample_idx <- goodfellars_idx | godfather_idx
sum(mob_sample_idx)
#> [1] 4

mob_titles <- movie_titles$titles[mob_sample_idx]
mob_idx <- which(mob_sample_idx)
length(mob_idx)
#> [1] 4

mob_idx
#> [1]   610  926   955  2341

mob_titles
# [1] "Goodfellas (1990)"               "Godfather, The (1972)"  
# [3] "Godfather: Part II, The (1974)"  "Godfather: Part III, The (1990)"

# library(gridExtra)

pmb12 <- qplot(r[,mob_idx[1]], 
               r[,mob_idx[2]], 
               xlab = mob_titles[1], 
               ylab = mob_titles[2])

pmb23 <- qplot(r[,mob_idx[2]], 
               r[,mob_idx[3]], 
               xlab = mob_titles[2], 
               ylab = mob_titles[3])

pmb13 <- qplot(r[,mob_idx[1]], 
               r[,mob_idx[3]], 
               xlab = mob_titles[1], 
               ylab = mob_titles[3])

grid.arrange(pmb12, pmb23 ,pmb13, ncol = 3)

#### Romance vs Mob movies sample ---------------------------

pr1_m1 <- qplot(r[,romance_idx[1]], 
                r[,mob_idx[1]], 
                xlab = romance_titles[1], 
                ylab = mob_titles[1])

pr2_m2 <- qplot(r[,romance_idx[2]], 
                r[,mob_idx[2]], 
                xlab = romance_titles[2], 
                ylab = mob_titles[2])

pr3_m3 <- qplot(r[,romance_idx[3]], 
                r[,mob_idx[3]], 
                xlab = romance_titles[3], 
                ylab = mob_titles[3])

grid.arrange(pr1_m1, pr2_m2 ,pr3_m3, ncol = 3)

#### Factor analysis -------------------------------------
# Reference: the Textbook section "24.1 Factor analysis"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/matrix-factorization.html#sec-factor-analysis

##### Romance movies correlation sample ---------------------------
romance_sample <- r[, romance_idx]
colnames(romance_sample) <- romance_titles

dim(romance_sample)
str(romance_sample)

# qplot(romance_sample[, 1], 
#       romance_sample[, 2], 
#       xlab = colnames(romance_sample)[1], 
#       ylab = colnames(romance_sample)[2])

cor_romance_sample <- cor(romance_sample, 
                          use="pairwise.complete") 
dim(cor_romance_sample)
str(cor_romance_sample)

cor_romance_sample |> 
  knitr::kable()
# |                              | Sleepless in Seattle (1993)| You've Got Mail (1998)| Look Who's Talking Now (1993)|
# |:-----------------------------|---------------------------:|----------------------:|-----------------------------:|
# |Sleepless in Seattle (1993)   |                   1.0000000|              0.5479674|                     0.2168854|
# |You've Got Mail (1998)        |                   0.5479674|              1.0000000|                     0.1213388|
# |Look Who's Talking Now (1993) |                   0.2168854|              0.1213388|                     1.0000000|

##### Mob movies correlation sample ---------------------------
mob_sample <- r[, c(mob_idx[1], mob_idx[2], mob_idx[3])]
colnames(mob_sample) <- c(mob_titles[1], mob_titles[2], mob_titles[3])
#str(mob_sample)

cor(mob_sample, 
    use="pairwise.complete") |> 
  knitr::kable()
# |                                | Godfather, The (1972)| Godfather: Part II, The (1974)| Godfather: Part III, The (1990)|
# |:-------------------------------|---------------------:|------------------------------:|-------------------------------:|
# |Godfather, The (1972)           |             1.0000000|                      0.7371036|                       0.2547627|
# |Godfather: Part II, The (1974)  |             0.7371036|                      1.0000000|                       0.2456683|
# |Godfather: Part III, The (1990) |             0.2547627|                      0.2456683|                       1.0000000|


##### Romance vs Mob movies correlation sample ---------------------------

idx <- c(mob_idx[1], mob_idx[2], mob_idx[3], 
         romance_idx[1], romance_idx[2], romance_idx[3])
mob_vs_romance_sample <- r[, idx]

colnames(mob_vs_romance_sample) <- 
  c(mob_titles[1], mob_titles[2], mob_titles[3], 
    romance_titles[1], romance_titles[2], romance_titles[3])

dim(mob_vs_romance_sample)
str(mob_vs_romance_sample)

cor(mob_vs_romance_sample, 
    use="pairwise.complete") |> 
  knitr::kable()
# |                               | Goodfellas (1990)| Godfather, The (1972)| Godfather: Part II, The (1974)|
# |:------------------------------|-----------------:|---------------------:|------------------------------:|
# |Goodfellas (1990)              |         1.0000000|             0.3956433|                      0.3791739|
# |Godfather, The (1972)          |         0.3956433|             1.0000000|                      0.7371036|
# |Godfather: Part II, The (1974) |         0.3791739|             0.7371036|                      1.0000000|
# |Sleepless in Seattle (1993)    |        -0.0914330|            -0.0552713|                     -0.0676995|
# |You've Got Mail (1998)         |        -0.1076334|            -0.1143261|                     -0.0941677|
# |Look Who's Talking Now (1993)  |        -0.2970970|            -0.1248146|                     -0.1629234|
# 
# |                               | Sleepless in Seattle (1993)| You've Got Mail (1998)| Look Who's Talking Now (1993)|
# |:------------------------------|---------------------------:|----------------------:|-----------------------------:|
# |Goodfellas (1990)              |                  -0.0914330|             -0.1076334|                    -0.2970970|
# |Godfather, The (1972)          |                  -0.0552713|             -0.1143261|                    -0.1248146|
# |Godfather: Part II, The (1974) |                  -0.0676995|             -0.0941677|                    -0.1629234|
# |Sleepless in Seattle (1993)    |                   1.0000000|              0.5479674|                     0.2168854|
# |You've Got Mail (1998)         |                   0.5479674|              1.0000000|                     0.1213388|
# |Look Who's Talking Now (1993)  |                   0.2168854|              0.1213388|                     1.0000000|

#> It seems there is positive correlation within mob and romance movies, 
#> and negative across the two genres.

# We can quantify a factor that distinguishes between mob and romance movies with:
q <- c(-1, -1, -1, 1, 1, 1)

#> To determine which users prefer each genre, we can fit a linear model 
#> to each user:

p <- t(qr.solve(crossprod(q)) %*% t(q) %*% t(mob_vs_romance_sample))

hist(p, breaks = seq(-2,2,0.1))

#> To see that we can approximate 
#> with $p_iq_j we convert the vectors to matrices and use linear algebra:

p <- matrix(p); q <- matrix(q)
plot(p %*% t(q), mob_vs_romance_sample)

##### Connection to PCA -----------------------------------------
# Reference: the Textbook section "24.2 Connection to PCA"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/matrix-factorization.html#connection-to-pca

e <- na.omit(mob_vs_romance_sample)
# str(e)
head(e)

#> Notice that if we perform PCA on the matrix `e`, 
#> we obtain a transformation `V` that permits us to rewrite:

# e = Z*t(V)

#> with `Z` the matrix of principal components.

#> Let’s perform `PCA` and examine the results:

pca <- prcomp(e, center = FALSE)

#> First, notice that the first three PCs explain over 87% of the variability:
vr <- pca$sdev^2/sum(pca$sdev^2)
vr
#> [1] 0.47103883 0.24029462 0.16280777 0.06112944 0.03701468 0.02771466
# sum(c(0.47103883, 0.24029462, 0.16280777))
sum(vr[1:3])
#> [1] 0.8741412

# Next, notice that the first column of `V`:
pca$rotation[,1]
#           Goodfellas (1990)          Godfather, The (1972) Godfather: Part II, The (1974) 
#                -0.603874574                   -0.565117204                   -0.497675827 
# Sleepless in Seattle (1993)         You've Got Mail (1998)  Look Who's Talking Now (1993) 
#                 0.002749586                    0.192951008                    0.176236103 

#> is assigning negative values to the mob movies and positive values to the romance movies.

# # The second column:
# pca$rotation[,2]
# #>            Godfather          Godfather 2           Goodfellas 
# #>                0.354                0.377               -0.382 
# #>     Scent of a Woman      You've Got Mail Sleepless in Seattle 
# #>                0.437               -0.448               -0.442
# 
# 


#### Principal Component Analysis (PCA) -----------------------------------
# Reference: the Textbook section "24.3 Case study: movie recommendations"
# https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/matrix-factorization.html#connection-to-pca

#> We will rewrite the model from the previous chapter to include factors 
#> to explain similarities between movies:

# Y[i,j] = μ + α[i] + β[j] + ∑{k=[1,K]}p[i,k]q[j,k] + ε[i,j]

#> Unfortunately, we can’t fit this model with prcomp due to the missing values. 
#> We introduce the `missMDA` package that provides an approach to fit such models 
#> when matrix entries are missing, a very common occurrence 
#> in movie recommendations, through the function `imputePCA`. 
#> Also, because there are small sample sizes for several movie pairs, 
#> it is useful to regularize the `p`s. The imputePCA function also permits 
#> regularization.

#> We use the estimates for `μ`, the `α`s and `β`s calculated above, 
#> and estimate two factors (ncp = 2). 
#> We fit the model to movies rated more than 25 times, and, finally, 
#> we use regularization by setting the parameter `coeff.ridge` to the same value 
#> used to estimate the `β`s.

library(missMDA)

sum(is.na(y))

start <- start_date()
ind <- colSums(!is.na(y)) >= 25
# imputed <- imputePCA(r[,ind], ncp = 2, coeff.ridge = lambda)
end_date(start)



# References: ------------------------------------------------------------------

#> .
#>

#> . 
#> 

#> .
#>

#> X. Chapter "24  Matrix Factorization" of the Textbook [1]:
#> https://rafalab.dfci.harvard.edu/dsbook-part-2/highdim/matrix-factorization.html

#> X+1. "Matrix Factorization Techniques for Recommendation Systems" by 
#> Yehuda Koren, Yahoo Research; Robert Bell and Chris Volinsky, AT&T Labs - Research:
# https://datajobs.com/data-science-repo/Recommender-Systems-[Netflix].pdf

#> . "Movie Recommendation System using R - BEST" by Amir Motefaker:
#> https://www.kaggle.com/code/amirmotefaker/movie-recommendation-system-using-r-best#Regularization-function

#> .
#> 

#> .
#> 

#> .
#> 

#> .
#> 

#> .
#> 


#> .
#>https://datajobs.com/data-science-repo/Recommender-Systems-[Netflix].pdf

#> . Matrix Factorization: 
#> https://d2l.ai/chapter_recommender-systems/mf.html

#> . Koren, Y., Bell, R., & Volinsky, C. (2009). Matrix factorization techniques for recommender systems.
#> https://d2l.ai/chapter_references/zreferences.html#id154

#> . Blog post: 
#> https://sifter.org/%7Esimon/journal/20061211.html

#> . Töscher, A., Jahrer, M., & Bell, R. M. (2009). The bigchaos solution to the Netflix grand prize.
#> https://d2l.ai/chapter_references/zreferences.html#id286
#> #> https://www.asc.ohio-state.edu/statistics/statgen/joul_aut2009/BigChaos.pdf


# . https://www.kaggle.com/code/amirmotefaker/movie-recommendation-system-using-r-best#Ratings

#> . recosystem Package:
#> https://cran.r-project.org/web/packages/recosystem/vignettes/introduction.html
#> 
#> https://cran.r-project.org/web/packages/recosystem/index.html
#> https://www.csie.ntu.edu.tw/~cjlin/papers/libmf/libmf_open_source.pdf
#> https://cran.r-project.org/web/packages/recosystem/recosystem.pdf
#> https://www.csie.ntu.edu.tw/~cjlin/papers/libmf/mf_adaptive_pakdd.pdf

#> . recommenderlab: An R Framework for Developing and Testing Recommendation Algorithms
#> https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf

#> . "Efficient Bayesian Hierarchical User Modeling for Recommendation Systems" by
#> Yi Zhang , Jonathan Koren:
#> https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=3d41d1d7d00b6666cb547b47957857f77836050e

#> . "Recommender Systems Handbook" by Francesco Ricci, Lior Rokach, Bracha Shapira, Paul B. Kantor:
#> https://www.cse.iitk.ac.in/users/nsrivast/HCC/Recommender_systems_handbook.pdf

#> .
#>

#> .
#>

#> .
#>

#> .
#>

#> .
#>

#> .
#>

#> .
#>

#> .
#>








