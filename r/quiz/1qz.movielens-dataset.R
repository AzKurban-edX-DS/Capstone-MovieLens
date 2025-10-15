# HarvardX: PH125.8x
# Data Science: Capstone
# R code from course videos

# Section 6. Model Fitting and Recommendation Systems

## Quiz: MovieLens Dataset

#### Q1

#> How many rows and columns are there in the edx dataset?

str(edx)
# 'data.frame':	9000055 obs. of  6 variables:
# $ userId   : int  1 1 1 1 1 1 1 1 1 1 ...
# $ movieId  : int  122 185 292 316 329 355 356 362 364 370 ...
# $ rating   : num  5 5 5 5 5 5 5 5 5 5 ...
# $ timestamp: int  838985046 838983525 838983421 838983392 838983392 838984474 838983653 838984885 838983707 838984596 ...
# $ title    : chr  "Boomerang (1992)" "Net, The (1995)" "Outbreak (1995)" "Stargate (1994)" ...
# $ genres   : chr  "Comedy|Romance" "Action|Crime|Thriller" "Action|Drama|Sci-Fi|Thriller" "Action|Adventure|Sci-Fi" ...

# Number of rows:
nrow(edx)
#> [1] 9000055

ncol(edx)
#> [1] 6

#--------------------------------------------------------
##### Explanation
# The number of rows and columns can be found using 
dim(edx) 

#========================================================

#### Q2
# How many zeros were given as ratings in the edx dataset?
sum(edx$rating == 0)
#> [1] 0

min(edx$rating)min(edx$rating)
#> 0.5

# How many threes were given as ratings in the edx dataset?
sum(edx$rating == 3)
#> [1] 2121240

library(dplyr)

edx |> group_by(rating) |>
  summarise(n = n())


#--------------------------------------------------------
##### Explanation
#> No movies have a rating of 0. Movies are rated from 0.5 to 5.0 in 0.5 increments. 
#> The number of 0s can be found using 
edx %>% filter(rating == 0) %>% tally().

# The number of 3s can be found using 
edx %>% filter(rating == 3) %>% tally()
#         n
# 1 2121240
#========================================================

#### Q3

# How many different movies are in the edx dataset?

#library(dplyr)

movies <- edx |> group_by(movieId) |>
  summarise(n = n())

dim(movies)
#> [1] 10677     2
#--------------------------------------------------------
##### Explanation
# The number of unique movies can be found using 
n_distinct(edx$movieId)
#========================================================

#### Q4

# How many different users are in the edx dataset?

n_distinct(edx$userId)
#> [1] 69878
#--------------------------------------------------------
##### Explanation
# The number of unique users can be found using 
n_distinct(edx$userId)

#========================================================

#### Q5

# How many movie ratings are in each of the following genres in the edx dataset?

library(stringr)

genres_ratings <- edx |> group_by(genres) |>
  summarise(ratings = n())

genres_ratings
# # A tibble: 797 × 2
# genres                                             ratings
# <chr>                                                <int>
#   1 (no genres listed)                                       7
# 2 Action                                               24482
# 3 Action|Adventure                                     68688
# 4 Action|Adventure|Animation|Children|Comedy            7467
# 5 Action|Adventure|Animation|Children|Comedy|Fantasy     187
# 6 Action|Adventure|Animation|Children|Comedy|IMAX         66
# 7 Action|Adventure|Animation|Children|Comedy|Sci-Fi      600
# 8 Action|Adventure|Animation|Children|Fantasy            737
# 9 Action|Adventure|Animation|Children|Sci-Fi              50
# 10 Action|Adventure|Animation|Comedy|Drama               1902
# # ℹ 787 more rows
# # ℹ Use `print(n = ...)` to see more rows

dim(genres_ratings)
#> [1] 797   2

sum(str_detect(genres_ratings[10,]$genres, "Drama"))
sum(str_detect(genres_ratings[9,]$genres, "Drama"))
sum(str_detect(genres_ratings$genres, "Drama"))

ind <- str_detect(genres_ratings$genres, "Drama")
dim(ind)

test <- c("Drama_Drama_Drama", "Drama_Drama", "Drama", "Comedy", "Thriller")
str_detect(test, "Drama")

# Drama:
drama_ratings <- genres_ratings[str_detect(genres_ratings$genres, "Drama"),]
drama_ratings
sum(drama_ratings$ratings)
#> [1] 3910127

# Comedy:
comedy_ratings <- genres_ratings[str_detect(genres_ratings$genres, "Comedy"),]
sum(comedy_ratings$ratings)
#> [1] 3540930

# Thriller:
thriller_ratings <- genres_ratings[str_detect(genres_ratings$genres, "Thriller"),]
sum(thriller_ratings$ratings)
#> [1] 2325899

# Romance:
romance_ratings <- genres_ratings[str_detect(genres_ratings$genres, "Romance"),]
sum(romance_ratings$ratings)
#> [1] 1712100


#--------------------------------------------------------
#### Hint (1 of 1): 
#> One option is to use the `str_detect` function from the `stringr` package. 
#> The `separate_rows` function from the `tidyr` package can also be used, 
#> although it will be much slower.

##### Explanation
# The following code can be used to do this analysis:

# str_detect
genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})


# separate_rows, much slower!
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#========================================================

#### Q6

# Which movie has the greatest number of ratings?

str(edx)

movie_ratings <- edx |> group_by(title) |>
  summarise(ratings = n())

dim(movie_ratings)
#> [1] 10677     2

str(movie_ratings)
head(movie_ratings)

movie_ratings[which.max(movie_ratings$ratings),]

# # A tibble: 1 × 2
#   title               ratings
#   <chr>                 <int>
# 1 Pulp Fiction (1994)   31362
#--------------------------------------------------------
##### Explanation
# The following code will rank the movies in order of number of ratings:

edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# # A tibble: 10,677 × 3
# # Groups:   movieId [10,677]
#    movieId title                                                        count
#      <int> <chr>                                                        <int>
#  1     296 Pulp Fiction (1994)                                          31362
#  2     356 Forrest Gump (1994)                                          31079
#  3     593 Silence of the Lambs, The (1991)                             30382
#  4     480 Jurassic Park (1993)                                         29360
#  5     318 Shawshank Redemption, The (1994)                             28015
#  6     110 Braveheart (1995)                                            26212
#  7     457 Fugitive, The (1993)                                         25998
#  8     589 Terminator 2: Judgment Day (1991)                            25984
#  9     260 Star Wars: Episode IV - A New Hope (a.k.a. Star Wars) (1977) 25672
# 10     150 Apollo 13 (1995)                                             24284
# # ℹ 10,667 more rows
# # ℹ Use `print(n = ...)` to see more rows
#========================================================

#### Q7

# What are the five most given ratings in order from most to least?

ratings <- edx |>  group_by(movieId, rating) |>
  summarise(count = n()) |>
  arrange(movieId, desc(rating))

head(ratings)

ratings <- edx |>  group_by(rating) |>
     summarise(count = n()) |>
     arrange(desc(count))

ratings
# # A tibble: 10 × 2
# rating   count
# <dbl>   <int>
#   1  4   2588430
# 2    3   2121240
# 3    5   1390114
# 4    3.5  791624
# 5    2    711422
# 6    4.5  526736
# 7    1    345679
# 8    2.5  333010
# 9    1.5  106426
# 10   0.5  85374
#----------------------
## Test
edx |>  group_by(rating) |>
        summarise(count = n())

edx |>  group_by(rating) |>
        summarise(count = n()) |> top_n(5)
#----------
edx |>  group_by(rating) |>
        summarise(count = n()) |> top_n(5) |>
  arrange(desc(count))
#--------------------------------------------------------
##### Explanation
# The top five ratings can be found using the following code:

edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))
# # A tibble: 5 × 2
#   rating   count
#    <dbl>   <int>
# 1    4   2588430
# 2    3   2121240
# 3    5   1390114
# 4    3.5  791624
# 5    2    711422

#---------------------------------
## Test
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(count)
# # A tibble: 5 × 2
#   rating   count
#    <dbl>   <int>
# 1    2    711422
# 2    3.5  791624
# 3    5   1390114
# 4    3   2121240
# 5    4   2588430

#========================================================

#### Q8

#> True or False: In general, half star ratings are less common than whole star ratings 
#> (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.).

ratings <- edx |>  group_by(rating) |>
  summarise(count = n()) |>
  arrange(desc(count))

ratings

# half_star_ratings = c(0.5, 1.5, 2.5, 3.5, 4.5)
# mean(edx$rating == half_star_ratings)
# 
# mean(edx$rating == 4.5)

half_star_ratings <- ratings |> filter(floor(rating) < rating)
half_star_ratings
nrow(edx)

sum(half_star_ratings$count)/nrow(edx)
#> [1] 0.2047954

#--------------------------------------------------------
##### Explanation
# Numerically, this can be determined using this code: 
edx %>% group_by(rating) %>% summarize(count = n())

# Visually, this can be seen using the following code:
library(caret)  

edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()

# The answer is: TRUE.
#========================================================

#### Q

#--------------------------------------------------------
##### Explanation
# 

#========================================================

#### Q

#--------------------------------------------------------
##### Explanation
# 

#========================================================



