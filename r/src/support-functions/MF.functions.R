mf.residual.dataframe <- function(train_set){
  train_set |> 
    left_join(edx.user_effect, by = "userId") |>
    left_join(rglr.UM_effect, by = "movieId") |>
    left_join(rglr.UMG_effect, by = "movieId") |>
    left_join(date_days_map, by = "timestamp") |>
    left_join(rglr.UMGY_effect, by='year') |>
    left_join(rglr.UMGYD_effect, by='days') |>
    mutate(rsdl = rating - (mu + a + b + g + ye + de_smoothed)) |>
    select(userId, movieId, rsdl)
}
UMGYDE_model.predict <- function(test_set) {
  test_set |>
    UMGY_SmoothedDay_effect.predict(rglr.UMGYD_effect)
}