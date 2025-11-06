# Overall Mean Rating Model
naive_model_MSEs <- function(mu) {
  sapply(edx_CV, function(cv_item){
    mse(cv_item$validation_set$rating - mu)
  })
}
naive_model_RMSE <- function(mu){
  sqrt(mean(naive_model_MSEs(mu)))
}
