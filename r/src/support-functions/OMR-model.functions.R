# Overall Mean Rating Model
naive_model_MSEs <- function(x) {
  sapply(edx_CV, function(cv_item){
    mse(cv_item$validation_set$rating - x)
  })
}
naive_model_RMSE <- function(x){
  sqrt(mean(naive_model_MSEs(x)))
}
