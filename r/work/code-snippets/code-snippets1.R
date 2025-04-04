


file_name_tmp <- "12.rg.UMGY-effect.RData"
file_path_tmp <- file.path(models_data_path, file_name_tmp)

if (file.exists(file_path_tmp)) {
  put_log1("Loading Regularized User+Movie+Genre Effect Model data from file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  load(file_path_tmp)
  put_end_date(start)
  put_log1("Regularized User+Movie+Genre Effect Model data has been loaded from file: %1", 
           file_path_tmp)
} else {

  
  put_log1("Saving User+Movie+Genre+Year Effect Model data to file: %1...", 
           file_path_tmp)
  start <- put_start_date()
  save(mu,
       user_effect,
       rg.UM_effect,
       rg.UMG_effect,
       cv.UMGDG_effect,
       rg.UMGY_effect,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Genre Effect Model data has been saved to file: %1", 
           file_path_tmp)
} 



file_name_tmp <- "12.UMGY.SmthDay_effect.RData"
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
  save(mu,
       user_effect,
       rg.UM_effect,
       rg.UMG_effect,
       cv.UMGDG_effect,
       rg.UMGY_effect,
       cv.UMGDG_Day_effect,
       cv.UMGY.SmthDay_effect,
       file = file_path_tmp)
  put_end_date(start)
  put_log1("User+Movie+Year+Day Smoothed Effect data has been saved to file: %1", 
           file_path_tmp)
} 


put_log("Function:
")

put_log("User+Movie+Genre Effect Model:
.")

put_log1("User+Movie+Genre Effect Model:
 for %1-Fold Cross Validation samples.", CVFolds_N)

put_log("User+Movie Effect Model:
.")

put_log("User Effect Model:
")

put_log("Overall Mean Rating Model:
")

put_log1("Naive RMSE:
 plotted for %1-Fold Cross Validation samples.", CVFolds_N)

put_log1("Naive RMSE:
: %1", )

put(str_glue("{}"))



put(c("",
      "",
      "."))



