# Logging Helper functions

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

print_log <- function(msg){
  print(str_glue(msg))
}
put_log <- function(msg){
  put(str_glue(msg))
}

get_log1 <- function(msg_template, arg1) {
  str_glue(str_replace_all(msg_template, "%1", as.character(arg1)))
}
print_log1 <- function(msg_template, arg1){
  print(get_log1(msg_template, arg1))
}
put_log1 <- function(msg_template, arg1){
  put(get_log1(msg_template, arg1))
}

get_log2 <- function(msg_template, arg1, arg2) {
  msg_template |> 
    str_replace_all("%1", as.character(arg1)) |>
    str_replace_all("%2", as.character(arg2)) |>
    str_glue()
}
print_log2 <- function(msg_template, arg1, arg2){
  print(get_log2(msg_template, arg1, arg2))
}
put_log2 <- function(msg_template, arg1, arg2){
  put(get_log2(msg_template, arg1, arg2))
}

get_log3 <- function(msg_template, arg1, arg2, arg3) {
  msg_template |> 
    str_replace_all("%1", as.character(arg1)) |>
    str_replace_all("%2", as.character(arg2)) |>
    str_replace_all("%3", as.character(arg2)) |>
    str_glue()
}
print_log3 <- function(msg_template, arg1, arg2, arg3){
  print(get_log3(msg_template, arg1, arg2, arg3))
}
put_log3 <- function(msg_template, arg1, arg2, arg3){
  put(get_log3(msg_template, arg1, arg2, arg3))
}

get_log4 <- function(msg_template, arg1, arg2, arg3, arg4) {
  msg_template |> 
    str_replace_all("%1", as.character(arg1)) |>
    str_replace_all("%2", as.character(arg2)) |>
    str_replace_all("%3", as.character(arg2)) |>
    str_replace_all("%4", as.character(arg2)) |>
    str_glue()
}
print_log4 <- function(msg_template, arg1, arg2, arg3, arg4){
  print(get_log4(msg_template, arg1, arg2, arg3, arg4))
}
put_log4 <- function(msg_template, arg1, arg2, arg3, arg4){
  put(get_log4(msg_template, arg1, arg2, arg3, arg4))
}

