# app/logic/changePassword

box::use(
  dplyr[filter,tbl,collect,mutate],
  sodium[password_store],
  DBI[dbExecute]
)

box::use(
  app/logic/myNotification[myNotification],
)

#' @export
changePassword = function(con2, userid, pwd) {
  userTable = tbl(con2,"user_data") %>%
    filter(user == userid) %>%
    collect() %>%
    mutate(password = password_store(pwd))
  if(nrow(userTable) == 1){
    dbExecute(conn = con2,statement = glue::glue_sql("UPDATE user_data
SET password = {userTable$password}
WHERE user = {userTable$user};", .con = con2))
  }
  userTableNew = tbl(con2,"user_data") %>%
    filter(user == userid) %>%
    collect()
  if(userTable$password == userTableNew$password){
    msg = "Password changed successfully"
    myNotification(msg)
  } else {
    msg = "Password change failed"
    myNotification(msg, type = "error")
  }
  return(msg)
}
