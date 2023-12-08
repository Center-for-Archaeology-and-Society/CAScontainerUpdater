# app/logic/myNotification

box::use(
  shiny[showNotification],
)

#' @export

myNotification = function(msg, type = 'default') {
  if (type %in% c("warning", "error")) {
    warning(msg)
  } else {
    print(msg)
  }
  try(showNotification(msg, type = type), silent = T)
}
