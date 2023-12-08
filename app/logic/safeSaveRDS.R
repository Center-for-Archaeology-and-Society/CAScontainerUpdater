# app/logic/safeSaveRDS.R

box::use(
  ssh[ssh_exec_wait],
  glue[glue],
)

box::use(
  app/logic/myNotification,
)

#' @export
safeSaveRDS = function(object, file,sshSession) {
  if (file.access(file, mode = 2) == 0) {
    try(saveRDS(object, file))
  } else {
    if (Sys.info()["sysname"] == "Linux") {
      tryCatch(
        ssh_exec_wait(
          sshSession,
          command = glue('sudo -S {Sys.getenv("pwd")} chmod +777 {file}')
        ),
        error = function(e) {
          msg = glue("unable to save {file}: {e}")
          warning(msg)
          myNotification(msg, type = 'warning')
        }
      )
    }
    try(saveRDS(object, file))
  }
}
