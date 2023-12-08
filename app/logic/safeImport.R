# app/logic/safeImport

box::use(
  rio[import],
  ssh[ssh_exec_wait],
  glue[glue],
)

box::use(
  app/logic/myNotification[myNotification],
)

#' @export
safeImport = function(file, sshSession, ...) {
  if (file.access(file, mode = 4) == 0) {
    object = tryCatch(
      import(file, setclass = 'tibble', ...),
      error =  function(e)
        return(NULL)
    )
  } else {
    if (Sys.info()["sysname"] == "Linux") {
      tryCatch(
        ssh_exec_wait(
          sshSession,
          command = glue('sudo -S {Sys.getenv("pwd")} chmod +777 {file}')
        ),
        error = function(e) {
          msg = glue("unable to read {file}: {e}")
          warning(msg)
          myNotification(msg, type = 'warning')
          return(NULL)
        }
      )
    }
    object = tryCatch(
      import(file, setclass = 'tibble' , ...),
      error =  function(e) {
        msg = glue("unable to read {file}: {e}")
        warning(msg)
        myNotification(msg, type = 'warning')
        return(NULL)
      }
    )
  }
  return(object)
}
