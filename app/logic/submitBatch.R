# app/logic/submitBatch

box::use(
  ssh[ssh_exec_wait, scp_upload, scp_download],
  dplyr[select],
  rio[export],
  tibble[tibble],
  magrittr[`%>%`],
)

box::use(
  app/logic/myNotification[myNotification],
)

#' @export
submitBatch = function(rvals, sshSession,dir){
  myNotification("submitting batch")
  tmpdir = tempdir()
  export(
    rvals$df %>% select(boxno, idno, type, relation, purpose, person, date),
    file.path(tmpdir, "importBoxMoves.xlsx")
  )
  scp_upload(
    session = sshSession,
    files = file.path(tmpdir, "importBoxMoves.xlsx"),
    to = file.path(dir, "importBoxMoves.xlsx")
  )
  rvals$df = tibble()
  ssh_exec_wait(sshSession, command = paste0(dir, '/boxtransferauto.sh'))
  ssh_exec_wait(
    sshSession,
    command = paste0(
      'mv ',
      dir,
      '/importBoxMoves.xlsx',
      ' ',
      dir,
      '/archives/importBoxMoves-',
      strftime(Sys.time(), format = "%Y-%m-%d-%H-%M-%OS3"),
      '.xlsx'
    )
  )
  file.remove(file.path(tmpdir, "importBoxMoves.xlsx"))
  logfn = paste0("log_", as.Date(Sys.Date()), ".txt")
  logdir = tempdir()
  scp_download(session = sshSession,
               files = file.path(dir, logfn),
               to = logdir)
  log = readLines(file.path(logdir, logfn)) %>%
    paste(collapse = "<br>")
  myNotification("completed")
  return(log)
}
