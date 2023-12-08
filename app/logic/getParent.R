# app/logic/getParent

box::use(
  stringr[str_detect,str_split_1,str_remove_all],
  magrittr[`%>%`],
)

#' @export
getParent = function(x) {
  seps = c("_s", "_u", "rw", "rm")
  for (s in seps) {
    if (stringr::str_detect(x, s)) {
      split = x %>% str_split_1(pattern = s)
      split = split[-length(split)]
      break
    } else
      split = ""
  }
  split = stringr::str_remove_all(split, "_$")
  return(split)
}
