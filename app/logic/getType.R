# /app/logic/getType

box::use(
  dplyr[mutate, select,case_when],
  tidyselect[any_of],
  magrittr[`%<>%`,`%>%`]
)

box::use(
  app/logic/idno2storage[idno2storage],
)


#' @export
getType = function(df){
  if (!"idno" %in% names(df))
    stop("No idno in storage locations")
  nms = c(names(df),"type")
  df %<>%
    idno2storage() %>%
    mutate(type = case_when(!is.na(shelf) & shelf != "" ~ "shelf",
                            !is.na(unit) & unit != "" ~ "unit",
                            !is.na(row) & row != "" ~ "row",
                            !is.na(room) & room != "" ~ "room",
                            !is.na(building) & building != "" ~ "building",
                            TRUE ~ NA_character_)) %>%
    select(any_of(nms))
  return(df)
}
