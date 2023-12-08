# app/logic/storage2idno

box::use(
  dplyr[mutate, case_when, select,rowwise,mutate_at,vars,ungroup],
  tidyr[unite],
  tidyselect[any_of],
  stringr[str_replace_all,str_sub],
  magrittr[`%>%`],
)

#' @export
storage2idno = function(df) {
  df %>%
    mutate(
      buildingid = case_when(!is.na(building) ~ paste0("b", building)),
      roomid = case_when(!is.na(room) ~ paste0("rm", room)),
      rowid = case_when(!is.na(row) ~ paste0("rw", row)),
      unitid = case_when(!is.na(unit) ~ paste0("u", unit)),
      shelfid = case_when(!is.na(shelf) ~ paste0("s", shelf)),
      shelfid = str_replace_all(shelfid, "floor", "fl"),
      unitid = str_replace_all(unitid, "in front of", "f"),
      unitid = str_replace_all(unitid, "across from", "a"),
      unitid = str_replace_all(unitid, "Top of filing cabinets", "tofc"),
      rowid = str_replace_all(rowid, "floor", "fl")
    ) %>%
    select(-any_of('idno')) %>%
    unite(
      'idno',
      c(buildingid, roomid, rowid, unitid, shelfid),
      sep = "_",
      na.rm = T,
      remove = T
    ) %>%
    mutate(idno = str_replace_all(idno, " ", "_")) %>%
    rowwise() %>%
    mutate_at(vars(idno), list(function(c) {
      if (nchar(c) > 30)
        str_sub(c, nchar(c) - 29, nchar(c))
      else
        c
    })) %>%
    ungroup()
}
