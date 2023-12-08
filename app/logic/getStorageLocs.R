# app/logic/getStorageLocs

box::use(
  DBI[dbGetQuery],
  magrittr[`%>%`],
)

box::use(
  app/logic/idno2storage[idno2storage],
)

#' @export
getStorageLocs = function(con){
  dbGetQuery(
    con,
    "select ca_storage_locations.location_id, ca_storage_locations.idno, ca_list_items.idno as type, value_longtext1 as barcode from ca_storage_locations join ca_attributes on ca_attributes.row_id = ca_storage_locations.location_id join ca_attribute_values on ca_attribute_values.attribute_id = ca_attributes.attribute_id join ca_list_items on ca_list_items.item_id = ca_storage_locations.type_id join ca_metadata_elements on ca_metadata_elements.element_id = ca_attributes.element_id where element_code = 'barcode' and ca_storage_locations.deleted = 0"
  ) %>% idno2storage()
}
