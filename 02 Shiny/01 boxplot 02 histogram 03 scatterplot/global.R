#require(data.world)
require(readr)
require(dplyr)

online0 = FALSE

if(online0) {
  globals = query(
    data.world(propsfile = "www/.data.world"),
    dataset="cannata/superstoreorders", type="sql",
    query="select Order_Date, CONTROL
    from SuperStoreOrders
    order by 1"
  ) 
} else {
  file_path = "www/CollegeScorecard.csv"
  df <- readr::read_csv(file_path) 
  globals <- df %>% dplyr::select(CCBASIC, CONTROL) %>% dplyr::distinct()
}
#globals$CCBASIC <- lubridate::year(globals$CCBASIC)

