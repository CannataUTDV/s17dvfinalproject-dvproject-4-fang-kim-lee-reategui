require(readr)
require(dplyr)

online0 = FALSE

# The following query is for the select list in the Boxplots -> Simple Boxplot tab, and Barcharts -> Barchart with Table Calculation tab.
if(online0) {
  df = query(
    data.world(propsfile = "www/.data.world"),
    dataset="jlee/s-17-dv-final-project", type="sql",
    query="select * from CollegeScorecard"
  ) # %>% View()
} else {
  print("Getting Data from csv")
  file_path = "www/CollegeScorecard.csv"
  df <- readr::read_csv(file_path)
}
