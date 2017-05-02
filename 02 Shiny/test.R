require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(leaflet)
require(plotly)
require(lubridate)

df <- query(
  data.world(propsfile = "www/.data.world"),
  dataset="jlee/s-17-dv-final-project", type="sql",
  query="
  SELECT distinct INSTNM, STABBR, avg(COSTT4_A) as avgCost
  from CollegeScorecard
  group by STABBR"
)


"Select STABBR,
        sum(INEXPFTE) / sum(COSTT4_A) as ratio, avg(COSTT4_A) as avgCost,
        from CollegeScorecard
        group by STABBR"

print (df)

p <- plot_geo(df, locationmode = 'USA-states') %>%
  add_trace(
    z = ~avgCost, locations = ~STABBR,
    color = ~avgCost, colors = 'Blues'
  ) %>%
  colorbar(title = "Average Tuition Costttt") %>%
  layout(
    title = 'Region Cost of Attendance Map',
    geo = g
  )




print (p)
