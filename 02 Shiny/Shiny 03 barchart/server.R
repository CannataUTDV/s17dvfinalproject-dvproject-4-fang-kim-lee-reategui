# server.R
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

online0 = TRUE

# The following query is for the select list in the Boxplots -> Simple Boxplot tab, and Barcharts -> Barchart with Table Calculation tab.
# if(online0) {
areas = query(
  data.world(propsfile = "www/.data.world"),
  dataset="jlee/s-17-dv-final-project", type="sql",
  query="select distinct AreaName as D, AreaName as R
  from `us_education_census.csv/us_education_census`
  order by 1"
) # %>% View()
# } else {
#   print("Getting Regions from csv")
#   file_path = "www/globalshipments.csv"
#   df <- readr::read_csv(file_path)
#   tdf1 = df %>% dplyr::distinct(Region) %>% arrange(Region) %>% dplyr::rename(D = Region)
#   tdf2 = df %>% dplyr::distinct(Region) %>% arrange(Region) %>% dplyr::rename(R = Region)
#   regions = bind_cols(tdf1, tdf2)
# }

region_list <- as.list(areas$D, areas$R)

region_list <- append(list("All" = "All"), region_list)

region_list5 <- region_list


shinyServer(function(input, output) { 
  # These widgets are for the Barcharts tab.
  online2 = reactive({input$rb2})
  output$areas2 <- renderUI({selectInput("selectedareas", "Choose the Area:", region_list, multiple = TRUE, selected='All') })
  
  # Begin Barchart Tab ------------------------------------------------------------------
  dfbc1 <- eventReactive(input$click2, {
    if(input$selectedareas == 'All') region_list <- input$selectedareas
    else region_list <- append(list("Skip" = "Skip"), input$selectedareas)
    # if(online2() == "SQL") {
    print("Getting from data.world")
    tdf = query(
      data.world(propsfile = "www/.data.world"),
      dataset="jlee/s-17-dv-final-project", type="sql",
      query="select `CONTROL`, AreaName, sum(`INEXPFTE`) as sum_cost 
      from `CollegeScorecard.csv/CollegeScorecard` g join `us_education_census.csv/us_education_census` c 
      on g.`STABBR` = c.`State`
      where (? = 'All' or AreaName in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?))
      group by `CONTROL`, AreaName",
      queryParameters = region_list
    ) # %>% View()
    # }
    # else {
    #   print("Getting from csv")
    #   file_path = "www/globalshipments.csv"
    #   df <- readr::read_csv(file_path)
    #   tdf = df %>% dplyr::filter(Country == "United States") %>% dplyr::filter(Region %in% input$selectedRegions | input$selectedRegions == "All") %>%
    #     dplyr::group_by(Region, `Sub-Category`) %>% 
    #     dplyr::summarize(sum_shippingcost = sum(`Shipping Cost`)) # %>% View()
    # }
    # The following two lines mimic what can be done with Analytic SQL. Analytic SQL does not currently work in data.world.
    tdf2 = tdf %>% group_by(AreaName) %>% summarize(window_avg_cost = mean(sum_cost))
    dplyr::left_join(tdf, tdf2, by = "AreaName")
    # Analytic SQL would look something like this:
    # select Category, Region, sum_sales, avg(sum_sales) 
    # OVER (PARTITION BY Category ) as window_avg_sales
    # from (select Category, Region, sum(Sales) sum_sales
    #       from SuperStoreOrders
    #      group by Category, Region)
  })
  output$barchartData1 <- renderDataTable({DT::datatable(dfbc1(),
                                                         rownames = FALSE,
                                                         extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  output$barchartPlot1 <- renderPlot({ggplot(dfbc1(), aes(x=`CONTROL`, y=sum_cost)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=10, hjust=0.5)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~AreaName, ncol=1) + 
      coord_flip() + 
      # Add sum_sales, and (sum_sales - window_avg_sales) label.
      geom_text(mapping=aes(x=`CONTROL`, y=sum_cost, label=round(sum_cost)),colour="black", hjust=-.5) +
      geom_text(mapping=aes(x=`CONTROL`, y=sum_cost, label=round(sum_cost - window_avg_cost)),colour="blue", hjust=-2) +
      # Add reference line with a label.
      geom_hline(aes(yintercept = round(window_avg_cost)), color="red") +
      geom_text(aes( -1, window_avg_cost, label = window_avg_cost, vjust = -.5, hjust = -.25), color="red")
  })
  
})
