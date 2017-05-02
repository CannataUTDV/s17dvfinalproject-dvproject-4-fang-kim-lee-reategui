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



shinyServer(function(input, output) { 
  # These widgets are for the Crosstabs tab.
  online1 = reactive({input$rb1})
  KPI_Low = reactive({input$KPI1})     
  KPI_Medium = reactive({input$KPI2})
  
  # Begin Crosstab Tab 1 ------------------------------------------------------------------
  df1 <- eventReactive(input$click1, {
    # if(online1() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="jlee/s-17-dv-final-project", type="sql",
        query="Select `CONTROL`, c.AreaName,  STABBR,
        sum(INEXPFTE) as sum_exp, 
        sum(COSTT4_A) as sum_cost, 
        sum(INEXPFTE) / sum(COSTT4_A) as ratio, avg(COSTT4_A) as avgCost,
        
        case
        when sum(INEXPFTE) / sum(COSTT4_A) < ? then '03 Low'
        when sum(INEXPFTE) / sum(COSTT4_A) < ? then '02 Medium'
        else '01 High'
        end AS kpi
        
        from `CollegeScorecard.csv/CollegeScorecard` g join `us_education_census.csv/us_education_census` c 
        on g.`STABBR` = c.`State`
        where `CCBASIC` in (18, 19, 21, 22, 23) 
        group by `CONTROL`, c.AreaName
        order by `CONTROL`, c.AreaName",
        queryParameters = list(KPI_Low(), KPI_Medium())
      ) # %>% View()

  })
  output$data1 <- renderDataTable({DT::datatable(df1(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot1 <- renderPlot({ggplot(df1()) + 
      theme(axis.text.x=element_text(size=15, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=10, hjust=0.5)) +
      geom_text(aes(x=`CONTROL`, y=AreaName, label=sum_cost), size=5) +
      geom_tile(aes(x=`CONTROL`, y=AreaName, fill=kpi), alpha=0.50)
  })
  # End Crosstab Tab 1 ___________________________________________________________
  
  #Start Map 1 _____________________________________________________________________________
  
  
  output$plot2 <- renderPlotly({plot_geo(df1(), locationmode = 'USA-states') %>%
      add_trace(z= ~ratio, color = ~ratio, colors = 'Blues', locations = ~STABBR) %>%
      colorbar(title = "Ratio") %>%
      layout(title = "Region Cost of Attendance Map", geo = g)
  })
  
})
  