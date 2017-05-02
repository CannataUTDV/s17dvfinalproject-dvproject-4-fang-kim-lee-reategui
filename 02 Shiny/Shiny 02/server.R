# server.R
require(plotly)
library (plotly)
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(leaflet)

require(lubridate)



shinyServer(function(input, output) { 
  # These widgets are for the Crosstabs tab.
  online1 = reactive({input$rb1})
  KPI_Low = reactive({input$KPI1})     
  KPI_Medium = reactive({input$KPI2})
  
  # These widgets are for the Box Plots tab.
  online5 = reactive({input$rb5})
  
  # These widgets are for the Histogram tab.
  online4 = reactive({input$rb4})
  
  # These widgets are for the Scatter Plots tab.
  online3 = reactive({input$rb3})
  
  
  
  # Begin Box Plot Tab ------------------------------------------------------------------
  dfbp1 <- eventReactive(input$click5, {
    if(online5() == "SQL") {
      print("Getting from data.world")
      df <- query(
        data.world(propsfile = "www/.data.world"),
        dataset="jlee/s-17-dv-final-project", type="sql",
        query="select UNITID, CONTROL, STABBR, CCBASIC, COSTT4_A
        from CollegeScorecard"
      ) # %>% View()
    }
    else {
      print("Getting from csv")
      file_path = "www/CollegeScorecard.csv"
      df <- readr::read_csv(file_path)
    }
    })
  # Datatable drawn
  output$boxplotData1 <- renderDataTable({DT::datatable(dfbp1(), rownames = FALSE,
                                                        extensions = list(Responsive = TRUE, 
                                                                          FixedHeader = TRUE)
  )
  })
  # Separate dataframe for playing with visualizations
  dfbp2 <- eventReactive(input$SelectCost4a, {
    if(online5() == "SQL") {
      print("Getting from data.world")
      df <- query(
        data.world(propsfile = "www/.data.world"),
        dataset="jlee/s-17-dv-final-project", type="sql",
        query="select UNITID, CONTROL, STABBR, CCBASIC, COSTT4_A
        from CollegeScorecard
        where (COSTT4_A < ?)"
        ,queryParameters = input$SelectCost4a
      ) # %>% View()
    }
    else {
      print("Getting from csv")
      file_path = "www/CollegeScorecard.csv"
      df <- readr::read_csv(file_path)
      tdf <- df %>% dplyr::filter(COSTT4_A < input$SelectCost4a)
    }
  })
  # Redraws the plot everytime the dropdown value changes. Doesn't touch the data tab table
  output$boxplotPlot1 <- renderPlotly({
    #View(dfbp3())
    p <- ggplot(dfbp2()) + 
      geom_boxplot(aes(x=CCBASIC, y=COSTT4_A, colour=factor(CONTROL))) + 
      theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5)) +
      labs(title="Average Cost of Attendance for Type of School Boxplot") + 
      labs(color="Control")
    ggplotly(p)
  })
  # End Box Plot Tab ___________________________________________________________
  
  # Begin Histogram Tab ------------------------------------------------------------------
  dfh1 <- eventReactive(input$click4, {
    if(online4() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="jlee/s-17-dv-final-project", type="sql",
        query="select UNITID, SAT_AVG, STABBR
        from CollegeScorecard"
      ) # %>% View()
    }
    else {
      print("Getting from csv")
      file_path = "www/CollegeScorecard.csv"
      df <- readr::read_csv(file_path)
      tdf <- df %>% dplyr::filter(STABBR == input$select)
      # df %>% dplyr::select(Shipping_Cost, Container) %>% dplyr::filter(Container == 'Small Box') # %>% View()
    }
    })
  
  output$histogramData1 <- renderDataTable({DT::datatable(dfh1(), rownames = FALSE,
                                                          extensions = list(Responsive = TRUE, 
                                                                            FixedHeader = TRUE)
  )
  })
  
  dfh2 <- eventReactive(input$select, {
    if(online4() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="jlee/s-17-dv-final-project", type="sql",
        query="select UNITID, SAT_AVG, STABBR, INSTNM
        from CollegeScorecard
        where (STABBR = ?)"
        ,queryParameters = input$select
      ) # %>% View()
    }
    else {
      print("Getting from csv")
      file_path = "www/CollegeScorecard.csv"
      df <- readr::read_csv(file_path)
      tdf <- df %>% dplyr::filter(STABBR == input$select)
      # df %>% dplyr::select(Shipping_Cost, Container) %>% dplyr::filter(Container == 'Small Box') # %>% View()
    }
  })
  
  output$histogramPlot1 <- renderPlotly({p <- ggplot(dfh2()) +
    geom_histogram(aes(x=SAT_AVG, color=INSTNM)) +
    theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5)) + 
    labs(title="AVERAGE SAT vs. Count Histogram") +
    labs(Color="Institution name/s") 
  ggplotly(p)
  })
  # End Histogram Tab ___________________________________________________________
  
  # Begin Scatter Plots Tab ------------------------------------------------------------------
  dfsc1 <- eventReactive(input$click3, {
    if(online3() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="jlee/s-17-dv-final-project", type="sql",
        query="select UNITID, INEXPFTE, TUITFTE, STABBR
        from CollegeScorecard"
      ) # %>% View()
    }
    else {
      print("Getting from csv")
      file_path = "www/CollegeScorecard.csv"
      df <- readr::read_csv(file_path)
      #df %>% dplyr::select(CONTROL, Profit, State) %>% dplyr::filter(State == 'Texas' | State == 'Florida') # %>% View()
    }
    })
  
  output$scatterData1 <- renderDataTable({DT::datatable(dfsc1(), rownames = FALSE,
                                                        extensions = list(Responsive = TRUE, 
                                                                          FixedHeader = TRUE)
  )
  })
  output$scatterPlot1 <- renderPlotly({p <- ggplot(dfsc1()) + 
    
    geom_point(aes(x=INEXPFTE, y=TUITFTE, colour=factor(STABBR)), size=2) +
    theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5)) + 
    theme(axis.text.y=element_text(size=8, hjust=0.5)) +
    labs(title="Scatter Plot") + 
    labs(color="State")
  ggplotly(p)
  })
  # End Scatter Plots Tab ___________________________________________________________
  
  
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
  #dfmap <-  data.frame(df1$STABBR, df1$ratio)
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
  
  
  output$plot3 <- renderPlotly({plot_geo(df1(), locationmode = 'USA-states') %>%
      add_trace(z= ~ratio, color = ~ratio, colors = 'Blues', locations = ~STABBR) %>%
      colorbar(title = "Ratio") %>%
      layout(title = "Region Cost of Attendance Map")
  })
    
  
  
    
  # Begin Crosstab Tab 2 ------------------------------------------------------------------
  df2 <- eventReactive(input$click2, {
    # if(online1() == "SQL") {
    print("Getting from data.world")
    query(
      data.world(propsfile = "www/.data.world"),
      dataset="jlee/s-17-dv-final-project", type="sql",
      query="Select `CONTROL`, c.AreaName, `UGDS_WHITE`, STABBR,  
      sum(TUITFTE) as sum_rev, 
      sum(COSTT4_A) as sum_cost, 
      sum(TUITFTE) / sum(COSTT4_A) as ratio,
      
      case
      when sum(TUITFTE) / sum(COSTT4_A) < ? then '03 Low'
      when sum(TUITFTE) / sum(COSTT4_A) < ? then '02 Medium'
      else '01 High'
      end AS kpi
      
      from `CollegeScorecard.csv/CollegeScorecard` g join `us_education_census.csv/us_education_census` c 
      on g.`STABBR` = c.`State`
      where `CCBASIC` in (18, 19, 21, 22, 23) 
      group by `CONTROL`, c.AreaName
      order by `CONTROL`, c.AreaName", 
        queryParameters = list(KPI_Low(), KPI_Medium())
      ) # %>% View()
    # }
    # else {
    #   print("Getting from csv")
    #   file_path = "www/CollegeScorecard.csv"
    #   df <- readr::read_csv(file_path)
    #   df %>%
    #     dplyr::group_by(`CONTROL`, `c.AreaName`) %>%
    #     dplyr::summarize(sum_exp = sum(INEXPFTE), sum_cost = sum(COSTT4_A),
    #                      ratio = sum(INEXPFTE) / sum(COSTT4_A),
    #                      kpi = if_else(ratio <= KPI_Low(), '03 Low',
    #                                    if_else(ratio <= KPI_Medium(), '02 Medium', '01 High'))) # %>% View()
    # }
  })
  output$data2 <- renderDataTable({DT::datatable(df2(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot2 <- renderPlot({ggplot(df2()) + 
      theme(axis.text.x=element_text(size=15, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=10, hjust=0.5)) +
      geom_text(aes(x=`CONTROL`, y=AreaName, label=sum_rev), size=5) +
      geom_tile(aes(x=`CONTROL`, y=AreaName, fill=kpi), alpha=0.50)
  })
  # End Crosstab Tab 2 ___________________________________________________________
  
  output$plot4 <- renderPlotly({plot_geo(df2(), locationmode = 'USA-states') %>%
      add_trace(z= ~ratio, color = ~ratio, colors = 'Reds', locations = ~STABBR) %>%
      colorbar(title = "Ratio") %>%
      layout(title = "Tuition Revenue to Total Cost Map")
  })
  
  
  
  })
