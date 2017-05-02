# server.R
require(quantreg)
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
#   STABBRs = query(
#     data.world(propsfile = "www/.data.world"),
#     dataset="jlee/s-17-dv-final-project", type="sql",
#     query="select distinct STABBR as D, STABBR as R
#     from SuperStoreOrders
#     order by 1"
#   ) # %>% View()
# } else {
  # print("Getting STABBRs from csv")
  # file_path = "www/CollegeScorecard.csv"
  # df <- readr::read_csv(file_path) 
# }
  
  #print("Getting STABBRs from csv")
  #file_path = "www/CollegeScorecard.csv"
  #df <- readr::read_csv(file_path) 

############################### Start shinyServer Function ####################
shinyServer(function(input, output) {   
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
  
  output$boxplotData1 <- renderDataTable({DT::datatable(dfbp1(), rownames = FALSE,
                                                        extensions = list(Responsive = TRUE, 
                                                                          FixedHeader = TRUE)
  )
  })
  
  gfbp1 <- eventReactive(input$SelectCost4a, {
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
  
  
  
  
  # You can access the value of the widget with input$select, e.g.
  #output$costvalue <- renderPrint({ input$select })
  


  output$boxplotPlot1 <- renderPlotly({
    #View(dfbp3())
    p <- ggplot(gfbp1()) + 
      geom_boxplot(aes(x=CCBASIC, y=COSTT4_A, colour=factor(CONTROL))) + 
      #ylim(0, input$boxCONTROLRange1[2]) +
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
  
  
  gfh1 <- eventReactive(input$select, {
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
  
  # You can access the value of the widget with input$select, e.g.
  #output$value <- renderPrint({ input$select })
  
  
  output$histogramPlot1 <- renderPlotly({p <- ggplot(gfh1()) +
      geom_histogram(aes(x=SAT_AVG, color=INSTNM)) +
      theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5)) + 
      labs(title="AVERAGE SAT vs. Count Histogram") +
      labs(Color="Institution name/s") 
      
      #geom_line(aes(y = ..count.., color="red")) +
      #geom_line(aes(fill=..count..), stat="bin", binwidth=10))
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
})
