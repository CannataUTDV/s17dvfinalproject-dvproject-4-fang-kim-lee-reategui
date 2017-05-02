#ui.R
require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Box Plots", tabName = "boxplot", icon = icon("dashboard")),
      menuItem("Histograms", tabName = "histogram", icon = icon("dashboard")),
      menuItem("Scatter Plots", tabName = "scatter", icon = icon("dashboard")),
      menuItem("Crosstabs 1", tabName = "crosstab1", icon = icon("dashboard")),
      menuItem("Crosstabs 2", tabName = "crosstab2", icon = icon("dashboard")),
      menuItem("Barcharts, Table Calculations", tabName = "barchart", icon = icon("dashboard"))
    )
  ),
  dashboardBody(    
    tabItems(
      # Begin Box Plots tab content.
      tabItem(tabName = "boxplot",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb5", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         actionButton(inputId = "click5",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("boxplotData1")
                ),
                tabPanel("Simple Box Plot", 
                         sliderInput("SelectCost4a", label = h4("Select Maximum cost of attendance"), 
                                     min = min(df$COSTT4_A, na.rm = TRUE), max = max(df$COSTT4_A, na.rm = TRUE),  
                                     value = max(df$COSTT4_A, na.rm = TRUE)),
                         plotlyOutput("boxplotPlot1", height=500))
              )
      ),
      # End Box Plots tab content.
      # Begin Histogram tab content.
      tabItem(tabName = "histogram",
              tabsetPanel(
                tabPanel("Data",  
                         
                         radioButtons("rb4", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         actionButton(inputId = "click4",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("histogramData1")
                ),
                tabPanel("Simple Histogram",
                         
                         selectInput("select", label = h4("Select State"), 
                                     choices = unique(df$STABBR), 
                                     selected = 1),
                         hr(),
                         fluidRow(column(3, verbatimTextOutput("value"))),
                         
                         
                         plotlyOutput("histogramPlot1", height=1000))
              )
      ),
      # End Histograms tab content.
      # Begin Scatter Plots tab content.
      tabItem(tabName = "scatter",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb3", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         uiOutput("scatterStates"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html,
                         actionButton(inputId = "click3",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("scatterData1")
                ),
                tabPanel("Simple Scatter Plot", plotlyOutput("scatterPlot1", height=1000))
              )
      ),
      # End Scatter Plots tab content.
      # Begin Crosstab 1 tab content.
      tabItem(tabName = "crosstab1",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb1", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         sliderInput("KPI1", "KPI_Low:", 
                                     min = 0, max = .35,  value = .1),
                         sliderInput("KPI2", "KPI_Medium:", 
                                     min = .35, max = .6,  value = .2),
                         actionButton(inputId = "click1",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         'Here is data for the "Instructional Expenditures to Total Cost Ratio" tab',
                         hr(),
                         DT::dataTableOutput("data1")
                ),
                tabPanel("Instructional Expenditures to Total Cost Ratio", plotOutput("plot1", height=1000)),
                tabPanel("Ratio Map", plotlyOutput("plot3"))
                
              )
      ),
      # End Crosstab 1 tab content.
      # Begin Crosstab 2 tab content.
      tabItem(tabName = "crosstab2",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb1", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         sliderInput("KPI1", "KPI_Low:", 
                                     min = 0, max = .2,  value = .1),
                         sliderInput("KPI2", "KPI_Medium:", 
                                     min = .2, max = .4,  value = .2),
                         actionButton(inputId = "click2",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         'Here is data for the "Tuition Revenue to Total Cost Ratio" tab',
                         hr(),
                         DT::dataTableOutput("data2")
                ),
                tabPanel("Tuition Revenue to Total Cost Ratio", plotOutput("plot2", height=1000)),
                tabPanel("Ratio Map", plotlyOutput("plot4"))
              )
      ),
      # End Crosstab tab 2 content.
      # Begin Barchart tab content.
      tabItem(tabName = "barchart",
              tabsetPanel(
                tabPanel("Data",
                         radioButtons("rb2", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         uiOutput("areas2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                         actionButton(inputId = "click10",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         'Here is data for the "Barchart with Table Calculation" tab',
                         hr(),
                         DT::dataTableOutput("barchartData1")
                ),
                tabPanel("Barchart with Table Calculation", "Black = Sum of Instructional Expense per Type of Institution, Red = Average Sum of Instructional Expense per Area, and  Blue = (Sum of Instructional Expense per Type of Institution - Average Sum of Instructional Expense per Area)", plotOutput("barchartPlot1", height=1500))
              )
      )
      # End Barchart tab content.
    )
  )
)
