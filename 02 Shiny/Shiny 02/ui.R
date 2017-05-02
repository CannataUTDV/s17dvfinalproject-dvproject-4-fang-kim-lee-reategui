#ui.R
require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Crosstabs 1", tabName = "crosstab1", icon = icon("dashboard")),
      menuItem("Crosstabs 2", tabName = "crosstab2", icon = icon("dashboard"))
    )
  ),
  dashboardBody(    
    tabItems(
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
                tabPanel("Instructional Expenditures to Total Cost Ratio", plotOutput("plot1", height=1000))
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
                tabPanel("Tuition Revenue to Total Cost Ratio", plotOutput("plot2", height=1000))
              )
      )
      # End Crosstab tab 2 content.
    )
  )
)
