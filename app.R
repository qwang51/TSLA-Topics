library(shiny)
library(shinythemes)
library(plotly)

dist <- readRDS('data_long.rds')
topics <- readRDS('topics.rds')

server <- function(input, output, session) {

  selectedData <- reactiveValues(data = dist)
  
  observeEvent(input$all, {
    selectedData$data <- dist
    updateSelectInput(session, 'topics', selected = unique(dist$variable))
  })
  
  observeEvent(input$reset, {
    selectedData$data <- NULL
    updateSelectInput(session, 'topics', selected = '')
  })
  
  observeEvent(input$topics, {
    selectedData$data <- dist[dist$variable %in% input$topics,]
  })

  output$myplot <- renderPlotly({
    
    if(is.null(selectedData$data)) return()
    
    plot_ly(selectedData$data, x=datetime, y=value, group=variable) %>%
      layout(xaxis = list(
               title = '',
               rangeselector = list(
                 buttons = list(
                   list(
                     count = 1,
                     label = "1 mo",
                     step = "month",
                     stepmode = "backward"),
                   list(
                     count = 3,
                     label = "3 mo",
                     step = "month",
                     stepmode = "backward"),
                   list(
                     count = 6,
                     label = "6 mo",
                     step = "month",
                     stepmode = "backward"),
                   list(
                     count = 1,
                     label = "1 yr",
                     step = "year",
                     stepmode = "backward"),
                   list(
                     count = 1,
                     label = "YTD",
                     step = "year",
                     stepmode = "todate"),
                   list(step = "all"))),
               
               rangeslider = list(type = "date", 
                                  autorange=TRUE,
                                  bordercolor = '#CDCBD2',
                                  bgcolor='#CFDAEF'))
             , yaxis = list(title = '')
             )
  })
  
  
  output$table <- DT::renderDataTable(
    
    topics[topics$topic %in% input$topics, ],
    options = list(autowidth=TRUE, scrollX=TRUE,
                   lengthChange = FALSE,
                   pageLength = 5)
    )
}


ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("TSLA Topics Learned by LDA"),
  br(), br(), 
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4('Topic Selection'),
      hr(),
      actionButton('all', 'Show All'),
      br(),br(),
      actionButton('reset', 'Reset'),
      hr(),
      h5('Select Multiple Topics:'),
      p('(click or type in)'),
      selectInput('topics',
                  label = NULL,
                  choices = unique(dist$variable),
                  multiple = TRUE,
                  selected = unique(dist$variable),
                  selectize = TRUE
                  ),
      hr(),hr(),hr(),hr()
      ),
    
    mainPanel(
      width = 9,
      plotlyOutput("myplot"),
      DT::dataTableOutput('table')
    )
  )
)

shinyApp(ui = ui, server = server)