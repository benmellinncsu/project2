library(shiny)
library(shinyalert)
library(tidyverse)
library(lubridate)

ui<-fluidPage(
  titlePanel("MN Vikings Plays Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("cat1", "Choose Play Type",
                  choices = c("run", "pass", "field_goal", "punt", "no_play", "qb_kneel", "qb_spike"),
                  selected = c("run", "pass", "field_goal", "punt", "no_play", "qb_kneel", "qb_spike"),
                  multiple = TRUE),
      
      selectInput("cat2", "Vikings on Offense, Defense, or Either?",
                  choices = c("offense", "defense"),
                  selected = c("offense","defense"),
                  multiple = TRUE),
      
      selectInput("cat3", "Choose Down",
                  choices = c("First"=1, "Second"=2, "Third"=3, "Fourth"=4),
                  selected = c(1, 2, 3, 4),
                  multiple = TRUE),
      
      
      sliderInput("yardline_range", "Select Yardline (from opponent end zone):",
                  min = 0,
                  max = 100,
                  value = c(0, 100)  
      ),
      
      # Slider for year
      sliderInput("year_range", "Select Year:",
                  min = min(2009),
                  max = max(2018),
                  value = c(2009,2018),  
                  step = 1,
                  sep = ""
      ),
      actionButton("subset_button", "Subset Data")
    
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("About", 
                 h3("About"),
                 plotOutput("plot1")
        ),
        tabPanel("Data Download", 
                 h3("Data Download"),
                 downloadLink('downloadData', 'Download'),
                 DT::dataTableOutput("subset_table")
        ),
        tabPanel("Data Exploration", 
                 h3("Data Exploration"),
                 verbatimTextOutput("summary1")
        )
      )

    )
  )
)

server <- function(input, output, session) {
  
  
  vikes_data <- read.csv("final_vikings_data.csv")
  
  
  rv <- reactiveValues(data = vikes_data)
  
  
  observeEvent(input$subset_button, {
    rv$data<-vikes_data|>
      filter(
        play_type %in% input$cat1,
       (( "offense" %in% input$cat2) & posteam =="MIN")|
       (("defense" %in% input$cat2) & posteam !="MIN"),
       down %in% input$cat3,
       yardline_100 >= input$yardline_range[1],
       yardline_100 <= input$yardline_range[2],
       year(game_date) >= input$year_range[1],
       year(game_date) <= input$year_range[2]
  
      )
    
  })
  
  output$subset_table <- DT::renderDataTable({
    rv$data
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(rv$data, con)
    }
  )
  
  
}

shinyApp(ui, server)