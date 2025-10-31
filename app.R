library(shiny)
library(shinyalert)
library(tidyverse)
library(lubridate)

ui<-fluidPage(
  useShinyalert(),
  titlePanel("MN Vikings Plays Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("cat1", "Choose Play Type",
                  choices = c("run", "pass", "field goal" = "field_goal", "punt", "no play" ="no_play","qb kneel"="qb_kneel","qb spike"="qb_spike"),
                  selected = c("run", "pass"),
                  multiple = TRUE),
      
      selectInput("cat2", "Vikings on Offense, Defense, or Either?",
                  choices = c("offense", "defense"),
                  selected = c("offense"),
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
      actionButton("subset_button", "Subset Data, Generate a Plot")
    
      
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
                 
                 tabsetPanel(
                   tabPanel("Play Type Per Down",
                            
                            selectInput(
                              "display_type", 
                              "Display:", 
                              choices = c("Plot Only" = "plot", 
                                          "Table Only" = "table", 
                                          "Both" = "both"), 
                              selected = "both"
                            ),
                            
                            conditionalPanel(
                              condition = "input.display_type == 'plot' || input.display_type == 'both'",
                              plotOutput("play_type_plot", height = "400px")
                            ),
                            
                            conditionalPanel(
                              condition = "input.display_type == 'table' || input.display_type == 'both'",
                              tableOutput("play_type_table")
                            )
                   ),
                   tabPanel("Offensive Metrics by Play Type over the Years",
                            selectInput(
                              "display_type", 
                              "Display:", 
                              choices = c("Plot Only" = "plot", 
                                          "Table Only" = "table", 
                                          "Both" = "both"), 
                              selected = "both"
                            ),
                            
                            selectInput("metric", "Choose Metric:",
                                        choices = c("epa" = "epa",
                                                    "wpa" = "wpa",
                                                    "yards" = "yards_gained"),
                                        selected = "epa"),
                            
                            conditionalPanel(
                              condition = "input.display_type == 'plot' || input.display_type == 'both'",
                              plotOutput("metric_by_year_plot")
                            ),
                            
                            conditionalPanel(
                              condition = "input.display_type == 'table' || input.display_type == 'both'",
                              DT::dataTableOutput("metric_by_year_table")
                            )
                   ),
                   
                   tabPanel("Offensive Metric, Field Position, and Play Type",
                            selectInput(
                              "display_type", 
                              "Display:", 
                              choices = c("Plot Only" = "plot", 
                                          "Table Only" = "table", 
                                          "Both" = "both"), 
                              selected = "both"
                            ),
                            
                            selectInput("pos_metric", "Choose Metric:",
                                        choices = c("epa" = "epa",
                                                    "wpa" = "wpa",
                                                    "yards" = "yards_gained"),
                                        selected = "epa"),
                            
                            conditionalPanel(
                              condition = "input.display_type == 'plot' || input.display_type == 'both'",
                              plotOutput("metric_by_position_plot")
                            ),
                            
                            conditionalPanel(
                              condition = "input.display_type == 'table' || input.display_type == 'both'",
                              DT::dataTableOutput("metric_by_position_table")
                            )
                            
                   ),
                 )
        )
        
        
        
      )

    )
  )
)

server <- function(input, output, session) {
  
  
  vikes_data <- read.csv("final_vikings_data.csv")
  
  
  rv <- reactiveValues(data = vikes_data)
  

  
  
  ### DATASET FILTERING AND SUBSETTING
  
  observeEvent(input$subset_button, {
    ## ERROR CHECKING
    if (is.null(input$cat1) || length(input$cat1) == 0) {
      shinyalert(
        title = "Error",
        text = "You must select at least one Play Type!",
        type = "error"
      )
      return()
    }
    
    if (is.null(input$cat2) || length(input$cat2) == 0) {
      shinyalert(
        title = "Error",
        text = "You must select at least Offense or Defense!",
        type = "error"
      )
      return()
    }
    
    if (is.null(input$cat3) || length(input$cat3) == 0) {
      shinyalert(
        title = "Error",
        text = "You must select at least One Down",
        type = "error"
      )
      return()
    }
    
    ## SUBSETTING AND FILTERING
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
  ##### PLAY TYPE BY DOWN LOGIC####
  
  output$play_type_table <- renderTable({
    vikes_data <- rv$data
    table(vikes_data$down, vikes_data$play_type)
  })

  output$play_type_plot <- renderPlot({
    plot_data <- as.data.frame(table(rv$data$down, rv$data$play_type))
    names(plot_data) <- c("Down", "Play_Type", "Count")
    
    ggplot(plot_data, aes(x = Down, y = Count, fill = Play_Type)) +
      geom_col(position = "dodge") +
      labs(
        title = "Play Type Counts per Down",
        x = "Down",
        y = "Number of Plays",
        fill = "Play Type"
      ) +
      theme_minimal()
  })
  
  ##### OFFENSIVE METRIC BY PLAY TYPE LOGIC####
  metric_summary_long <- eventReactive(input$subset_button, {
    
    metric_col <- sym(input$metric)
    
    summary_wide <- rv$data |>
      mutate(year = year(game_date)) |>
      group_by(year,play_type) |>
      summarize(
        average_metric = mean(!!metric_col, na.rm = TRUE),
        .groups = "drop"
      )
    

    summary_wide
  })
        

  output$metric_by_year_plot<-renderPlot({
    summary_data <- metric_summary_long()
    req(summary_data)
    
    ggplot(summary_data, aes(x = factor(year), y = average_metric, fill = play_type)) +
      geom_col(position = "dodge") +
      labs(
        title = paste("Average", input$metric, "per Play by Year"),
        x = "Year",
        y = paste("Average", input$metric),
        fill = "Play Type"
      ) +
      scale_fill_manual(
        values = c("run" = "purple", "pass" = "gold", "punt" = "green", "field_goal" = "blue", "no_play" = "gray", "qb_kneel" = "pink", "qb_spike" = "red")
      ) +
      theme_minimal()
  })
  
  output$metric_by_year_table <- DT::renderDataTable({
    metric_summary_long()
  })
  
  
  ##### OFFENSIVE METRIC BY FIELD POSITION LOGIC, SUBSET BY PLAY TYPE####
  
  pos_metric_summary_long <- eventReactive(input$subset_button, {
    
    metric_col <- sym(input$pos_metric)
    
    summary_wide <- rv$data |>
      group_by(yardline_100,play_type)|>
      summarize(
        average_metric = mean(!!metric_col, na.rm = TRUE),
        .groups = "drop"
      )
    
    
    summary_wide
  })
  
  output$metric_by_position_table <- DT::renderDataTable({
    pos_metric_summary_long()
  })
  
  output$metric_by_position_plot<-renderPlot({
    summary_data <- pos_metric_summary_long()
    req(summary_data)
    
    ggplot(summary_data, aes(x=yardline_100, y=1, fill=average_metric))+
      geom_tile()+
      scale_fill_gradient2(
        low = "green",      # color for lowest values
        mid = "blue",       # color for middle
        high = "red",       # color for highest values
        midpoint = 0,        # center the gradient around 0 (adjust as needed)
        name = paste0("Average ", toupper(input$pos_metric))
      )+
      facet_wrap(~play_type, ncol = 1) +
      labs(
        title = paste0("Offensive ", input$pos_metric, " by Field Position and Play Type"),,
        x = "Yards from Opponent End Zone",
        y = "",
        fill = paste0("Average ", input$pos_metric)
      )
  })
  
  
  
}

shinyApp(ui, server)