library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(RColorBrewer)
library(DT)

annual_MaxIceCover <- read.csv("data/annual_MaxIceCover.csv")
annual_MaxIceCover$year <- as.numeric(annual_MaxIceCover$year)

lake_names <- c(
  "bas" = "Great Lakes",
  "mic" = "Lake Michigan",
  "eri" = "Lake Erie",
  "sup" = "Lake Superior",
  "hur" = "Lake Huron",
  "ont" = "Lake Ontario"
)

# Ensure all coverage columns are numeric
coverage_columns <- c("sup", "mic", "hur", "eri", "ont", "bas")
annual_MaxIceCover[coverage_columns] <- lapply(annual_MaxIceCover[coverage_columns], as.numeric)

# Define the UI
ui <- fluidPage(
  titlePanel(tags$strong("Historical Ice Cover")),
  p("NOAA/GLERL has been monitoring and documenting Great Lakes ice cover since the early 1970's using the ice products developed by the U.S. National Ice Center and the Canadian Ice Service. Research conducted on hydrometeorological processes and regional climate trends has led to models of lake thermal structure that play an integral role in ecosystem forecasting."),
  titlePanel(tags$strong("Annual Maximum Ice Coverage")),
  sidebarLayout(
    sidebarPanel(
      selectInput("lake", "Select Lake:", 
                  choices = c("Great Lakes" = "bas", 
                              "Lake Michigan" = "mic", 
                              "Lake Erie" = "eri", 
                              "Lake Superior" = "sup", 
                              "Lake Huron" = "hur", 
                              "Lake Ontario" = "ont"),
                  selected = "Great Lakes"),
    checkboxInput("show_avg", "Show Long-term Average Line", value = TRUE),
    sliderInput("year_range", "Select Year Range:",
                min = min(annual_MaxIceCover$year),
                max = max(annual_MaxIceCover$year),
                value = c(min(annual_MaxIceCover$year), max(annual_MaxIceCover$year)),
                sep = "")
    ),
    
    mainPanel(
      plotlyOutput("iceCover"),
      tags$hr(),
      titlePanel("Ice Coverage Data Table"),
      DTOutput("data_table")
    )
  )
)

# Define the Server
server <- function(input, output) {
  
  output$iceCover <- renderPlotly({
    # Filter data and remove NAs for the selected lake
    filtered_data <- annual_MaxIceCover %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
      filter(!is.na(year) & !is.na(.data[[input$lake]])) %>%
      arrange(year)  
    
    long_term_avg <- mean(filtered_data[[input$lake]], na.rm = TRUE)
    
    lake_full_name <- lake_names[input$lake]
    
      p <- ggplot(filtered_data,
                  aes(x = year, y = .data[[input$lake]], group = 1,
                      text = paste("Year:", year, "<br>Ice Coverage:", .data[[input$lake]]))) +
        geom_point(shape = 21, stroke = 0.5, fill = "white") +
        geom_line(size = 0.2) +
        labs(
          title = paste("Maximum Ice Cover - ", lake_full_name),
          x = "",
          y = "Percent Ice Cover"
        ) +
        scale_x_continuous(breaks = seq(min(filtered_data$year), max(filtered_data$year), by = 5)) + 
        scale_y_continuous(limits = c(10,100), breaks = seq(0,100, by = 20)) +
        theme_minimal()+
        theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
              plot.title = element_text(hjust = 0.5, face = "bold"))
      
      if (input$show_avg) {
        p <- p + geom_hline(yintercept = long_term_avg, linetype = "dashed", color = "red", linewidth = 0.5)
      }
      
      ggplotly(p, tooltip = "text") %>%
        layout(hovermode = "closest") %>%
        config(displayModeBar = FALSE)
    
  })
  
  output$data_table <- renderDT({
    annual_MaxIceCover %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
      select(year, lake = .data[[input$lake]]) %>%
      datatable()
  })
}

# Run Shiny app
shinyApp(ui = ui, server = server)

