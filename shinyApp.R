library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(DT)  # For interactive data tables

# Custom color palette "
starry_night_palette <- c("#071930", "#203D63", "#416C8E", "#6490B1", "#91B2CC", "#BBCEDA", "#C1A470", "#A3833A", "#584821")

# UI
ui <- fluidPage(
  titlePanel("Diabetes Dataset Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Dataset (CSV)"),
      checkboxGroupInput("variables", "Select Variables:", choices = NULL),
      actionButton("loadData", "Load Dataset"),
      selectInput("plotType", "Select Plot Type:", 
                  choices = c("Histogram", "Scatter Plot", "Box Plot", "Bar Plot"),
                  selected = "Histogram")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Graphs", 
                 plotOutput("selectedPlot")),
        tabPanel("Statistics", 
                 br(),
                 h4("Summary Statistics"),
                 DTOutput("statsTable"),
                 br(),
                 p("Note: This table displays summary statistics for the selected variables.")
        )
      ),
      style = "background-color: #f8f9fa;"
    )
  )
)

# Server
server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file)
    df <- read_csv(input$file$datapath)
    updateCheckboxGroupInput(session, "variables", choices = colnames(df))
    return(df)
  })
  
  observeEvent(input$loadData, {
    dataset()
  })
  
  output$selectedPlot <- renderPlot({
    req(input$variables)
    
    var <- input$variables
    
    plot_type <- input$plotType
    plot_function <- switch(
      plot_type,
      "Histogram" = ggplot(dataset(), aes(x = !!as.name(var))) +
        geom_histogram(binwidth = 10, fill = starry_night_palette[1], color = "black") +
        labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
        theme(panel.background = element_rect(fill = starry_night_palette[8])),
      "Scatter Plot" = ggplot(dataset(), aes(x = !!as.name(var), y = BloodPressure)) +
        geom_point() +
        labs(title = paste("Scatter Plot of", var, "vs. Blood Pressure"), x = var, y = "Blood Pressure") +
        theme(panel.background = element_rect(fill = starry_night_palette[8])),
      "Box Plot" = ggplot(dataset(), aes(x = Outcome, y = !!as.name(var), fill = Outcome)) +
        geom_boxplot() +
        labs(title = paste("Box Plot of", var, "by Outcome"), x = "Outcome", y = var) +
        scale_fill_manual(values = c(starry_night_palette[1], starry_night_palette[5])) +
        theme(panel.background = element_rect(fill = starry_night_palette[8])),
      "Bar Plot" = ggplot(dataset(), aes(x = !!as.name(var))) +
        geom_bar(fill = starry_night_palette[2], color = "black") +
        labs(title = paste("Bar Plot of", var), x = var, y = "Count") +
        theme(panel.background = element_rect(fill = starry_night_palette[8]))
    )
    
    print(plot_function)
  })
  
  # Summary statistics
  output$statsTable <- renderDT({
    if (!is.null(input$variables)) {
      summary_stats <- summary(dataset()[, input$variables, drop = FALSE])
      datatable(summary_stats, options = list(scrollX = TRUE), rownames = FALSE) %>%
        formatStyle(names(summary_stats), fontSize = "90%")
    }
  })
}

shinyApp(ui, server)
