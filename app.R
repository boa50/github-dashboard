library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(bigrquery)
library(commits.calendar)

ui <- fluidPage(
    titlePanel("For how long are you commiting"),
    
    fluidRow(
      plotOutput("calendar", height = "600px")
    )
)


server <- function(input, output) {
  con <- dbConnect(
    bigquery(),
    project = "github-dashboard-378513",
    dataset = "interactions",
    billing = "github-dashboard-378513"
  )
  
  df_commits <- tbl(con, "commits") %>% collect()

  output$calendar <- renderPlot({
    plot_calendar(df_commits)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
