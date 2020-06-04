library(shiny)

# Define UI for application that draws a histogram
ui = fluidPage(
  
  # Application title
  titlePanel("Age-structured population growth"),
  
  # Sidebar with a slider input for distance 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "F1",
                  label = "Fecundity at age 1",
                  min = 1,
                  max = 8,
                  value = 1),
      sliderInput(inputId = "F2",
                  label = "Fecundity at age 2",
                  min = 1,
                  max = 8,
                  value = 8),            
      sliderInput(inputId = "F3",
                  label = "Fecundity at age 3",
                  min = 1,
                  max = 8,
                  value = 5),
      sliderInput(inputId = "S12",
                  label = "Survival from ages 1 to 2 (%)",
                  min = 1,
                  max = 100,
                  value = 1),
      sliderInput(inputId = "S23",
                  label = "Survival from ages 2 to 3 (%)",
                  min = 1,
                  max = 100,
                  value = 100),
      width=4),
    
    # Show the plots
    mainPanel(
      tableOutput(outputId = "leslie"),
      #plotOutput(outputId = "grow"),
      width=8
      )
    )
  )


# Define server logic
server = function(input, output) {
  
  output$leslie <- renderTable({
    matrix(
      c("Age 1", input$F1     ,      input$F2, input$F3,
        "Age 2", input$S12/100,             0,        0,
        "Age 3",             0, input$S23/100,        0),
      ncol = 4, 
      byrow = T, 
      dimnames = list(NULL, c(" ","Age 1","Age 2", "Age 3"))
           )
    })
  
  }


# Run the application 
shinyApp(ui = ui, server = server)

# include diagram + plot


