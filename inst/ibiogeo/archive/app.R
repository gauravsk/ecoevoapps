library(shiny)

source("functions.R")

# Define UI for application that draws a histogram
ui = fluidPage(

    # Application title
    titlePanel("The Island Biogeography Theory"),

    # Sidebar with a slider input for distance 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "D1",
                        label = "Distance from mainland (Island A)",
                        min = 1,
                        max = 8,
                        value = 1),
            sliderInput(inputId = "D2",
                        label = "Distance from mainland (Island B)",
                        min = 1,
                        max = 8,
                        value = 4),            
            sliderInput(inputId = "A1",
                        label = "Area of island A",
                        min = .1,
                        max = 4,
                        value = 1),
            sliderInput(inputId = "A2",
                        label = "Area of island B",
                        min = .1,
                        max = 4,
                        value = .5),
            sliderInput(inputId = "M",
                        label = "Number of species in the mainland",
                        min = 1,
                        max = 100,
                        value = 100),
            #sliderInput(inputId = "k",
            #            label = "Scaling constant",
            #            min = .001,
            #            max = 1,
            #            value = 0.015),
            width=4),

        # Show the plots
        mainPanel(
            p("Based on the seminal work of MacArthur and  Wilson (1967)."),
            plotOutput(outputId = "map"),
            plotOutput(outputId = "rate"),
            width=8
            )
        )
    )


# Define server logic
server = function(input, output) {

    output$map <- renderPlot({
        # generate plots based on input$bins from ui.R
        mapNrates(D=c(input$D1,input$D2),A=c(input$A1,input$A2),M=input$M,k=0.015)
        })
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
